#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/epoll.h>
#include <time.h>
#include <stdarg.h>
#include <stdbool.h>
#include <signal.h>

#if ENABLE_AIO
#include <libaio.h>
#include <sys/eventfd.h>
#endif

#include <pthread.h>

#include "variable_queue.h"
#include "mainloop.h"

#define MAX_EVENTS 64
#define MAX_AIO_EVENTS 100 // dunno what this should be...
#define MAX_ITERS 50
#define MAX_THREADS 8

int thread_count = MAX_THREADS;

int next_tid = 0;

static struct {
	int epoll_fd;
#if ENABLE_AIO
	io_context_t aio_ctx;
	int aio_eventfd;
	struct event_t *aio_dummy_event;
#endif
	rt_mutex_t sched_lock;
	thread_queue_t sched_queue;
	// Sorted queue of sleeping threads; the "right thing" would be a
	// priority queue or something
	thread_queue_t sleep_queue;
	int events_until_epoll;
} state;

static inline void sched_lock(void) {
	rt_mutex_lock(&state.sched_lock);
}
static inline void sched_unlock(void) {
	rt_mutex_unlock(&state.sched_lock);
}

// refcount stuff
void inc_refcount(void *p) {
	int *rc = p;
	assert(*rc > 0);
	rt_atomic_fetch_add(rc, 1);
}
void dec_refcount(void *p) {
	int *rc = p;
	assert(*rc > 0);
	if (!rt_atomic_fetch_add(rc, -1))
		free(p);
}

static buf_t *get_buffer(char *buf) {
	return (buf_t *)(buf - offsetof(buf_t, buffer));
}
void inc_buf_refcount(char *buf) { inc_refcount(get_buffer(buf)); }
void dec_buf_refcount(char *buf) { dec_refcount(get_buffer(buf)); }

// XXX: we want something faster than this
// and maybe want to recover from failure
char *new_buf(thread_t *thread, int size) {
	buf_t *buf = malloc(sizeof(buf_t) + size);
	if (!buf) fail(1, "allocating buffer");
	Q_INIT_ELEM(buf, gc_link);
	Q_INSERT_TAIL(&thread->bufs, buf, gc_link);
	return buf->buffer;
}
char *new_rc_buf(thread_t *thread, int size) {
	buf_t *buf = malloc(sizeof(buf_t) + size);
	if (!buf) fail(1, "allocating buffer");
	buf->rc = 1;
	return buf->buffer;
}

channel_t *new_channel(void) {
	channel_t *ch = calloc(1, sizeof(channel_t));
	if (!ch) fail(1, "allocating channel");
	ch->rc = 1;
	rt_mutex_init(&ch->lock);
	ch->ev.type = EVENT_CHANNEL;
	ch->ev.u.ch = ch;
	return ch;
}


// XXX: we want something faster than this
// and maybe want to recover from failure
event_t *mk_event(void) {
	event_t *e = calloc(1, sizeof(event_t));
	if (!e) fail(1, "allocating event");
	return e;
}

event_t *mk_nb_event(thread_t *thread, int fd, int mode)
{
	event_t *e = mk_event();
	e->type = EVENT_NB;
	e->u.nb.fd = fd;
	e->u.nb.mode = mode;

	if (epoll_ctler(EPOLL_CTL_ADD, fd, 0, e) < 0)
		fail(1, "epoll_ctl: %d", fd);

	Q_INSERT_TAIL(&thread->nb_events, e, gc_link);

	return e;
}

static void free_nb_event(event_t *event)
{
	epoll_ctler(EPOLL_CTL_DEL, event->u.nb.fd, 0, NULL);
	close(event->u.nb.fd);
	free(event);
}


//// Time garbage
// sigh. the C++ standard library has really nice time facilities.

// timespecs technically represent /points/ in time, not durations,
// so strictly speaking all of this time_norm/time_diff stuff is an abuse.
// N.B time_norm will normalize all "negative" timespecs to have a
// negative tv_sec
struct timespec time_norm(struct timespec t) {
	if (t.tv_nsec > 1000000000 || t.tv_nsec < 0) {
		int sign = t.tv_nsec < 0 ? 1 : -1;
		t.tv_nsec += sign * 1000000000;
		t.tv_sec -= sign;
	}
	return t;
}

struct timespec time_diff(struct timespec t1, struct timespec t0)
{
	struct timespec temp = {.tv_sec = t1.tv_sec-t0.tv_sec,
	                        .tv_nsec = t1.tv_nsec-t0.tv_nsec};
	return time_norm(temp);
}
int time_lt(struct timespec t0, struct timespec t1)
{
	return time_diff(t0, t1).tv_sec < 0;
}

// Compute the timeout in millis
int compute_timeout(struct timespec wakeup)
{
	// don't sleep on null timeout
	if (!wakeup.tv_sec && !wakeup.tv_nsec) return -1;

	// compute actual timeout
	struct timespec time;
	clock_gettime(CLOCK_REALTIME, &time);
	struct timespec diff = time_diff(wakeup, time);
	if (diff.tv_sec < 0) return 0;

	int millis = diff.tv_sec*1000 + diff.tv_nsec/1000000;
	if (!millis && diff.tv_nsec) millis++; // round up to 1ms
	return millis;
}


event_t *setup_sleep_event_abs(thread_t *t, struct timespec timeout)
{
	event_t *ev = &t->sleep_event;
	ev->type = EVENT_SLEEP;
	ev->u.wakeup = timeout;
	return ev;
}

event_t *setup_sleep_event(thread_t *t, int timeout_millis)
{
	struct timespec timeout;
	clock_gettime(CLOCK_REALTIME, &timeout);
	timeout.tv_nsec += 1000000*timeout_millis;
	return setup_sleep_event_abs(t, time_norm(timeout));
}

static void free_buf(buf_t *buf)
{
	free(buf);
}

void free_thread(thread_t *thread)
{
	event_t *e;
	while ((e = Q_GET_HEAD(&thread->nb_events))) {
		Q_REMOVE(&thread->nb_events, e, gc_link);
		free_nb_event(e);
	}
	buf_t *buf;
	while ((buf = Q_GET_HEAD(&thread->bufs))) {
		Q_REMOVE(&thread->bufs, buf, gc_link);
		free_buf(buf);
	}

	free(thread);
}


void register_nb_event(thread_t *thread, event_t *e)
{
	// helgrind will report a data race having to do with this, but
	// I'm pretty sure it's actually fine. The event pointer is handed
	// off through epoll.

	// XXX: Actually, there is maybe a concurrency hazard since another
	// thread could pick up the event in epoll immediately and try
	// to run this thread before it has returned fully. This should
	// be fine, though, since nothing else will be happening to this
	// thread.
	e->thread = thread;
	// XXX: this only makes sense for epoll events, not AIO ones
	if (epoll_ctler(EPOLL_CTL_MOD, e->u.nb.fd,
	                e->u.nb.mode|EPOLLONESHOT, e) < 0)
		fail(1, "epoll_ctl: %d", e->u.nb.fd);
}

void register_channel_event(thread_t *thread, event_t *e)
{
	channel_t *ch = e->u.ch;

	// if there is data in the channel, keep running
	rt_mutex_lock(&ch->lock);
	if (Q_GET_HEAD(&ch->msgs)) {
		make_runnable(thread);
	} else {
		e->thread = thread;
	}
	rt_mutex_unlock(&ch->lock);
}

void register_sleep_event(thread_t *me, event_t *e)
{
	sched_lock();
	thread_t *t;
	Q_SEARCH(t, &state.sleep_queue, q_link,
	         time_lt(e->u.wakeup, t->sleep_event.u.wakeup));
	if (t) {
		Q_INSERT_BEFORE(&state.sleep_queue, t, me, q_link);
	} else {
		Q_INSERT_TAIL(&state.sleep_queue, me, q_link);
	}
	sched_unlock();
}

void register_event(thread_t *thread, event_t *e)
{
	switch (e->type) {
	case EVENT_NB:
		register_nb_event(thread, e);
		break;
	case EVENT_CHANNEL:
		register_channel_event(thread, e);
		break;
	case EVENT_SLEEP:
		register_sleep_event(thread, e);
		break;
	}
}

int epoll_ctler(int op, int fd, uint32_t events, void *ptr)
{
	struct epoll_event ev;
	ev.events = events;
	ev.data.ptr = ptr;
	return epoll_ctl(state.epoll_fd, op, fd, &ev);
}


// need to have the sched lock
struct timespec get_next_wakeup(void) {
	struct timespec empty = {0, 0};
	if (Q_GET_HEAD(&state.sleep_queue)) {
		return Q_GET_HEAD(&state.sleep_queue)->sleep_event.u.wakeup;
	}
	return empty;
}
// need to have the sched lock
void wakeup_sleepers(void) {
	struct timespec time;
	clock_gettime(CLOCK_REALTIME, &time);

	thread_t *t;
	while ((t = Q_GET_FRONT(&state.sleep_queue)) &&
	       time_lt(t->sleep_event.u.wakeup, time)) {
		Q_REMOVE(&state.sleep_queue, t, q_link);
		Q_INSERT_TAIL(&state.sched_queue, t, q_link);
	}
}


void make_runnable(thread_t *t)
{
	sched_lock();
	Q_INSERT_TAIL(&state.sched_queue, t, q_link);
	sched_unlock();
}


static void handle_event(event_t *event)
{
	thread_t *t = event->thread;
	// If nobody gives a shit, just leave
	if (!t) return;
	event->thread = NULL;

	// Schedule the thread we woke up
	make_runnable(t);
}

void channel_send(channel_t *ch, int tag, data_t payload)
{
	msg_t *msg = calloc(1, sizeof(msg_t));
	if (!msg) fail(1, "allocating msg");
	msg->data.tag = tag;
	msg->data.payload = payload;

	rt_mutex_lock(&ch->lock);
	Q_INSERT_TAIL(&ch->msgs, msg, q_link);
	handle_event(&ch->ev);
	rt_mutex_unlock(&ch->lock);
}
msg_data_t channel_recv(channel_t *ch)
{
	msg_data_t ret = {-1, {0}};
	msg_t *msg;

	rt_mutex_lock(&ch->lock);
	if ((msg = Q_GET_HEAD(&ch->msgs))) {
		Q_REMOVE(&ch->msgs, msg, q_link);
		ret = msg->data;

		rt_mutex_unlock(&ch->lock);
		free(msg);
	} else {
		rt_mutex_unlock(&ch->lock);
	}


	return ret;
}

#if ENABLE_AIO
static void handle_aio_event(struct io_event *event)
{
	abort();
	/*
	event_t *ev = event->data;
	ev->complete = true;
	handle_event(ev);
	*/
}
#endif
static void handle_epoll_event(struct epoll_event *event)
{
	event_t *ev = event->data.ptr;
	handle_event(ev);
}

// Poll epoll and aio and handle any events. If can_sleep is true
// and no aio events are expected, epoll will block.
static void do_poll(bool can_sleep, struct timespec next_wakeup)
{
	struct epoll_event epoll_events[MAX_EVENTS];
#if ENABLE_AIO
	struct io_event aio_events[MAX_EVENTS];

	// We need to be careful to not let either aio or epoll events
	// starve the other. We do this by always doing nonblocking polls
	// of aio and only having epoll block if we aren't expecting aio
	// events. When both types of events are coming in, we switch
	// between processing the two types.
	// Poll for aio events. Don't block.
	int aio_cnt = io_getevents(state.aio_ctx, 0, MAX_EVENTS,
	                           aio_events, NULL);
	if (aio_cnt < 0) { fail2(1, -aio_cnt, "io_getevents"); }
	bool expect_aio = aio_cnt == MAX_EVENTS;
	for (int i = 0; i < aio_cnt; i++) {
		handle_aio_event(&aio_events[i]);
	}
#else
#define expect_aio 0
#endif

	// If we aren't expecting aio, block indefinitely, otherwise
	// just poll.
	int epoll_timeout = expect_aio || !can_sleep ? 0 :
		compute_timeout(next_wakeup);
	int epoll_cnt = epoll_wait(state.epoll_fd, epoll_events,
	                           MAX_EVENTS, epoll_timeout);
	for (int i = 0; i < epoll_cnt; i++) {
#if ENABLE_AIO
		if (epoll_events[i].data.ptr == state.aio_dummy_event) {
			uint64_t eventfd_val;
			if (read(state.aio_eventfd, &eventfd_val,
			         sizeof(eventfd_val)) < 0)
				fail(1, "eventfd read");
		} else
#endif
		{
			handle_epoll_event(&epoll_events[i]);
		}
	}
}

static void run_thread(void *threadp)
{
	thread_t *thread = threadp;
	//printf("running %d\n", thread->tid);
	bool runnable = true;
	// Run the thread until it has run for a while or has stopped being
	// runnable.
	for (int i = 0; runnable && i < MAX_ITERS; i++) {
		runnable = thread->cont(thread);
	}
	if (runnable) make_runnable(thread);
}

// A single threaded main loop
void work_loop(void)
{
	for (;;) {
		thread_t *thread;
		if ((thread = Q_GET_HEAD(&state.sched_queue))) {
			Q_REMOVE(&state.sched_queue, thread, q_link);
			run_thread(thread);
			state.events_until_epoll--;
		}
		bool can_sleep = !Q_GET_HEAD(&state.sched_queue);
		if (can_sleep || state.events_until_epoll == 0) {
			struct timespec next_wakeup = get_next_wakeup();
			do_poll(can_sleep, next_wakeup);
			state.events_until_epoll = Q_GET_SIZE(&state.sched_queue);
			wakeup_sleepers(); // maybe not every time through?
		}
	}
}

// A multithreaded main loop
void *work_loop_mt(void *p)
{
	for (;;) {
		thread_t *thread;
		sched_lock();
		wakeup_sleepers(); // maybe not every time through?
		if ((thread = Q_GET_HEAD(&state.sched_queue))) {
			Q_REMOVE(&state.sched_queue, thread, q_link);
			sched_unlock();
			run_thread(thread);
			sched_lock();
		}
		bool can_sleep = !Q_GET_HEAD(&state.sched_queue);
		struct timespec next_wakeup = get_next_wakeup();
		sched_unlock();
		do_poll(can_sleep, next_wakeup);
	}
	return NULL;
}


void main_loop(void)
{
	state.events_until_epoll = Q_GET_SIZE(&state.sched_queue);

#if MT_RUNTIME
	pthread_t thread;
	for (int i = 0; i < thread_count - 1; i++) {
		int ret = pthread_create(&thread, NULL, work_loop_mt, NULL);
		if (ret != 0) fail(1, "pthread_create");
	}
	work_loop_mt(NULL);
#else
	work_loop();
#endif
}


void setup_main_loop(void)
{
	signal(SIGPIPE, SIG_IGN);

	state.epoll_fd = epoll_create(1);
	if (state.epoll_fd < 0) fail(1, "epoll_create");

	rt_mutex_init(&state.sched_lock);

#if ENABLE_AIO
	int ret;

	static event_t aio_dummy_event;
	state.aio_dummy_event = &aio_dummy_event;

	ret = io_setup(MAX_AIO_EVENTS, &state.aio_ctx);
	if (ret < 0) fail2(1, -ret, "io_setup");

	state.aio_eventfd = eventfd(0, EFD_NONBLOCK);
	if (state.aio_eventfd < 0) fail(1, "eventfd");

	ret = epoll_ctler(EPOLL_CTL_ADD, state.aio_eventfd,
	                  EPOLLIN, state.aio_dummy_event);
	if (ret < 0) fail(1, "epoll_ctl eventfd");
#endif
}

/* fail:  prints out the process name, and an error message,
   and the error associated with the current errno, then exits
   with exit status status*/
void fail(int status, char *fmt, ...)
{
	va_list ap;
	if (process_name)
		fprintf(stderr, "%s: ", process_name);

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	fprintf(stderr, ": %s\n", strerror(errno));

	exit(status);
}
void fail2(int status, int err, char *fmt, ...)
{
	va_list ap;
	if (process_name)
		fprintf(stderr, "%s: ", process_name);

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	fprintf(stderr, ": %s\n", strerror(err));

	exit(status);
}
