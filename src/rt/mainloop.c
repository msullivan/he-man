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

#define MAX_EVENTS 10
#define MAX_AIO_EVENTS 100 // dunno what this should be...
#define MAX_ITERS 50
#define MAX_THREADS 1

int thread_count = MAX_THREADS;

int next_tid = 0;

static struct {
	int epoll_fd;
#if ENABLE_AIO
	io_context_t aio_ctx;
	int aio_eventfd;
	struct event_t *aio_dummy_event;
#endif
#if MT_RUNTIME
	pthread_mutex_t sched_lock;
#endif
	thread_queue_t sched_queue;
} state;

static inline void sched_lock(void) {
#if MT_RUNTIME
	pthread_mutex_lock(&state.sched_lock);
#endif
}
static inline void sched_unlock(void) {
#if MT_RUNTIME
	pthread_mutex_unlock(&state.sched_lock);
#endif
}

// refcount stuff
void inc_refcount(void *p) {
	int *rc = p;
	assert(*rc > 0);
	(*rc)++; // XXX: smp
}
void dec_refcount(void *p) {
	int *rc = p;
	assert(*rc > 0);
	if (!--(*rc)) // XXX: smp
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
	e->thread = thread;
	// XXX: this only makes sense for epoll events, not AIO ones
	if (epoll_ctler(EPOLL_CTL_MOD, e->u.nb.fd,
	                e->u.nb.mode|EPOLLONESHOT, e) < 0)
		fail(1, "epoll_ctl: %d", e->u.nb.fd);
}

void register_channel_event(thread_t *thread, event_t *e)
{
	// XXX: locking
	channel_t *ch = e->u.ch;

	// if there is data in the channel, keep running
	if (Q_GET_HEAD(&ch->msgs)) {
		make_runnable(thread);
	} else {
		e->thread = thread;
	}
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
	}
}


int epoll_ctler(int op, int fd, uint32_t events, void *ptr)
{
	struct epoll_event ev;
	ev.events = events;
	ev.data.ptr = ptr;
	return epoll_ctl(state.epoll_fd, op, fd, &ev);
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
	// XXX: locking
	msg_t *msg = calloc(1, sizeof(msg_t));
	if (!msg) fail(1, "allocating msg");
	msg->data.tag = tag;
	msg->data.payload = payload;
	Q_INSERT_TAIL(&ch->msgs, msg, q_link);
	handle_event(&ch->ev);
}
msg_data_t channel_recv(channel_t *ch)
{
	msg_data_t ret = {-1, {0}};
	msg_t *msg;
	// XXX: locking
	if ((msg = Q_GET_HEAD(&ch->msgs))) {
		Q_REMOVE(&ch->msgs, msg, q_link);
		ret = msg->data;
		free(msg);
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
static void do_poll(bool can_sleep)
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
	int epoll_timeout = expect_aio || !can_sleep ? 0 : -1;
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
void *work_loop(void *p)
{
	for (;;) {
		thread_t *thread;
		sched_lock();
		if ((thread = Q_GET_HEAD(&state.sched_queue))) {
			Q_REMOVE(&state.sched_queue, thread, q_link);
			sched_unlock();
			run_thread(thread);
			sched_lock();
		}
		bool can_sleep = !Q_GET_HEAD(&state.sched_queue);
		sched_unlock();
		do_poll(can_sleep);
	}
	return NULL;
}

void main_loop(void)
{
#if MT_RUNTIME
	pthread_t thread;
	for (int i = 0; i < thread_count - 1; i++) {
		int ret = pthread_create(&thread, NULL, work_loop, NULL);
		if (ret != 0) fail(1, "pthread_create");
	}
#endif

	work_loop(NULL);
}


void setup_main_loop(void)
{
	int ret;

	signal(SIGPIPE, SIG_IGN);

	state.epoll_fd = epoll_create(1);
	if (state.epoll_fd < 0) fail(1, "epoll_create");

#if ENABLE_AIO
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
