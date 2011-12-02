#include <unistd.h>
#include <stdio.h>
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

#include <libaio.h>
#include <sys/eventfd.h>

#include "variable_queue.h"
#include "mainloop.h"
//#include "thrpool.h"

#define MAX_EVENTS 10
#define MAX_AIO_EVENTS 100 // dunno what this should be...
#define MAX_ITERS 50

static struct {
	bool expect_aio;
	int epoll_fd;
	io_context_t aio_ctx;
	int aio_eventfd;
	struct event_t *aio_dummy_event;
	//thread_pool_t sched_queue;
	thread_queue_t sched_queue;
} state;

// XXX: we want something faster than this
// and maybe want to recover from failure
event_t *mk_event(void) {
	event_t *e = calloc(1, sizeof(event_t));
	if (!e) fail(1, "allocating event");
	return e;
}

event_t *mk_nb_event(int fd, int mode)
{
	event_t *e = mk_event();
	e->id = fd;

	if (epoll_ctler(EPOLL_CTL_ADD, fd, mode|EPOLLET, e) < 0)
		fail(1, "epoll_ctl");

	return e;
}

void register_event(thread_t *thread, event_t *event)
{
	Q_INSERT_TAIL(&thread->pending_events, event, q_link);
	event->thread = thread;
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
	Q_INSERT_TAIL(&state.sched_queue, t, q_link);
	//thread_pool_push(&state.sched_queue, t);
}


static void handle_event(event_t *event)
{
	thread_t *t = event->thread;
	// If nobody gives a shit, just leave
	if (!t) return;

	// Indicate what event finished
	assert(!t->finished_event);
	t->finished_event = event;

	// Clear out other events we are waiting on
	event_t *ev;
	Q_ASSERT_CONSISTENT(&t->pending_events, event, q_link);
	Q_FOREACH(ev, &t->pending_events, q_link) {
		ev->thread = NULL;
	}
	Q_INIT_HEAD(&t->pending_events);

	// Schedule the thread we woke up
	make_runnable(t);
}

static void handle_aio_event(struct io_event *event)
{
	event_t *ev = event->data;
	ev->complete = true;
	handle_event(ev);
}
static void handle_epoll_event(struct epoll_event *event)
{
	event_t *ev = event->data.ptr;
	handle_event(ev);
}

// Poll epoll and aio and handle any events. If can_sleep is true
// and no aio events are expected, epoll will block.
static void do_poll(bool can_sleep)
{
	struct io_event aio_events[MAX_EVENTS];
	struct epoll_event epoll_events[MAX_EVENTS];

	// We need to be careful to not let either aio or epoll events
	// starve the other. We do this by always doing nonblocking polls
	// of aio and only having epoll block if we aren't expecting aio
	// events. When both types of events are coming in, we switch
	// between processing the two types.
	// Poll for aio events. Don't block.
	int aio_cnt = io_getevents(state.aio_ctx, 0, MAX_EVENTS,
	                           aio_events, NULL);
	if (aio_cnt < 0) { fail2(1, -aio_cnt, "io_getevents"); }
	state.expect_aio = aio_cnt == MAX_EVENTS;
	for (int i = 0; i < aio_cnt; i++) {
		handle_aio_event(&aio_events[i]);
	}

	// If we aren't expecting aio, block indefinitely, otherwise
	// just poll.
	int epoll_timeout =	state.expect_aio || !can_sleep ? 0 : -1;
	int epoll_cnt = epoll_wait(state.epoll_fd, epoll_events,
	                           MAX_EVENTS, epoll_timeout);
	for (int i = 0; i < epoll_cnt; i++) {
		if (epoll_events[i].data.ptr == state.aio_dummy_event) {
			uint64_t eventfd_val;
			state.expect_aio = true;
			if (read(state.aio_eventfd, &eventfd_val,
			         sizeof(eventfd_val)) < 0)
				fail(1, "eventfd read");
		} else {
			handle_epoll_event(&epoll_events[i]);
		}
	}
}

static void run_thread(thread_t *thread)
{
	printf("running %d\n", thread->tid);
	bool runnable = true;
	// Run the thread until it has run for a while or has stopped being
	// runnable.
	for (int i = 0; runnable && i < MAX_ITERS; i++) {
		runnable = thread->cont(thread);
	}
	if (runnable) make_runnable(thread);
}

// A single threaded main loop
void main_loop(void)
{
	for (;;) {
		thread_t *thread;
		if ((thread = Q_GET_HEAD(&state.sched_queue))) {
			Q_REMOVE(&state.sched_queue, thread, q_link);
			run_thread(thread);
		}
		do_poll(!Q_GET_HEAD(&state.sched_queue));
	}
}

void setup_main_loop(void)
{
	int ret;
	static event_t aio_dummy_event;
	state.aio_dummy_event = &aio_dummy_event;

	state.epoll_fd = epoll_create(1);
	if (state.epoll_fd < 0) fail(1, "epoll_create");

	ret = io_setup(MAX_AIO_EVENTS, &state.aio_ctx);
	if (ret < 0) fail2(1, -ret, "io_setup");

	state.aio_eventfd = eventfd(0, EFD_NONBLOCK);
	if (state.aio_eventfd < 0) fail(1, "eventfd");

	ret = epoll_ctler(EPOLL_CTL_ADD, state.aio_eventfd,
	                  EPOLLIN, state.aio_dummy_event);
	if (ret < 0) fail(1, "epoll_ctl eventfd");
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
