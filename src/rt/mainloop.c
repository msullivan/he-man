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

#include "variable_queue.h"
//#include "thrpool.h"

static char *process_name;
void fail(int status, char *fmt, ...);

#define MAX_EVENTS 10

Q_NEW_HEAD(event_queue_t, event_t);
Q_NEW_HEAD(thread_queue_t, thread_t);


static struct {
	bool expect_aio;
	int epoll_fd;
	io_context_t aio_ctx;
	int aio_eventfd;
	struct event_t *aio_dummy_event;
	//thread_pool_t sched_queue;
	thread_queue_t sched_queue;
} state;


typedef struct thread_t {
	//work_item_t work_item;
	Q_NEW_LINK(thread_t) q_link;
	int tid;
	event_queue_t pending_events;
	struct event_t *finished_event;
} thread_t;

typedef struct event_t {
	Q_NEW_LINK(event_t) q_link;
	int id;
	bool complete;
	thread_t *thread;
} event_t;

static void handle_event(event_t *event) {
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
	Q_INSERT_TAIL(&state.sched_queue, t, q_link);
	//thread_pool_push(&state.sched_queue, t);
}

static void handle_aio_event(struct io_event *event) {
	event_t *ev = event->data;
	ev->complete = true;
	handle_event(ev);
}
static void handle_epoll_event(struct epoll_event *event) {
	event_t *ev = event->data.ptr;
	handle_event(ev);
}

// Poll epoll and aio and handle any events. If can_sleep is true
// and no aio events are expected, epoll will block.
void do_poll(bool can_sleep)
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
	if (aio_cnt < 0) fail(1, "io_getevents: %s", strerror(-aio_cnt));
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
				fail(1, "eventfd read: %s", strerror(errno));
		} else {
			handle_epoll_event(&epoll_events[i]);
		}
	}
}

void run_thread(thread_t *thread)
{
	// XXX: run thread
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

int main(int argc, char *argv[])
{
	process_name = argv[0];

	return 0;
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
