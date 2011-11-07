#ifndef MAINLOOP_H
#define MAINLOOP_H

#include <libaio.h>
#include "variable_queue.h"

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

#endif
