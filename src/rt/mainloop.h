#ifndef MAINLOOP_H
#define MAINLOOP_H

#include <libaio.h>
#include "variable_queue.h"

Q_NEW_HEAD(event_queue_t, event_t);
Q_NEW_HEAD(thread_queue_t, thread_t);

typedef bool thread_cont(struct thread_t *thread);

int epoll_ctler(int epfd, int op, int fd, uint32_t events, void *ptr);
void make_runnable(struct thread_t *t);

typedef struct thread_t {
	//work_item_t work_item;
	Q_NEW_LINK(thread_t) q_link;
	int tid;
	event_queue_t pending_events;
	struct event_t *finished_event;
	thread_cont *cont;
} thread_t;

// XXX: track read/write event interest
typedef struct event_t {
	Q_NEW_LINK(event_t) q_link;
	int id;
	bool complete;
	thread_t *thread;
} event_t;

#endif
