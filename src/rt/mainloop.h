#ifndef MAINLOOP_H
#define MAINLOOP_H

#include <stdbool.h>
#include <stdint.h>
#include <libaio.h>
#include "variable_queue.h"

extern int next_tid;

#define DECLARE_THREAD(thread_name) \
    struct thread_name *mk_ ## thread_name(void) { \
        struct thread_name *t = calloc(1, sizeof(struct thread_name)); \
        if (!t) fail(1, "allocating thread"); \
        t->thread.tid = next_tid++; \
        return t; \
    }

char *process_name;
void fail(int status, char *fmt, ...);
void fail2(int status, int err, char *fmt, ...);

Q_NEW_HEAD(event_queue_t, event_t);
Q_NEW_HEAD(thread_queue_t, thread_t);
Q_NEW_HEAD(buf_queue_t, buf_t);

typedef bool thread_cont(struct thread_t *thread);

typedef struct buf_t {
	Q_NEW_LINK(buf_t) q_link;
	char buffer[];
} buf_t;

typedef struct thread_t {
	//work_item_t work_item;
	Q_NEW_LINK(thread_t) q_link;
	int tid;
	event_queue_t pending_events;
	event_queue_t nb_events;
	buf_queue_t bufs;
	struct event_t *finished_event;
	thread_cont *cont;
} thread_t;

// XXX: track read/write event interest
typedef struct event_t {
	Q_NEW_LINK(event_t) q_link;
	Q_NEW_LINK(event_t) gc_link;
	int id;
	bool complete;
	thread_t *thread;
} event_t;
typedef event_t * event_handle;

char *new_buf(thread_t *t, int size);
event_t *mk_nb_event(thread_t *t, int fd, int mode);
int epoll_ctler(int op, int fd, uint32_t events, void *ptr);
void make_runnable(struct thread_t *t);
void register_event(thread_t *t, event_t *event);
void free_thread(thread_t *thread);

void setup_main_loop(void);
void main_loop(void);

#endif
