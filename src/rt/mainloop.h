#ifndef MAINLOOP_H
#define MAINLOOP_H

#include <stdbool.h>
#include <stdint.h>
#include "variable_queue.h"

#if MT_RUNTIME
#include <pthread.h>
typedef pthread_mutex_t rt_mutex_t;
#define rt_mutex_init(p) pthread_mutex_init(p, NULL)
#define rt_mutex_lock(p) pthread_mutex_lock(p)
#define rt_mutex_unlock(p) pthread_mutex_unlock(p)
static inline int rt_atomic_fetch_add(int *p, int amt) {
	return __sync_fetch_and_add(p, amt);
}
#else
typedef struct { } rt_mutex_t;
static inline int __dummy_func() { return 0; }
#define rt_mutex_init(p) __dummy_func()
#define rt_mutex_lock(p) __dummy_func()
#define rt_mutex_unlock(p) __dummy_func()
static inline int rt_atomic_fetch_add(int *p, int amt) {
	int val = *p;
	*p += amt;
	return val;
}
#endif

typedef enum event_type_t { EVENT_NB, EVENT_CHANNEL, /* EVENT_AIO */ }
	event_type_t;

extern int next_tid;

#define DECLARE_THREAD(thread_name) \
    struct thread_name *mk_ ## thread_name(void) { \
        struct thread_name *t = calloc(1, sizeof(struct thread_name)); \
        if (!t) fail(1, "allocating thread"); \
        t->thread.tid = rt_atomic_fetch_add(&next_tid, 1); \
        return t; \
    }

char *process_name;
void fail(int status, char *fmt, ...);
void fail2(int status, int err, char *fmt, ...);

Q_NEW_HEAD(event_queue_t, event_t);
Q_NEW_HEAD(thread_queue_t, thread_t);
Q_NEW_HEAD(buf_queue_t, buf_t);
Q_NEW_HEAD(msg_queue_t, msg_t);

typedef bool thread_cont(struct thread_t *thread);

typedef struct event_t {
	Q_NEW_LINK(event_t) gc_link;
	event_type_t type;
	union {
		struct {int fd; int mode;} nb;
		struct channel_t *ch;
	} u;
	struct thread_t *thread;
} event_t;
typedef event_t * event_handle;

typedef union data_t {
	int64_t i;
	double f;
	void *p;
} data_t;

typedef struct msg_data_t {
	int tag;
	data_t payload;
} msg_data_t;
typedef struct msg_t {
	Q_NEW_LINK(msg_t) q_link;
	msg_data_t data;
} msg_t;

typedef struct channel_t {
	int rc;
	rt_mutex_t lock;
	msg_queue_t msgs;
	struct event_t *pending_event;
	event_t ev;
} channel_t;


typedef struct buf_t {
	int rc;
	Q_NEW_LINK(buf_t) gc_link;
	char buffer[];
} buf_t;

typedef struct thread_t {
	Q_NEW_LINK(thread_t) q_link;
	int tid;
	event_queue_t nb_events;
	buf_queue_t bufs;
	thread_cont *cont;
} thread_t;


void inc_refcount(void *p);
void dec_refcount(void *p);

char *new_buf(thread_t *t, int size);
char *new_rc_buf(thread_t *t, int size);
void inc_buf_refcount(char *buf);
void dec_buf_refcount(char *buf);

channel_t *new_channel(void);
void channel_send(channel_t *ch, int tag, data_t payload);
msg_data_t channel_recv(channel_t *ch);

event_t *mk_nb_event(thread_t *t, int fd, int mode);
int epoll_ctler(int op, int fd, uint32_t events, void *ptr);
void make_runnable(struct thread_t *t);
void register_event(thread_t *t, event_t *event);
void free_thread(thread_t *thread);

void setup_main_loop(void);
void main_loop(void);

#endif
