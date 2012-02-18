#ifndef THRPOOL_H
#define THRPOOL_H

#include <pthread.h>
#include <stdio.h>
#include "variable_queue.h"

/* The data being handles *must* begin with a work_item! */

typedef void (work_func)(void *);
typedef void (print_func)(void *, FILE *);

typedef struct work_item_t {
	Q_NEW_LINK(work_item_t) q_link;
} work_item_t;

typedef struct worker_thread_t {
	pthread_t thread;
	int id;
	void *current_work;
	struct thread_pool_t *pool;
	Q_NEW_LINK(worker_thread_t) q_link;
} worker_thread_t;

Q_NEW_HEAD(work_queue_t, work_item_t);
Q_NEW_HEAD(worker_thread_list_t, worker_thread_t);

typedef struct thread_pool_t {
	work_func *func;
	int num_threads;
	int max_threads;
	worker_thread_list_t working_threads;
	worker_thread_list_t idle_threads;
	work_queue_t work_queue;
	pthread_mutex_t pool_lock;
	pthread_cond_t work_ready;
} thread_pool_t;

int thread_pool_init(thread_pool_t *pool, work_func *func, int max_threads);
int thread_pool_push(thread_pool_t *pool, void *data);
void thread_pool_debug(thread_pool_t *pool, print_func *print, FILE *out);

#endif
