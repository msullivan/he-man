#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include "variable_queue.h"
#include "thrpool.h"

static void thread_pool_spawn_new(thread_pool_t *pool, int id);
static void *thread_pool_bottom(void *data);

int thread_pool_init(thread_pool_t **pool, work_func *func, int max_threads)
{
	int i, num;
	
	/* XXX: things could fail... */
	thread_pool_t *p = malloc(sizeof(thread_pool_t));
	
	p->func = func;
	p->num_threads = 0;
	p->max_threads = max_threads;
	Q_INIT_HEAD(&p->idle_threads);
	Q_INIT_HEAD(&p->working_threads);
	Q_INIT_HEAD(&p->work_queue);
	pthread_mutex_init(&p->pool_lock, NULL);
	pthread_cond_init(&p->work_ready, NULL); /*XXX*/

	/* spawn up our threads - XXX: this is special. */
	num = p->max_threads;
	for (i = 0; i < num; i++)
		thread_pool_spawn_new(p, i+1);

	*pool = p;	
	return 0;
}

static void thread_pool_spawn_new(thread_pool_t *pool, int id)
{
	worker_thread_t *thr = malloc(sizeof(worker_thread_t));
	Q_INIT_ELEM(thr, q_link);
	thr->id = id;
	thr->pool = pool;
	pthread_create(&thr->thread, NULL, thread_pool_bottom, thr);
}

static void *thread_pool_bottom(void *data)
{
	worker_thread_t *me = data;
	thread_pool_t *pool = me->pool;
	work_item_t *work;

	pthread_mutex_lock(&pool->pool_lock);
	pool->num_threads++;
	Q_INSERT_FRONT(&pool->idle_threads, me, q_link);

	/* and, loop for jobs */
	for (;;) {
		while ((work = Q_GET_FRONT(&pool->work_queue)) == NULL)
			pthread_cond_wait(&pool->work_ready, &pool->pool_lock);

		/* grab work */
		Q_REMOVE(&pool->work_queue, work, q_link);
		/* update the thread lists */
		Q_REMOVE(&pool->idle_threads, me, q_link);
		Q_INSERT_FRONT(&pool->working_threads, me, q_link);
		me->current_work = work;
		
		/* unlock the pool, then work */
		pthread_mutex_unlock(&pool->pool_lock);

		pool->func(me->current_work);

		pthread_mutex_lock(&pool->pool_lock);

		/* update the thread lists */
		Q_REMOVE(&pool->working_threads, me, q_link);
		Q_INSERT_FRONT(&pool->idle_threads, me, q_link);
		me->current_work = NULL;
	}

	abort();
	return NULL;
}

int thread_pool_push(thread_pool_t *pool, void *data)
{
	work_item_t *item = data;
	Q_INIT_ELEM(item, q_link);

	pthread_mutex_lock(&pool->pool_lock);
	
	Q_INSERT_TAIL(&pool->work_queue, item, q_link);
	pthread_cond_signal(&pool->work_ready);

	pthread_mutex_unlock(&pool->pool_lock);
	
	return 0;
}

void thread_pool_debug(thread_pool_t *pool, print_func *print, FILE *out)
{
	worker_thread_t *thr;
	work_item_t *work;
	
	pthread_mutex_lock(&pool->pool_lock);

	fprintf(out, "==========================\n");

	fprintf(out, "idle threads: ");
	Q_FOREACH(thr, &pool->idle_threads, q_link) {
		fprintf(out, "%d", thr->id);
		if (Q_GET_NEXT(thr, q_link)) fprintf(out, ", ");
	}

	fprintf(out, "\nworking threads: ");
	Q_FOREACH(thr, &pool->working_threads, q_link) {
		fprintf(out, "%d: ", thr->id);
		print(thr->current_work, out);
		if (Q_GET_NEXT(thr, q_link)) fprintf(out, ", ");
	}

	fprintf(out, "\ndata in queue: ");
	Q_FOREACH(work, &pool->work_queue, q_link) {
		print(work, out);
		if (Q_GET_NEXT(work, q_link)) fprintf(out, ", ");
	}


	fprintf(out, "\n==========================\n");

	pthread_mutex_unlock(&pool->pool_lock);
}
