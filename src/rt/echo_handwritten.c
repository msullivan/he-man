/* Borrowed some code from
 * http://www.paulgriffiths.net/program/c/echoserv.php and
 * https://banu.com/blog/2/how-to-use-epoll-a-complete-example-in-c/
 * */

#include "lib.h"
#include "mainloop.h"

#define ECHO_PORT          (2002)
#define MAX_BUF            (4096)
#define Q_LIMIT 1024

static int port = ECHO_PORT;

typedef struct echo_thread {
	thread_t thread;
	int fd;
	event_t *event;
	char buf[MAX_BUF];
	int amt_read, amt_written;
	// stuff
} echo_thread_t;

DECLARE_THREAD(echo_thread)

bool read_state(struct thread_t *thread);

bool write_state(struct thread_t *thread)
{
	echo_thread_t *e = (echo_thread_t *)thread;
	thread->finished_event = NULL;
	thread->cont = read_state;

	int left = e->amt_read - e->amt_written;
	int count = write(e->fd, e->buf + e->amt_written, left);
	if (count < 0 && errno == EAGAIN) {
		register_event(thread, e->event);
		return false;
	} else if (count <= 0) {
		free_thread(thread);
		return false;
	}

	e->amt_written += count;
	if (e->amt_written == e->amt_read) {
		e->amt_written = e->amt_read = 0;
		thread->cont = read_state;
	}
	
	return true;
}


bool read_state(struct thread_t *thread)
{
	echo_thread_t *e = (echo_thread_t *)thread;
	thread->finished_event = NULL;
	thread->cont = read_state;
	
	int count = read(e->fd, e->buf, sizeof(e->buf));
	if (count < 0 && errno == EAGAIN) {
		register_event(thread, e->event);
		return false;
	} else if (count <= 0) {
		free_thread(thread);
		return false;
	}

	e->amt_read = count;
	e->amt_written = 0;
	thread->cont = write_state;

	return true;
}

bool setup_thread(struct thread_t *thread)
{
	echo_thread_t *e = (echo_thread_t *)thread;
	make_socket_non_blocking(e->fd);

	e->event = mk_nb_event(thread, e->fd, EVENT_RDWR);

	thread->cont = read_state;

	return true;
}

bool accept_state(struct thread_t *thread)
{
	echo_thread_t *e = (echo_thread_t *)thread;
	thread->finished_event = NULL;
	thread->cont = accept_state;
	
	int fd = accept(e->fd, NULL, NULL);
	if (fd < 0 && errno == EAGAIN) {
		register_event(thread, e->event);
		return false;
	} else if (fd < 0) fail(1, "accept");

	// Setup a new thread
	echo_thread_t *new_thread = mk_echo_thread();
	new_thread->fd = fd;
	new_thread->thread.cont = setup_thread;
	make_runnable(&new_thread->thread);

	printf("got one!\n");
	return true;
}

bool setup(struct thread_t *thread)
{
	echo_thread_t *e = (echo_thread_t *)thread;

	e->fd = socket(AF_INET, SOCK_STREAM, 0);
	if (e->fd < 0) fail(1, "socket");

	make_socket_non_blocking(e->fd);

	sock_bind_v4(e->fd, INADDR_ANY, port);
	
	if (listen(e->fd, Q_LIMIT) < 0) fail(1, "listen");

	e->event = mk_nb_event(thread, e->fd, EVENT_RD);

	thread->cont = accept_state;
	
	return true;
}



int main(int argc, char *argv[])
{
	process_name = argv[0];

	if (argc == 2) port = atoi(argv[1]);
	
	echo_thread_t *main_thread = mk_echo_thread();
	main_thread->thread.cont = setup;
	make_runnable(&main_thread->thread);

	setup_main_loop();
	main_loop();

	return 0;
}
