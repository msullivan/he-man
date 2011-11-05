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

static char *process_name;
void fail(int status, char *fmt, ...);


#define MAX_EVENTS 10

static struct {
	int epoll_fd;
	io_context_t aio_ctx;
	int aio_eventfd;
} state;

static void handle_aio_event(struct io_event *event) { }
static void handle_epoll_event(struct epoll_event *event) { }

void main_loop()
{
	struct io_event aio_events[MAX_EVENTS];
	struct epoll_event epoll_events[MAX_EVENTS];

	bool expect_aio = false;

	// We need to be careful to not let either aio or epoll events
	// starve the other. We do this by always doing nonblocking polls
	// of aio and only having epoll block if we aren't expecting aio
	// events. When both types of events are coming in, we switch
	// between processing the two types.
	for (;;) {
		// Poll for aio events. Don't block.
		int aio_cnt = io_getevents(state.aio_ctx, 0, MAX_EVENTS,
		                           aio_events, NULL);
		if (aio_cnt < 0) fail(1, "io_getevents: %s", strerror(-aio_cnt));
		expect_aio = aio_cnt == MAX_EVENTS;
		for (int i = 0; i < aio_cnt; i++) {
			handle_aio_event(&aio_events[i]);
		}

		// If we aren't expecting aio, block indefinitely, otherwise
		// just poll.
		int epoll_timeout = expect_aio ? 0 : -1;
		int epoll_cnt = epoll_wait(state.epoll_fd, epoll_events,
		                           MAX_EVENTS, epoll_timeout);
		for (int i = 0; i < epoll_cnt; i++) {
			if (epoll_events[i].data.fd == state.aio_eventfd) { //XXX
				uint64_t eventfd_val;
				expect_aio = true;
				if (read(state.aio_eventfd, &eventfd_val,
				         sizeof(eventfd_val)) < 0)
					fail(1, "eventfd read: %s", strerror(errno));
			} else {
				handle_epoll_event(&epoll_events[i]);
			}
		}
	}
}

int main(int argc, char *argv[]) {
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
