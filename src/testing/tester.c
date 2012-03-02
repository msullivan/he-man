// Really stupid program for doing some testing of an echo server
// I run it like:
// nc.traditional server port -e ./tester

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

int make_socket_non_blocking(int sfd)
{
	int flags, s;

	flags = fcntl(sfd, F_GETFL, 0);
	if (flags == -1) exit(1);

	flags |= O_NONBLOCK;
	s = fcntl (sfd, F_SETFL, flags);
	if (s == -1) exit(1);

	return 0;
}


int main() {
	char buf[1024];
	srand(getpid());
	make_socket_non_blocking(0);
	while (1) {
		if (write(1, "hello\n", 6) < 0 && errno != EAGAIN) {
			//perror("write");
			return 1;
		}
		int err = read(0, buf, sizeof(buf));
		sleep(rand() % 10);
	}
}
