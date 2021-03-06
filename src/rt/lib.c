#include "lib.h"
#include "mainloop.h"

int set_sock_reuse(int fd)
{
	int value = 1;
	return setsockopt(fd, SOL_SOCKET, SO_REUSEADDR,
	                  &value, sizeof(value));
}

int set_sock_cork(int fd, int value)
{
	return setsockopt(fd, IPPROTO_TCP, TCP_CORK,
	                  &value, sizeof(value));
}

int make_socket_non_blocking(int sfd)
{
	int flags, s;

	flags = fcntl(sfd, F_GETFL, 0);
	if (flags == -1) fail(1, "fcntl");

	flags |= O_NONBLOCK;
	s = fcntl (sfd, F_SETFL, flags);
	if (s == -1) fail(1, "fcntl");

	return 0;
}

int sock_bind_v4(int fd, int addr, int port)
{
	struct sockaddr_in servaddr;

	memset(&servaddr, 0, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(addr);
	servaddr.sin_port        = htons(port);

	// TODO: real error handling?
	if (bind(fd, (struct sockaddr *)&servaddr,
	         sizeof(servaddr)) < 0)
		fail(1, "bind");

	return 0;
}

int print_int(int n) { return printf("%d\n", n); }
