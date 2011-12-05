#include <string.h>
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
#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>

#define EVENT_RD (EPOLLIN)
#define EVENT_WR (EPOLLOUT)
#define EVENT_RDWR (EPOLLIN|EPOLLOUT)

int set_sock_reuse(int fd);
int make_socket_non_blocking(int sfd);
int sock_bind_v4(int fd, int addr, int port);
int print_int(int n);
