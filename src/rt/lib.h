#ifndef LIB_H
#define LIB_H

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
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/sendfile.h>

#define EVENT_RD (EPOLLIN)
#define EVENT_WR (EPOLLOUT)
#define EVENT_RDWR (EPOLLIN|EPOLLOUT)

int set_sock_reuse(int fd);
int set_sock_cork(int fd);
int make_socket_non_blocking(int sfd);
int sock_bind_v4(int fd, int addr, int port);
int print_int(int n);

#define prepare_event(event, new_mode) (event->u.nb.mode = new_mode)
#define read_msg_tag(msg) (msg.tag)
#define read_msg_payload(msg) (msg.payload)
#define buf_to_data(buf) ({data_t data; data.p = buf; data;})
#define data_to_buf(data) (data.p)
#define int_to_data(n) ({data_t data; data.i = n; data;})
#define data_to_int(data) (data.i)
#define get_channel_event(ch) (&ch->ev)

// This shouldn't be here. Programs should be able to include other headers
int http_parse(char *buf, int len);
int http_make_hdr(char *buf, int len, int size);
int get_file_size(int fd);

#endif
