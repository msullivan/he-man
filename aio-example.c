/*
 * Simplistic version of copy command using async i/o
 *
 * From:  Stephen Hemminger <shemminger@osdl.org>
 * Copy file by using a async I/O state machine.
 * 1. Start read request
 * 2. When read completes turn it into a write request
 * 3. When write completes decrement counter and free resources
 *
 *
 * Usage: aiocp file(s) desination
 */

#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>

#include <libaio.h>

#define AIO_BLKSIZE (64*1024)
#define AIO_MAXIO   32

static int busy = 0;          // # of I/O's in flight
static int tocopy = 0;        // # of blocks left to copy
static int dstfd = -1;        // destination file descriptor
static const char *dstname = NULL;
static const char *srcname = NULL;


/* Fatal error handler */
static void io_error(const char *func, int rc)
{
	if (rc == -ENOSYS)
		fprintf(stderr, "AIO not in this kernel\n");
	else if (rc < 0 && -rc < sys_nerr)
		fprintf(stderr, "%s: %s\n", func, sys_errlist[-rc]);
	else
		fprintf(stderr, "%s: error %d\n", func, rc);

	if (dstfd > 0)
		close(dstfd);
	if (dstname)
		unlink(dstname);
	exit(1);
}

/*
 * Write complete callback.
 * Adjust counts and free resources
 */
static void wr_done(io_context_t ctx, struct iocb *iocb, long res, long res2)
{
	if (res2 != 0) {
		io_error("aio write", res2);
	}
	if (res != iocb->u.c.nbytes) {
		fprintf(stderr, "write missed bytes expect %lu got %ld\n",
		        iocb->u.c.nbytes, res2);
		exit(1);
	}
	--tocopy;
	--busy;
	free(iocb->u.c.buf);

	memset(iocb, 0xff, sizeof(iocb));   // paranoia
	free(iocb);
	write(2, "w", 1);
}

/*
 * Read complete callback.
 * Change read iocb into a write iocb and start it.
 */
static void rd_done(io_context_t ctx, struct iocb *iocb, long res, long res2)
{
	/* library needs accessors to look at iocb? */
	int iosize = iocb->u.c.nbytes;
	char *buf = iocb->u.c.buf;
	off_t offset = iocb->u.c.offset;

	if (res2 != 0)
		io_error("aio read", res2);
	if (res != iosize) {
		fprintf(stderr, "read missing bytes expect %lu got %ld\n",
		        iocb->u.c.nbytes, res);
		exit(1);
	}


	/* turn read into write */
	io_prep_pwrite(iocb, dstfd, buf, iosize, offset);
	io_set_callback(iocb, wr_done);
	if (1 != (res = io_submit(ctx, 1, &iocb)))
		io_error("io_submit write", res);
	write(2, "r", 1);
}


int main(int argc, char *const *argv)
{
	int srcfd;
	struct stat st;
	off_t length = 0, offset = 0;
	io_context_t myctx;

	if (argc != 3 || argv[1][0] == '-') {
		fprintf(stderr, "Usage: aiocp SOURCE DEST\n");
		exit(1);
	}
	if ((srcfd = open(srcname = argv[1], O_RDONLY)) < 0) {
		perror(srcname);
		exit(1);
	}
	if (fstat(srcfd, &st) < 0) {
		perror("fstat");
		exit(1);
	}
	length = st.st_size;

	if ((dstfd = open(dstname = argv[2], O_WRONLY | O_CREAT, 0666)) < 0) {
		close(srcfd);
		perror(dstname);
		exit(1);
	}

	/* initialize state machine */
	memset(&myctx, 0, sizeof(myctx));
	io_queue_init(AIO_MAXIO, &myctx);
	tocopy = howmany(length, AIO_BLKSIZE);

	while (tocopy > 0) {
		int i, rc;
		/* Submit as many reads as once as possible upto AIO_MAXIO */
		int n = MIN(MIN(AIO_MAXIO - busy, AIO_MAXIO / 2),
		            howmany(length - offset, AIO_BLKSIZE));
		if (n > 0) {
			struct iocb *ioq[n];

			for (i = 0; i < n; i++) {
				struct iocb *io = (struct iocb *) malloc(sizeof(struct iocb));
				int iosize = MIN(length - offset, AIO_BLKSIZE);
				char *buf = (char *) malloc(iosize);

				if (NULL == buf || NULL == io) {
					fprintf(stderr, "out of memory\n");
					exit(1);
				}

				io_prep_pread(io, srcfd, buf, iosize, offset);
				io_set_callback(io, rd_done);
				ioq[i] = io;
				offset += iosize;
			}

			rc = io_submit(myctx, n, ioq);
			if (rc < 0)
				io_error("io_submit", rc);

			busy += n;
		}

		// Handle IO's that have completed
		rc = io_queue_run(myctx);
		if (rc < 0)
			io_error("io_queue_run", rc);

		// if we have maximum number of i/o's in flight
		// then wait for one to complete
		if (busy == AIO_MAXIO) {
			rc = io_queue_wait(myctx, NULL);
			if (rc < 0)
				io_error("io_queue_wait", rc);
		}

	}

	close(srcfd);
	close(dstfd);
	exit(0);
}

/*
 * Results look like:
 * [alanm@toolbox ~/MOT3]$ ../taio kernel-source-2.4.8-0.4g.ppc.rpm abc
 * rrrrrrrrrrrrrrrwwwrwrrwwrrwrwwrrwrwrwwrrwrwrrrrwwrwwwrrwrrrwwwwwwwwwwwwwwwww
 * rrrrrrrrrrrrrrwwwrrwrwrwrwrrwwwwwwwwwwwwwwrrrrrrrrrrrrrrrrrrwwwwrwrwwrwrwrwr
 * wrrrrrrrwwwwwwwwwwwwwrrrwrrrwrrwrwwwwwwwwwwrrrrwwrwrrrrrrrrrrrwwwwwwwwwwwrww
 * wwwrrrrrrrrwwrrrwwrwrwrwwwrrrrrrrwwwrrwwwrrwrwwwwwwwwrrrrrrrwwwrrrrrrrwwwwww
 * wwwwwwwrwrrrrrrrrwrrwrrwrrwrwrrrwrrrwrrrwrwwwwwwwwwwwwwwwwwwrrrwwwrrrrrrrrrr
 * rrwrrrrrrwrrwwwwwwwwwwwwwwwwrwwwrrwrwwrrrrrrrrrrrrrrrrrrrwwwwwwwwwwwwwwwwwww
 * rrrrrwrrwrwrwrrwrrrwwwwwwwwrrrrwrrrwrwwrwrrrwrrwrrrrwwwwwwwrwrwwwwrwwrrrwrrr
 * rrrwwwwwwwrrrrwwrrrrrrrrrrrrwrwrrrrwwwwwwwwwwwwwwrwrrrrwwwwrwrrrrwrwwwrrrwww
 * rwwrrrrrrrwrrrrrrrrrrrrwwwwrrrwwwrwrrwwwwwwwwwwwwwwwwwwwwwrrrrrrrwwwwwwwrw
 */
