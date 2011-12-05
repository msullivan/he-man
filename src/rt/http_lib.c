#include "lib.h"

// buffer needs
#define MAXLINE 4096

#define ROOT_DIR "."

int str_prefix(char *haystack, char *needle)
{
	while (*needle && *haystack == *needle)
		haystack++, needle++;
	return !*needle;
}

int http_parse(char *buf, int len)
{
	/* allocating this much space on the stack really bothers me... */
	char method[MAXLINE], path[MAXLINE], version[MAXLINE];
	
	if (len >= MAXLINE) return -1;
	buf[len] = '\0';
	if (strlen(buf) != len) return -2;

	char *line_end = strstr(buf, "\r\n");
	// also allow '\n'
	if (!line_end) line_end = strchr(buf, '\n');
	if (!line_end) return 0;

	sscanf(buf, "%s %s %s", method, path, version);

	/* we only handle GET */
	if (strcmp(method, "GET"))
		return 501; // Not implemented

	if (!str_prefix(version, "HTTP/1."))
		return 400; // Bad prefix

    // BUG: need to sanity check path
	
	if (snprintf(buf, MAXLINE, "%s%s", ROOT_DIR, path) >= MAXLINE)
		return 500; // Internal server error; something better?

	printf("opening file: %s\n", buf);
	return 1;
}
