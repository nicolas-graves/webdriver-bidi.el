/* native_relay.c - Minimal Firefox Native Messaging relay */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#define MAX_MESSAGE_SIZE (1024 * 1024) /* 1MB safety limit */

/* Read exactly n bytes or fail */
static int read_fully(int fd, void *buf, size_t n) {
    size_t total = 0;
    while (total < n) {
        ssize_t r = read(fd, (char*)buf + total, n - total);
        if (r <= 0) return -1;
        total += r;
    }
    return 0;
}

/* Write exactly n bytes or fail */
static int write_fully(int fd, const void *buf, size_t n) {
    size_t total = 0;
    while (total < n) {
        ssize_t w = write(fd, (const char*)buf + total, n - total);
        if (w <= 0) return -1;
        total += w;
    }
    return 0;
}

int main(void) {
    uint32_t len;
    char *buf = NULL;
    size_t buf_size = 0;

    /* Binary mode for Windows compatibility */
    #ifdef _WIN32
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
    #endif

    while (1) {
        /* Read 4-byte length prefix (little-endian) */
        if (read_fully(STDIN_FILENO, &len, 4) < 0) {
            break;
        }

        /* Validate length */
        if (len == 0 || len > MAX_MESSAGE_SIZE) {
            return 1;
        }

        /* Allocate/resize buffer if needed */
        if (len > buf_size) {
            char *new_buf = realloc(buf, len);
            if (!new_buf) {
                free(buf);
                return 1;
            }
            buf = new_buf;
            buf_size = len;
        }

        /* Read message */
        if (read_fully(STDIN_FILENO, buf, len) < 0) {
            free(buf);
            return 1;
        }

        /* Echo: write length prefix and message back */
        if (write_fully(STDOUT_FILENO, &len, 4) < 0 ||
            write_fully(STDOUT_FILENO, buf, len) < 0) {
            free(buf);
            return 1;
        }

        fflush(stdout);
    }

    free(buf);
    return 0;
}
