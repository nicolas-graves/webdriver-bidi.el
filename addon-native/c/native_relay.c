#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>

#define SOCKET_PATH "/tmp/bidi.sock"
#define BUFFER_SIZE 1024 * 1024

// Global state to share the active connection between threads
// -1 means no client is currently connected.
volatile int global_client_fd = -1;

void handle_signal(int sig) {
    unlink(SOCKET_PATH);
    exit(0);
}

/*
 * THREAD: Client Reader (One per connection)
 * Reads lines from connected client -> stdout (Browser)
 */
void *handle_client_input(void *arg) {
    int sock_fd = *(int *)arg;
    free(arg); // Free the pointer allocated in acceptor

    FILE *sock_fp = fdopen(sock_fd, "r");
    if (!sock_fp) {
        close(sock_fd);
        global_client_fd = -1;
        return NULL;
    }

    char *line_buf = malloc(BUFFER_SIZE);

    // Read from client until it closes connection
    while (fgets(line_buf, BUFFER_SIZE, sock_fp)) {
        size_t len = strlen(line_buf);
        if (len > 0 && line_buf[len-1] == '\n') len--;
        if (len == 0) continue;

        // Write to Browser (Standard Native Messaging)
        uint32_t msg_len = (uint32_t)len;
        fwrite(&msg_len, sizeof(uint32_t), 1, stdout);
        fwrite(line_buf, 1, len, stdout);
        fflush(stdout);
    }

    // Cleanup when client disconnects
    free(line_buf);
    fclose(sock_fp); // This closes sock_fd too

    // Mark global state as disconnected so Main Thread stops writing
    global_client_fd = -1;
    return NULL;
}

/*
 * THREAD: Connection Accepter
 * Loops forever, waiting for client to connect.
 */
void *accept_loop(void *arg) {
    int server_fd = *(int *)arg;

    while (1) {
        int *client_fd_ptr = malloc(sizeof(int));
        *client_fd_ptr = accept(server_fd, NULL, NULL);

        if (*client_fd_ptr == -1) {
            free(client_fd_ptr);
            continue;
        }

        // We have a new connection! Update global state.
        // Note: In a production server we'd need a Mutex here,
        // but for a single-client relay, atomic int assignment is fine.
        global_client_fd = *client_fd_ptr;

        pthread_t tid;
        if (pthread_create(&tid, NULL, handle_client_input, client_fd_ptr) != 0) {
            close(*client_fd_ptr);
            free(client_fd_ptr);
            global_client_fd = -1;
        } else {
            pthread_detach(tid); // Let it run independently
        }
    }
    return NULL;
}

/*
 * MAIN THREAD: Browser (Stdin) -> Client (Socket)
 */
int main() {
    // Ignore SIGPIPE. If we write to a closed client socket,
    // we want write() to return -1, not crash the program.
    signal(SIGPIPE, SIG_IGN);
    signal(SIGINT, handle_signal);
    signal(SIGTERM, handle_signal);

    // 1. Setup Server Socket
    int server_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, SOCKET_PATH, sizeof(addr.sun_path) - 1);
    unlink(SOCKET_PATH);

    if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) == -1) {
        perror("bind"); return 1;
    }
    if (listen(server_fd, 5) == -1) {
        perror("listen"); return 1;
    }

    // 2. Start the Accept Thread (Handles client connections)
    pthread_t accept_tid;
    pthread_create(&accept_tid, NULL, accept_loop, &server_fd);

    // 3. Main Loop: Read from Browser -> Write to client
    uint32_t msg_len;
    char *read_buf = malloc(BUFFER_SIZE);

    while (fread(&msg_len, sizeof(uint32_t), 1, stdin) == 1) {
        if (msg_len > BUFFER_SIZE) continue; // Skip huge messages

        // Read the exact payload
        if (fread(read_buf, 1, msg_len, stdin) != msg_len) break;

        // If client is connected, forward the data
        if (global_client_fd != -1) {
            if (write(global_client_fd, read_buf, msg_len) == -1 ||
                write(global_client_fd, "\n", 1) == -1) {
                // Write failed (client likely closed socket unexpectedly)
                // The Reader thread will handle the cleanup/reset of global_client_fd
            }
        } else {
            // client is not connected.
            // fprintf(stderr, "Dropped message: Client not connected\n");
        }
    }

    free(read_buf);
    close(server_fd);
    unlink(SOCKET_PATH);
    return 0;
}
