
#include <stdlib.h>
#include <string.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_net.h>
#include "communication.h"
#include "assert.h"
#include "read.h"
#include "debug.h"
#include "memory.h"
#include "worker.h"

Int communication_loop(void *data);
void communication_receive(TCPsocket socket);
void communication_receive_lisp(TCPsocket socket, Unt size, Unt frame);

/* Int communication_log = -1; */

void communication_initialize(Unt port) {
    /* TODO: initialize logging */
    SDLNet_Init();
    atexit(SDLNet_Quit);

    communication_parsed_queue = list_create_empty();
    communication_parsed_queue_lock = mutex_create();

    /* Ensure the port is located on the heap, not the stack as it can change */
    Unt *port_heap = memory_malloc(sizeof(Unt));
    *port_heap = port;
    SDL_Thread *thread = SDL_CreateThread(communication_loop, "communication", port_heap);
    assert(thread);
}


Int communication_loop(void *data) {
    IPaddress ip;
    Int error;

    memory_register_thread();

    assert(data);
    Unt port = *((Unt *) data);
    memory_free(data);

    error = SDLNet_ResolveHost(&ip, INADDR_ANY, port);
    if (error == -1) {
        log_fatal("Unable to resolve host: %s", SDLNet_GetError());
    }

    TCPsocket server = SDLNet_TCP_Open(&ip);
    if (!server) {
        log_fatal("Unable to open socket: %s", SDLNet_GetError());
    }

    SDLNet_SocketSet set = SDLNet_AllocSocketSet(1);
    assert(set);
    error = SDLNet_TCP_AddSocket(set, server);
    assert(error != -1);


    while (true) {
        Int ready = SDLNet_CheckSockets(set, -1);
        /* TODO: Find out if CheckSockets returns -1 on timeout */
        if (ready == -1) {
            perror("CheckSockets"); /* Only prints on stderr */
            log_fatal("CheckSockets returned -1: %s", SDLNet_GetError());
        }
        if (ready > 0) {
            if (SDLNet_SocketReady(server)) {
                TCPsocket client = SDLNet_TCP_Accept(server);
                if (client) {
                    communication_receive(client);
                }
                SDLNet_TCP_Close(client);
            }
        }
    }
    return 0;
}
void communication_receive(TCPsocket socket) {
    /* TODO: Start a thread to handle this */
    char header[OPTION_HEADER_SIZE + 1];
    header[OPTION_HEADER_SIZE] = '\0';
    Int result = SDLNet_TCP_Recv(socket, header, OPTION_HEADER_SIZE);
    if (result != OPTION_HEADER_SIZE) {
        log_error("Received header with wrong length, %d bytes received, %d bytes expected", result, OPTION_HEADER_SIZE);
        return;
    }

    /* strtok_r is threadsafe but is not standard, only in posix */
    /* char **saveptr; */
    /* char *names = strtok_r(header, ";", saveptr); */
    /* char *time = strtok_r(NULL, ";", saveptr); */
    /* char *command = strtok_r(NULL, ";", saveptr); */
    /* char *options = strtok_r(NULL, ";", saveptr); */

    /* NOTE: THIS IS NOT THREADSAFE */
    /* But strtok is only used in this thread currently */
    char *names = strtok(header, ";");
    char *time = strtok(NULL, ";");
    char *command = strtok(NULL, ";");
    char *options = strtok(NULL, ";");

    if (!names || !time || !command || !options) {
        log_error("Header misformed");
        return;
    }

    Unt frame;
    Int scanned = sscanf(time, "%u", &frame);
    if (scanned != 1) {
        log_error("Header time invalid");
        return;
    }

    if (strcmp(command, "lisp") == 0) {
        Unt size;
        Int scanned = sscanf(options, "%u", &size);
        if (scanned != 1) {
            log_error("Size option for lisp command in header invalid");
        }

        communication_receive_lisp(socket, size, frame);
    } else if (strcmp(command, "ready?")) {
        log_error("ready? command not yet implemented");
        assert(false);
    } else if (strcmp(command, "unfreeze")) {
        worker_unfreeze = true;
    } else {
        log_error("Header command not defined: %s", command);
    }
    return;


}

void communication_receive_lisp(TCPsocket socket, Unt size, Unt frame) {
    char body[size + 1];
    body[size] = '\0';
    Int result = SDLNet_TCP_Recv(socket, body, size);
    if (result != size) {
        log_error("Received body with wrong length, %d bytes received, %d bytes expected from header", result, size);
        return;
    }

    Value parsed = read_from_str(body);
    mutex_lock(communication_parsed_queue_lock);
    list_push_front(communication_parsed_queue, parsed);
    mutex_unlock(communication_parsed_queue_lock);

}
