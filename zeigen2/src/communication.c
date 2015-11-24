
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
    z_assert(SDL_WasInit(0));

    SDLNet_Init();
    atexit(SDLNet_Quit);

    communication_parsed_queue = list_create_empty();
    communication_parsed_queue_lock = mutex_create();

    IPaddress ip;
    Int error;
    error = SDLNet_ResolveHost(&ip, INADDR_ANY, port);
    if (error == -1) {
        log_fatal("Unable to resolve host: %s", SDLNet_GetError());
    }

    TCPsocket server = SDLNet_TCP_Open(&ip);
    if (!server) {
        log_fatal("Unable to open socket: %s", SDLNet_GetError());
    }

    SDL_Thread *thread = SDL_CreateThread(communication_loop, "communication", server);
    z_assert(thread);
}


Int communication_loop(void *data) {
    Int error;

    memory_register_thread();

    z_assert(data);
    TCPsocket server = (TCPsocket) data;

    SDLNet_SocketSet set = SDLNet_AllocSocketSet(1);
    z_assert(set);
    error = SDLNet_TCP_AddSocket(set, server);
    z_assert(error != -1);

    while (true) {
        Int ready = SDLNet_CheckSockets(set, -1);
        /* TODO: Find out if CheckSockets returns -1 on timeout */
        if (ready == -1) {
            /* perror("CheckSockets"); /\* Only prints on stderr *\/ */
            log_fatal("CheckSockets returned -1: %s", SDLNet_GetError());
        } else if (ready > 0) {
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
    /* TODO: This can still delay other messages while waiting for timeout,
       maybe use threads or a common socketset */
    Int error;
    SDLNet_SocketSet set = SDLNet_AllocSocketSet(1);
    z_assert(set);
    error = SDLNet_TCP_AddSocket(set, socket);
    z_assert(error != -1);

    int ready = SDLNet_CheckSockets(set, OPTION_HEADER_TIMEOUT);
    SDLNet_FreeSocketSet(set);
    if (ready < 0) {
        perror("CheckSockets");
        return;
    } else if (ready == 0) {
        SDLNet_FreeSocketSet(set);
        log_error("Connection timed out before recieving header");
        return;
    }

    char header[OPTION_HEADER_SIZE + 1];
    header[OPTION_HEADER_SIZE] = '\0';
    Int result = SDLNet_TCP_Recv(socket, header, OPTION_HEADER_SIZE);
    if (result <= 0) {
        log_error("Connection failed before receiving header");
    } else if (result != OPTION_HEADER_SIZE) {
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

    if (!names || !time || !command) {
        log_error("Header malformed");
        return;
    }

    Unt frame;
    Int scanned = sscanf(time, "%u", &frame);
    if (scanned != 1) {
        log_error("Header time invalid");
        return;
    }

    if (strcmp(command, "lisp") == 0) {
        if (!options) {
            log_error("Header malformed, missing options");
            return;
        }
        Unt size;
        Int scanned = sscanf(options, "%u", &size);
        if (scanned != 1) {
            log_error("Size option for lisp command in header invalid");
        }

        communication_receive_lisp(socket, size, frame);
    } else if (strcmp(command, "ready?") == 0) {
        log_error("ready? command not yet implemented");
        z_assert(false);
    } else if (strcmp(command, "abort") == 0) {
        mutex_lock(communication_parsed_queue_lock);
        /* list_clear(communication_parsed_queue); */
        while (communication_parsed_queue -> length > 0) {
            (void) list_pop_back(communication_parsed_queue);
        }
        mutex_unlock(communication_parsed_queue_lock);
        log_info("Abort");
        worker_abort = true;
    } else if (strcmp(command, "blank") == 0) {
        log_info("Blank");
        worker_blank = true;
    } else if (strcmp(command, "flush_dirty_cache") == 0) {
        log_info("Flush dirty cache");
        flush_dirty_cache = true;
    } else if (strcmp(command, "flush_entire_cache") == 0) {
        log_info("Flush entire cache");
        flush_entire_cache = true;
    } else {
        log_error("Header command not defined: %s", command);
    }
    return;
}

void communication_receive_lisp(TCPsocket socket, Unt size, Unt frame) {
    Int error;
    SDLNet_SocketSet set = SDLNet_AllocSocketSet(1);
    z_assert(set);
    error = SDLNet_TCP_AddSocket(set, socket);
    z_assert(error != -1);

    int ready = SDLNet_CheckSockets(set, OPTION_BODY_TIMEOUT);
    SDLNet_FreeSocketSet(set);
    if (ready < 0) {
        perror("CheckSockets");
        return;
    } else if (ready == 0) {
        SDLNet_FreeSocketSet(set);
        log_error("Connection timed out before recieving body");
        return;
    }

    char body[size + 1];
    body[size] = '\0';
    Int result = SDLNet_TCP_Recv(socket, body, size);
    if (result <= 0) {
        log_error("Connection failed before receiving body");
    } else if (result != size) {
        log_error("Received body with wrong length, %d bytes received, %d bytes expected from header", result, size);
        return;
    }

    Value parsed = read_from_str(body);
    mutex_lock(communication_parsed_queue_lock);
    list_push_front(communication_parsed_queue, parsed);
    mutex_unlock(communication_parsed_queue_lock);

}
