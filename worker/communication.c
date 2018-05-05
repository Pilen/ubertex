
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_net.h>

#include "headers.h"

typedef struct Communication_node_s {
    Unt frame;
    Value value;
    struct Communication_node_s *next;
} Communication_node;

Communication_node *communication_queue;
Mutex *communication_queue_lock;


Int communication_loop(void *data);
void communication_receive(TCPsocket socket);
void communication_receive_lisp(TCPsocket socket, Int size, Unt frame);
void communication_reset(void);


/* Int communication_log = -1; */

void communication_initialize(Unt port) {
    /* TODO: initialize logging */
    w_assert(SDL_WasInit(0));

    SDLNet_Init();
    atexit(SDLNet_Quit);

    communication_queue = NULL;
    communication_queue_lock = mutex_create();

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
    w_assert(thread);
}


Int communication_loop(void *data) {
    Int error;

    memory_register_thread();

    w_assert(data);
    TCPsocket server = (TCPsocket) data;

    SDLNet_SocketSet set = SDLNet_AllocSocketSet(1);
    w_assert(set);
    error = SDLNet_TCP_AddSocket(set, server);
    w_assert(error != -1);

    while (true) {
        Int ready = SDLNet_CheckSockets(set, -1);
        if (ready == -1) {
            /* perror("CheckSockets"); /\* Only prints on stderr *\/ */
            log_fatal("CheckSockets returned -1: %s", SDLNet_GetError());
        } else if (ready > 0) {
            if (SDLNet_SocketReady(server)) {
                TCPsocket client = SDLNet_TCP_Accept(server);
                if (client) {
                    communication_receive(client);
                    char *response = "Got it. Bye\n";
                    int length = strlen(response) + 1;
                    SDLNet_TCP_Send(client, response, length);
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
    w_assert(set);
    error = SDLNet_TCP_AddSocket(set, socket);
    w_assert(error != -1);

    int ready = SDLNet_CheckSockets(set, OPTION_HEADER_TIMEOUT);
    SDLNet_FreeSocketSet(set);
    if (ready < 0) {
        /* TODO: Use actual logging */
        perror("CheckSockets");
        return;
    } else if (ready == 0) {
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
            return;
        }
        if (size >= INT_MAX) {
            log_error("Only positive int sized messages are supported");
            return;
        }
        communication_receive_lisp(socket, size, frame);
    } else if (strcmp(command, "ping") == 0) {
        w_assert(frame == 0); // TODO: Frames other than 0 not yet handled
        return;
    } else if (strcmp(command, "ready?") == 0) {
        log_error("ready? command not yet implemented");
        w_assert(false);
        w_assert(frame == 0); // TODO: Frames other than 0 not yet handled
    } else if (strcmp(command, "abort") == 0) {
        w_assert(frame == 0); // TODO: Frames other than 0 not yet handled
        log_info("Abort");
        /* communication_queue should be reset before signaling */
        communication_reset();
        loop_abort = true;
    } else if (strcmp(command, "resync") == 0) {
        w_assert(frame == 0); // TODO: Frames other than 0 not yet handled
        log_info("Resync");
        /* communication_queue should be reset before signaling */
        communication_reset();
        Unt seed;
        Int scanned = sscanf(options, "%u", &seed);
        if (scanned != 1) {
            log_warning("Missing seed for reseed, using 0");
            seed = 0;
        }
        loop_new_seed = seed; /* Theese 3 assignments must happen in this order to avoid raceconditions!!! */
        loop_resync = true;   /* Theese 3 assignments must happen in this order to avoid raceconditions!!! */
        loop_abort = true;    /* Theese 3 assignments must happen in this order to avoid raceconditions!!! */
    } else if (strcmp(command, "blank") == 0) {
        w_assert(frame == 0); // TODO: Frames other than 0 not yet handled
        log_info("Blank");
        loop_blank = true;
    } else if (strcmp(command, "flush_dirty_cache") == 0) {
        w_assert(frame == 0); // TODO: Frames other than 0 not yet handled
        log_info("Flush dirty cache");
        flush_dirty_cache = true;
    } else if (strcmp(command, "flush_entire_cache") == 0) {
        w_assert(frame == 0); // TODO: Frames other than 0 not yet handled
        log_info("Flush entire cache");
        flush_entire_cache = true;
    } else {
        log_error("Header command not defined: %s", command);
    }
    return;
}

void communication_receive_lisp(TCPsocket socket, Int size, Unt frame) {
    w_assert(size >= 0);
    Int error;
    SDLNet_SocketSet set = SDLNet_AllocSocketSet(1);
    w_assert(set);
    error = SDLNet_TCP_AddSocket(set, socket);
    w_assert(error != -1);

    int ready = SDLNet_CheckSockets(set, OPTION_BODY_TIMEOUT);
    SDLNet_FreeSocketSet(set);
    if (ready < 0) {
        /* TODO: Use actual logging */
        perror("CheckSockets");
        return;
    } else if (ready == 0) {
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
    mutex_lock(communication_queue_lock);
    communication_add(frame, parsed);
    mutex_unlock(communication_queue_lock);

}

void communication_reset(void) {
    mutex_lock(communication_queue_lock);
    communication_queue = NULL;
    mutex_unlock(communication_queue_lock);
}
void communication_add(Unt frame, Value value) {
    Communication_node *new = NEW(Communication_node);
    new -> frame = frame;
    new -> value = value;

    mutex_lock(communication_queue_lock);

    if (communication_queue == NULL || frame < communication_queue -> frame) {
        new -> next = communication_queue;
        communication_queue = new;
    } else {
        Communication_node *current = communication_queue;
        while (current -> next != NULL &&
               current -> next -> frame <= frame) {
            current = current -> next;
        }
        new -> next = current -> next;
        current -> next = new;
    }
    mutex_unlock(communication_queue_lock);
}

Bool communication_extract(Unt frame, Value *result, Unt *designated_frame) {
    /* NOTE: Freeing of node left for GC */
    Bool retval = false;
    mutex_lock(communication_queue_lock);
    if (communication_queue && communication_queue -> frame <= frame) {
        *result = communication_queue -> value;
        *designated_frame = communication_queue -> frame;
        communication_queue = communication_queue -> next;
        retval = true;
    }
    mutex_unlock(communication_queue_lock);
    return retval;
}
