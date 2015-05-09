
#include <stdlib.h>
#include <nanomsg/nn.h>
#include <nanomsg/pair.h>
#include <nanomsg/pubsub.h>
#include <SDL2/SDL.h>
#include "communication.h"
#include "assert.h"
#include "read.h"
#include "debug.h"

Int communication_loop(void *data);

Int communication_log = -1;

void communication_initialize(char *host) {
    communication_queue = list_create_empty();
    communication_queue_lock = SDL_CreateMutex();
    assertp(communication_queue_lock);

    Int log = nn_socket(AF_SP, NN_PAIR);
    assert(log != -1);
    Int error = nn_bind(log, "inproc://communication_log");
    assert(error != -1);

    SDL_Thread *thread = SDL_CreateThread(communication_loop, "communication", host);
    assert(thread != NULL);

    /* Ensure the communication thread is ready before continuing */
    Int ready = nn_recv(log, NULL, 0, 0);
    assert(ready >= 0);
    communication_log = log;
}


Int communication_loop(void *data) {
    Int error;

    Int log_input = nn_socket(AF_SP, NN_PAIR);
    assert(log_input != -1);
    error = nn_connect(log_input, "inproc://communication_log");
    assert(error != -1);

    char *host = (char *) data;

    Int incoming = nn_socket(AF_SP, NN_SUB);
    assert(incoming != -1);
    debug(host);
    error = nn_bind(incoming, host);
    if (error == -1) {
        log_fatal("Unable to bind incoming socket: %s", nn_strerror(errno));
    }

    Int outgoing = nn_socket(AF_SP, NN_PUB);
    assert(outgoing != -1);
    /* Nothing to connect to yet */

    /* Send a message that we are ready, content not important */
    error = nn_send(log_input, "ready", 5, 0);
    assert(error != -1);

    struct nn_pollfd sockets[] = {
        {log_input, NN_POLLIN, 0},
        {incoming, NN_POLLIN, 0},
    };

    while (true) {
        debug("hund");
        nn_poll(sockets, 2, -1);
        debug("fisk");

        if (sockets[0].revents & NN_POLLIN) {
            char* buffer = NULL;
            Int bytes = nn_recv(log_input, &buffer, NN_MSG, 0);
            assert(bytes >= 0);


            error = nn_send(outgoing, buffer, bytes, 0);
            assert(error != -1);
            debug("log_input");
        }

        if (sockets[1].revents & NN_POLLIN) {
            char *buffer = NULL;
            Int bytes = nn_recv(incoming, &buffer, NN_MSG, 0);
            assert(bytes >= 0);

            Value parsed = read_from_str(buffer);

            debug("incomming");
            if (SDL_LockMutex(communication_queue_lock) == 0) {
                list_push_front(communication_queue, parsed);
                SDL_UnlockMutex(communication_queue_lock);
            } else {
                assert(false);
            }
        }
    }

    return 0;
}
