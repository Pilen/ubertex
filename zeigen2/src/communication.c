
#include <stdlib.h>
#include <zmq.h>
#include <SDL2/SDL.h>
#include "communication.h"
#include "assert.h"

Int communication_loop(void *data);

void *communication_context;
void *communication_log;

void communication_initialize(char *host) {
    communication_log = NULL;

    communication_context = zmq_ctx_new();
    assert(communication_context != NULL); /* log_fatal("Could not create zmq context"); */
    void *log = zmq_socket(communication_context, ZMQ_PAIR);
    assert(log != NULL); /* log_fatal("Could not create zmq socket for communication_log"); */
    Int bind_error = zmq_bind(log, "inproc://communication_log");
    assert(bind_error == 0); /* log_fatal("Could not bind communication_log"); */

    SDL_Thread *thread = SDL_CreateThread(communication_loop, "communication", host);
    assert(thread != NULL); /* log_fatal("Could not create communication thread"); */

    /* Ensure the communication thread is ready before continuing */
    Int ready = zmq_recv(log, NULL, 0, 0);
    assert(ready >= 0);
    communication_log = log;
}


Int communication_loop(void *data) {
    Int error;

    void *log_input = zmq_socket(communication_context, ZMQ_PAIR);
    assert(log_input != NULL);
    error = zmq_connect(log_input, "inproc://communication_log");
    assert(error == 0);

    char *host = (char *) data;

    void *incoming = zmq_socket(communication_context, ZMQ_SUB);
    assert(incoming != NULL);
    error = zmq_bind(incoming, host);
    if (error) {
        log_fatal("Unable to bind incoming socket: %s", zmq_strerror(errno));
    }

    void *outgoing = zmq_socket(communication_context, ZMQ_PUB);
    assert(outgoing != NULL);
    /* Nothing to connect to yet */

    /* Send a message that we are ready, content not important */
    Int send_error = zmq_send(log_input, "ready", 5, 0);
    assert(send_error == 0);

    zmq_pollitem_t sockets[] = {
        {log_input, 0, ZMQ_POLLIN, 0},
        {incoming, 0, ZMQ_POLLIN, 0},
    };

    while (true) {
        zmq_poll(sockets, 2, -1);

        if (sockets[0].revents & ZMQ_POLLIN) {
            zmq_msg_t msg;
            error = zmq_msg_init(&msg);
            assert(error == 0);
            error = zmq_msg_recv(&msg, log_input, 0);
            assert(error == 0);

            error = zmq_msg_send(&msg, outgoing, 0);
            assert(error == 0);
        }

        if (sockets[1].revents & ZMQ_POLLIN) {
            /* zmq_msg_t header; */
            /* error = zmq_msg_init(&header); */
            /* assert(error == 0); */

            zmq_msg_t body;
            error = zmq_msg_init(&body);
            assert(error == 0);
            char *text = zmq_msg_data(body);
            Value lisp = read_from_str(text);

        }
    }

    return 0;
}
