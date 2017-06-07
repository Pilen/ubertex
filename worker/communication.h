#ifndef W_COMMUNICATION_H
#define W_COMMUNICATION_H

void communication_initialize(Unt port);
void communication_add(Unt frame, Value value);
Bool communication_extract(Unt before, Value *result, Unt *designated_frame);

#endif
