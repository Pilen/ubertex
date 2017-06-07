#ifndef W_COMPONENT_H
#define W_COMPONENT_H

/* layer.h is the 'first' part of this header, to avoid cyclic dependencies *\/ */

Component *component_create(Value name, Value args, Environment *environment);
void component_update_all(Environment *environment);
Layer *component_layer_create(Int index);
void component_layer_insert_component(Int index, Component *component, Environment *environment);
void component_destroy_all(Environment *environment);
void component_remove(Component *component, Environment *environment);

#endif
