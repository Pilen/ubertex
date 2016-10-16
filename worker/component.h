#ifndef W_COMPONENT_H
#define W_COMPONENT_H

#include "layer.h" /* First part of this header, to avoid cyclic dependencies */
#include "types.h"
#include "environment.h"

void component_define(Value name, Value constructor, Value parameters, String *docstring, Value body, Environment *environment);
Component *component_create(Value name, Value args, Environment *environment);
void component_update_all(Component *component, Environment *environment);
Layer *component_layer_create(Int index);
void component_layer_insert_component(Int index, Value component, Environment *environment);

#endif
