#ifndef Z_LISP_H
#define Z_LISP_H

#include "types.h"
#include "list.h"
#include "environment.h"

typedef Value (*c_lisp_function) (List *args, Environment *environment, List *call_stack);

struct Function_s {
    Bool eval;
    Bool c_code;
    c_lisp_function c_function;
    List *parameters;
    Value body;
    String *docstring;
};

void lisp_initialize(Environment *environment);

void lisp_register_builtin(Value symbol, c_lisp_function c_function, Bool eval, String *docstring, Environment *environment);

#define LISP_BUILTIN(name, docstring)                                   \
    char *lisp_docstr_for_##name = (docstring);                         \
    Value lisp_builtin_##name(List *args, Environment *environment, List *call_stack)

#define LISP_REGISTER_BUILTIN(symbol, c_function_name, eval, environment) \
    {                                                                   \
        Value lisp_builtin_##c_function_name(List *args, Environment *environment, List *call_stack); \
        extern char *lisp_docstr_for_##c_function_name;                 \
        (void)lisp_docstr_for_##c_function_name;                        \
        /* String *lisp_docstring_for_##c_function_name = string_create_from_str(lisp_docstr_for_##c_function_name); \ */\
        String *lisp_docstring_for_##c_function_name = NULL;            \
        lisp_register_builtin(symbol, &lisp_builtin_##c_function_name, eval, lisp_docstring_for_##c_function_name, environment); \
    }

#define LISP_REGISTER_BUILTIN_FROM_RAW(symbol_name, c_function_name, eval, environment) \
    {                                                                   \
        Value symbol = symbol_get(VALUE_STRING(string_create_from_str(#symbol_name))); \
                                                                        \
        Value lisp_builtin_##c_function_name(List *args, Environment *environment, List *call_stack); \
        extern char *lisp_docstr_for_##c_function_name;                 \
        String *lisp_docstring_for_##c_function_name = string_create_from_str(lisp_docstr_for_##c_function_name); \
        lisp_register_builtin(symbol, &lisp_builtin_##c_function_name, eval, lisp_docstring_for_##c_function_name, environment); \
    }



#endif
