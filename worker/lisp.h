#ifndef W_FUNCTION_H
#define W_FUNCTION_H

#include "types.h"
#include "list.h"
#include "environment.h"
#include "function.h"

void lisp_initialize(Environment *environment);

void lisp_register_builtin(Value symbol, c_lisp_function c_function, Bool eval, String *docstring, Environment *environment);

#define LISP_BUILTIN(name, docstring)                                   \
    char *lisp_docstr_for_##name = (docstring);                         \
    Value lisp_builtin_##name(Value args, Environment *environment)

#define LISP_REGISTER_BUILTIN(symbol, c_function_name, eval, environment) \
    {                                                                   \
        Value lisp_builtin_##c_function_name(Value args, Environment *environment); \
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
        Value lisp_builtin_##c_function_name(Value args, Environment *environment); \
        extern char *lisp_docstr_for_##c_function_name;                 \
        String *lisp_docstring_for_##c_function_name = string_create_from_str(lisp_docstr_for_##c_function_name); \
        lisp_register_builtin(symbol, &lisp_builtin_##c_function_name, eval, lisp_docstring_for_##c_function_name, environment); \
    }



#endif
