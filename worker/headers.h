#ifndef W_HEADERS_H
#define W_HEADERS_H

#include "options.h"       //
#include "log.h"           //

#include "types.h"         // log.h
#include "assert.h"        // types.h options.h
#include "profiler.h"      // types.h options.h
#include "print.h"         // types.h
#include "debug.h"         // options.h print.h log.h

#include "math.h"          // types.h
#include "lock.h"          // types.h
#include "memory.h"        // types.h options.h log.h
#include "file.h"          // types.h
#include "readline.h"      //

#include "layer.h"         // types.h hash.h
#include "environment.h"   // types.h hash.h layer.h

#include "initialize.h"    // environment.h
#include "loop.h"          // types.h environment.h

#include "symbol.h"        // types.h
#include "string.h"        // types.h
#include "list.h"          // types.h
#include "vector.h"        // types.h
#include "hash.h"          // types.h
#include "function.h"      // types.h
#include "component.h"     // layer.h types.h environment.h
#include "color.h"         // types.h
#include "sound.h"         // types.h environment.h
#include "image.h"         // types.h environment.h
#include "pdf.h"           // types.h environment.h

#include "read.h"          // types.h
#include "eval.h"          // types.h environment.h lisp.h

#include "communication.h" // types.h




#include "lisp.h"          // types.h list.h environment.h function.h
#include "graphics.h"      // types.h color.h environment.h
#include "resource.h"      // environment.h lock.h
#include "basic.h"         // types.h
#include "message.h"       // types.h environment.h
#include "text.h"          // types.h environment.h

#include "macros.h"        // types.h list.h
#include "headers.h"       // all

#endif
