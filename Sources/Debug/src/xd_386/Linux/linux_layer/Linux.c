/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* The module that joins several .c files together. */


#include "linux_layer/defs.h"
#include "linux_layer/linux-low.h"

#include "linux_layer/events.c"
#include "linux_layer/inferior.c"
#include "linux_layer/breakpoint.c"
#include "linux_layer/cleanup.c"
#include "linux_layer/linux-low.c"
#include "linux_layer/syscall.c"
#include "linux_layer/messages.c"
#include "linux_layer/proc-service.c"
#include "linux_layer/solib.c"
#include "linux_layer/bfd-redirector.c"
#include "linux_layer/thread.c"
#include "linux_layer/signals.c"


