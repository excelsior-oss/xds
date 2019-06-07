/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Breakpoint stuff.  */

#ifndef _BREAKPOINT_H_
#define _BREAKPOINT_H_

#include "defs.h"


enum breakpoint_type {
    BPT_TYPE_UNDEF = 0,
    BPT_TYPE_DEFAULT,   // just "int 3"    instruction
    BPT_TYPE_BPT_RET,   // "int 3; ret"    instructions
    BPT_TYPE_BPT_RETXX  // "int 3; ret XX" instructions
};

typedef enum breakpoint_type breakpoint_type_t;

struct breakpoint {
    /* Breakpoint type. */
    breakpoint_type_t type;

    /* The address of the breakpoint. */
    CORE_ADDR address;

    /* The original opcode. */
    uint_t opcode;

    /* The length of the opcode. */
    uint_t opcode_len;

    /* Indicates that the breakpoint is already set in the debugee's
       memory area. */
    uint_t is_set :1;
};

typedef struct breakpoint * breakpoint_t;


#define BREAKPOINT_INSTR_LEN    1
#define RET_INSTR_LEN           1


/* this is a system depended function! */
inline xbool is_breakpoint(void *addr) {
    return *((unsigned char*)addr) == (unsigned char)0xCC;
}



/* Creates the breakpoint of given type object. */
extern breakpoint_t create_breakpoint (breakpoint_type_t type);

/* Sets/removes the breakpoint. Returns false on errors. */
extern int set_breakpoint (breakpoint_t bpt, CORE_ADDR address);

extern int set_breakpointXX (breakpoint_t bpt, CORE_ADDR address, unsigned short size);

extern int remove_breakpoint (breakpoint_t bpt);


#endif // _BREAKPOINT_H_
