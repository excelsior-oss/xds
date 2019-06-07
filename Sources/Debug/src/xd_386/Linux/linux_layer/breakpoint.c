/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Breakpoint stuff.  */

#include "breakpoint.h"
#include "inferior.h"
#include "cleanup.h"
#include "linux-low.h"

/* Creates the breakpoint object. */
breakpoint_t 
create_breakpoint (breakpoint_type_t type)
{
    breakpoint_t bpt = (breakpoint_t) malloc (sizeof (struct breakpoint));
    bpt->is_set = xfalse;
    bpt->type = type;
    
    register_cleanup (PCLEANUP, (cleanup_func_cb)free, bpt);

    switch (type) {
      case BPT_TYPE_DEFAULT:
        bpt->opcode_len = BREAKPOINT_INSTR_LEN;
        break;

      case BPT_TYPE_BPT_RET:
        bpt->opcode_len = BREAKPOINT_INSTR_LEN + RET_INSTR_LEN;
        break;

      case BPT_TYPE_BPT_RETXX:
        bpt->opcode_len = BREAKPOINT_INSTR_LEN + RET_INSTR_LEN + 2;
        break;

      default:
        ASSERT_FALSE ();
    }
    return bpt;
}


/* Sets/removes the breakpoint. Returns false on errors. */

int 
set_breakpoint (breakpoint_t bpt, CORE_ADDR address)
{
    const uint_t opcode = 0x9090c3cc; // "int 3; ret; nop; nop;"

    if (bpt->is_set) {
        return 1;
    }
    bpt->address = address;

    if (!proc_get_memory (proc_info->main_thread, address, bpt->opcode_len, &bpt->opcode)) {
        return 0;
    }

    if (!proc_set_memory (proc_info->main_thread, &opcode, bpt->opcode_len, address)) {
        return 0;
    }

    bpt->is_set = xtrue;
    return 1;
}

int 
set_breakpointXX (breakpoint_t bpt, CORE_ADDR address, unsigned short size)
{
    const char opcode [] = {0xcc, 0xc2, size & 0xff, size >> 8}; // "int 3; ret XX;"

    if (bpt->is_set) {
        return 1;
    }
    bpt->address = address;

    if (!proc_get_memory (proc_info->main_thread, address, bpt->opcode_len, &bpt->opcode)) {
        return 0;
    }

    if (!proc_set_memory (proc_info->main_thread, &opcode, bpt->opcode_len, address)) {
        return 0;
    }

    bpt->is_set = xtrue;
    return 1;
}


int 
remove_breakpoint (breakpoint_t bpt)
{
    if (!bpt->is_set) {
        return 1;
    }

    if (!proc_set_memory (proc_info->main_thread, &bpt->opcode, bpt->opcode_len, bpt->address)) {
        return 0;
    }
    bpt->is_set = xfalse;

//    info ("remove_breakpoint 2");
    return 1;
}


