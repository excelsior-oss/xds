/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Solib stuff.  */

#include "solib.h"
#include "inferior.h"
#include "list.h"
#include "cleanup.h"
#include "bfd.h"
#include "breakpoint.h"
#include "linux-low.h"

#include <string.h>
#include <elf.h>
#include <link.h>
#include <bfd.h>



static char* rdebug_names[] =
{
    "_r_debug",
    0
};

static char* solib_break_names[] =
{
    "_dl_debug_state",
    0
};

static char* text_segment_names[] =
{
    ".text",
    ".plt",
    ".init",
    ".fini",
    0
};



/* Sets breakpoint to the given address. */
static void 
create_solib_event_breakpoint (CORE_ADDR address)
{
/*
    unsigned int opcode = 0xc3cc; // int 3; ret

    ptid_t ptid = pid_to_ptid (proc_info->pid);

    proc_info->solib_event->address = address;

    proc_set_memory (ptid, &opcode, 3, address);
/**/
    set_breakpoint (proc_info->solib_event, address);
}


/* Creates solib event handler. Return false on errors. */
int 
enable_solib_event ()
{
    asection *interp_sect;
    int interp_sect_size;
    void *buf;
    bfd *tmp_bfd;
    struct link_map *inferior_sos;
    int load_addr, sym_addr, rdebug_addr;
    char **bkpt_namep;

    thread_info_t thread_info;

//    ptid_t ptid = pid_to_ptid (proc_info->pid);
    thread_info = proc_info->last_signaled_thread;//find_thread_info (proc_info->pid);

    bfd * debugee_bfd = proc_info->debugee_bfd;


    interp_sect = bfd_get_section_by_name (debugee_bfd, ".interp");
    if (interp_sect == 0) {
        error ("failed to find .interp section of ");
        return 0;
    }

    set_breakpoint (proc_info->entry_point, bfd_get_start_address (debugee_bfd));

    // Read the contents of the .interp section into a local buffer;
    // the contents specify the dynamic linker this program uses.
    interp_sect_size = bfd_section_size (debugee_bfd, interp_sect);
    buf = alloca (interp_sect_size);
    bfd_get_section_contents (debugee_bfd, interp_sect,
                          buf, 0, interp_sect_size);

    // Now we need to figure out where the dynamic linker was
    // loaded so that we can load its symbols and place a breakpoint
    // in the dynamic linker itself.

    //  info ("bfd_openr(%s)", buf);
    tmp_bfd = bfd_openr ((char*)buf, 0);//gnutarget);

    if (tmp_bfd == 0) {
        error ("failed to open interp '%s'", buf);
        return 0;
    }

    // Make sure the dynamic linker's really a useful object.
    if (!bfd_check_format (tmp_bfd, bfd_object)) {
        bfd_close (tmp_bfd);
        error ("bad interp format '%s'", buf);
        return 0;
    }

    // We find the dynamic linker's base address by examining
    // the current pc (which should point at the entry point for the
    // dynamic linker) and subtracting the offset of the entry point.
    // info ("proc=%#x\n", proc);
    load_addr = proc_get_pc (thread_info) - tmp_bfd->start_address;

    // Now try to set a breakpoint in the dynamic linker.
    for (bkpt_namep = solib_break_names; *bkpt_namep != 0; bkpt_namep++) {
        sym_addr = bfd_lookup_symbol (tmp_bfd, *bkpt_namep);
        if (sym_addr != 0) {
            break;
        }
    }

    // Now try to set a breakpoint in the dynamic linker.
    for (bkpt_namep = rdebug_names; *bkpt_namep != 0; bkpt_namep++) {
        rdebug_addr = bfd_lookup_symbol (tmp_bfd, *bkpt_namep);
        if (rdebug_addr != 0) {
            break;
        }
    }

    // We're done with the temporary bfd.
    bfd_close (tmp_bfd);

    if (sym_addr == 0 || rdebug_addr == 0) {
        error ("failed to find _dl_debug_state or _r_debug");
        return 0;
    }

    create_solib_event_breakpoint (load_addr + sym_addr);
    proc_info->rdebug = (CORE_ADDR) (load_addr + rdebug_addr);

    return 1;
}


/* Removes solib event handler. Return false on errors. */
/*
int 
disable_solib_event ()
{
    return 0;
}
*/

/* Returns true if given process is stopped by the solib event handler. */
int 
check_solib_event (thread_info_t thread_info)
{
    CORE_ADDR pc = (CORE_ADDR)proc_get_pc (thread_info) - BREAKPOINT_INSTR_LEN;
    return check_solib_event2 (pc);
}

int
check_solib_event2 (CORE_ADDR address)
{
    return proc_info->solib_event->address == address;
}


solib_t *
find_solib_if (xbool (*comp)(solib_t*, void*), void * data)
{
    list_head_t * pos;
    solib_t * solib;

    list_for_each (pos, &proc_info->loaded_solib) {
        solib = list_entry (pos, solib_t, list);

        if (comp(solib, data))
            return solib;
    }
    return 0;
}


/* Finds proper solib_t object that correspond to given values among registered. */
solib_t *
find_solib_entry (const char *solib_name, CORE_ADDR solib_base)
{
    list_head_t * pos;
    solib_t * solib;

    list_for_each (pos, &proc_info->loaded_solib) {

        solib = list_entry (pos, solib_t, list);

        if ((solib->base == solib_base) &&
            (strcmp (solib->name, solib_name) == 0))
        {
            return solib;
        }
    }
    return 0;
}

/* Destroys given solib object. */
static void 
free_solib (solib_t * solib)
{
    if (solib == 0) {
        return;
    }

    ASSERT (solib->name != 0);

    free (solib->name);

    if (solib->abfd) {
        bfd_close (solib->abfd);
    }
    free (solib);
}


/* Creates proper solib_t object that corresponds to given values and 
   registers it. */
solib_t *
create_solib_entry (const char *solib_name, CORE_ADDR solib_base)
{
    solib_t * solib = (solib_t *)malloc (sizeof (solib_t));

    solib->name = strdup (solib_name);
    solib->base = solib_base;

    solib->abfd = bfd_openr (solib_name, 0);//gnutarget);

    if (solib->abfd == 0) {
        error ("bfd_openr (%s) failed\n", solib_name);

    } else if (!bfd_check_format (solib->abfd, bfd_object)) {
        bfd_close (solib->abfd);
        error ("bad format (%s)\n", solib_name);
        solib->abfd = 0;
    }

    list_add_tail (&solib->list, &proc_info->loaded_solib);
    
    register_cleanup (PCLEANUP, (cleanup_func_cb)free_solib, solib);

    return solib;
}


void
print_solib_list ()
{
    thread_info_t thread_info = proc_info->main_thread;

    struct r_debug  target_r_debug;
    struct link_map target_r_map, *cur_lm;

    char buffer [256];
    proc_get_memory (thread_info, proc_info->rdebug, sizeof (target_r_debug), &target_r_debug);
    cur_lm = target_r_debug.r_map;

info ("print_solib_list: rdebug=%#x", proc_info->rdebug);
    while (cur_lm) {
        proc_get_memory (thread_info, (CORE_ADDR)cur_lm, sizeof (target_r_map), &target_r_map);

        buffer [sizeof (buffer)-1] = 0;
        proc_get_memory (thread_info, (CORE_ADDR)target_r_map.l_name, sizeof (buffer)-1, buffer);

        if (buffer [0] == 0) {
            // empty name means the initial program
            strcpy (buffer, proc_info->debugee_name);
        }
info ("print_solib_list: see library '%s'", buffer);
        cur_lm = target_r_map.l_next;
    }
info ("print_solib_list: ");
}


/* Updates proc_info's list of handled solibs.
   As a result the [out]solib_t would contain either 0 or proper solib entry.
   Returns 0 if the list consists with the process' dynamic linker state.
   Returns 1 if the solib entry was added to the list (solib loaded).
   Returns 2 if the solib entry was removed from the list (solib unloaded). */
int 
update_solib_list (solib_t ** result_solib, thread_info_t thread_info)
{
    solib_t * solib;
    struct list_head * pos;

    struct r_debug  target_r_debug;
    struct link_map target_r_map, *cur_lm;

    char buffer [256];
    xbool main_solib;

    list_for_each (pos, &proc_info->loaded_solib) {
        solib = list_entry (pos, solib_t, list);
        solib->checked = xfalse;
    }

    proc_get_memory (thread_info, proc_info->rdebug, sizeof (target_r_debug), &target_r_debug);
    cur_lm = target_r_debug.r_map;

    while (cur_lm) {
        proc_get_memory (thread_info, (CORE_ADDR)cur_lm, sizeof (target_r_map), &target_r_map);

        buffer [sizeof (buffer)-1] = 0;
        proc_get_memory (thread_info, (CORE_ADDR)target_r_map.l_name, sizeof (buffer)-1, buffer);

        main_solib = xfalse;
        if (buffer [0] == 0) {
            // empty name means the initial program
            strcpy (buffer, proc_info->debugee_name);
            main_solib = xtrue;
        }

        solib = find_solib_entry (buffer, (CORE_ADDR)target_r_map.l_addr);
        
        if (solib == 0) {
            // found unregistered solib
            solib = create_solib_entry (buffer, (CORE_ADDR)target_r_map.l_addr);
            solib->main_solib = main_solib;

//            solib->hfile = fopen (solib->name, "r");
            // solib loaded
//info ("update_solib_list: '%s' loaded", solib->name);
            *result_solib = solib;
            return 1;
        }
        solib->checked = xtrue;

        cur_lm = target_r_map.l_next;
    }

    list_for_each (pos, &proc_info->loaded_solib) {
        solib = list_entry (pos, solib_t, list); 

        if (!solib->checked) {
            list_del (&solib->list);
            *result_solib = solib;

            // solib unloaded
//info ("update_solib_list: '%s' unloaded", solib->name);
            return 2;
        }
    }

    // consistent
    *result_solib = 0;
//info ("update_solib_list: consistent");
    return 0;
}


