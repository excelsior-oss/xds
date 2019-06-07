/**
 * The "proc_service.h" implementation.
 * The pthread_db library connects to debugger through this interface.
 */

#include "proc-service.h"
#include "inferior.h"
#include "linux-low.h"
#include "thread.h"

#include <signal.h>


/* Stop the target process PH.  */
ps_err_e
ps_pstop (const struct ps_prochandle * ph)
{
    //info ("%s called", __FUNCTION__);

    // Here we must stop the process (including all threads).
    int res = kill (ph->proc_info->pid, SIGSTOP);

    return res ? PS_ERR : PS_OK;
}

/* Resume the target process PH.  */

ps_err_e
ps_pcontinue (const struct ps_prochandle * ph)
{
    //info ("%s called", __FUNCTION__);
    // Pretend we did successfully continue the process.
    return PS_OK;
}

/* Stop the lightweight process LWPID within the target process PH.  */

ps_err_e
ps_lstop (const struct ps_prochandle * ph, lwpid_t lwpid)
{
    // Here we must stop given lightweight process.
    thread_info_t thread_info;

    //info ("%s called", __FUNCTION__);

    thread_info = find_thread_info_entry (lwpid);
    ASSERT (thread_info != 0);

    int res = kill (lwpid, SIGSTOP);

    return res ? PS_ERR : PS_OK;
}

/* Resume the lightweight process (LWP) LWPID within the target
   process PH.  */

ps_err_e
ps_lcontinue (const struct ps_prochandle * ph, lwpid_t lwpid)
{
    //info ("%s called", __FUNCTION__);
    // Pretend we did successfully continue LWPID.  GDB will take care
    // of it later on.
//    int target_pid = lwpid ? lwpid : ph->pid;
//    int res = kill (target_pid, 
    return PS_OK;
}

/* Get the size of the architecture-dependent extra state registers
   for LWP LWPID within the target process PH and return it in
   *XREGSIZE.  */

ps_err_e
ps_lgetxregsize (struct ps_prochandle * ph, lwpid_t lwpid, int *xregsize)
{
  //info ("%s called", __FUNCTION__);
  // FIXME: Not supported yet.
  return PS_OK;
}

/* Get the extra state registers of LWP LWPID within the target
   process PH and store them in XREGSET.  */

ps_err_e
ps_lgetxregs (struct ps_prochandle * ph, lwpid_t lwpid, caddr_t xregset)
{
    //info ("%s called", __FUNCTION__);
    // FIXME: Not supported yet.
    return PS_OK;
}

/* Set the extra state registers of LWP LWPID within the target
   process PH from XREGSET.  */

ps_err_e
ps_lsetxregs (struct ps_prochandle * ph, lwpid_t lwpid, caddr_t xregset)
{
    //info ("%s called", __FUNCTION__);
    // FIXME: Not supported yet.
    return PS_OK;
}

/* Log (additional) diognostic information.  */

void
ps_plog (const char *fmt, ...)
{
    //info ("%s called", __FUNCTION__);
//  va_list args;

//  va_start (args, fmt);
//  vfprintf (stderr, fmt, args);
}

/* Search for the symbol named NAME within the object named OBJ within
   the target process PH.  If the symbol is found the address of the
   symbol is stored in SYM_ADDR.  */

ps_err_e
ps_pglobal_lookup (struct ps_prochandle * ph, const char *obj,
                   const char *name, psaddr_t *sym_addr)
{
    list_head_t * head = &proc_info->loaded_solib;
    solib_t * last_loaded_solib;
    CORE_ADDR symaddr;
    bfd * abfd = 0;

//    info ("%s (%#x, %s, %s) called", __FUNCTION__, ph, obj, name);

    // check last loaded solib symbols
    if (head->prev == head) {
        error ("solib list is empty");
        return PS_NOSYM;
    }
    last_loaded_solib = list_entry (head->prev, solib_t, list);
/*
    if (last_loaded_solib->l_name[0]) {
        abfd = last_loaded_solib->abfd;
    } else {
        abfd = ph->exec_bfd;
    }
*/
    abfd = last_loaded_solib->abfd;

    if (abfd == 0) {
        error ("abfd was not initialized for (%s)", last_loaded_solib->name);
        return PS_NOSYM;
    }

    // try to lookup symbol
    symaddr = bfd_lookup_symbol (last_loaded_solib->abfd, name);

    if (symaddr == 0) {
        return PS_NOSYM;
    }
    *sym_addr = (psaddr_t) (symaddr + last_loaded_solib->base);

//    info ("Symbol Found (%#x) !!!", *sym_addr);
    return PS_OK;
}

/* Read SIZE bytes from the target process PH at address ADDR and copy
   them into BUF.  */

ps_err_e
ps_pdread (struct ps_prochandle * ph, psaddr_t addr,
           void* buf, size_t size)
{
/*
    pid_t pid = ph->pid;
    int res;
    info ("%s (ph=%#x, addr=%#x, size=%d) called", __FUNCTION__, ph, addr, size);
    if (pid == 0) {
        warning ("%s, ph->pid == 0", __FUNCTION__);
        pid = proc_info->pid;
    }
    res = proc_get_memory (pid_to_ptid (pid), (CORE_ADDR)addr, size, buf);
    return res ? PS_OK : PS_ERR;
*/
    int res;
//    info ("%s (ph=%#x, addr=%#x, size=%d) called", __FUNCTION__, ph, addr, size);
    res = proc_get_memory (ph->proc_info->main_thread, (CORE_ADDR)addr, size, buf);
//    res = proc_get_memory (proc_info->last_ptid, (CORE_ADDR)addr, size, buf);
    return res ? PS_OK : PS_ERR;

}

/* Write SIZE bytes from BUF into the target process PH at address ADDR.  */

ps_err_e
ps_pdwrite (struct ps_prochandle * ph, psaddr_t addr,
            const void * buf, size_t size)
{
/*
    pid_t pid = ph->pid;
    int res;
    info ("%s (addr=%#x, size=%d) called", __FUNCTION__, addr, size);
    if (pid == 0) {
        warning ("%s, ph->pid == 0", __FUNCTION__);
        pid = proc_info->pid;
    }
    res = proc_set_memory (pid_to_ptid (pid), buf, size, (CORE_ADDR)addr);
    return res ? PS_OK : PS_ERR;
*/
    int res;
//    info ("%s (addr=%#x, size=%d) called", __FUNCTION__, addr, size);
    res = proc_set_memory (ph->proc_info->main_thread, buf, size, (CORE_ADDR)addr);
//    res = proc_set_memory (proc_info->last_ptid, buf, size, (CORE_ADDR)addr);
    return res ? PS_OK : PS_ERR;
}

/* Read SIZE bytes from the target process PH at address ADDR and copy
   them into BUF.  */

ps_err_e
ps_ptread (struct ps_prochandle * ph, psaddr_t addr,
           void* buf, size_t size)
{
    //info ("%s called", __FUNCTION__);
    int res = target_get_memory (ph->proc_info->pid, (CORE_ADDR)addr, size, buf);
    return res ? PS_OK : PS_ERR;
}

/* Write SIZE bytes from BUF into the target process PH at address ADDR.  */

ps_err_e
ps_ptwrite (struct ps_prochandle * ph, psaddr_t addr,
            const void* buf, size_t size)
{
    //info ("%s called", __FUNCTION__);
    int res = target_set_memory (ph->proc_info->pid, buf, size, (CORE_ADDR)addr);
    return res ? PS_OK : PS_ERR;
}

/* Get the general registers of LWP LWPID within the target process PH
   and store them in GREGSET.  */

ps_err_e
ps_lgetregs (struct ps_prochandle * ph, lwpid_t lwpid, prgregset_t gregset)
{
    //info ("%s called", __FUNCTION__);
    int res = target_get_regs (lwpid, (long*)gregset);
    return res ? PS_OK : PS_ERR;
}

/* Set the general registers of LWP LWPID within the target process PH
   from GREGSET.  */

ps_err_e
ps_lsetregs (struct ps_prochandle * ph, lwpid_t lwpid, const prgregset_t gregset)
{
    //info ("%s called", __FUNCTION__);
    int res = target_set_regs (lwpid, (long*)gregset);
    return res ? PS_OK : PS_ERR;
}

/* Get the floating-point registers of LWP LWPID within the target
   process PH and store them in FPREGSET.  */

ps_err_e
ps_lgetfpregs (struct ps_prochandle * ph, lwpid_t lwpid,
               prfpregset_t *fpregset)
{
    //info ("%s called", __FUNCTION__);
    return PS_OK;
}

/* Set the floating-point registers of LWP LWPID within the target
   process PH from FPREGSET.  */

ps_err_e
ps_lsetfpregs (struct ps_prochandle * ph, lwpid_t lwpid,
               const prfpregset_t *fpregset)
{
    //info ("%s called", __FUNCTION__);
    return PS_OK;
}

/* Return overall process id of the target PH.  Special for GNU/Linux
   -- not used on Solaris.  */

pid_t
ps_getpid (struct ps_prochandle * ph)
{
    pid_t pid = ph->proc_info->pid;
    return pid;
}

void
_initialize_proc_service (void)
{
    //info ("%s called", __FUNCTION__);
    // This function solely exists to make sure this module is linked
    //   into the final binary.
}
