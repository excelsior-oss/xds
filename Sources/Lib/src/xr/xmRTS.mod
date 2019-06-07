<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
<*+ m2extensions*>
IMPLEMENTATION MODULE xmRTS; (*  08-15-95 04:46pm *)

IMPORT SYSTEM;

<* IF NOT __GEN_C__ THEN *>
IMPORT xrMM;
<* END *>

TYPE X2C_MUTEX = SYSTEM.ADDRESS;

PROCEDURE ["C"] X2C_INIT_RTS;
BEGIN
  (* Intentionally empty. Nothing to initialize *)
END X2C_INIT_RTS;

<* IF __GEN_C__ THEN *>
PROCEDURE ["C"] / X2C_MODULEXE (md: X2C_MD; hnd: SYSTEM.ADDRESS);

PROCEDURE ["C"] X2C_MODULE(md: X2C_MD);
(* Registry of starting Oberon-2 module. C,X86 *)
BEGIN
  X2C_MODULEXE (md, NIL)
END X2C_MODULE;

<* ELSE *>

PROCEDURE ["C"] X2C_FINALIZE_RT ();
BEGIN
  xrMM.X2C_MemoryManager_exit ();
END X2C_FINALIZE_RT;

<* END *>

END xmRTS.
