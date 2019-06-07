(* Copyright (c) 1999-2002 Excelsior, LLC. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
(*
  Do not enable dynamic checks!
  Infinite recursion in X2C_PROC_INP & X2C_PROC_OUT
*)
<*- GENDEBUG     *>
<*- CHECKINDEX   *>
<*- CHECKNIL     *>
<*- CHECKSET     *>
<*- CHECKRANGE   *>
<*- CHECKPROC    *>
<*- iOVERFLOW    *>

IMPLEMENTATION MODULE TERMINATION;
(** Sem 22-Sep-94, VitVit 31-Jan-97. *)

IMPORT
  SYSTEM,
  X2C,
  RTS := xmRTS,
  OS  := xrtsOS;

FROM SYSTEM IMPORT ADDRESS, CAST;


VAR
  is_terminating :BOOLEAN;
  has_halted     :BOOLEAN;

PROCEDURE IsTerminating(): BOOLEAN;
BEGIN
  RETURN is_terminating;
END IsTerminating;

PROCEDURE HasHalted(): BOOLEAN;
BEGIN
  RETURN has_halted;
END HasHalted;


(* use you favorite exit code here *)
CONST
  ABORT_EXIT_CODE = 9;

(* procedure is declared in xmRTS.d *)
PROCEDURE ["C"] X2C_HALT* ( x :SYSTEM.INT32 ); (* operator HALT(n) *)
BEGIN
  has_halted := TRUE;
  OS.X2C_doexit(x);
END X2C_HALT;

(* procedure is declared in xmRTS.d *)
PROCEDURE ["C"] X2C_ABORT*; (* operator HALT *)
BEGIN
  X2C_HALT(ABORT_EXIT_CODE);
END X2C_ABORT;




TYPE
  Final = POINTER TO FinalRec;

  FinalRec = RECORD
               proc :PROC;
               next :Final;
             END;

VAR
  finals :Final; (*  EXE FINALLY list head *)


(* Modification ( VitVit 7.5.97 )

  Because of each xc-generated DLL may contain its own FINALLY list,
  all of them have an instance of the "finals" variable which defined
  in its startup code and set in zero (NIL)
  ( see xstartd.asm ).
*)


PROCEDURE do_final ( p :PROC );
BEGIN
  p();
EXCEPT
  RETURN;
  (* last finally part can terminate abnormally *)
END do_final;


PROCEDURE ["C"] atexit_procedure;
VAR
  p :PROC;
  f :Final;
BEGIN
  is_terminating := TRUE;

  WHILE ( finals # NIL) DO
    f           := finals;
    finals      := f^.next;
    p           := f^.proc;
    do_final(p);
  END;

  RTS.X2C_show_profile;
END atexit_procedure;

------------------------------------------


(* register final procedure for EXE *)

PROCEDURE ["C"] X2C_FINALEXE* ( proc :PROC );
VAR
  f :Final;
BEGIN
  f := OS.X2C_AllocMem(SIZE(f^));
  IF ( f=NIL ) THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  f^.proc := proc;
  f^.next := finals;
  finals  := f;
END X2C_FINALEXE;



(* register final procedure for DLL *)
PROCEDURE ["C"] X2C_FINALDLL* ( VAR finalHead :ADDRESS; proc :PROC );
VAR
  f :Final;
BEGIN
  f := OS.X2C_AllocMem(SIZE(f^));
  IF ( f=NIL ) THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  f^.proc   := proc;
  f^.next   := finalHead;
  finalHead := f;
END X2C_FINALDLL;


<* IF __GEN_C__ THEN *>
  PROCEDURE ["C"] X2C_FINALLY* ( proc :PROC );
  BEGIN
    X2C_FINALEXE( proc);
  END X2C_FINALLY;
<* END *>


(*
  execute EXE final routines which was registered by X2C_atexit
  ( The execution of an EXE final list is one of that routines
    wheareas in the DLL case that is only action at termination.
    This is the MAIN difference between EXE & DLL termination )   
*)

PROCEDURE ["C"] X2C_EXIT* (); (* RETURN from main module body *)
BEGIN
  OS.X2C_doexit (0);
END X2C_EXIT;



PROCEDURE do_finald ( p :PROC );
BEGIN
  p();
EXCEPT
  RETURN;
END do_finald;

(* execute DLL final proc list *)
PROCEDURE ["C"] X2C_EXITDLL* ( VAR finalHead :ADDRESS ); (* finalization of DLL module  *)
VAR
  p :PROC;
  f :Final;
BEGIN
  WHILE (finalHead # NIL) DO
    f          := CAST ( Final, finalHead );
    finalHead  := f^.next;
    p          := f^.proc;
    do_finald (p);
  END;
END X2C_EXITDLL;


PROCEDURE ["C"] X2C_ini_termination* ();
BEGIN
  is_terminating := FALSE;
  has_halted     := FALSE;

  (*
        RTS.X2C_ini_profiler;
                 profile:=NIL;
     total_time:=0; <- должно быть сделано до инициализации xmm !!!!
  *)
  OS.X2C_iniexit;
  OS.X2C_atexit ( atexit_procedure ); -- register the procedure
END X2C_ini_termination;

END TERMINATION.

