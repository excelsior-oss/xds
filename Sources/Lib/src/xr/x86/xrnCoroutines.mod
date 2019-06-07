(* Copyright (c) XDS Ltd. 1995,97,98.  All Rights Reserved *)
<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*+ M2EXTENSIONS*>
<*- gendebug    *>
IMPLEMENTATION MODULE xrnCoroutines;

IMPORT
   SYSTEM
  ,xmRTS
  ,M2EXCEPTION
  ,xrnsetjmp
  ,lib:=xrtsOS
<* IF multithread THEN *>
  ,xrThreads
<* END *>
  ;

CONST
  FRAME_ALIGMENT = 4;
  MAGIC = 67CAH;
  ABORT_EXIT_CODE = 9;

TYPE
  X2C_Coroutine = xmRTS.X2C_Coroutine;
  ADDRESS       = SYSTEM.ADDRESS;

VAR
  IntWaiting  :ARRAY [0..255] OF X2C_Coroutine;
  Current_str :xmRTS.X2C_Coroutine_STR;
  ring        :xmRTS.X2C_Coroutine;
<* IF multithread THEN *>
  list_lock   :xmRTS.X2C_MUTEX;
<* END *>

<* IF multithread THEN *>

PROCEDURE ["C"] X2C_LockCoroutines;
BEGIN
  xmRTS.X2C_LockMutex(list_lock);
END X2C_LockCoroutines;

PROCEDURE ["C"] X2C_UnlockCoroutines;
BEGIN
  xmRTS.X2C_FreeMutex(list_lock);
END X2C_UnlockCoroutines;

<* END *>

PROCEDURE ["C"] restoreHandlers;
  VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO 255 DO lib.X2C_RestoreIptHandler(i) END;
END restoreHandlers;

PROCEDURE [2] X2C_TRANSFER(VAR from: X2C_Coroutine; to: X2C_Coroutine);
BEGIN
  from:=xmRTS.X2C_GetCurrent();
  IF (from = to) THEN RETURN END;
  IF (to=NIL) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.invalidLocation)) END;
  IF (from^.magic # MAGIC) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  IF (to^.magic # MAGIC) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  lib.X2C_DisableIpts();
  from^.stk_end:=lib.X2C_StackTop();
  IF (xrnsetjmp.X2C_setjmp(from^.buf) = 0) THEN
    xmRTS.X2C_SetCurrent(to);
    xrnsetjmp.X2C_longjmp (to^.buf, 1);
  END;
  to:=xmRTS.X2C_GetCurrent();
  to^.stk_end:=NIL;
  IF (to^.prot # UNINTERRUPTIBLE) THEN lib.X2C_EnableIpts() END;
END X2C_TRANSFER;

PROCEDURE [2] X2C_SYNC_STATE();
VAR from: X2C_Coroutine;
BEGIN
  from:=xmRTS.X2C_GetCurrent();
  IF (from^.magic # MAGIC) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  from^.stk_end := lib.X2C_StackTop();
  xrnsetjmp.X2C_setjmp(from^.buf);
END X2C_SYNC_STATE;

PROCEDURE [2] X2C_IOTRANSFER(VAR from: X2C_Coroutine; to: X2C_Coroutine);
  VAR vec: INTEGER;
BEGIN
  from:=xmRTS.X2C_GetCurrent();
  vec:=from^.int_no;
  IF from=to THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  IF (to=NIL) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.invalidLocation)) END;
  IF (from^.magic # MAGIC) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  IF (to^.magic # MAGIC) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  lib.X2C_DisableIpts();
  from^.stk_end:=lib.X2C_StackTop();
  IF (xrnsetjmp.X2C_setjmp(from^.buf)=0) THEN
    IntWaiting[vec]:=from;
    lib.X2C_SetIptHandler(vec);
    xmRTS.X2C_SetCurrent(to);
    xrnsetjmp.X2C_longjmp(to^.buf,1);
  END;
  lib.X2C_RestoreIptHandler(vec);
  from:=xmRTS.X2C_GetCurrent();
  to:=IntWaiting[vec];
  xmRTS.X2C_SetCurrent(to);
  IntWaiting[vec]:=NIL;
  to^.stk_end:=NIL;
  IF to^.prot#UNINTERRUPTIBLE THEN lib.X2C_EnableIpts() END;
END X2C_IOTRANSFER;

PROCEDURE [2] X2C_INT_HANDLER(no: CARDINAL);
  VAR current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  current^.stk_end:=lib.X2C_StackTop();
  IF xrnsetjmp.X2C_setjmp(current^.buf)#0 THEN
    xrnsetjmp.X2C_longjmp(IntWaiting[no]^.buf, 1);
  END;
  current^.stk_end:=NIL;
END X2C_INT_HANDLER;

PROCEDURE [2] X2C_PROTECT(VAR o: PROTECTION; n: PROTECTION);
  VAR current: xmRTS.X2C_Coroutine;
BEGIN
  lib.X2C_DisableIpts();
  current:=xmRTS.X2C_GetCurrent();
  o:=current^.prot;
  current^.prot:=n;
  IF n#UNINTERRUPTIBLE THEN lib.X2C_EnableIpts() END;
END X2C_PROTECT;

PROCEDURE startCoroutines;
  VAR current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  current^.stk_end:=NIL;
  current^.proc();
  xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException));
END startCoroutines;

PROCEDURE ["C"] X2C_InitCoroutine(p: X2C_Coroutine; stk: ADDRESS);
BEGIN
  p^.magic:=MAGIC;
  p^.stk_start:=stk;
  p^.stk_end:=NIL;
  p^.handler:=NIL;
  p^.prot:=INTERRUPTIBLE;
  p^.proc:=NIL;
  p^.int_no:=-1;
  p^.his_cnt:=0;
  p^.reg_dsize:=0;
  p^.his_msg[0]:=0C;
END X2C_InitCoroutine;

PROCEDURE TieCoroutine(p: X2C_Coroutine);
BEGIN
  IF ring=NIL THEN
    p^.fwd:=p; p^.bck:=p; ring:=p;
  ELSE
    p^.fwd:=ring;
    p^.bck:=ring^.bck;
    p^.fwd^.bck:=p; p^.bck^.fwd:=p;
  END;
END TieCoroutine;

PROCEDURE UntieCoroutine(p: X2C_Coroutine);
BEGIN
  IF p=ring THEN
    ring:=ring^.fwd;
    IF ring=p THEN ring:=NIL END;
  END;
  IF ring#NIL THEN
    p^.fwd^.bck:=p^.bck;
    p^.bck^.fwd:=p^.fwd;
    p^.fwd:=NIL; p^.bck:=NIL;
  END;
END UntieCoroutine;

PROCEDURE ["C"] X2C_RegisterCoroutine(c: X2C_Coroutine);
BEGIN
  <* IF multithread THEN *> X2C_LockCoroutines; <* END *>
    TieCoroutine(c);
  <* IF multithread THEN *> X2C_UnlockCoroutines; <* END *>
END X2C_RegisterCoroutine;

PROCEDURE ["C"] X2C_UnregisterCoroutine(c: X2C_Coroutine);
BEGIN
  <* IF multithread THEN *> X2C_LockCoroutines; <* END *>
    UntieCoroutine(c);
  <* IF multithread THEN *> X2C_UnlockCoroutines; <* END *>
END X2C_UnregisterCoroutine;

PROCEDURE ["C"] X2C_CopyJmpBuf ( c :X2C_Coroutine );
BEGIN
  IF (xmRTS.X2C_REGSIZE*SIZE(SYSTEM.CARD32) < SIZE(xrnsetjmp.X2C_jmp_buf) ) THEN
    xmRTS.X2C_ASSERT_F(100H);
  END;
    
  <* IF (env_target="x86nt") OR (env_target="x86os2") OR (env_target="x86linux") THEN *>
    c^.reg_dsize := 6 * SIZE(CARDINAL); -- ebx,ecx,edx,esi,edi,ebp (see xrnsetjmp.def)
  <* ELSE *>
    c^.reg_dsize := SIZE(xrnsetjmp.X2C_jmp_buf);
  <* END *>

  SYSTEM.MOVE ( SYSTEM.ADR(c^.buf), SYSTEM.ADR(c^.reg_dump), SIZE(xrnsetjmp.X2C_jmp_buf) );
END X2C_CopyJmpBuf;

----

PROCEDURE IniAlignment(VAR wsp: ADDRESS; VAR size: CARDINAL);
  VAR inc: CARDINAL;
BEGIN
  IF (size<FRAME_ALIGMENT*2) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  inc:=SYSTEM.CAST(CARDINAL,wsp) MOD FRAME_ALIGMENT;
  inc:=(FRAME_ALIGMENT-inc)  MOD FRAME_ALIGMENT;
  wsp:=SYSTEM.ADDADR(wsp,inc);
  size:=size-inc;
  size:=size-(size MOD FRAME_ALIGMENT);
END IniAlignment;

PROCEDURE IniCoroutine ( wsp :ADDRESS; size :CARDINAL ) :X2C_Coroutine;
VAR
  p           :X2C_Coroutine;
  szOfCParams :CARDINAL;
BEGIN
  szOfCParams := SYSTEM.CAST(CARDINAL, lib.X2C_StackTop());
  p  := wsp;
  IF (size < SIZE(xmRTS.X2C_Coroutine_STR)) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  DEC (size, SIZE(xmRTS.X2C_Coroutine_STR) );
  wsp := SYSTEM.ADDADR(wsp,SIZE(xmRTS.X2C_Coroutine_STR));
  X2C_InitCoroutine (p, NIL);
  X2C_RegisterCoroutine (p);
  szOfCParams :=  szOfCParams - SYSTEM.CAST(CARDINAL, lib.X2C_StackTop()) + 4;

  (* this is a number of bytes which will be purchased from the stack right
     after a "return" from longjmp - "setjmp" is the last "C" procedure in this
     block so it's a very subtle effect - DO NOT CHANGE *)

  IF (size < szOfCParams+32) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
  IF (xrnsetjmp.X2C_setjmp(p^.buf) # 0) THEN
    startCoroutines;
    (* from startCoroutines the control will never be returned so
       a coroutine must contain an infinite loop ( otherwise an exception will
       be raised ) *)
  END;

  IniAlignment( wsp, size );

  p^.stk_start := wsp+size;
  p^.stk_end   := p^.stk_start;
  p^.buf.esp   := SYSTEM.CAST(CARDINAL, p^.stk_start) - szOfCParams;
  p^.buf.ss    := p^.buf.ds;

  RETURN p;
END IniCoroutine;


---

PROCEDURE ["C"] X2C_NEWPROCESS ( proc      :PROC;
                                 lspace    :ADDRESS;
                                 lsize     :CARDINAL;
                                 prot      :PROTECTION;
                             VAR _this_prs :X2C_Coroutine );
VAR
  size  :CARDINAL;
  space :ADDRESS;
BEGIN
  size  := lsize;
  space := lspace;
  IF (space = NIL) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.invalidLocation)) END;
  _this_prs := IniCoroutine(space,size);
  _this_prs^.prot := prot;
  _this_prs^.proc := proc;
  IF (_this_prs^.magic#MAGIC) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException)) END;
END X2C_NEWPROCESS;

PROCEDURE ["C"] X2C_ini_coroutines (sstart :ADDRESS);
  VAR
    i       :CARDINAL;
    current :xmRTS.X2C_Coroutine;
    res     :SYSTEM.INT32;
BEGIN
  <* IF multithread THEN *>
    res:=xrThreads.X2C_InitThreads();
    IF res#0 THEN
      lib.X2C_StdOut("#RTS: Error ",12);
      lib.X2C_StdOutD(res,0);
      lib.X2C_StdOut(" initializing threads",21);
      lib.X2C_StdOutN;
      lib.X2C_StdOutFlush;
      HALT(res);
    END;
  <* END *>
  xmRTS.X2C_SetCurrent(NIL);
  ring:=NIL;
  current:=SYSTEM.ADR(Current_str);
  X2C_InitCoroutine(current,sstart);
  TieCoroutine(current);
  xmRTS.X2C_SetCurrent(current);
  current^.stk_end:=NIL;
  lib.X2C_atexit(restoreHandlers);
  <* IF multithread THEN *>
    xmRTS.X2C_NewMutex(list_lock);
  <* END *>
END X2C_ini_coroutines;


PROCEDURE ["C"] X2C_reg_stackbotm( stk :SYSTEM.ADDRESS );
VAR
  cc :xmRTS.X2C_Coroutine;
BEGIN
  (* ASSUME : main coroutine is already initialized *)

  cc := xmRTS.X2C_GetCurrent();
  IF ( CARDINAL(stk) > CARDINAL(cc^.stk_start) ) THEN
    cc^.stk_start := stk;
  END;
  (* this proc resolves the following problem: the system (NT)  
     calls init parts of all DLLs after that purges a large stack frame
     ( about 5K ) and finally gives the control back to the EXE entry
     point. If the XDS RT library was initialized by one of those DLLs,
     the registered stack bottom may be wrong - VitVit
   *) 
END X2C_reg_stackbotm;


END xrnCoroutines.
