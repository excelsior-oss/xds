(* Copyright (c) 1994,95 xTech Ltd, Russia. All Rights Reserved. *)
(*
 * XDS runtime library
 *
 *)
<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*+ M2EXTENSIONS*>
<*- GENDEBUG    *>
IMPLEMENTATION MODULE xrExceptions; (*  08-22-95 11:13am *)

IMPORT
   SYSTEM
  ,xmRTS
  ,setjmp := 
<* IF __GEN_C__ THEN *> xrsetjmp <* ELSE *> xrnsetjmp <* END *>
  ,X2C
  ,xrtsOS
  ,M2EXCEPTION
  <* IF (env_target = "x86nt") OR (env_target = "x86linux") THEN *>
  ,xrHistory
  <* END *>
  ;

CONST
  st_normal=xmRTS.X2C_st_normal;
  st_exceptional=xmRTS.X2C_st_exceptional;
  st_off=xmRTS.X2C_st_off;
  st_reraise=xmRTS.X2C_st_reraise;

PROCEDURE [2] X2C_XInitHandler*(x: xmRTS.X2C_XHandler);
  VAR current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  x^.state:=st_normal;
  x^.source:=NIL;
  x^.next:=current^.handler;
<* IF ~__GEN_X86__ THEN *>
  xmRTS.X2C_HIS_SAVE(x^.history);
<* END *>
  current^.handler:=x;
END X2C_XInitHandler;

PROCEDURE [2] X2C_XOFF*;
  VAR current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  current^.handler^.state:=st_off;
END X2C_XOFF;

PROCEDURE [2] X2C_XON*;
  VAR current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  current^.handler^.state:=st_reraise;
END X2C_XON;

PROCEDURE [2] X2C_XRETRY*;
  VAR current: xmRTS.X2C_Coroutine;
      x: xmRTS.X2C_XHandler;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  x:=current^.handler;
  x^.state:=st_normal;
<* IF ~ __GEN_X86__ THEN *>
  xmRTS.X2C_HIS_RESTORE(x^.history);
<* END *>
<* IF multithread THEN *>
  IF x^.source#NIL THEN 
    xmRTS.X2C_FreeMutex(x^.source^.lock);
  END;
<* END *>
  x^.source:=NIL;
  setjmp.X2C_longjmp(x^.buf,1);
END X2C_XRETRY;

VAR
  sysSourceRec,assertSourceRec: xmRTS.X2C_XSource_STR;

PROCEDURE [2] X2C_init_exceptions*;
BEGIN
  xmRTS.X2C_rtsSource:=SYSTEM.ADR(sysSourceRec);
  WITH xmRTS.X2C_rtsSource^ DO
    number:=0;
    message[0]:=0C;
    <* IF multithread THEN *>
      xmRTS.X2C_NewMutex(lock);
    <* END *>
  END;
  xmRTS.X2C_assertSrc:=SYSTEM.ADR(assertSourceRec);
  WITH xmRTS.X2C_assertSrc^ DO
    number:=0;
    message[0]:=0C;
    <* IF multithread THEN *>
      xmRTS.X2C_NewMutex(lock);
    <* END *>
  END;
END X2C_init_exceptions;

PROCEDURE dectostr(VAR s: ARRAY OF CHAR; VAR pos: CARDINAL; no: SYSTEM.CARD32);
  VAR l,i: CARDINAL;
        x: SYSTEM.CARD32;
BEGIN
  l:=0; x:=no;
  WHILE x>0 DO x:=x DIV 10; INC(l) END;
  IF l=0 THEN l:=1 END;
  pos:=pos+l;
  i:=pos;
  WHILE l>0 DO
    DEC(i);
    s[i]:=CHR(ORD("0")+(no MOD 10));
    no:=no DIV 10;
    DEC(l)
  END;
END dectostr;

PROCEDURE doRaise(source: xmRTS.X2C_XSource);
  VAR x: xmRTS.X2C_XHandler;
  VAR current: xmRTS.X2C_Coroutine;

  PROCEDURE app(VAR d: ARRAY OF CHAR; M: SYSTEM.CARD32; VAR pos: SYSTEM.CARD32; s-: ARRAY OF CHAR);
    VAR i: CARDINAL;
  BEGIN
    i:=0;
    WHILE (pos<M) & (s[i]#0C) DO
      d[pos]:=s[i]; INC(pos); INC(i)
    END;
  END app;

  PROCEDURE form_msg(cur: xmRTS.X2C_Coroutine; source: xmRTS.X2C_XSource);
    VAR pos: SYSTEM.CARD32;
  BEGIN
    pos:=0;
    app(cur^.his_msg,xmRTS.X2C_MSG_LEN,pos,"#RTS: unhandled exception #");
    xrtsOS.X2C_DecToStr(cur^.his_msg,pos,source^.number);
    IF source^.message[0]#0C THEN
      app(cur^.his_msg,xmRTS.X2C_MSG_LEN,pos,": ");
      app(cur^.his_msg,xmRTS.X2C_MSG_LEN,pos,source^.message);
    END;
    IF pos>=xmRTS.X2C_MSG_LEN THEN pos:=xmRTS.X2C_MSG_LEN-1 END;
    cur^.his_msg[pos]:=0C;
  END form_msg;

BEGIN
  current:=xmRTS.X2C_GetCurrent();
  x:=current^.handler;
  WHILE (x#NIL) & (x^.state # st_normal) DO x:=x^.next END;
  current^.handler:=x;
  IF x=NIL THEN
    xrtsOS.X2C_StdOut("\n#RTS: unhandled exception #",28);
    xrtsOS.X2C_StdOutD(source^.number,0);
    IF source^.message[0]#0C THEN
      xrtsOS.X2C_StdOut(": ",2);
      xrtsOS.X2C_StdOut(source^.message,LENGTH(source^.message));
    END;
    xrtsOS.X2C_StdOutN;
    form_msg(current,source);
    xmRTS.X2C_show_history;
    xrtsOS.X2C_StdOutFlush;
    <* IF (env_target = "x86nt") OR (env_target = "x86linux") THEN *>
    xrHistory.X2C_PrintExceptionInfo ();
    <* END *>
    xmRTS.X2C_ABORT;
  ELSE
    x^.source:=source;
    x^.state:=st_exceptional;
  END;
  <* IF __GEN_C__ THEN *>
  xmRTS.X2C_HIS_RESTORE(x^.history);
  <* END *>
  setjmp.X2C_longjmp(x^.buf,2);
END doRaise;

PROCEDURE [2] X2C_doRaise(source: xmRTS.X2C_XSource);
  <* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
    VAR current: xmRTS.X2C_Coroutine;
  <* END *>
BEGIN
  <* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
    current:=xmRTS.X2C_GetCurrent();
    xmRTS.X2C_scanStackHistory(SYSTEM.SUBADR(SYSTEM.ADR(source),4),current^.stk_start,FALSE);
  <* END *>
  doRaise(source);
END X2C_doRaise;

PROCEDURE [2] X2C_XREMOVE*;
  VAR
    x: xmRTS.X2C_XHandler;
    s: xmRTS.X2C_XSource;
    current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  x:=current^.handler;
  IF x=NIL THEN xmRTS.X2C_TRAP_F(X2C.X2C_internalError) END;
  IF x^.state=st_reraise THEN
    s:=x^.source;
    current^.handler:=x^.next;
    doRaise(s);
  ELSE
    <* IF multithread THEN *>
    IF x^.source#NIL THEN 
      xmRTS.X2C_FreeMutex(x^.source^.lock);
    END; 
    <* END *>
    current^.handler:=x^.next;
  END;
END X2C_XREMOVE;

PROCEDURE trap_message(VAR msg: ARRAY OF CHAR; no: SYSTEM.CARD32);
BEGIN
  CASE no OF
    |ORD(M2EXCEPTION.indexException):
      COPY("invalid index",msg);
    |ORD(M2EXCEPTION.rangeException):
      COPY("expression out of bounds",msg);
    |ORD(M2EXCEPTION.caseSelectException):
      COPY("invalid case in CASE statement",msg);
    |ORD(M2EXCEPTION.invalidLocation):
      COPY("invalid location",msg);
    |ORD(M2EXCEPTION.functionException):
      COPY("function without RETURN statement",msg);
    |ORD(M2EXCEPTION.wholeValueException):
      COPY("whole overflow",msg);
    |ORD(M2EXCEPTION.wholeDivException):
      COPY("zero or negative divisor",msg);
    |ORD(M2EXCEPTION.realValueException):
      COPY("real overflow",msg);
    |ORD(M2EXCEPTION.realDivException):
      COPY("float division by zero",msg);
    |ORD(M2EXCEPTION.complexValueException):
      COPY("complex overflow",msg);
    |ORD(M2EXCEPTION.complexDivException):
      COPY("complex division by zero",msg);
    |ORD(M2EXCEPTION.protException):
      COPY("protection error",msg);
    |ORD(M2EXCEPTION.sysException):
      COPY("SYSTEM exception",msg);
    |ORD(M2EXCEPTION.coException):
      COPY("COROUTINE exception",msg);
    |ORD(M2EXCEPTION.exException):
      COPY("EXCEPTIONS exception",msg);
  ELSE
    (* Oberon-2 Exceptions *)
    IF no=X2C.X2C_assertException THEN
      COPY("ASSERT",msg);
    ELSIF no=X2C.X2C_guardException THEN
      COPY("type guard check",msg);

    (* RTS Exceptions *)
    ELSIF no=X2C.X2C_noMemoryException THEN
      COPY("out of heap space",msg);
    ELSIF no=X2C.X2C_unreachDLL THEN
      COPY("call to unloaded DLL",msg);
    ELSIF no=X2C.X2C_internalError THEN
      COPY("RTS internal error",msg);
    ELSIF no=X2C.X2C_castError THEN
      COPY("invalid type cast",msg);
    ELSIF no=X2C.X2C_UserBreak THEN
      COPY("USER BREAK",msg);
    ELSIF no=X2C.X2C_stack_overflow THEN
      COPY("stack overflow",msg);
    ELSE
      msg[0]:=0C;
    END;
  END;
END trap_message;

PROCEDURE [2] X2C_TRAP_F(no: SYSTEM.INT32);
  VAR pos: CARDINAL;
  current: xmRTS.X2C_Coroutine;
BEGIN
  <* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
  current:=xmRTS.X2C_GetCurrent();
  xmRTS.X2C_scanStackHistory(SYSTEM.SUBADR(SYSTEM.ADR(no),4),current^.stk_start,FALSE);
  <* END *>
  IF (xmRTS.X2C_rtsSource = NIL) THEN
    X2C_init_exceptions();
  END;
  <* IF multithread THEN *>
  xmRTS.X2C_LockMutex(xmRTS.X2C_rtsSource^.lock);
  <* END *>
  trap_message(xmRTS.X2C_rtsSource^.message,no);
  IF xmRTS.X2C_rtsSource^.message[0]=0C THEN
    WITH xmRTS.X2C_rtsSource^ DO
      COPY("TRAP(",message);
      pos:=5;
      dectostr(message,pos,no);
      message[pos]:=")";
      message[pos+1]:=0C;
    END;
  END;
  xmRTS.X2C_rtsSource^.number:=no;
  doRaise(xmRTS.X2C_rtsSource);
END X2C_TRAP_F;

PROCEDURE ["C"] append(VAR d: ARRAY OF CHAR; M: CARDINAL; VAR pos: CARDINAL; s: ARRAY OF CHAR);
  VAR i: CARDINAL;
BEGIN
  i:=0;
  WHILE (pos<M) & (s[i]#0C) DO
    d[pos]:=s[i]; INC(pos); INC(i);
  END;
END append;

<* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
PROCEDURE [2] X2C_TRAP_FC_IMPL(no: SYSTEM.INT32; file: xmRTS.X2C_pCHAR; line: SYSTEM.CARD32);
<* ELSE *>
PROCEDURE [2] X2C_TRAP_FC(no: SYSTEM.INT32; file: xmRTS.X2C_pCHAR; line: SYSTEM.CARD32);
<* END *>
  VAR pos: CARDINAL;
      ls: ARRAY [0..15] OF CHAR;
      current: xmRTS.X2C_Coroutine;
BEGIN
  <* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
  current:=xmRTS.X2C_GetCurrent();
  xmRTS.X2C_scanStackHistory(SYSTEM.SUBADR(SYSTEM.ADR(no),4),current^.stk_start,FALSE);
  <* END *>
  IF (xmRTS.X2C_rtsSource = NIL) THEN
    X2C_init_exceptions();
  END;
  <* IF multithread THEN *>
  xmRTS.X2C_LockMutex(xmRTS.X2C_rtsSource^.lock);
  <* END *>
  pos:=0;
  dectostr(ls,pos,line); ls[pos]:=0C; 
  trap_message(xmRTS.X2C_rtsSource^.message,no);
  pos:=0;
  WHILE xmRTS.X2C_rtsSource^.message[pos] # 0C DO
    INC(pos);
  END;

  IF xmRTS.X2C_rtsSource^.message[0]=0C THEN
    WITH xmRTS.X2C_rtsSource^ DO
      COPY("TRAP(",message);
      pos:=5;
      dectostr(message,pos,no);
      message[pos]:=")";
      message[pos+1]:=0C;
      INC(pos,1);
    END;
  END;
  IF file#NIL THEN
    append(xmRTS.X2C_rtsSource^.message,xmRTS.X2C_MSG_LEN-1,pos," at line ");
    append(xmRTS.X2C_rtsSource^.message,xmRTS.X2C_MSG_LEN-1,pos,ls);
    append(xmRTS.X2C_rtsSource^.message,xmRTS.X2C_MSG_LEN-1,pos," of ");
    append(xmRTS.X2C_rtsSource^.message,xmRTS.X2C_MSG_LEN-1,pos,file);
    IF pos>=xmRTS.X2C_MSG_LEN THEN pos:=xmRTS.X2C_MSG_LEN-1 END;
    xmRTS.X2C_rtsSource^.message[pos]:=0C;
  END;
  xmRTS.X2C_rtsSource^.number:=no;
  doRaise(xmRTS.X2C_rtsSource);
<* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
END X2C_TRAP_FC_IMPL;
<* ELSE *>
END X2C_TRAP_FC;
<* END *>

PROCEDURE [2] X2C_ASSERT_F(no: SYSTEM.CARD32);
  VAR pos: CARDINAL;
  current: xmRTS.X2C_Coroutine;
BEGIN
  <* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
  current:=xmRTS.X2C_GetCurrent();
  xmRTS.X2C_scanStackHistory(SYSTEM.SUBADR(SYSTEM.ADR(no),4),current^.stk_start,FALSE);
  <* END *>
  IF (xmRTS.X2C_assertSrc = NIL) THEN
    X2C_init_exceptions();
  END;
  <* IF multithread THEN *>
  xmRTS.X2C_LockMutex(xmRTS.X2C_assertSrc^.lock);
  <* END *>
  WITH xmRTS.X2C_assertSrc^ DO
    COPY("ASSERT(FALSE, ",message);
    pos:=14;
    dectostr(message,pos,no);
    message[pos]:=")";
    message[pos+1]:=0C;
    number:=no;
  END;
  doRaise(xmRTS.X2C_assertSrc);
END X2C_ASSERT_F;

<* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
PROCEDURE [2] X2C_TRAP_G(no: SYSTEM.INT32);
  VAR pos: CARDINAL;
  current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  xmRTS.X2C_scanStackHistory(SYSTEM.SUBADR(SYSTEM.ADR(no),4),current^.stk_start,TRUE);
  IF (xmRTS.X2C_rtsSource = NIL) THEN
    X2C_init_exceptions();
  END;
  <* IF multithread THEN *>
  xmRTS.X2C_LockMutex(xmRTS.X2C_rtsSource^.lock);
  <* END *>
  trap_message(xmRTS.X2C_rtsSource^.message,no);
  IF xmRTS.X2C_rtsSource^.message[0]=0C THEN
    WITH xmRTS.X2C_rtsSource^ DO
      COPY("TRAP(",message);
      pos:=5;
      dectostr(message,pos,no);
      message[pos]:=")";
      message[pos+1]:=0C;
    END;
  END;
  xmRTS.X2C_rtsSource^.number:=no;
  doRaise(xmRTS.X2C_rtsSource);
END X2C_TRAP_G;
<* END *>

PROCEDURE ["C"] X2C_ASSERT_FC(code: SYSTEM.CARD32; file: xmRTS.X2C_pCHAR; line: SYSTEM.CARD32);

  VAR pos: CARDINAL;
      cs,ls: ARRAY [0..15] OF CHAR;
      current: xmRTS.X2C_Coroutine;
BEGIN
  <* IF __GEN_X86__ OR NATIVE_LIBRARY THEN *>
  current:=xmRTS.X2C_GetCurrent();
  xmRTS.X2C_scanStackHistory(SYSTEM.SUBADR(SYSTEM.ADR(code),4),current^.stk_start,FALSE);
  <* END *>
  IF (xmRTS.X2C_assertSrc = NIL) THEN
    X2C_init_exceptions();
  END;
  <* IF multithread THEN *>
  xmRTS.X2C_LockMutex(xmRTS.X2C_assertSrc^.lock);
  <* END *>
  pos:=0;
  dectostr(cs,pos,code); cs[pos]:=0C; pos:=0;
  dectostr(ls,pos,line); ls[pos]:=0C; pos:=0;
  append(xmRTS.X2C_assertSrc^.message,xmRTS.X2C_MSG_LEN-1,pos,"ASSERT(FALSE,");
  append(xmRTS.X2C_assertSrc^.message,xmRTS.X2C_MSG_LEN-1,pos,cs);
  append(xmRTS.X2C_assertSrc^.message,xmRTS.X2C_MSG_LEN-1,pos,") at line ");
  append(xmRTS.X2C_assertSrc^.message,xmRTS.X2C_MSG_LEN-1,pos,ls);
  append(xmRTS.X2C_assertSrc^.message,xmRTS.X2C_MSG_LEN-1,pos," of ");
  append(xmRTS.X2C_assertSrc^.message,xmRTS.X2C_MSG_LEN-1,pos,file);
  IF pos>=xmRTS.X2C_MSG_LEN THEN pos:=xmRTS.X2C_MSG_LEN-1 END;
  xmRTS.X2C_assertSrc^.message[pos]:=0C;
  xmRTS.X2C_assertSrc^.number:=code;
  doRaise(xmRTS.X2C_assertSrc);
END X2C_ASSERT_FC;

END xrExceptions.
