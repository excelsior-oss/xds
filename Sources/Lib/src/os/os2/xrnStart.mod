<*- checkindex  *>
<*- checknil    *>
<*- checkdindex *>
<*- checktype   *>
<*- checkrange  *>
<*- checkset    *>
<*- checkdiv    *>
<*- checkproc   *>
<*- assert      *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*+ m2extensions*>
IMPLEMENTATION MODULE xrnStart;

IMPORT
  SYSTEM
  ,xOS2
  ,xrtsOS
  ,xmRTS
  ,xrnRTT
  ,xrnsetjmp
  ,xrHistory
  ,X2C;

CONST
  MAX_ARGS = 7FFFH;
  TAB = 11C;

TYPE
  ARGC  = SYSTEM.int;
  pCHAR = POINTER TO CHAR;
  ARGV  = POINTER TO ARRAY [0..MAX_ARGS] OF pCHAR;

  MAINTYPE = PROCEDURE ["C"] ( ARGC, SYSTEM.ADDRESS) :SYSTEM.CARD32;
VAR
  main :MAINTYPE;

PROCEDURE ["C"] X2C_EstablishMain ( mainAdr :SYSTEM.ADDRESS );
BEGIN
  main := SYSTEM.CAST ( MAINTYPE, mainAdr );
END X2C_EstablishMain;


VAR alloc_buf: SYSTEM.ADDRESS;
    buf_sz: CARDINAL;
    buf_cnt: CARDINAL;

PROCEDURE alloc(VAR a: SYSTEM.ADDRESS; sz: CARDINAL);
BEGIN
  IF sz + buf_cnt > buf_sz THEN
    buf_sz := sz + 0FFFH;
    buf_sz := SYSTEM.CAST(CARDINAL, SYSTEM.CAST(BITSET, buf_sz) * {12..31});
    IF xOS2.DosAllocMem (alloc_buf, buf_sz,
             xOS2.PAG_COMMIT + xOS2.PAG_READ + xOS2.PAG_WRITE) # xOS2.ok
    THEN
      HALT
    END;
    buf_cnt := 0;
  END;
  a := SYSTEM.ADDADR(alloc_buf, buf_cnt);
  buf_cnt := buf_cnt + sz;
END alloc;

PROCEDURE skip(VAR pc: pCHAR);
BEGIN
  WHILE (pc^=" ") OR (pc^=TAB) DO pc:=SYSTEM.ADDADR(pc,SIZE(CHAR)) END;
END skip;

PROCEDURE onearg(VAR pc: pCHAR; VAR l: CARDINAL; des: pCHAR);
  VAR fl: BOOLEAN; ch: CHAR;
BEGIN
  fl:=FALSE; l:=0;
  LOOP
    IF pc^=0C THEN EXIT END;
    ch:=pc^;
    pc:=SYSTEM.ADDADR(pc,SIZE(CHAR));
    IF ch="\" THEN
      IF pc^='"' THEN
        ch:='"'; pc:=SYSTEM.ADDADR(pc,SIZE(CHAR));
      END;
    ELSIF ch='"' THEN
      fl:=NOT fl;
      ch:=0C;
    ELSIF (ch=" ") OR (ch=TAB) THEN
      IF NOT fl THEN EXIT END;
    END;
    IF ch#0C THEN
      IF des#NIL THEN des^:=ch; des:=SYSTEM.ADDADR(des,SIZE(CHAR)) END;
      INC(l)
    END;
  END;
  IF des#NIL THEN des^:=0C END;
END onearg;

PROCEDURE scan(pc: pCHAR; VAR ac: ARGC; av: ARGV);
  VAR cc: ARGC;
      as: pCHAR;
      al: CARDINAL;
BEGIN
  cc:=0;
  as:=pc;
  onearg(pc,al,NIL);
  IF al>0 THEN
    IF av#NIL THEN
      alloc(av^[cc],al+1);
      onearg(as,al,av^[cc]);
    END;
    INC(cc);
  END;
  pc:=SYSTEM.ADDADR(pc,SIZE(CHAR));
  LOOP
    skip(pc);
    IF pc^=0C THEN EXIT END;
    as:=pc;
    onearg(pc,al,NIL);
    IF al>0 THEN
      IF av#NIL THEN
        alloc(av^[cc],al+1);
        onearg(as,al,av^[cc]);
      END;
      INC(cc);
    END;
  END;
  ac:=cc;
END scan;

PROCEDURE dispatch(astr: pCHAR; VAR ac: ARGC; VAR av: ARGV);
VAR
  pc :pCHAR;
BEGIN
  ac := 0;
  av := NIL;
  X2C.X2C_sysarg := NIL;
  IF (astr = NIL) THEN RETURN END;

  pc := astr;
  skip (pc);
  WHILE (pc^ # ' ') & (pc^ # 0C) DO pc := pc + 1 END;
  IF (pc^ # 0C) THEN X2C.X2C_sysarg := pc END;

  scan(astr, ac, NIL);
  IF (ac > 0) THEN
    alloc(av,VAL(CARDINAL,ac)*SIZE(pCHAR));
    scan(astr,ac,av);
  END;
END dispatch;

CONST
  X2C_INDEX        = 0;
  X2C_RANGE        = 1;
  X2C_INV_LOCATION = 3;
  X2C_OVERFLOW     = 5;
  X2C_DIVISION     = 6;
  X2C_FLT_OVERFL   = 7;
  X2C_FLT_DIV      = 8;

(* means for debugging of exception hangling

PROCEDURE wn(n: CARDINAL);
  VAR s: ARRAY [0..7] OF CHAR;
    i,d: CARDINAL;
BEGIN
  FOR i := 7 TO 0 BY -1 DO
    d := n MOD 16;
    n := n DIV 16;
    IF d<10 THEN s[i] := CHR(ORD("0")+d);
    ELSE s[i] := CHR(ORD("A")+d-10);
    END;
  END;
  xrtsOS.X2C_StdOut(s, 8);
END wn;

PROCEDURE ws(s-: ARRAY OF CHAR);
BEGIN
  xrtsOS.X2C_StdOut(s, LENGTH(s));
END ws;

PROCEDURE wln;
BEGIN
  xrtsOS.X2C_StdOut(""+12C+15C,2);
END wln;

PROCEDURE ["SysCall"] / DosBeep
( freq : CARDINAL;
  dur  : CARDINAL
) : xOS2.APIRET;

*)

VAR
  bHandler :BreakHandler;

PROCEDURE ["C"] X2C_SetBreakHandler(h: BreakHandler): BreakHandler;
VAR
  o :BreakHandler;
BEGIN
  o := bHandler;
  bHandler:=h;
  RETURN o;
END X2C_SetBreakHandler;


PROCEDURE ["C"] / X2C_SEL2FLAT (seloffs: SYSTEM.CARD32) :SYSTEM.CARD32;

PROCEDURE ["C"] filter
( VAR report       : xOS2.EXCEPTIONREPORTRECORD;
  registration_ptr : xOS2.PEXCEPTIONREGISTRATIONRECORD;
  VAR context      : xOS2.CONTEXTRECORD;
  dis_ptr          : SYSTEM.ADDRESS
)                  : SYSTEM.CARD32;

VAR
  num      :SYSTEM.CARD32;
  code     :SYSTEM.CARD32;
  jb       :xrnsetjmp.X2C_jmp_buf;
  is32bitx :BOOLEAN;        -- is this X raised from 32-bit code? 
BEGIN
  xrnsetjmp.X2C_setjmp(jb);                    -- just to get registers
  is32bitx := (context.SegSs = jb.ss);  

  num := report.ExceptionNum;
  IF num = xOS2.XCPT_FLOAT_DIVIDE_BY_ZERO THEN
    code := X2C_FLT_DIV;
  (* xrnRTT.X2C_ErrCode := X2C_FLT_DIV; *)
  ELSIF
    (num = xOS2.XCPT_FLOAT_DENORMAL_OPERAND)  OR
    (num = xOS2.XCPT_FLOAT_INEXACT_RESULT)    OR
    (num = xOS2.XCPT_FLOAT_INVALID_OPERATION) OR
    (num = xOS2.XCPT_FLOAT_OVERFLOW)          OR
    (num = xOS2.XCPT_FLOAT_STACK_CHECK)       OR
    (num = xOS2.XCPT_FLOAT_UNDERFLOW)
  THEN
    code := X2C_FLT_OVERFL;
  ELSIF num = xOS2.XCPT_INTEGER_DIVIDE_BY_ZERO THEN
    code := X2C_DIVISION;
  ELSIF num = xOS2.XCPT_INTEGER_OVERFLOW THEN
    code := X2C_OVERFLOW;
  ELSIF num = xOS2.XCPT_ARRAY_BOUNDS_EXCEEDED THEN
    code := X2C_INDEX;
  ELSIF num = xOS2.XCPT_ACCESS_VIOLATION THEN
    code := X2C_INV_LOCATION;
  ELSIF (num = xOS2.XCPT_SIGNAL) & ((report.ExceptionInfo[0] = xOS2.XCPT_SIGNAL_INTR) OR
        (report.ExceptionInfo[0] = xOS2.XCPT_SIGNAL_BREAK))
  THEN
    IF (bHandler # NIL) & bHandler() THEN
      RETURN xOS2.XCPT_CONTINUE_EXECUTION
    ELSE
      IF (is32bitx) THEN
      (* it never returns from X2C_TRAP_BREAK 
         There is a way to register "at exit" API proc (DosExitList)
         to carry out some actions in non-exceptional state but no need
       *)
        xrnRTT.X2C_TRAP_BREAK(context.RegEip, context.RegEsp);
      ELSE
        (* an exception from 16-bit code is raised that is joppa *)
        xrnRTT.X2C_TRAP_BREAK(
        X2C_SEL2FLAT(context.RegEip+10000H*context.SegCs),
        X2C_SEL2FLAT(context.RegEsp+10000H*context.SegSs) );
      END;
    END;
  ELSE
    RETURN xOS2.XCPT_CONTINUE_SEARCH
  END;

  IF ((code=X2C_FLT_DIV) OR (code=X2C_FLT_OVERFL)) &
     (context.ContextFlags*xOS2.CONTEXT_FLOATING_POINT#{}) THEN
    context.env[0] := SYSTEM.CAST(CARDINAL,
         SYSTEM.CAST(BITSET,context.env[0]) * {6..15}  -- Unmask all exceptions
         + {4,5});                                     -- Mask precision loss
    context.env[1] := SYSTEM.CAST(CARDINAL,
         SYSTEM.CAST(BITSET,context.env[1]) * {8..10, 14,15}); -- Clear exceptions,
                                                               -- reset TOS to 0
    context.env[2] := SYSTEM.CAST(CARDINAL,
         SYSTEM.CAST(BITSET,context.env[2]) + {0..15});        -- Empty stack

    context.RegEdx := context.env[3];
(*    xrnRTT.X2C_ErrEIP := context.env[3]; (* ErrorOffset from FPU *) *)

  ELSIF (context.ContextFlags*xOS2.CONTEXT_CONTROL#{}) THEN
    IF (is32bitx) THEN
      context.RegEdx := context.RegEip;
    ELSE
      context.RegEdx := X2C_SEL2FLAT(context.RegEip+10000H*context.SegCs);
    END;
(*    xrnRTT.X2C_ErrEIP := SYSTEM.CAST(SYSTEM.ADDRESS, context.RegEip); *)
  ELSE
    RETURN xOS2.XCPT_CONTINUE_SEARCH
  END;
  context.RegEax := code;
  context.RegEip := SYSTEM.CAST(SYSTEM.CARD32, xrnRTT.X2C_TrapFJump);
  RETURN xOS2.XCPT_CONTINUE_EXECUTION;
END filter;

PROCEDURE ["SysCall"] / DosQueryModuleName
( mod : CARDINAL;
  size: CARDINAL;
  psz:  SYSTEM.ADDRESS
) : xOS2.APIRET;

PROCEDURE ["SysCall"] / DosQuerySysInfo
( from : CARDINAL;
  to   : CARDINAL;
  buf  : SYSTEM.ADDRESS;
  size : CARDINAL
) : xOS2.APIRET;

PROCEDURE ["SysCall"] / DosSetSignalExceptionFocus
( flag      : CARDINAL;
  VAR times : CARDINAL
) : xOS2.APIRET;

CONST QSV_MAX_PATH_LENGTH = 1;

PROCEDURE set_arg0(mod: CARDINAL; av: ARGV);
  VAR a0: SYSTEM.ADDRESS; MaxPathLen : CARDINAL;
BEGIN
  IF av = NIL THEN RETURN END;
  IF DosQuerySysInfo(QSV_MAX_PATH_LENGTH, QSV_MAX_PATH_LENGTH, SYSTEM.ADR(MaxPathLen), 4) # xOS2.ok THEN RETURN END;
  alloc(a0, MaxPathLen);
  IF DosQueryModuleName(mod, MaxPathLen, a0) # xOS2.ok THEN RETURN END;
  av^[0] := a0;
END set_arg0;

PROCEDURE ["C"] X2C_SetOSXHandler(VAR eh :xOS2.EXCEPTIONREGISTRATIONRECORD) :xOS2.APIRET;
BEGIN
  eh.prev_structure   := NIL;
  eh.ExceptionHandler := SYSTEM.CAST(xOS2.ERR, filter);
  RETURN xOS2.DosSetExceptionHandler(eh);
END X2C_SetOSXHandler;

PROCEDURE ["C"] X2C_xStart ( mod_handle :CARDINAL;         (* ESP+4   *)
                             zero       :CARDINAL;         (* ESP+8   *)
                             env_ptr    :SYSTEM.ADDRESS;   (* ESP+12  *)
                             cmd_line   :SYSTEM.ADDRESS ); (* ESP+16  *)
VAR
  ac    :ARGC;
  av    :ARGV;
  times :SYSTEM.CARD32;
  eh    :xOS2.EXCEPTIONREGISTRATIONRECORD;  (* !!! must be on the stack *)
BEGIN
  xrHistory.X2C_stkScan := NIL;
  xrHistory.X2C_hisShow := NIL;
  bHandler := NIL;
  IF X2C_SetOSXHandler(eh) = xOS2.ok THEN
    DosSetSignalExceptionFocus(1, times);
    buf_cnt := 0;
    buf_sz  := 0;
    dispatch (cmd_line, ac, av);
    set_arg0 (mod_handle, av);     -- set program's path as arg[0]
    IF main(ac, av) # 0 THEN END;
    xmRTS.X2C_EXIT();
  END;
END X2C_xStart;


END xrnStart.
