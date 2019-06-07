(* Copyright (c) xTech 1995,96.  All Rights Reserved *)
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
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xrnStart;

IMPORT
   SYSTEM
  ,xWin32
  ,xrtsOS
  ,xrnRTT
  ,xrHistory
  ,xmRTS
  ,X2C;

FROM xrHistory IMPORT ExcInfo;

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

PROCEDURE alloc(VAR a: SYSTEM.ADDRESS; sz: CARDINAL);
BEGIN
  a:=xWin32.HeapAlloc(xWin32.GetProcessHeap(),xWin32.HEAP_GENERATE_EXCEPTIONS,sz);
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

  scan(astr,ac,NIL);
  IF ac>0 THEN
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
  X2C_USER_BREAK   = 21;
  X2C_STACK_OVERFLOW = 23;

PROCEDURE ["StdCall"] X2C_xFilter*(VAR data: xWin32.EXCEPTION_POINTERS):
                                                                SYSTEM.CARD32;
  VAR code, err: SYSTEM.CARD32;
      esp :SYSTEM.ADDRESS;
      i   :INTEGER;
BEGIN
  IF data.ExceptionRecord^.ExceptionFlags#0 THEN
    xrtsOS.X2C_StdOut("#RTS: Unable to handle an exception.",36);
    xrtsOS.X2C_StdOutN;
    xrtsOS.X2C_StdOutFlush;
    RETURN xWin32.EXCEPTION_EXECUTE_HANDLER;
  END;

  ExcInfo.EAX := data.ContextRecord^.Eax;
  ExcInfo.EBX := data.ContextRecord^.Ebx;
  ExcInfo.ECX := data.ContextRecord^.Ecx;
  ExcInfo.EDX := data.ContextRecord^.Edx;
  ExcInfo.ESI := data.ContextRecord^.Esi;
  ExcInfo.EDI := data.ContextRecord^.Edi;
  ExcInfo.EBP := data.ContextRecord^.Ebp;
  ExcInfo.ESP := data.ContextRecord^.Esp;
  ExcInfo.EIP := data.ContextRecord^.Eip;

  esp := SYSTEM.CAST (SYSTEM.ADDRESS, data.ContextRecord^.Esp);
  FOR i := 0 TO xrnRTT.STK_ENTRIES-1 DO
    SYSTEM.GET (esp, ExcInfo.stk[i]);
    esp := esp + SIZE(SYSTEM.CARD32);
  END;


  code:=data.ExceptionRecord^.ExceptionCode;
  IF code=xWin32.EXCEPTION_FLOAT_DIVIDE_BY_ZERO THEN
    err:=X2C_FLT_DIV;
  ELSIF
    (code=xWin32.EXCEPTION_FLOAT_DENORMAL_OPERAND)  OR
    (code=xWin32.EXCEPTION_FLOAT_INEXACT_RESULT)    OR
    (code=xWin32.EXCEPTION_FLOAT_INVALID_OPERATION) OR
    (code=xWin32.EXCEPTION_FLOAT_OVERFLOW)          OR
    (code=xWin32.EXCEPTION_FLOAT_STACK_CHECK)       OR
    (code=xWin32.EXCEPTION_FLOAT_UNDERFLOW)
    THEN
    err:=X2C_FLT_OVERFL;
  ELSIF code=xWin32.EXCEPTION_INTEGER_DIVIDE_BY_ZERO THEN
    err:=X2C_DIVISION;
  ELSIF code=xWin32.EXCEPTION_INTEGER_OVERFLOW THEN
    err:=X2C_OVERFLOW;
  ELSIF code=xWin32.EXCEPTION_ARRAY_BOUNDS_EXCEEDED THEN
    err:=X2C_INDEX;
  ELSIF code=xWin32.EXCEPTION_ACCESS_VIOLATION THEN
    err:=X2C_INV_LOCATION;
  ELSIF code=xWin32.EXCEPTION_STACK_OVERFLOW THEN
    err:=X2C_STACK_OVERFLOW;
  ELSE
    RETURN xWin32.EXCEPTION_EXECUTE_HANDLER;
  END;
  IF ((err=X2C_FLT_DIV) OR (err=X2C_FLT_OVERFL)) &
     (data.ContextRecord^.flags*{3,16}={3,16}) THEN
  (* xrnRTT.X2C_ErrEIP:=SYSTEM.CAST(SYSTEM.ADDRESS,data.ContextRecord^.floatSave.ErrorOffset); *)
    data.ContextRecord^.Edx:=data.ContextRecord^.floatSave.ErrorOffset;
  ELSE
    data.ContextRecord^.Edx:=data.ContextRecord^.Eip;
  END;
  data.ContextRecord^.Eax:=err;
  data.ContextRecord^.Eip:=SYSTEM.CAST(SYSTEM.CARD32,xrnRTT.X2C_TrapFJump);
  RETURN xWin32.EXCEPTION_CONTINUE_EXECUTION;
END X2C_xFilter;

VAR
  mainThread: xWin32.HANDLE;
  bHandler: BreakHandler;

PROCEDURE ["C"] X2C_SetBreakHandler(h: BreakHandler): BreakHandler;
  VAR o: BreakHandler;
BEGIN
  o:=bHandler;
  bHandler:=h;
  RETURN o;
END X2C_SetBreakHandler;

PROCEDURE ["StdCall"] breakHandler(code: xWin32.DWORD): xWin32.BOOL;
  VAR ctx: xWin32.CONTEXT;
BEGIN
  IF code>1 THEN RETURN FALSE END;
  IF (bHandler#NIL) & bHandler() THEN RETURN TRUE END;
(*  xWin32.SuspendThread(mainThread); *)
  ctx.flags:=xWin32.CONTEXT_CONTROL+xWin32.CONTEXT_INTEGER;
  IF NOT xWin32.GetThreadContext(mainThread,ctx) THEN
    RETURN FALSE
  END;
  ctx.Edx:=ctx.Eip;
  ctx.Eax:=X2C_USER_BREAK;
  ctx.Eip:=SYSTEM.CAST(xWin32.DWORD,xrnRTT.X2C_TrapFJump);
  ctx.flags:=xWin32.CONTEXT_CONTROL+xWin32.CONTEXT_INTEGER;
  IF NOT xWin32.SetThreadContext(mainThread,ctx) THEN
    RETURN FALSE
  END;
(*  xWin32.ResumeThread(mainThread); *)
  RETURN TRUE
END breakHandler;

PROCEDURE SetBreakHandler;
  VAR prs: xWin32.HANDLE;
BEGIN
  bHandler:=NIL;
  prs:=xWin32.GetCurrentProcess();
  IF xWin32.DuplicateHandle(prs,xWin32.GetCurrentThread(),prs,
        mainThread,{},FALSE,xWin32.DUPLICATE_SAME_ACCESS) THEN
    xWin32.SetConsoleCtrlHandler(breakHandler,TRUE);
  END;
END SetBreakHandler;

PROCEDURE GetImageSubsystem(): xWin32.WORD;
VAR
  p : POINTER TO RECORD
        CASE : BOOLEAN OF
        |TRUE :  word: xWin32.WORD;
        |FALSE: dword: xWin32.DWORD;
        END;
     END;
BEGIN
  p := xWin32.GetModuleHandle(NIL);
  (* if DOS based file *)
  IF p^.word = xWin32.IMAGE_DOS_SIGNATURE THEN
    (* file image header offset exists after DOS header *)
    p := SYSTEM.ADDADR(p,60);
    p := SYSTEM.ADDADR(p,p^.dword-60);
  END;
  (* At this point, p must point at the PE signature *)
  IF p^.dword = xWin32.IMAGE_NT_SIGNATURE THEN
    p := SYSTEM.ADDADR(p,20);
    (* p now points at the size of the optional header *)
    IF p^.word # 0 THEN
      p := SYSTEM.ADDADR(p,72);
      RETURN p^.word;
    END;
  END;
  RETURN xWin32.IMAGE_SUBSYSTEM_UNKNOWN;
END GetImageSubsystem;

PROCEDURE ["C"] / X2C_StdOutInit(h: xWin32.HANDLE);
(* defined in xosFileIO,mod *)

PROCEDURE ["StdCall"] X2C_xStart;
  VAR ac  :ARGC;
      av  :ARGV;
      buf :SYSTEM.ADDRESS;
      l   :CARDINAL;
BEGIN
  dispatch (xWin32.GetCommandLine(), ac, av);
  IF (ac > 0) THEN
    alloc (buf, 260);
    IF (buf # NIL) THEN
      l:=xWin32.GetModuleFileName(0,buf,260);
      IF l>0 THEN av^[0]:=buf END;
    END;
  END;
  IF xWin32.SetErrorMode(xWin32.SEM_FAILCRITICALERRORS)=SYSTEM.SET16{} THEN END;
  IF xWin32.SetUnhandledExceptionFilter(X2C_xFilter)=X2C_xFilter THEN END;
  SetBreakHandler;

  xrHistory.X2C_stkScan:=NIL;
  xrHistory.X2C_hisShow:=NIL;

  IF GetImageSubsystem() = xWin32.IMAGE_SUBSYSTEM_WINDOWS_GUI THEN
    X2C_StdOutInit(xWin32.INVALID_HANDLE_VALUE);
  ELSE
    X2C_StdOutInit(xWin32.GetStdHandle(xWin32.STD_ERROR_HANDLE));
  END;

  IF main(ac, av) # 0 THEN END; 
  xmRTS.X2C_EXIT();
END X2C_xStart;


END xrnStart.
