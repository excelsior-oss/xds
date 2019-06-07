IMPLEMENTATION MODULE XCPT_Msg;

IMPORT fmt := FormStr;
IMPORT m2e := M2EXCEPTION;

<* IF TARGET_x86 THEN *>

<* IF env_target = 'x86nt' THEN *>

IMPORT win := Windows;

<* ELSIF env_target = 'x86os2' THEN *>

IMPORT OS2;

<* END *>

<* ELSIF DEST_K26 THEN *>

IMPORT KrnTypes;

<* WOFF301+ *>

<* END *>


PROCEDURE Get_XCPT_Msg (xcpt, ext: CARDINAL; VAR msg: ARRAY OF CHAR);
BEGIN

<* IF TARGET_x86 THEN *>

<* IF env_target = 'x86nt' THEN *>

  CASE xcpt OF
  | win.EXCEPTION_ACCESS_VIOLATION         : COPY('ACCESS VIOLATION', msg);
  | win.EXCEPTION_DATATYPE_MISALIGNMENT    : COPY('DATATYPE_MISALIGNMENT', msg);
  | win.EXCEPTION_BREAKPOINT               : COPY('BREAKPOINT', msg);
  | win.EXCEPTION_SINGLE_STEP              : COPY('SINGLE STEP', msg);
  | win.EXCEPTION_ARRAY_BOUNDS_EXCEEDED    : COPY('ARRAY BOUNDS EXCEEDED', msg);
  | win.EXCEPTION_FLT_DENORMAL_OPERAND     : COPY('FLOAT DENORMAL OPERAND', msg);
  | win.EXCEPTION_FLT_DIVIDE_BY_ZERO       : COPY('FLOAT DIVIDE BY ZERO', msg);
  | win.EXCEPTION_FLT_INEXACT_RESULT       : COPY('FLOAT INEXACT RESULT', msg);
  | win.EXCEPTION_FLT_INVALID_OPERATION    : COPY('FLOAT INVALID OPERATION', msg);
  | win.EXCEPTION_FLT_OVERFLOW             : COPY('FLOAT OVERFLOW', msg);
  | win.EXCEPTION_FLT_STACK_CHECK          : COPY('FLOAT STACK CHECK', msg);
  | win.EXCEPTION_FLT_UNDERFLOW            : COPY('FLOAT UNDERFLOW', msg);
  | win.EXCEPTION_INT_DIVIDE_BY_ZERO       : COPY('INTEGER DIVIDE BY ZERO', msg);
  | win.EXCEPTION_INT_OVERFLOW             : COPY('INTEGER OVERFLOW', msg);
  | win.EXCEPTION_PRIV_INSTRUCTION         : COPY('PRIVILEGED INSTRUCTION', msg);
  | win.EXCEPTION_IN_PAGE_ERROR            : COPY('IN PAGE ERROR', msg);
  | win.EXCEPTION_ILLEGAL_INSTRUCTION      : COPY('ILLEGAL INSTRUCTION', msg);
  | win.EXCEPTION_NONCONTINUABLE_EXCEPTION : COPY('NONCONTINUABLE EXCEPTION', msg);
  | win.EXCEPTION_STACK_OVERFLOW           : COPY('STACK OVERFLOW', msg);
  | win.EXCEPTION_INVALID_DISPOSITION      : COPY('INVALID DISPOSITION', msg);
  | win.EXCEPTION_GUARD_PAGE               : COPY('GUARD PAGE VIOLATION', msg);
  | win.DBG_EXCEPTION_NOT_HANDLED          : COPY('EXCEPTION NOT HANDLED', msg);
  | win.DBG_CONTROL_C                      : COPY('CONTROL C', msg);
  | win.DBG_CONTROL_BREAK                  : COPY('CONTROL BREAK', msg);

<* ELSIF env_target = 'x86os2' THEN *>

  CASE xcpt OF
  | OS2.XCPT_UNABLE_TO_GROW_STACK     : COPY('UNABLE TO GROW STACK', msg);
  | OS2.XCPT_BREAKPOINT               : COPY('BREAKPOINT', msg);
  | OS2.XCPT_SINGLE_STEP              : COPY('SINGLE STEP', msg);
  | OS2.XCPT_ILLEGAL_INSTRUCTION      : COPY('ILLEGAL INSTRUCTION', msg);
  | OS2.XCPT_FLOAT_DENORMAL_OPERAND   : COPY('FLOAT DENORMAL OPERAND', msg);
  | OS2.XCPT_FLOAT_DIVIDE_BY_ZERO     : COPY('FLOAT DIVIDE BY ZERO', msg);
  | OS2.XCPT_FLOAT_INEXACT_RESULT     : COPY('FLOAT INEXACT RESULT', msg);
  | OS2.XCPT_FLOAT_INVALID_OPERATION  : COPY('FLOAT INVALID OPERATION', msg);
  | OS2.XCPT_FLOAT_OVERFLOW           : COPY('FLOAT OVERFLOW', msg);
  | OS2.XCPT_FLOAT_STACK_CHECK        : COPY('FLOAT STACK CHECK', msg);
  | OS2.XCPT_FLOAT_UNDERFLOW          : COPY('FLOAT UNDERFLOW', msg);
  | OS2.XCPT_INTEGER_DIVIDE_BY_ZERO   : COPY('INTEGER DIVIDE BY ZERO', msg);
  | OS2.XCPT_INTEGER_OVERFLOW         : COPY('INTEGER OVERFLOW', msg);
  | OS2.XCPT_PRIVILEGED_INSTRUCTION   : COPY('PRIVILEGED INSTRUCTION', msg);
  | OS2.XCPT_PROCESS_TERMINATE        : COPY('PROCESS TERMINATE', msg);
  | OS2.XCPT_NONCONTINUABLE_EXCEPTION : COPY('NONCONTINUABLE EXCEPTION', msg);
  | OS2.XCPT_INVALID_DISPOSITION      : COPY('INVALID DISPOSITION', msg);
  | OS2.XCPT_INVALID_LOCK_SEQUENCE    : COPY('INVALID LOCK SEQUENCE', msg);
  | OS2.XCPT_ARRAY_BOUNDS_EXCEEDED    : COPY('ARRAY BOUNDS EXCEEDED', msg);
  | OS2.XCPT_B1NPX_ERRATA_02          : COPY('B1NPX ERRATA 02', msg);
  | OS2.XCPT_UNWIND                   : COPY('UNWIND', msg);
  | OS2.XCPT_BAD_STACK                : COPY('BAD STACK', msg);
  | OS2.XCPT_INVALID_UNWIND_TARGET    : COPY('INVALID UNWIND TARGET', msg);
  | OS2.XCPT_IN_PAGE_ERROR:
    fmt.print(msg, 'IN PAGE ERROR, ADDRESS 0x$8X', ext);
  | OS2.XCPT_ASYNC_PROCESS_TERMINATE:
    fmt.print(msg, 'ASYNC PROCESS TERMINATE, TID %u', ext);
  | OS2.XCPT_GUARD_PAGE_VIOLATION:
     CASE ext OF
     | OS2.XCPT_READ_ACCESS  : COPY('GUARD PAGE VIOLATION, READ ACCESS', msg);
     | OS2.XCPT_WRITE_ACCESS : COPY('GUARD PAGE VIOLATION, WRITE ACCESS', msg);
     ELSE
       COPY('GUARD PAGE VIOLATION', msg);
     END;
  | OS2.XCPT_DATATYPE_MISALIGNMENT:
     CASE ext OF
     | OS2.XCPT_READ_ACCESS  : COPY('DATATYPE MISALIGNMENT, READ ACCESS', msg);
     | OS2.XCPT_WRITE_ACCESS : COPY('DATATYPE MISALIGNMENT, WRITE ACCESS', msg);
     ELSE
       COPY('DATATYPE MISALIGNMENT', msg);
     END;
  | OS2.XCPT_ACCESS_VIOLATION:
    CASE ext OF
    | OS2.XCPT_UNKNOWN_ACCESS : COPY('UNKNOWN ACCESS VIOLATION', msg);
    | OS2.XCPT_READ_ACCESS    : COPY('READ ACCESS VIOLATION', msg);
    | OS2.XCPT_WRITE_ACCESS   : COPY('WRITE ACCESS VIOLATION', msg);
    | OS2.XCPT_EXECUTE_ACCESS : COPY('EXECUTE ACCESS VIOLATION', msg);
    | OS2.XCPT_SPACE_ACCESS   : COPY('SPACE ACCESS VIOLATION', msg);
    | OS2.XCPT_LIMIT_ACCESS   : COPY('LIMIT ACCESS VIOLATION', msg);
    | OS2.XCPT_DATA_UNKNOWN   : COPY('DATA UNKNOWN ACCESS VIOLATION', msg);
    ELSE
      COPY('ACCESS VIOLATION', msg);
    END;
  | OS2.XCPT_SIGNAL:
    CASE ext OF
    | OS2.XCPT_SIGNAL_INTR     : COPY('SIGNAL INTR', msg);
    | OS2.XCPT_SIGNAL_KILLPROC : COPY('SIGNAL KILLPROC', msg);
    | OS2.XCPT_SIGNAL_BREAK    : COPY('SIGNAL BREAK', msg);
    ELSE
      COPY('SIGNAL', msg);
    END;

  <* END *>
<* ELSIF TARGET_M68K THEN *>
  CASE xcpt OF

<* ELSIF DEST_K26 THEN *>

  CASE xcpt OF
  | KrnTypes.XCPT_ABORT          : COPY('ABORT', msg);
  | KrnTypes.XCPT_TRAP           : COPY('TRAP', msg);
  | KrnTypes.XCPT_FAULT          : COPY('FAULT', msg);
  | KrnTypes.XCPT_INTERRUPT      : COPY('INTERRUPT', msg);
  | KrnTypes.XCPT_PRHALT         : COPY('PRHALT', msg);
  | KrnTypes.XCPT_NO_ENTRY_POINT : COPY('NO ENTRY POINT', msg);
  | KrnTypes.XCPT_USER_BREAK     : COPY('USER CONTROL BREAK', msg);
  | KrnTypes.XCPT_WRITE_ROM      : COPY('WRITE ROM', msg);
  | KrnTypes.XCPT_WRITE_CODE     : COPY('WRITE CODE', msg);

<* END *>  

  | UNKNOWN_BREAKPOINT : COPY('BREAKPOINT EXCEPTION', msg);
  | GENERAL_EXCEPTION  : COPY('GENERAL EXCEPTION', msg);
  ELSE
    fmt.print(msg, '0x%$8X', xcpt);
  END;
END Get_XCPT_Msg;


CONST
  -- Константы определены в RTS xr\xrn\X2C.def

  X2C_Assert            = 15;
  (* Oberon-2 Exceptions *)
  X2C_assertException   = 16;
  X2C_guardException    = 17;
  (* RTS Exceptions *)
  X2C_noMemoryException = 18;
  X2C_internalError     = 19;
  X2C_castError         = 20;
  X2C_UserBreak         = 21;
  X2C_unreachDLL        = 22;
  X2C_stack_overflow    = 23;


PROCEDURE Get_RTS_XCPT_Msg (xcpt: CARDINAL; VAR msg: ARRAY OF CHAR);
BEGIN
  CASE xcpt OF
  | ORD(m2e.indexException):
    COPY("invalid index",msg);
  | ORD(m2e.rangeException):
    COPY("expression out of bounds",msg);
  | ORD(m2e.caseSelectException):
    COPY("invalid case in CASE statement",msg);
  | ORD(m2e.invalidLocation):
    COPY("invalid location",msg);
  | ORD(m2e.functionException):
    COPY("function without RETURN statement",msg);
  | ORD(m2e.wholeValueException):
    COPY("whole overflow",msg);
  | ORD(m2e.wholeDivException):
    COPY("zero or negative divisor",msg);
  | ORD(m2e.realValueException):
    COPY("real overflow",msg);
  | ORD(m2e.realDivException):
    COPY("float division by zero",msg);
  | ORD(m2e.complexValueException):
    COPY("complex overflow",msg);
  | ORD(m2e.complexDivException):
    COPY("complex division by zero",msg);
  | ORD(m2e.protException):
    COPY("protection error",msg);
  | ORD(m2e.sysException):
    COPY("SYSTEM exception",msg);
  | ORD(m2e.coException):
    COPY("COROUTINE exception",msg);
  | ORD(m2e.exException):
    COPY("EXCEPTIONS exception",msg);
  ELSE
    IF xcpt = X2C_Assert THEN
      COPY("ASSERT", msg);
    (* Oberon-2 Exceptions *)
    ELSIF xcpt = X2C_assertException THEN
      COPY("O2 ASSERT", msg);
    ELSIF xcpt = X2C_guardException THEN
      COPY("type guard check", msg);
    (* RTS Exceptions *)
    ELSIF xcpt = X2C_noMemoryException THEN
      COPY("out of heap space", msg);
    ELSIF xcpt = X2C_unreachDLL THEN
      COPY("call to unloaded DLL",msg);
    ELSIF xcpt = X2C_internalError THEN
      COPY("RTS internal error", msg);
    ELSIF xcpt = X2C_castError THEN
      COPY("invalid type cast", msg);
    ELSIF xcpt = X2C_UserBreak THEN
      COPY("USER BREAK", msg);
    ELSIF xcpt = X2C_stack_overflow THEN
      COPY("stack overflow", msg);
    ELSE
      COPY("undefined exception", msg);
    END
  END;
END Get_RTS_XCPT_Msg;


END XCPT_Msg.
