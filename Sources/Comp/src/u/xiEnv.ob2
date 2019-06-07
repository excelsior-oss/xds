(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0: abstract interface level *)
<* IF ~ DEFINED(STATANALYSIS) THEN *> <* NEW STATANALYSIS- *> <* END *>
<* IF ~ DEFINED(COMPONENT_TESTCOVERAGE) THEN *> <* NEW COMPONENT_TESTCOVERAGE- *> <* END *>
MODULE xiEnv; (* Ned 19-Feb-94. *)

(* modifications:
  18-Oct-95 Ned: shell interface is added.
*)

IMPORT  SYSTEM, COMPILER, DStrings;

CONST (* config results *)
  isEquation*      =-2; (** for Parse only *)
  isOption*        =-1; (** for Parse only *)
  ok*              = 0; (** after Parse means: empty line or comment *)
  unknownOption*   = 1;
  unknownEquation* = 2;
  definedOption*   = 3;
  definedEquation* = 4;
  wrongSyntax*     = 5;

  max_lines     = 70000000H;
  max_cols      = 7000H;

TYPE
  CompilerOption *= (** compiler options *)
  ( assert_check,--  * =  0;
    card_ovfl     ,--* =  1;
    dynamic_check ,--* =  2;
    float_ovfl    ,--* =  3;
    guard_check   ,--* =  4;
    int_ovfl      ,--* =  5;
    nil_check     ,--* =  6;
    proc_check    ,--* =  7;
    quo_check     ,--* =  8;
    range_check   ,--* =  9;
    index_check   ,--* = 10;
    set_check     ,--* = 11;
    m2_ext        ,--* = 12; (* used in pcM2 *)
    o2_ext        ,--  = 13;
    o2_kwd        ,--  = 14;
    ts_ext        ,--* = 15;
    gencpref,
    no_struct,
    co_aux18,
    co_aux19,
    co_aux20,
    co_aux21,
    co_aux22,
    co_aux23,
    co_aux24,
    co_aux25,
    co_aux26,
    co_aux27,
    (* bits 16-19 are reserved for interface with back-end *)
    disable_code    --= 28; (* internal option used in DEMO version.
                    --         ATTENTION! Don't change the value.
                    --         It is used in xmCFM file managers.
                    --       *)
  );
  CompilerOptionSet *= PACKEDSET OF CompilerOption;

  String*  = DStrings.String;
  PROC*    = PROCEDURE;
  EQ_ACTION* = PROCEDURE (VAR val: String);  (** See NewActiveEquation *)
  OP_ACTION* = PROCEDURE (val: BOOLEAN); (** See NewActiveOption   *)
  MESSAGE* = ARRAY 128 OF CHAR; (** any message string must be shorter *)
  OPNAME*  = ARRAY 64 OF CHAR;  (** option or equation name *)
  TimeStamp* = SYSTEM.CARD32; (** see TimeConv.def *)

  Equations* = RECORD END;
  Options*   = RECORD END;
  Config* = POINTER TO ConfigDesc;
  ConfigDesc* = RECORD
    tags*: CompilerOptionSet;
    res *: INTEGER; (** result of operation *)
    lev *: INTEGER; (** value assigment level for options & equations *)
  END;

    (** language flags *)
    Lang *= (
    flag_o2,
    flag_m2,
    flag_c,
    flag_java,
    flag_jbc, (** used only in project system to distinguish Java FEs *)
    flag_sl1,
    flag_p,    (** Pascal *)
    flag_bnrp, (** BNR Pascal *)
    flag_stdcall, (** for Win32 *)
    flag_syscall, (** for system call *)
    flag_vmcall,   (** like StdCall but save all regs *)
    flag_javacall,
    flag_lightcall (** like StdCall but save no regs *)
  );

  Mno *= INTEGER;
  Info* = POINTER TO InfoDesc;
  InfoDesc* = RECORD
    language* : Lang; (** 0 - Oberon, 1 - Modula *)
    file*     : String;  (** file name *)
    code_file*: String;  (** code file name *)
    module*   : String;  (** module name *)
    title*    : String;  (** compiler title **)
    lines*    : LONGINT; (** number of lines *)
    newSF*    : BOOLEAN; (** new symbol file *)
    newDF*    : BOOLEAN; (** new pseudo-def file *)
    mod_no*   : Mno; (** no in pcK.mods *)
    main*     : BOOLEAN; (** main module *)
    en_b_end* : BOOLEAN; (** enable backend *)
  END;

  TPOS* = RECORD
    line : LONGINT;
    file : INTEGER;
    col  : INTEGER;
  END;

  Errors* = POINTER TO ErrorsDesc;
  ErrorsDesc* = RECORD
    soft_fault* : BOOLEAN;
    err_cnt*    : INTEGER;
    wrn_cnt*    : INTEGER;
    notice_cnt*    : INTEGER;
    env_err_cnt*: INTEGER; (** counter of environment errors *)
  END;

  Args* = POINTER TO ArgsDesc;
  ArgsDesc* = RECORD
  END;

  Shell* = POINTER TO ShellDesc;
  ShellDesc* = RECORD
  END;

  FNAMES = POINTER TO ARRAY OF String;

VAR
  config*   : Config;
  info*     : Info;
  errors*   : Errors;
  args*     : Args;
  shell*    : Shell;
  fnames    : FNAMES;
  fnames_cnt: INTEGER;
  fnames_cur: INTEGER;

CONST
  null_pos* = TPOS {MAX(LONGINT), MAX(INTEGER), MAX(INTEGER)};
  INVMno  *= Mno{-1};
  ZEROMno *= Mno{0};

  (** bits in "err_sum" *)
  warning* = 1; (** compilation warning *)
  error*   = 2; (** compilation error *)
  fault*   = 3; (** compilation fault *)
  notice*  = 4;
  other*   = 5; (** other fault (file not found, etc) *)

<* IF COMPONENT_TESTCOVERAGE THEN *>  
  EQU_TESTCOVERAGEMASK *= "TESTCOVERAGEMASK";
  OPT_TESTCOVERAGE     *= "TESTCOVERAGE";
<* END *>

VAR
  err_sum*: SET; (** errors summary *)

<* IF TARGET_IDB THEN *>
  InterViewMode* : BOOLEAN; (* =interview is used.*)
<* END *>

CONST
  (** flags in Info.decor *)
  dc_header*      = 0;  (** utility header *)
  dc_tailer*      = 1;  (** total results *)
  dc_compiler*    = 2;  (** compiler header *)
  dc_report*      = 3;  (** compiler report *)
  dc_progress*    = 4;  (** compiler progress report *)

VAR
  decor*: SET; (** decoration flags *)

(*-------------------------------------------------------------*)

PROCEDURE (VAR e: Equations) Do*(name-: ARRAY OF CHAR; val: String): BOOLEAN;
BEGIN RETURN TRUE
END Do;

PROCEDURE (VAR o: Options) Do*(name-: ARRAY OF CHAR; val: BOOLEAN): BOOLEAN;
BEGIN RETURN TRUE
END Do;

(*------------------------  Config  ---------------------------*)

PROCEDURE (c: Config) NewOption*(name-: ARRAY OF CHAR;
                                   val: BOOLEAN;
                                   bit: CompilerOption);
(** bit < 0, to ignore *)
END NewOption;

PROCEDURE (c: Config) NewActiveOption*(name- : ARRAY OF CHAR;
                                          val: BOOLEAN;
                                       action: OP_ACTION);
(** Create option and set action that will be called each time
    a value is changed.
*)
END NewActiveOption;

PROCEDURE (c: Config) SetOption*(name-: ARRAY OF CHAR; val: BOOLEAN);
END SetOption;

PROCEDURE (c: Config) SetOptionAt*(ps: TPOS; name-: ARRAY OF CHAR; val: BOOLEAN);
END SetOptionAt;

PROCEDURE (c: Config) Option*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
END Option;

PROCEDURE (c: Config) OptionAt*(ps: TPOS; name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
END OptionAt;

PROCEDURE (c: Config) ListOptions*(VAR o: Options; all: BOOLEAN);
BEGIN
END ListOptions;

PROCEDURE (c: Config) Synonym*(old-,new-: ARRAY OF CHAR);
END Synonym;

PROCEDURE (c: Config) NewEquation*(name-: ARRAY OF CHAR);
END NewEquation;

PROCEDURE (c: Config) NewActiveEquation*(name- : ARRAY OF CHAR;
                                         action: EQ_ACTION);
(** Create equation and set action that will be called each time
    a value is changed.
*)
END NewActiveEquation;

PROCEDURE (c: Config) SetEquation*(name-,val-: ARRAY OF CHAR);
END SetEquation;

PROCEDURE (c: Config) Equation*(name-: ARRAY OF CHAR; VAR val: String);
BEGIN
  val:=NIL;
END Equation;

PROCEDURE (c: Config) ListEquations*(VAR e: Equations;
                                      name-: ARRAY OF CHAR;
                                       all: BOOLEAN);
BEGIN
END ListEquations;

PROCEDURE (c: Config) Save*;
END Save;

PROCEDURE (c: Config) Restore*;
END Restore;

PROCEDURE (c: Config) RestoreAt*(pos: TPOS);
END RestoreAt;

PROCEDURE (c: Config) ClearPos*;
END ClearPos;

PROCEDURE (c: Config) Level*(): INTEGER;
BEGIN
  RETURN 0
END Level;

PROCEDURE (c: Config) Parse*(s-: ARRAY OF CHAR; VAR name: String);
(** Returns "name" if c.res#wrongSyntax;
  Uses aditional "res" to distinguish between options and equations.
*)
BEGIN
  name:=NIL;
END Parse;

PROCEDURE (c: Config) IsValidTag*(s-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN FALSE
END IsValidTag;

PROCEDURE (c: Config) GetTimeStamp*(): TimeStamp;
BEGIN
  RETURN 0
END GetTimeStamp;

(*----------------------------------------------------------------*)

PROCEDURE (i: Info) compiler_phase*(n: INTEGER);
(** ABSTRACT *)
END compiler_phase;

PROCEDURE (i: Info) Version*(VAR version: String);
(** Prints compiler version *)
END Version;

PROCEDURE (i: Info) Header*(language: Lang);
(** Prints compiler header *)
END Header;

PROCEDURE (i: Info) Report*;
END Report;

PROCEDURE (i: Info) Total*;
(** Prints compiler total report *)
END Total;

PROCEDURE (i: Info) print*(format-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
(** To print help, utility headers, etc *)
END print;

PROCEDURE (i: Info) Reset*;
(** CONCRETE. Clears all fields. *)
BEGIN
  i.language:=SYSTEM.VAL(Lang,-1);
  i.file:=NIL;
  i.code_file:=NIL;
  i.module:=NIL;
  i.mod_no:=INVMno;
  i.main:=FALSE;
  i.lines:=0;
  i.newSF:=FALSE;
  i.newDF:=FALSE;
 <* IF ~STATANALYSIS THEN *> -- this assignment should be moved from here 
  i.en_b_end:=TRUE;          -- to xmErrors.ob2 for statical analysis tools 
 <* END *> 
END Reset;

(*----------------------------------------------------------------*)

PROCEDURE (e: Errors) PrintMsg*(ps: TPOS;
                              type: CHAR;
                              fmt-: ARRAY OF CHAR;
                             SEQ x: SYSTEM.BYTE);
(** ABSTRACT.
  type:
    'e' - error,
    'w' - warning,
    'f' - fatal error
    'v' - environment error
    'm' - message
*)
BEGIN ASSERT(FALSE);
END PrintMsg;

PROCEDURE (e : Errors) GetLastMsg*(VAR s : String);
(** ABSTRACT. Returns text of last output error message *)
BEGIN ASSERT(FALSE);  
END GetLastMsg;

PROCEDURE (e: Errors) GetMsg*(no: INTEGER; VAR s: MESSAGE);
(** ABSTRACT. Returns text of message *)
BEGIN ASSERT(FALSE);
END GetMsg;

PROCEDURE (e: Errors) Message*(no: INTEGER; SEQ x: SYSTEM.BYTE);
(** CONCRETE *)
  VAR buf: MESSAGE;
BEGIN
  e.GetMsg(no,buf);
  e.PrintMsg(null_pos,'m',buf,x);
END Message;

PROCEDURE (e: Errors) EnvError*(no: INTEGER; SEQ x: SYSTEM.BYTE);
(** CONCRETE: environment error *)
  VAR buf: MESSAGE;
BEGIN
  e.GetMsg(no,buf);
  e.PrintMsg(null_pos,'v',buf,x);
  INC(e.env_err_cnt);
  INCL(err_sum,other);
END EnvError;

PROCEDURE (e: Errors) Warning*(ps: TPOS; no: INTEGER; SEQ x: SYSTEM.BYTE);
(** CONCRETE *)
  VAR buf: MESSAGE;
BEGIN
  e.GetMsg(no,buf);
  e.PrintMsg(ps,'w',buf,x);
END Warning;

PROCEDURE (e: Errors) Error*(ps: TPOS; no: INTEGER; SEQ x: SYSTEM.BYTE);
(** CONCRETE *)
  VAR buf: MESSAGE;
BEGIN
  e.GetMsg(no,buf);
  e.PrintMsg(ps,'e',buf,x);
END Error;

PROCEDURE (e: Errors) Fault*(ps: TPOS; no: INTEGER; SEQ x: SYSTEM.BYTE);
(** CONCRETE *)
  VAR buf: MESSAGE;
BEGIN
  e.GetMsg(no,buf);
  e.PrintMsg(ps,'f',buf,x);
END Fault;

PROCEDURE (e: Errors) Execute*(proc: PROC; VAR fault: BOOLEAN);
(** ABSTRACT. Executes proc. *)
BEGIN ASSERT(FALSE)
END Execute;

PROCEDURE (e: Errors) Reset*;
(** CONCRETE. Sets counters to zero *)
BEGIN
  e.soft_fault:=FALSE;
  e.err_cnt:=0;
  e.wrn_cnt:=0;
  e.env_err_cnt:=0;
END Reset;

(*----------------------------------------------------------------*)

PROCEDURE getFnamesLen*():LONGINT;
BEGIN
  RETURN LEN(fnames^);
END getFnamesLen;

PROCEDURE getFnameIndex*(ps-:TPOS):LONGINT;
BEGIN
  RETURN ps.file;
END getFnameIndex;

PROCEDURE (VAR ps: TPOS) pack*(fname-: ARRAY OF CHAR; line,col: LONGINT);
  VAR fnm,i: INTEGER; w: FNAMES;
BEGIN
  ASSERT((line>=0) & (col>=0));
  IF col>=max_cols THEN col:=max_cols-1 END;
  IF line>=max_lines THEN line:=max_lines-1 END;
  IF (fnames_cur<fnames_cnt) & (fnames[fnames_cur]^=fname) THEN
    fnm:=fnames_cur;
  ELSE
    fnm:=0;
    LOOP
      IF fnm=fnames_cnt THEN
        IF fnames_cnt=LEN(fnames^) THEN
          NEW(w,LEN(fnames^)*2);
          FOR i:=0 TO fnames_cnt-1 DO w[i]:=fnames[i] END;
          fnames:=w;
        END;
        NEW(fnames[fnm],LENGTH(fname)+1);
        COPY(fname,fnames[fnm]^);
        INC(fnames_cnt);
        EXIT;
      ELSIF fnames[fnm]^=fname THEN
        fnames_cur:=fnm;
        EXIT;
      ELSE
        INC(fnm);
      END;
    END;
  END;
  ps.line:=line;
  ps.file:=fnm;
  ps.col:=VAL(INTEGER, col);
END pack;

PROCEDURE (VAR ps: TPOS) unpack*(VAR fname: String;
                        VAR line,col: LONGINT);
BEGIN
  IF ps.line=MAX(LONGINT) THEN
    line:=-1; col:=-1; fname:=NIL;
  ELSE
    fname:=fnames[ps.file];
    line:=ps.line;
    col:=ps.col;
  END;
END unpack;

PROCEDURE (VAR ps: TPOS) IsNull*(): BOOLEAN;
BEGIN
  RETURN ps.line=MAX(LONGINT);
END IsNull;

PROCEDURE (VAR x: TPOS) cmp*(y-: TPOS): INTEGER;
BEGIN
  IF x.file<y.file THEN RETURN -1 END;
  IF x.file>y.file THEN RETURN  1 END;
  IF x.line<y.line THEN RETURN -1 END;
  IF x.line>y.line THEN RETURN  1 END;
  IF x.col <y.col  THEN RETURN -1 END;
  IF x.col >y.col  THEN RETURN  1 END;
  RETURN 0;
END cmp;

<* IF TARGET_IDB THEN *>

PROCEDURE (VAR x: TPOS) get_fid*(): INTEGER;
BEGIN
  RETURN x.file;
END get_fid;

PROCEDURE get_file_name*(id:INTEGER):String;
BEGIN
 RETURN fnames[id];
END get_file_name;

<* END *>


PROCEDURE (VAR x: TPOS) equ*(y-: TPOS): BOOLEAN;
BEGIN
  RETURN x.cmp(y)=0;
END equ;

PROCEDURE (VAR x: TPOS) neq*(y-: TPOS): BOOLEAN;
BEGIN
  RETURN x.cmp(y)#0;
END neq;

PROCEDURE (VAR x: TPOS) lss*(y-: TPOS): BOOLEAN;
BEGIN
  RETURN x.cmp(y)<0;
END lss;

PROCEDURE (VAR x: TPOS) gtr*(y-: TPOS): BOOLEAN;
BEGIN
  RETURN x.cmp(y)>0;
END gtr;

PROCEDURE (VAR x: TPOS) leq*(y-: TPOS): BOOLEAN;
BEGIN
  RETURN x.cmp(y)<=0;
END leq;

PROCEDURE (VAR x: TPOS) geq*(y-: TPOS): BOOLEAN;
BEGIN
  RETURN x.cmp(y)>=0;
END geq;

(*----------------------------------------------------------------*)

PROCEDURE (a: Args) Number*(): INTEGER;
BEGIN RETURN 0
END Number;

PROCEDURE (a: Args) GetArg*(i: INTEGER; VAR val: String);
END GetArg;

PROCEDURE (a: Args) DeleteArg*(i: INTEGER);
END DeleteArg;

PROCEDURE (a: Args) ProgramName*(VAR name: String);
(** returns internal representation of program file name *)
END ProgramName;

PROCEDURE (a: Args) EnvString*(name-: ARRAY OF CHAR; VAR s: String);
END EnvString;

PROCEDURE (a: Args) Parse*;
END Parse;

(*----------------------------------------------------------------*)

PROCEDURE (sh: Shell) Active*(): BOOLEAN;
(** Is shell active? *)
BEGIN
  RETURN FALSE
END Active;

PROCEDURE (sh: Shell) String* (s-: ARRAY OF CHAR);
(** Send the string to output window *)
END String;

PROCEDURE (sh: Shell) Caption* (s-: ARRAY OF CHAR);
(** Send global caption (for example, "Making project xxx") *)
END Caption;

CONST (** Shell error types *)
  MSG_NOTICE  *= 'N';
  MSG_WARNING *= 'W';
  MSG_ERROR   *= 'E';
  MSG_SEVERE  *= 'S';

PROCEDURE (sh: Shell) Error* (type: CHAR;
                                no: INTEGER;
                              x, y: LONGINT;
                        fname-, s-: ARRAY OF CHAR);
(** Send a message "s" in file "fname", line "l", column "c",
   error type "type".
   File name or message body can be empty strings.
   If file name is not empty, it should contain ABSOLUTE path.
   Possible classes:
     MSG_NOTICE  - just some string to be drawn in messages window.
     MSG_WARNING - warning, message that does not affect results of compiling.
     MSG_ERROR   - error
     MSG_SEVERE  - fault - makes further compilation impossible
*)
END Error;

PROCEDURE (sh: Shell) Comment* (s-: ARRAY OF CHAR);
(** Send current caption (for example, "Compiling xxx.mod", or
    "Linking xxx.exe")
*)
END Comment;

PROCEDURE (sh: Shell) StartJob* (s-: ARRAY OF CHAR; progress_limit: LONGINT);
(** Start a job and draw progress indicator;
   progress_limit - maximal value of progress indicator
   comment - string to be drawn to the left of progress indicator
             (for example, "Lines compiled: 1250").
             Can contain "%ld" in place of current progress.
*)
END StartJob;

PROCEDURE (sh: Shell) Progress* (comment_progress, progress: LONGINT);
(** Move progress indicator and increase the number in the comment.
   When comment_progress and progress are non-negative, they are treated
   as offsets to current values.
   Otherwise, their absolute values are treated as new values.
*)
END Progress;

PROCEDURE (sh: Shell) StartFileList*;
(** Start output a list of files in a project *)
END StartFileList;

PROCEDURE (sh: Shell) AppendFile* (fullname-: ARRAY OF CHAR);
(** Append a file to a project *)
END AppendFile;

PROCEDURE (sh: Shell) EndFileList*;
(** Finish output a list of files in a project (so that the shell knows
    that the list is OK, compiler hadn't died during outputting)
*)
END EndFileList;

(*----------------------------------------------------------------*)

PROCEDURE OptionList* (s-: ARRAY OF CHAR);
  VAR i,n: INTEGER; name: OPNAME; ch: CHAR;
BEGIN
  i:=0;
  REPEAT
    n:=0;
    LOOP
      ch := s[i]; INC(i);
      IF (ch = '-') OR (ch = '+') THEN EXIT END;
      name[n] := ch;
      INC(n);
    END;
    name[n] := 0X;
    config.NewOption(name, (ch='+'), SYSTEM.VAL(CompilerOption,-1));
  UNTIL s[i] = 0X;
END OptionList;

PROCEDURE EquationList* (s-: ARRAY OF CHAR);
  VAR i,n: INTEGER; name: OPNAME; val: ARRAY 16 (*!!!*) OF CHAR;
BEGIN
  i:=0;
  REPEAT
    n:=0;
    WHILE (s[i]#';') & (s[i]#'=') DO name[n]:=s[i]; INC(n); INC(i) END;
    name[n]:=0X;
    config.NewEquation(name);
    IF s[i]='=' THEN
      INC(i); n:=0;
      WHILE s[i]#';' DO val[n]:=s[i]; INC(n); INC(i) END;
      val[n]:=0X;
      config.SetEquation(name,val);
    END;
    INC(i)
  UNTIL s[i]=0X;
END EquationList;

PROCEDURE Capitalize*(VAR str: String);
(** This procedure can be used as EQ_ACTION itself, also it can be used
    as a part of EQ_ACTION *)
  VAR i: INTEGER;
BEGIN
  IF str=NIL THEN RETURN END;
  i:=0;
  WHILE (i<LEN(str^)) & (str[i]#0X) DO
    str[i]:=CAP(str[i]); INC(i);
  END;
END Capitalize;

PROCEDURE IsValueIn*(val-,set-: ARRAY OF CHAR): INTEGER;
(** This procedure checks that value "val" is in the value set "set" and
    returns it's index (starting from 0). Iv "val" is not in "set" it
    returns -1.
    The "set" syntax is semicolon-separated string: i.e.
      "VALONE;VALTWO;VALTHREE" *)
  VAR ch0,ch1: CHAR; n,i,x: INTEGER;
BEGIN
  n:=0; x:=0;
  REPEAT
    i:=0;
    REPEAT
      IF n>=LEN(set) THEN ch0:=0X ELSE ch0:=set[n] END; INC(n);
      IF i>=LEN(val) THEN ch1:=0X ELSE ch1:=val[i] END; INC(i);
    UNTIL (ch0#ch1) OR (ch1=0X);
    IF ((ch0=";") OR (ch0=0X)) & (ch1=0X) THEN RETURN x END;
    INC(x);
    WHILE (n<LEN(set)) & (set[n]#";") & (set[n]#0X) DO INC(n) END;
    IF (n<LEN(set)) & (set[n]=";") THEN INC(n) END;
  UNTIL (n>=LEN(set)) OR (set[n]=0X);
  RETURN -1
END IsValueIn;

PROCEDURE Clear*;
BEGIN
  IF errors#NIL THEN errors.Reset    END;
  IF info  #NIL THEN info.Reset      END;
  IF config#NIL THEN config.ClearPos END;
END Clear;

BEGIN
  NEW(shell);
  fnames_cnt:=0;
  fnames_cur:=0;
  NEW(fnames,128);
  Clear;
(*  null_pos.line:=MAX(LONGINT);
  null_pos.col :=MAX(INTEGER);
  null_pos.file:=MAX(INTEGER);
*)
  err_sum:={};
  decor:={};
END xiEnv.
