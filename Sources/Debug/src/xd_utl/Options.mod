IMPLEMENTATION MODULE Options;
(* Набор переменных, определяющих режим работы отладчика *)

<* IF DEST_XDS THEN *>

IMPORT Translit;

<* END *>



<* IF DEFINED (xd_debug) & xd_debug THEN *>

VAR
  Debug_Info : DEBUG_INFO;   (* Выдавать отладочную информацию *)


PROCEDURE DebugOn (d: DEBUG_MODE); (* Включить режим *)
BEGIN
  INCL(Debug_Info, d);
END DebugOn;


PROCEDURE Debug (d: DEBUG_MODE) : BOOLEAN; (* Включен ли режим? *)
BEGIN
  RETURN d IN Debug_Info;
END Debug;

<* END *>

PROCEDURE SetXY (x, y: CARDINAL);
BEGIN
  X := x;
  Y := y;
END SetXY;


BEGIN
  DialogMode  := FALSE; (* по умолчанию пакетный режим *)
  tst_name    := '';
  prog_name   := '';
  prog_args   := '';
  Stop_Pack   := FALSE;
  in_dialog   := FALSE;
  name_only   := FALSE;
  Code        := FALSE;
  WarningBell := TRUE;
  SaveOpt     := FALSE;
  CodeHilight := TRUE;
  CallHilight := FALSE;
  WholeHex    := FALSE;
  KbdFile     := '';

  ShowAllModules := FALSE;
  InitDumpType   := 0;

  SetXY (0, 0);

(*
  JumpToMainEntry := TRUE;
  JumpToProgramEntry  := FALSE;
*)
  StopImmediately:= FALSE;
  SkipDisasm       := TRUE;

  ShowModuleWithoutSource  := TRUE; (* Показывать модули без исходного текста *)
  DisplayDerefencePointer  := FALSE;
  CatchExceptInternalError := TRUE;
  UseSingleStructureWindow := TRUE;
  MergeEqualTypes          := FALSE;

<* IF DEST_K26 THEN *>

  DisasmMode           := FALSE;
  ConvertVar2Ref       := FALSE;   (* Представление переменной как ссылки (см. Expr.def) *)
  TableModel           := FALSE;
  TraceRegisters       := TRUE;
  IgnoreWriteProtected := FALSE;

<* ELSIF DEST_XDS THEN *>

  DisasmMode     := TRUE;
  ConvertVar2Ref := FALSE;

  ExceptionOnFirstChance := TRUE;
  ShowSoftwareException  := TRUE;
  SSS_Delay              := 500;
  CorrectObjectName      := TRUE;

  StripPathFromFullName    := FALSE;
  StripPathFromPartialName := FALSE;
  TranslirateTextFromTo    := Translit.nn; -- no transliterate

  RemoteMode      := FALSE;
  RemoteHost      := '';
  RemotePort      := 0;
  RemoteTransport := '';

  IgnoreWriteProtected := TRUE;
  KernelInRemoteMode   := FALSE;
  AutoDetectActualType := TRUE; (* Определять настоящий тип обьектов      *)

<* END *>


<* IF DEFINED (xd_debug) & xd_debug THEN *>
  Debug_Info := DEBUG_INFO {};
<* END *>

END Options.
