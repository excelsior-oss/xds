-- Главный модуль компоненты xd: отладчика

MODULE XD_Main;

IMPORT com := COMPILER;
IMPORT tim := TimeConv;
IMPORT arg := ProgEnv;

IMPORT xdt := XD_Title;
IMPORT xdp := XD_Parse;

IMPORT xStr;
IMPORT pro := Protocol;
IMPORT red := RedFile;
IMPORT opt := Options;

<* IF DEFINED(help_included) AND help_included THEN *>
IMPORT hlp := Help;
<* END*>

<* IF xd_batch_included THEN *>
IMPORT pkt := Pack;
<* END *>

IMPORT dlg := Dlg_Main;

FROM Printf IMPORT printf;



PROCEDURE Copyright;
VAR
  d: tim.DateTime;
BEGIN
  tim.unpack (d, com.TIMESTAMP);
  printf("\n%s, Version %s ", xdt.PRODUCT, xdt.VERSION);
  printf("[build %$2d.%$2d.%4d]\n", d.day, d.month, d.year);
  printf("(c) %s\n\n", xdt.COPYRIGHT);
END Copyright;


(* Краткий/полный Help! *)
<* PUSH *>
<* WOFF301+ *>
PROCEDURE Help (full: BOOLEAN);
<* POP *>

<* IF DEFINED(help_included) AND help_included THEN *>

  (* Выдача m секции Help! *)
  PROCEDURE WriteHelpSection (m: CARDINAL);
  VAR
    i: CARDINAL;
    s: xStr.txt_ptr;
    l: xStr.String;
  BEGIN
    FOR i := 0 TO hlp.GetNLine(m) DO
      s := hlp.GetLine(m,i);
      COPY(s^, l);
      printf("%s\n", l);
    END;
  END WriteHelpSection;

BEGIN
  WriteHelpSection(1);
  IF full THEN WriteHelpSection(2); END;
  HALT (7);
<* END *>
END Help;



VAR
  ok  : BOOLEAN;
  help: BOOLEAN;

BEGIN
  Copyright;

  ok := xdp.ParseCommandLine(help);

  red.InitRedirection;
  pro.InitProtocol ('');
 <* IF DEFINED(help_included) AND help_included THEN *>
  hlp.CreateHelp;
 <* END *>

  IF NOT ok OR help THEN Help(help); END;

  IF opt.DialogMode THEN
    dlg.Start;
  ELSE
   <* IF xd_batch_included THEN *>
    pkt.Start;
   <* ELSE *>
    printf("Batch mode is not supported in this version.\n");
   <* END *>
  END;
EXCEPT
  pro.WriteMsg ('*** Sorry, bug in debugger ***', TRUE, TRUE);
  pro.WriteMsg ('', TRUE, TRUE);
END XD_Main.

