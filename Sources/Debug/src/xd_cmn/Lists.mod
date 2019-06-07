<* Storage + *>

IMPLEMENTATION MODULE Lists;

IMPORT sys := SYSTEM;

IMPORT krn := KrnTypes;

IMPORT xs  := xStr;


TYPE
  Label = RECORD
            LabelName: xs.String;
            Paket    : CARDINAL;
            Line     : CARDINAL;
          END;

  LabelPtr = POINTER TO ARRAY OF Label;
  LabelsRec = RECORD
                List : LabelPtr;
                Count: CARDINAL;
              END;


  Call = RECORD
            Pr   : BOOLEAN;
            Paket: CARDINAL;
            Line : CARDINAL;
         END;

  CallPtr = POINTER TO ARRAY OF Call;
  CallsRec = RECORD
                List : CallPtr;
                Count: CARDINAL;
             END;


  EquName = RECORD
              Name   : xs.String;
              EquName: xs.String;
              Engaged: BOOLEAN;
            END;

  EquNamePtr = POINTER TO ARRAY OF EquName;
  EquNamesRec = RECORD
                   List : EquNamePtr;
                   Count: CARDINAL;
                 END;


  JumpByError = RECORD
                  Error: CARDINAL;
                  Paket: CARDINAL;
                  Line : CARDINAL;
                END;
  JumpByErrorPtr = POINTER TO ARRAY OF JumpByError;

  JumpByErrorsRec = RECORD
                      List : JumpByErrorPtr;
                      Count: CARDINAL;
                    END;

  BREAK_INFO = RECORD
                 Name  : xs.String;
                 Number: CARDINAL;
               END;

  PABREAK_INFO = POINTER TO ARRAY OF BREAK_INFO;

  BREAKS_INFO = RECORD
                  List : PABREAK_INFO;
                  Count: CARDINAL;
                END;

  PAARGUMENTS = POINTER TO ARRAY OF xs.String;

  ARGUMENTS = RECORD
                List : PAARGUMENTS;
                Count: CARDINAL;
              END;


VAR
  Labels      : LabelsRec;       (* Список меток                        *)
  Calls       : CallsRec;        (* Стек вызовов подпрограм и вариантов *)
  EquNames    : EquNamesRec;     (* Список экв. имен                    *)
  JumpByErrors: JumpByErrorsRec; (* Список адресов переходов по ошибке  *)
  BreaksInfo  : BREAKS_INFO;     (* Список точек останова               *)
  Arguments   : ARGUMENTS;


PROCEDURE getlabel (VAR Labels: LabelsRec;
                    st-:ARRAY OF CHAR;
                    VAR paket,line:CARDINAL) : BOOLEAN;
VAR
  p : CARDINAL;
BEGIN
  WITH Labels DO
    IF Count = 0 THEN RETURN FALSE; END;
    p := 0;
    LOOP
      IF List^[p].LabelName = st THEN
        paket := List^[p].Paket;
        line := List^[p].Line;
        RETURN TRUE;
      ELSE
        INC(p);
        IF p >= Count THEN EXIT; END;
      END;
    END;
    RETURN FALSE;
  END;
END getlabel;


PROCEDURE putlabel (VAR Labels: LabelsRec;
                    st-:ARRAY OF CHAR;
                    paket,line:CARDINAL) ;
VAR
  pkt,lin : CARDINAL;
  tmp : LabelPtr;
BEGIN
  WITH Labels DO
    IF NOT getlabel(Labels, st, pkt, lin) THEN
      Duplicate := FALSE;
      IF (List = NIL) THEN
        NEW(List, 8);
        ASSERT(List#NIL);
        Count := 0;
      ELSIF Count > HIGH(List^) THEN
        NEW(tmp, 2*(HIGH(List^)+1));
        ASSERT(tmp#NIL);
        sys.MOVE(sys.ADR(List^), sys.ADR(tmp^), SIZE(List^));
        DISPOSE(List);
        List := tmp;
      END;
      WITH List^[Count] DO
        COPY(st, LabelName);
        Paket := paket;
        Line := line;
      END;
      INC(Count);
    ELSE
      Duplicate := TRUE;
    END;
  END;
END putlabel;


PROCEDURE IsLabel (st: ARRAY OF CHAR): BOOLEAN;
VAR
  paket, line: CARDINAL;
BEGIN
  RETURN getlabel (Labels, st, paket, line);
END IsLabel;


PROCEDURE GetLabel (st-:ARRAY OF CHAR; VAR paket,line:CARDINAL) : BOOLEAN;
BEGIN
  RETURN getlabel(Labels, st, paket, line);
END GetLabel;


PROCEDURE PutLabel (st-:ARRAY OF CHAR; paket,line:CARDINAL) ;
BEGIN
  putlabel(Labels, st, paket, line);
END PutLabel;


PROCEDURE PopCall (VAR paket, line:CARDINAL);
BEGIN
  WITH Calls DO
    ASSERT((List # NIL) AND (Count # 0));
    paket := List^[Count-1].Paket;
    line := List^[Count-1].Line;
    DEC(Count);
  END;
END PopCall;

PROCEDURE PushCall (paket,line:CARDINAL; pr:BOOLEAN);
VAR
  tmp : CallPtr;
BEGIN
  WITH Calls DO
    IF (List = NIL) THEN
      NEW(List, 8);
      ASSERT(List#NIL);
      Count := 0;
    ELSIF Count > HIGH(List^) THEN
      NEW(tmp, 2*(HIGH(List^)+1));
      ASSERT(tmp#NIL);
      sys.MOVE(sys.ADR(List^), sys.ADR(tmp^), SIZE(List^));
      DISPOSE(List);
      List := tmp;
    END;
    WITH List^[Count] DO
      Pr := pr;
      Paket := paket;
      Line := line;
    END;
    INC(Count);
  END;
END PushCall;

PROCEDURE IsEmptyCall () : BOOLEAN;
BEGIN
  RETURN (Calls.Count = 0);
END IsEmptyCall;

PROCEDURE IsGOSUBCall () : BOOLEAN;
BEGIN
  WITH Calls DO RETURN List^[Count-1].Pr; END;
END IsGOSUBCall;


PROCEDURE IsEquName (name-:ARRAY OF CHAR) : BOOLEAN;
VAR
  p : CARDINAL;
BEGIN
  WITH EquNames DO
    IF Count = 0 THEN RETURN FALSE; END;
    p := 0;
    LOOP
      IF (List^[p].Name = name) THEN
        RETURN TRUE;
      ELSE
        INC(p);
        IF p >= Count THEN EXIT; END;
      END;
    END;
  END;
  RETURN FALSE;
END IsEquName;


PROCEDURE EquNamesNo (): CARDINAL;
BEGIN
  RETURN EquNames.Count;
END EquNamesNo;


PROCEDURE GetName (i: CARDINAL; VAR name: ARRAY OF CHAR);
BEGIN
  COPY(EquNames.List^[i].Name, name);
END GetName;


PROCEDURE GetEquName (name-:ARRAY OF CHAR; VAR equname:ARRAY OF CHAR);
VAR
  p : CARDINAL;
BEGIN
  WITH EquNames DO
    IF Count = 0 THEN RETURN; END;
    p := 0;
    LOOP
      IF List^[p].Name = name THEN
        COPY(List^[p].EquName, equname);
        RETURN;
      ELSE
        INC(p);
        IF p >= Count THEN EXIT; END;
      END;
    END;
  END;
END GetEquName;



PROCEDURE SetEngaged (name-:ARRAY OF CHAR);
VAR
  p : CARDINAL;
BEGIN
  WITH EquNames DO
    IF Count = 0 THEN RETURN; END;
    p := 0;
    LOOP
      IF List^[p].Name = name THEN
        List^[p].Engaged := TRUE;
        RETURN;
      ELSE
        INC(p);
        IF p >= Count THEN EXIT; END;
      END;
    END;
  END;
END SetEngaged;

PROCEDURE DelEngaged (name-:ARRAY OF CHAR);
VAR
  p : CARDINAL;
BEGIN
  WITH EquNames DO
    IF Count = 0 THEN RETURN; END;
    p := 0;
    LOOP
      IF List^[p].Name = name THEN
        List^[p].Engaged := FALSE;
        RETURN;
      ELSE
        INC(p);
        IF p >= Count THEN EXIT; END;
      END;
    END;
  END;
END DelEngaged;

PROCEDURE IsEngaged  (name-:ARRAY OF CHAR) : BOOLEAN;
VAR
  p : CARDINAL;
BEGIN
  WITH EquNames DO
    IF Count = 0 THEN RETURN FALSE; END;
    p := 0;
    LOOP
      IF List^[p].Name = name THEN
        RETURN List^[p].Engaged;
      ELSE
        INC(p);
        IF p >= Count THEN EXIT; END;
      END;
    END;
    RETURN FALSE;
  END;
END IsEngaged;


PROCEDURE DelEquName (name-:ARRAY OF CHAR);
VAR
  p : CARDINAL;
BEGIN
  WITH EquNames DO
    IF Count = 0 THEN RETURN; END;
    FOR p := 0 TO Count-1 DO
      IF List^[p].Name = name THEN
        IF p < Count-1 THEN
          sys.MOVE(sys.ADR(List^[p+1]), sys.ADR(List^[p]), (Count-1-p)* SIZE(EquName));
        END;
        DEC(Count);
        RETURN;
      END;
    END;
  END;
END DelEquName;





PROCEDURE PutEquName (name-:ARRAY OF CHAR; equname-:ARRAY OF CHAR);
VAR
  tmp : EquNamePtr;
BEGIN
  DelEquName(name);
  WITH EquNames DO
    IF NOT IsEquName(name) THEN
      Duplicate := FALSE;
      IF (List = NIL) THEN
        NEW(List, 8);
        ASSERT(List#NIL);
        Count := 0;
      ELSIF Count > HIGH(List^) THEN
        NEW(tmp, 2*(HIGH(List^)+1));
        ASSERT(tmp#NIL);
        sys.MOVE(sys.ADR(List^), sys.ADR(tmp^), SIZE(List^));
        DISPOSE(List);
        List := tmp;
      END;
      WITH List^[Count] DO
        COPY(name, Name);
        COPY(equname, EquName);
        Engaged := FALSE;
      END;
      INC(Count);
    ELSE
      Duplicate := TRUE;
    END;
  END;
END PutEquName;



(* Процедуры для работы с переходами по ошибке *)
PROCEDURE GetJumpByError (N: CARDINAL; VAR paket,line: CARDINAL) : BOOLEAN;
VAR
  p : CARDINAL;
BEGIN
  WITH JumpByErrors DO
    IF Count = 0 THEN RETURN FALSE; END;
    p := 0;
    LOOP
      IF List^[p].Error = N THEN
        paket := List^[p].Paket;
        line := List^[p].Line;
        RETURN TRUE;
      ELSE
        INC(p);
        IF p >= Count THEN EXIT; END;
      END;
    END;
    RETURN FALSE;
  END;
END GetJumpByError;


PROCEDURE PutJumpByError (N: CARDINAL; paket,line: CARDINAL) ;
VAR
  tmp : JumpByErrorPtr;
BEGIN
  WITH JumpByErrors DO
    IF (List = NIL) THEN
      NEW(List, 8);
      ASSERT(List#NIL);
      Count := 0;
    ELSIF Count > HIGH(List^) THEN
      NEW(tmp, 2*(HIGH(List^)+1));
      ASSERT(tmp#NIL);
      sys.MOVE(sys.ADR(List^), sys.ADR(tmp^), SIZE(List^));
      DISPOSE(List);
      List := tmp;
    END;
    WITH List^[Count] DO
      Error := N;
      Paket := paket;
      Line := line;
    END;
    INC(Count);
  END;
END PutJumpByError;


PROCEDURE DelJumpByError (N: CARDINAL);
VAR
  p : CARDINAL;
BEGIN
  WITH JumpByErrors DO
    IF Count = 0 THEN RETURN; END;
    FOR p := 0 TO Count-1 DO
      IF List^[p].Error = N THEN
        sys.MOVE(sys.ADR(List^[p+1]), sys.ADR(List^[p]), (Count-1-p)* SIZE(JumpByError));
        DEC(Count);
        RETURN;
      END;
    END;
  END;
END DelJumpByError;


(* Процедуры для работы с точками останова *)
PROCEDURE PutBreak (name: ARRAY OF CHAR; N: CARDINAL);
VAR
  tmp: PABREAK_INFO;
BEGIN
  WITH BreaksInfo DO
    IF List = NIL THEN
      NEW(List, 8);
      ASSERT(List#NIL);
      Count := 0;
    ELSIF Count > HIGH(List^) THEN
      NEW(tmp, 2*(HIGH(List^)+1));
      ASSERT(tmp#NIL);
      sys.MOVE(sys.ADR(List^), sys.ADR(tmp^), SIZE(List^));
      DISPOSE(List);
      List := tmp;
    END;
    WITH List^[Count] DO
      COPY(name, Name);
      Number := N;
    END;
    INC(Count);
  END;
END PutBreak;


PROCEDURE GetBreakNumber (name: ARRAY OF CHAR; VAR N: CARDINAL): BOOLEAN;
VAR
  p: CARDINAL;
BEGIN
  WITH BreaksInfo DO
    IF Count > 0 THEN
      FOR p := 0 TO Count-1 DO
        IF List^[p].Name = name THEN
          N := List^[p].Number;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END;
END GetBreakNumber;


PROCEDURE GetBreakName (VAR name: ARRAY OF CHAR; N: CARDINAL): BOOLEAN;
VAR
  p: CARDINAL;
BEGIN
  WITH BreaksInfo DO
    IF Count > 0 THEN
      FOR p := 0 TO Count-1 DO
        IF List^[p].Number = N THEN
          COPY(List^[p].Name, name);
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END;
END GetBreakName;


PROCEDURE DelBreak (name: ARRAY OF CHAR);
VAR
  p, i: CARDINAL;
BEGIN
  WITH BreaksInfo DO
    IF Count > 0 THEN
      FOR p := 0 TO Count-1 DO
        IF List^[p].Name = name THEN
          FOR i := 1 TO Count-1-p DO
            List^[p+i-1] := List^[p+i];
          END;
          DEC(Count);
          RETURN;
        END;
      END;
    END;
  END;
END DelBreak;


(* Процедуры для работы с аргументами пакетного отладчика *)
PROCEDURE PutArg (name: ARRAY OF CHAR);
VAR
  tmp: PAARGUMENTS;
BEGIN
  WITH Arguments DO
    IF List = NIL THEN
      NEW(List, 8);
      ASSERT(List#NIL);
      Count := 0;
    ELSIF Count > HIGH(List^) THEN
      NEW(tmp, 2*(HIGH(List^)+1));
      ASSERT(tmp#NIL);
      sys.MOVE(sys.ADR(List^), sys.ADR(tmp^), SIZE(List^));
      DISPOSE(List);
      List := tmp;
    END;
    COPY(name, List^[Count]);
    INC(Count);
  END;
END PutArg;

PROCEDURE GetArg (n: CARDINAL; VAR name: ARRAY OF CHAR): BOOLEAN;
BEGIN
  WITH Arguments DO
    IF n < Count THEN
      COPY(List^[n], name);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END;
END GetArg;


PROCEDURE ReleaseMemory;
BEGIN
  WITH Labels DO
    IF List # NIL THEN DISPOSE(List); END;
    Count := 0;
  END;
  WITH Calls DO
    IF List # NIL THEN DISPOSE(List); END;
    Count := 0;
  END;
  WITH EquNames DO
    IF List # NIL THEN DISPOSE(List); END;
    Count := 0;
  END;
  WITH JumpByErrors DO
    IF List # NIL THEN DISPOSE(List); END;
    Count := 0;
  END;
  WITH BreaksInfo DO
    IF List # NIL THEN DISPOSE(List); END;
    Count := 0;
  END;
  WITH Arguments DO
    IF List # NIL THEN DISPOSE(List); END;
    Count := 0;
  END;
END ReleaseMemory;



BEGIN
  Labels       := LabelsRec {NIL, 0};
  Calls        := CallsRec {NIL, 0};
  EquNames     := EquNamesRec {NIL, 0};
  JumpByErrors := JumpByErrorsRec {NIL, 0};
  BreaksInfo   := BREAKS_INFO {NIL, 0};
  Arguments    := ARGUMENTS {NIL, 0};
FINALLY
  ReleaseMemory;
END Lists.
