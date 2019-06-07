<* Storage+ *>

IMPLEMENTATION MODULE KrnIndex;

IMPORT sys := SYSTEM;

CONST
  HN = 32;

(* Добавит новый индекс, вернет его номер *)
PROCEDURE AddIndex (I: INDEX_REC): CARDINAL;
VAR
  tmp: PINDEX;
  pos: CARDINAL;
  i  : CARDINAL;
BEGIN
  WITH Index DO
    IF Index = NIL THEN
      NEW(Index, HN);
      ASSERT(Index # NIL);
      Free := 0;
      pos  := 0;
    ELSE
<* WOFF312+ *>
      LOOP
        FOR i := 0 TO Free-1 DO
          IF NOT Index^[i].Busy THEN
            Index^[i] := I;
            RETURN i+1;
          END;
        END;
        IF Free > HIGH(Index^) THEN
          NEW(tmp, HIGH(Index^)+1 + HN);
          ASSERT(tmp # NIL);
          sys.MOVE(sys.ADR(Index^[0]), sys.ADR(tmp^[0]), (HIGH(Index^)+1)*SIZE(INDEX_REC));
          DISPOSE(Index);
          Index := tmp;
        END;
        pos := Free;
        EXIT;
      END;
<* WOFF312- *>
    END;
    Index^[pos] := I;
    INC(Free);
  END;
  RETURN pos+1;
END AddIndex;



(* Удалит индекс по номеру, если не было - вернет FALSE *)
PROCEDURE DelIndex (N: CARDINAL; VAR I: INDEX_REC): BOOLEAN;
BEGIN
  WITH Index DO
    IF (Index = NIL) OR (N = 0) OR (N > Free) THEN RETURN FALSE; END;
    IF Index^[N-1].Busy THEN
      Index^[N-1].Busy := FALSE;
      I := Index^[N-1];
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END;
END DelIndex;


(* Существует ли подобный в индексе *)
PROCEDURE Find (N: CARDINAL): BOOLEAN;
VAR
  I: INDEX_REC;
  i: CARDINAL;
BEGIN
  WITH Index DO
    IF (Index = NIL) OR (N = 0) OR (N > Free) THEN RETURN FALSE; END;
    I := Index^[N-1];
    FOR i := 0 TO Free-1 DO
      WITH Index^[i] DO
        IF (i # N-1) AND Busy AND (Access = I.Access)  THEN
          CASE Access OF
          | READ, WRITE : IF (Addr = I.Addr) AND (Len = I.Len) THEN RETURN TRUE; END;
          | READ_REG    ,
            WRITE_REG   : IF (RegNo = I.RegNo) THEN RETURN TRUE; END;
          | EXECUTE     : IF (Addr = I.Addr) THEN RETURN TRUE; END;
          ELSE
            ASSERT(FALSE);
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END Find;


BEGIN
  Index := INDEX{ NIL, 0 };
END KrnIndex.
