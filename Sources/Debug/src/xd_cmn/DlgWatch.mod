<* Storage+ *>
IMPLEMENTATION MODULE DlgWatch;

IMPORT sys := SYSTEM;

IMPORT mod := DlgMods;
IMPORT dv  := Dlg_Vars;
IMPORT dlt := DlgTypes;

IMPORT exp := Expr;
IMPORT Str := xStr;

IMPORT dt  := DI_Types;



PROCEDURE RecalcWatches;
VAR
  i: CARDINAL;
BEGIN
  WITH Watches DO
    IF count > 0 THEN
      FOR i := 0 TO count - 1 DO
        WITH Watch^[i] DO
          res_ := res;
          exp.CalcExpr(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, expr^,res);
          dfn := exp.dfn;
          error := exp.error;
        END;
      END;
    END;
  END;
END RecalcWatches;

PROCEDURE NewWatch(): BOOLEAN;
CONST
  Q_WATCH = 32;
VAR
  tmp: PWATCH;
  m  : CARDINAL;
BEGIN
  WITH Watches DO
    IF count > 0 THEN
      FOR m := 0 TO count-1 DO
        IF dv.expr = Watch^[m].expr^ THEN
          WatchPos := m;
          RETURN TRUE;
        END;
      END;
    END;
    IF WatchPos = MAX(CARDINAL) THEN
      IF Watch = NIL THEN
        NEW(Watch, Q_WATCH);
        ASSERT(Watch # NIL);
        count := 0;
      ELSIF count >= HIGH(Watch^)+1 THEN
        NEW(tmp,HIGH(Watch^)+1+Q_WATCH);
        ASSERT(tmp # NIL);
        sys.MOVE(sys.ADR(Watch^), sys.ADR(tmp^), SIZE(Watch^));
        DISPOSE(Watch);
        Watch := tmp;
      END;
      Str.alloc_from(Watch^[count].expr, dv.expr);
      IF mod.Curr # NIL THEN
        m := mod.Curr^.Pos.ModN;
      ELSE
        m := dt.Invalid_Module;
      END;
      exp.CalcExpr(mod.Curr^.Pos.ComN, m, dv.expr, Watch^[count].res);
      Watch^[count].res_ := Watch^[count].res;
      Watch^[count].dfn := exp.dfn;
      Watch^[count].error := exp.error;
      WatchPos := count;
      INC(count);
    ELSE
      Str.dealloc_str(Watch^[WatchPos].expr);
      Str.alloc_from(Watch^[WatchPos].expr, dv.expr);
      WITH Watch^[WatchPos] DO
        IF mod.Curr # NIL THEN
          m := mod.Curr^.Pos.ModN;
        ELSE
          m := dt.Invalid_Module;
        END;
        exp.CalcExpr(mod.Curr^.Pos.ComN, m, expr^, res);
        res_ := res;
        dfn := exp.dfn;
        error := exp.error;
      END;
    END;
  END;
  RETURN TRUE;
END NewWatch;

PROCEDURE WatchFromPack(str-: ARRAY OF CHAR);
BEGIN
  COPY(str, dv.expr);
  WatchPos := MAX(CARDINAL);
  sys.EVAL(NewWatch());
  dv.expr := '';
END WatchFromPack;



PROCEDURE WriteWatches (out: dlt.OUT_PROC);
VAR
  i: CARDINAL;
BEGIN
  WITH Watches DO
    IF count > 0 THEN
      FOR i := 0 TO count-1 DO
        WITH Watch^[i] DO
          out ('  WATCH %s\n', expr^);
        END;
      END;
      out ('\n');
    END;
  END;
END WriteWatches;




BEGIN
  Watches     := WATCH{0, NIL};
  WatchPos    := MAX(CARDINAL);
END DlgWatch.