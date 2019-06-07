<* IF TARGET_68K OR TARGET_RISC OR TARGET_VAX THEN *>
This module can be used only in TARGET_386
<* END *>
MODULE opTune_I;
IMPORT ir, Calc, opAttrs, opTune;
IMPORT pc := pcK;

TYPE
    opTune_386_IDB = POINTER TO RECORD(opTune.opTune_IDB_Rec) END;
VAR
    IDB : opTune_386_IDB;

(* Имеет ли смысл SR умножение на эту целую константу *)
PROCEDURE(idb : opTune_386_IDB)
         NeedSR (v: ir.VALUE; t: ir.TypeType; s: ir.SizeType): BOOLEAN;
BEGIN
    RETURN NOT (((s = 2) OR (s = 4)) &
                (Calc.CompareWithInt (pc.sb_equ, v, 2, t, s) OR
                 Calc.CompareWithInt (pc.sb_equ, v, 4, t, s) OR
                 Calc.CompareWithInt (pc.sb_equ, v, 8, t, s)));
END NeedSR;

(* Имеет ли смысл "безопасное" умножение заменять сдвигами и сложениями *)
PROCEDURE(idb : opTune_386_IDB)
         NeedAdd(v: ir.VALUE; t: ir.TypeType; s: ir.SizeType): BOOLEAN;
VAR o: ir.INT;
BEGIN
    IF Calc.IsNegative (v, s) THEN
        RETURN FALSE;
    END;
    o := Calc.NOnes (v, s);
    IF NOT (opAttrs.SPACE IN opAttrs.COMP_MODE) THEN
        RETURN o < 6;
    END;
    RETURN (o = 1) OR (o = 2) & Calc.CompareWithInt (pc.sb_lss, v, 10, t, s);
END NeedAdd;

PROCEDURE(idb : opTune_386_IDB)
        MinLocalStart (proc:pc.OBJECT):LONGINT;(* конец первого локала - далее убывают *)
BEGIN
  IF proc.flag=pc.flag_vmcall THEN
    RETURN -28;
  ELSE
    RETURN -16;
  END;
END MinLocalStart;

BEGIN
  NEW(IDB);
  IDB.ld_real_sz := 10;
  opTune.IDB := IDB;
END opTune_I.
