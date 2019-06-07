<* IF TARGET_68K OR TARGET_RISC OR TARGET_VAX THEN *>
This module can be used only in TARGET_386
<* END *>
MODULE DAG_I;

IMPORT ir, DAG, Color, RD := RDefs, R := r386, atr:=opAttrs, reg := reg386;
IMPORT at := opAttrs;
IMPORT D := desc386;
IMPORT BurgNT;
<* IF ~ NODEBUG THEN *>
IMPORT opIO;
IMPORT EmitGAS;
<* END *>
IMPORT Emit;
IMPORT SYSTEM;
IMPORT env := xiEnv;
TYPE
  DAG_386_IDB = POINTER TO RECORD(DAG.DAG_IDB_Rec) END;
VAR
  IDB : DAG_386_IDB;

PROCEDURE(idb : DAG_386_IDB) InitNode(p : ir.TriadePtr) : RD.DAGNODE;
VAR q : RD.DAGNODE;
BEGIN
  q := idb.InitNode^(p);
  Emit.InitAddrMode(q.a);
  q^.place.r := D.UNDEF_REG;
  q^.place.v := ir.UNDEFINED;
  RETURN q;
END InitNode;

(*
  Еще до начала первой итерации разместить часть переменных в памяти
*)
PROCEDURE(idb : DAG_386_IDB) SetLocations;
VAR i: ir.VarNum;
BEGIN
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
        IF NOT RD.Loc^[i].temp THEN
            IF NOT ((atr.nooptimize IN atr.COMP_MODE) AND (ir.Vars^[i].LocalNo <> ir.UNDEFINED))
            &  (Color.CalcRegProfit (i) > 0)
            &  ((i >= Color.NNonTempVars) OR
                (ir.Vars^[i].Def^.ResType <> ir.t_float))
            &  ~( ((ir.Vars^[i].Def^.ResType = ir.t_int) & (ir.Vars^[i].Def^.ResSize = 8))
                     AND ~env.config.Option("regint8")
                )
       -- AVY: for the most number of variables initially located in memory
        AND NOT (at.NoRegVars IN at.COMP_MODE)
        AND (RD.Loc^[i].tag # BurgNT.NTlocal)
        AND (RD.Loc^[i].tag # BurgNT.NTmem)
            THEN
                RD.Loc^[i].tag := BurgNT.NTreg;
                RD.Loc^[i].reg := RD.UNDEF_REG;
            ELSE
                idb.SetInMem (i);
            END;
        ELSE
            IF  ~( (ir.Vars^[i].Def^.ResType = ir.t_int) & (ir.Vars^[i].Def^.ResSize = 8) ) THEN
                RD.Loc^[i].tag := BurgNT.NTreg;
                RD.Loc^[i].reg := RD.UNDEF_REG;
            ELSE
                idb.SetInMem (i);
            END;
        END;
    END;
END SetLocations;

(*
  Перед началом итерации очистить разметку регистров; кроме того,
  сбросить в память выгруженные на предыдущем шаге переменные
  (а для вытесненных байтами - установить, что регистры для них
  надо выбирать, начиная со словных)
*)
PROCEDURE(idb : DAG_386_IDB) ClearLocations(last_iteration: BOOLEAN);
VAR i, j: ir.VarNum;
    r:    D.Reg;
BEGIN
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
        IF RD.Trees^[i] <> NIL THEN
            RD.Trees^[i]^.place.r    := RD.UNDEF_REG;
            RD.Trees^[i]^.a.place1.r := RD.UNDEF_REG;
            RD.Trees^[i]^.a.place1.v := ir.UNDEFINED;
            RD.Trees^[i]^.a.place2.r := RD.UNDEF_REG;
            RD.Trees^[i]^.a.place2.v := ir.UNDEFINED;
        END;
        IF RD.Loc^[i].tag <> BurgNT.NTlocal THEN
            r := RD.Loc^[i].reg;
            RD.Loc^[i].reg := RD.UNDEF_REG;
            IF RD.Loc^[i].isSpilled THEN
                IF (ir.o_SpilledByByte IN ir.Vars^[i].Options)
                   & NOT (ir.o_Backwards IN ir.Vars^[i].Options)
                   & (ir.Vars^[i].Def^.ResSize <> 1)
                   & (ir.Vars^[i].Def^.ResType <> ir.t_float)
                THEN
                    INCL (ir.Vars^[i].Options, ir.o_Backwards);
                    RD.Loc^[i].tag := BurgNT.NTreg;
                    RD.Loc[i].isSpilled := FALSE;
                ELSE
                    j := i;
(*  kevin
                    WHILE RD.Loc^[j].temp DO
                        ASSERT (ir.Vars^[j].Use^.triade^.Tag = ir.y_Variable);
                        j := ir.Vars^[j].Use^.triade^.Name;
                    END;
*)
                    idb.SetInMem (j);
                END;
            ELSIF (RD.Loc^[i].tag <> BurgNT.NTtos) &
                  NOT last_iteration & NOT RD.Loc^[i].temp
            THEN
                RD.Loc^[i].tag := BurgNT.NTreg;
            END;
        END;
    END;
END ClearLocations;

<* IF ~ NODEBUG THEN *>
PROCEDURE(idb : DAG_386_IDB) PrintLocations();
VAR
    i: ir.VarNum;
    a: Emit.AddrMode;
BEGIN
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
      ir.PrintVar(i);
      opIO.print("\t- ");
      IF (RD.Loc^[i].tag = BurgNT.NTlocal) THEN
        R.FormMem(a, i);
      ELSIF
         (RD.Loc^[i].tag = BurgNT.NTmem) THEN
        a := RD.Trees[i].a;
      END;
      IF (RD.Loc^[i].tag = BurgNT.NTlocal) OR
         (RD.Loc^[i].tag = BurgNT.NTmem) THEN
        IF at.GENASM IN at.COMP_MODE THEN
          IF a.local#ir.UNDEFINED THEN
            EmitGAS.OutMem(a);
            EmitGAS.Enter();
          ELSE
            opIO.print("in mem\n");
          END;
        ELSE
          opIO.print("in mem\n");
        END;
      ELSE
(*
        IF RD.Trees[i] # NIL THEN
          reg.PrintReg(RD.Trees[i].place.r);
        ELSE
*)
          reg.PrintReg(RD.Loc^[i].reg);
--        END;
      opIO.print("\n");
      END;
    END;
END PrintLocations;
<* END *>

BEGIN
  NEW(IDB);
  DAG.IDB := IDB;
END DAG_I.
