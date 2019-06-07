<*+WOFF*>
-- source grammar = e386.b
MODULE Burg;
IMPORT BurgNT;
--------------------------------------------------------------------------------
--
-- Попытка написать простую грамматику для x86
--
-- Стоимость правила: 10 x количество тактов + количество команд
--
-- В правилах вида reg: reg + mem всегда учтена стоимость пересылки
-- даже если левый reg = правому reg и на самом деле пересылки не будет
-- За счет этого регистровое сложение всегда делается с помощью LEA
-- и поэтому уже семантика разбирается с этим и вместо LEA подставляет
-- ADD, INC или DEC; если же делать иначе и эту стоимость не учитывать,
-- то будет плохо в способе адресации - вместо адресации по двум регистрам,
-- один из которых только что загружен из памяти, будет генерироваться
-- сложение (на одну команду дольше - та самая неучтенная пересылка)
--
-- Хвосты:
-- 1) ПРОСЛЕДИТЬ, ЧТО В АДРЕСЕ - НЕ БОЛЕЕ ОДНОГО esp
--    (проявляется только в SYSTEM.VAL (LONGINT, SYSTEM.ADR (a)) +
--                          SYSTEM.VAL (LONGINT, SYSTEM.ADR (b)),
--     a и b локалы)
--
--------------------------------------------------------------------------------

IMPORT ir,
       Color,
       at := opAttrs,
       RD := RDefs,
       R := r386,
       Emit;
IMPORT P := p386;
IMPORT reg := reg386;
IMPORT H := Heuristics;
IMPORT D := desc386;
IMPORT env := xiEnv;
IMPORT tune := opTune;

TYPE NT = BurgNT.NT;
CONST NTnowhere = BurgNT.NTnowhere;
CONST NTstm = BurgNT.NTstm;
CONST NTreg = BurgNT.NTreg;
CONST NTrc = BurgNT.NTrc;
CONST NTmrc = BurgNT.NTmrc;
CONST NTmem = BurgNT.NTmem;
CONST NTbased = BurgNT.NTbased;
CONST NTscaled = BurgNT.NTscaled;
CONST NTaddr = BurgNT.NTaddr;
CONST NTlocal = BurgNT.NTlocal;
CONST NTtos = BurgNT.NTtos;
CONST NTconst = BurgNT.NTconst;
CONST NTimem = BurgNT.NTimem;


PROCEDURE NTactions* (c: BurgNT.Rule; n: RD.DAGNODE);
BEGIN
  CASE c OF
    |BurgNT.Rule{ 1 }:
	  n^.nt :=  NTreg; 
	(* rc: reg *)

    |BurgNT.Rule{ 2 }:
	  n^.nt :=  NTconst; 
	(* rc: o_par *)

	(* mrc: rc *)

    |BurgNT.Rule{ 4 }:
	  n^.nt :=  NTmem; 
	(* mrc: mem *)

    |BurgNT.Rule{ 5 }:
	  R.RegToBased (n); 
	(* based: reg *)

    |BurgNT.Rule{ 6 }:
	  n.a := RD.Trees^[n.par^.name]^.a; n^.nt :=  NTbased; 
	(* based: o_par *)

    |BurgNT.Rule{ 7 }:
	  R.StackAddrToBased (n^.a, n^.par); n^.nt :=  NTbased; 
	(* based: o_par *)

    |BurgNT.Rule{ 8 }:
	  R.AddAddrParToAddr (n, n.l, n.r); n^.nt :=  NTbased; 
	(* based: o_add(based,o_par) *)

    |BurgNT.Rule{ 9 }:
	  R.SubAddrParToAddr (n, n.l, n.r); n^.nt :=  NTbased; 
	(* based: o_sub(based,o_par) *)

    |BurgNT.Rule{ 10 }:
	  n.a := RD.Trees^[n.par^.name]^.a; n^.nt :=  NTscaled; 
	(* scaled: o_par *)

    |BurgNT.Rule{ 11 }:
	  R.MultToScaled (n, n.l, n.r); 
	(* scaled: o_mul(reg,o_par) *)

    |BurgNT.Rule{ 12 }:
	  R.ShiftToScaled (n, n.l, n.r); 
	(* scaled: o_shift(reg,o_par) *)

    |BurgNT.Rule{ 13 }:
	  R.AddAddrParToAddr (n, n.l, n.r); n^.nt :=  NTscaled; 
	(* scaled: o_add(scaled,o_par) *)

    |BurgNT.Rule{ 14 }:
	  R.SubAddrParToAddr (n, n.l, n.r); n^.nt :=  NTscaled; 
	(* scaled: o_sub(scaled,o_par) *)

    |BurgNT.Rule{ 15 }:
	  n^.nt :=  NTaddr; 
	(* addr: based *)

    |BurgNT.Rule{ 16 }:
	  R.AddAddrAddrToAddr (n, n.l, n.r, n.l^.a.place1, n.r^.a.place1, D.x1); 
	(* addr: o_add(based,based) *)

    |BurgNT.Rule{ 17 }:
	  n^.nt :=  NTaddr; 
	(* addr: scaled *)

    |BurgNT.Rule{ 18 }:
	  R.AddAddrAddrToAddr (n, n.l, n.r, n.l^.a.place1, n.r^.a.place2, n.r^.a.scale ); 
	(* addr: o_add(based,scaled) *)

    |BurgNT.Rule{ 19 }:
	  R.AddAddrAddrToAddr (n, n.r, n.l, n.r^.a.place1, n.l^.a.place2, n.l^.a.scale ); 
	(* addr: o_add(scaled,based) *)

    |BurgNT.Rule{ 20 }:
	  n.a := RD.Trees^[n.par^.name]^.a; n^.nt :=  NTaddr; 
	(* addr: o_par *)

    |BurgNT.Rule{ 21 }:
	  R.ParToAddr (n^.a, n^.par); n^.nt :=  NTaddr; 
	(* addr: o_par *)

    |BurgNT.Rule{ 22 }:
	  R.AddAddrParToAddr (n, n.l, n.r); n^.nt :=  NTaddr; 
	(* addr: o_add(addr,o_par) *)

    |BurgNT.Rule{ 23 }:
	  R.SubAddrParToAddr (n, n.l, n.r); n^.nt :=  NTaddr; 
	(* addr: o_sub(addr,o_par) *)

    |BurgNT.Rule{ 24 }:
	  R.MultToAddr (n, n.l, n.r); 
	(* addr: o_mul(reg,o_par) *)

    |BurgNT.Rule{ 25 }:
	  R.MultToScaled (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_mul(reg,o_par) *)

    |BurgNT.Rule{ 26 }:
	  R.ShiftToScaled (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_shift(reg,o_par) *)

    |BurgNT.Rule{ 27 }:
	  R.MultToAddr (n, n.l, n.r); R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_mul(reg,o_par) *)

    |BurgNT.Rule{ 28 }:
	  R.TryAddPosition (n); R.LEA (n, 4); 
	(* reg: based *)

    |BurgNT.Rule{ 29 }:
	  R.AddAddrParToAddr (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_add(based,o_par) *)

    |BurgNT.Rule{ 30 }:
	  R.SubAddrParToAddr (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize) 
	(* reg: o_sub(based,o_par) *)

    |BurgNT.Rule{ 31 }:
	  R.TryAddPosition (n); R.LEA (n, 4); 
	(* reg: scaled *)

    |BurgNT.Rule{ 32 }:
	  R.AddAddrParToAddr (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_add(scaled,o_par) *)

    |BurgNT.Rule{ 33 }:
	  R.SubAddrParToAddr (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize) 
	(* reg: o_sub(scaled,o_par) *)

    |BurgNT.Rule{ 34 }:
	  R.TryAddPosition (n); R.LEA (n, 4); 
	(* reg: addr *)

    |BurgNT.Rule{ 35 }:
	  R.AddAddrParToAddr (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_add(addr,o_par) *)

    |BurgNT.Rule{ 36 }:
	  R.SubAddrParToAddr (n, n.l, n.r);
          R.LEA (n, n^.tr^.ResSize) 
	(* reg: o_sub(addr,o_par) *)

    |BurgNT.Rule{ 37 }:
	  R.AddAddrAddrToAddr (n, n.l, n.r, n.l^.a.place1, n.r^.a.place1, D.x1);
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_add(based,based) *)

    |BurgNT.Rule{ 38 }:
	  R.AddAddrAddrToAddr (n, n.l, n.r, n.l^.a.place1, n.r^.a.place2, n.r^.a.scale );
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_add(based,scaled) *)

    |BurgNT.Rule{ 39 }:
	  R.AddAddrAddrToAddr (n, n.r, n.l, n.r^.a.place1, n.l^.a.place2, n.l^.a.scale );
          R.LEA (n, n^.tr^.ResSize); 
	(* reg: o_add(scaled,based) *)

    |BurgNT.Rule{ 40 }:
	  R.LoadrToReg (n, n.l, n^.tr^.ResSize); 
	(* reg: o_loadr(addr) *)

    |BurgNT.Rule{ 41 }:
	  n^.place.r := RD.Loc^[n^.par.name].reg;
          n^.place.v := n^.par.name;
          n^.nt :=  NTreg; 
	(* reg: o_par *)

    |BurgNT.Rule{ 42 }:
	  R.TryAddPosition (n); R.ImmToReg (n, n^.sz); 
	(* reg: o_par *)

    |BurgNT.Rule{ 43 }:
	  R.TryAddPosition (n);
          R.MemToReg (n, n^.sz); 
	(* reg: mem *)

    |BurgNT.Rule{ 44 }:
	  n^.a := n.l^.a; n^.a.bv := n^.tr^.Read; n^.nt :=  NTmem; 
	(* mem: o_loadr(addr) *)

    |BurgNT.Rule{ 45 }:
	  n^.a := RD.Trees^[n^.par.name]^.a; n^.nt :=  NTmem; 
	(* mem: o_par *)

    |BurgNT.Rule{ 46 }:
	  n^.nt :=  NTlocal; 
	(* local: o_loadr(addr) *)

    |BurgNT.Rule{ 47 }:
	  R.TryAddPosition (n);
          R.RegToLocal (n, n^.tr^.ResSize); n^.nt :=  NTlocal; 
	(* local: reg *)

    |BurgNT.Rule{ 48 }:
	  R.FormMem (n^.a, n^.par^.name); n^.nt :=  NTmem; 
	(* mem: o_par *)

    |BurgNT.Rule{ 49 }:
	  R.GetParToReg (n); 
	(* reg: o_getpar *)

    |BurgNT.Rule{ 50 }:
	  n^.nt :=  NTlocal; 
	(* local: o_getpar *)

    |BurgNT.Rule{ 51 }:
	  R.BinOp (n, n.l, n.r); 
	(* reg: o_add(reg,mrc) *)

    |BurgNT.Rule{ 52 }:
	  R.BinOp (n, n.r, n.l); 
	(* reg: o_add(mrc,reg) *)

    |BurgNT.Rule{ 53 }:
	  R.UnOp (n, n.l); 
	(* reg: o_add(mrc,o_par) *)

    |BurgNT.Rule{ 54 }:
	  R.BinOpM (n.l, n.r, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_add(mem,rc) *)

    |BurgNT.Rule{ 55 }:
	  R.BinOpM (n.r, n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_add(rc,mem) *)

    |BurgNT.Rule{ 56 }:
	  R.UnOpM (n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_add(mem,o_par) *)

    |BurgNT.Rule{ 57 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.BinOpM (n.l, n.r.r, n.r^.tr); 
	(* stm: o_storer(addr,o_add(mem,rc)) *)

    |BurgNT.Rule{ 58 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.BinOpM (n.l, n.r.l, n.r^.tr); 
	(* stm: o_storer(addr,o_add(rc,mem)) *)

    |BurgNT.Rule{ 59 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.UnOpM (n.l, n.r^.tr); 
	(* stm: o_storer(addr,o_add(mem,o_par)) *)

    |BurgNT.Rule{ 60 }:
	  R.BinOp (n, n.l, n.r); 
	(* reg: o_sub(reg,mrc) *)

    |BurgNT.Rule{ 61 }:
	  R.UnOp (n, n.l); 
	(* reg: o_sub(mrc,o_par) *)

    |BurgNT.Rule{ 62 }:
	  R.BinOpM (n.l, n.r, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_sub(mem,rc) *)

    |BurgNT.Rule{ 63 }:
	  R.UnOpM (n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_sub(mem,o_par) *)

    |BurgNT.Rule{ 64 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.BinOpM (n.l, n.r.r, n.r^.tr); 
	(* stm: o_storer(addr,o_sub(mem,rc)) *)

    |BurgNT.Rule{ 65 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.UnOpM (n.l, n.r^.tr); 
	(* stm: o_storer(addr,o_sub(mem,o_par)) *)

    |BurgNT.Rule{ 66 }:
	  R.UnOp (n, n.l); 
	(* reg: o_neg(mrc) *)

    |BurgNT.Rule{ 67 }:
	  R.UnOpM (n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_neg(mem) *)

    |BurgNT.Rule{ 68 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.UnOpM (n.l, n.r^.tr); 
	(* stm: o_storer(addr,o_neg(mem)) *)

    |BurgNT.Rule{ 69 }:
	  R.UnOp (n, n.l); 
	(* reg: o_logical(mrc,o_par) *)

    |BurgNT.Rule{ 70 }:
	  R.UnOpM (n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_logical(mem,o_par) *)

    |BurgNT.Rule{ 71 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.UnOpM (n.l, n.r^.tr); 
	(* stm: o_storer(addr,o_logical(mem,o_par)) *)

    |BurgNT.Rule{ 72 }:
	  R.BinOp (n, n.l, n.r); 
	(* reg: o_logical(reg,mrc) *)

    |BurgNT.Rule{ 73 }:
	  R.BinOp (n, n.r, n.l); 
	(* reg: o_logical(mrc,reg) *)

    |BurgNT.Rule{ 74 }:
	  R.BinOpM (n.l, n.r, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_logical(mem,rc) *)

    |BurgNT.Rule{ 75 }:
	  R.BinOpM (n.r, n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_logical(rc,mem) *)

    |BurgNT.Rule{ 76 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.BinOpM (n.l, n.r.r, n.r^.tr); 
	(* stm: o_storer(addr,o_logical(mem,rc)) *)

    |BurgNT.Rule{ 77 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.BinOpM (n.l, n.r.l, n.r^.tr); 
	(* stm: o_storer(addr,o_logical(rc,mem)) *)

    |BurgNT.Rule{ 78 }:
	  R.UnOp (n, n.l); 
	(* reg: o_not(mrc) *)

    |BurgNT.Rule{ 79 }:
	  R.UnOp (n, n.l); 
	(* reg: o_cap(mrc) *)

    |BurgNT.Rule{ 80 }:
	  R.UnOpM (n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_not(mem) *)

    |BurgNT.Rule{ 81 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.UnOpM (n.l, n.r^.tr); 
	(* stm: o_storer(addr,o_not(mem)) *)

    |BurgNT.Rule{ 82 }:
	  R.ShiftOp (n, n.l, n.r); 
    |BurgNT.Rule{ -82 }:
	  reg.TryAssignReg (n.r, D.ECX); 
	(* reg: o_shift(reg,mrc) *)

    |BurgNT.Rule{ 83 }:
	  R.ShiftOpM (n, n.l, n.r); n^.nt :=  NTlocal; 
	(* local: o_shift(mem,o_par) *)

    |BurgNT.Rule{ 84 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.ShiftOpM (n.r, n.l, n.r.r); 
	(* stm: o_storer(addr,o_shift(mem,o_par)) *)

    |BurgNT.Rule{ 85 }:
	  R.BinOp (n, n.l, n.r); 
	(* reg: o_mul(reg,mrc) *)

    |BurgNT.Rule{ 86 }:
	  R.BinOp (n, n.r, n.l); 
	(* reg: o_mul(mrc,reg) *)

    |BurgNT.Rule{ 87 }:
	  R.Mul (n, n.l, n.r, D.RegTable[D.EAXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -87 }:
	  reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]);
          reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]); 
	(* reg: o_mul(rc,mrc) *)

    |BurgNT.Rule{ 88 }:
	  R.Mul (n, n.r, n.l, D.RegTable[D.EAXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -88 }:
	  reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]);
          reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]); 
	(* reg: o_mul(mrc,rc) *)

    |BurgNT.Rule{ 89 }:
	  R.Mul (n, n.l, n.r, D.RegTable[D.EAXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -89 }:
	  reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]);
          reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]); 
	(* local: o_mul(rc,mrc) *)

    |BurgNT.Rule{ 90 }:
	  R.Mul (n, n.r, n.l, D.RegTable[D.EAXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -90 }:
	  reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]);
          reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]); 
	(* local: o_mul(mrc,rc) *)

    |BurgNT.Rule{ 91 }:
	  R.Mul (n, n.l, n.r, D.RegTable[D.EDXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -91 }:
	  reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]);
          reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]); 
	(* reg: o_mulh(rc,mrc) *)

    |BurgNT.Rule{ 92 }:
	  R.Mul (n, n.r, n.l, D.RegTable[D.EDXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -92 }:
	  reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]);
          reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]); 
	(* reg: o_mulh(mrc,rc) *)

    |BurgNT.Rule{ 93 }:
	  R.Mul (n, n.l, n.r, D.RegTable[D.EDXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -93 }:
	  reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]);
          reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]); 
	(* local: o_mulh(rc,mrc) *)

    |BurgNT.Rule{ 94 }:
	  R.Mul (n, n.r, n.l, D.RegTable[D.EDXp,n^.tr^.ResSize]); 
    |BurgNT.Rule{ -94 }:
	  reg.TryAssignReg (n.r, D.RegTable[D.EAXp,n.r.sz]);
          reg.TryAssignReg (n.l, D.RegTable[D.EAXp,n.l.sz]); 
	(* local: o_mulh(mrc,rc) *)

    |BurgNT.Rule{ 95 }:
	  R.IMulByConst (n, n.l, n.r); 
	(* reg: o_mul(mem,o_par) *)

    |BurgNT.Rule{ 96 }:
	  R.IMulByConst (n, n.l, n.r); 
	(* reg: o_mul(reg,o_par) *)

    |BurgNT.Rule{ 97 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -97 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* reg: o_div(rc,mrc) *)

    |BurgNT.Rule{ 98 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -98 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* local: o_div(rc,mrc) *)

    |BurgNT.Rule{ 99 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -99 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* reg: o_dvd(rc,mrc) *)

    |BurgNT.Rule{ 100 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -100 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* local: o_dvd(rc,mrc) *)

    |BurgNT.Rule{ 101 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -101 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* reg: o_rem(rc,mrc) *)

    |BurgNT.Rule{ 102 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -102 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* local: o_rem(rc,mrc) *)

    |BurgNT.Rule{ 103 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -103 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* reg: o_mod(rc,mrc) *)

    |BurgNT.Rule{ 104 }:
	  R.Div (n, n.l, n.r); 
    |BurgNT.Rule{ -104 }:
	  reg.TryAssignReg (n.l, D.EAX); 
	(* local: o_mod(rc,mrc) *)

    |BurgNT.Rule{ 105 }:
	  R.UnOp (n, n.l); 
	(* reg: o_sgnext(mrc) *)

    |BurgNT.Rule{ 106 }:
	  R.UnOp (n, n.l); 
	(* reg: o_val(mrc) *)

    |BurgNT.Rule{ 107 }:
	  R.UnOp (n, n.l);
	(* reg: o_hiword(mrc) *)

    |BurgNT.Rule{ 108 }:
	  R.MemCast (n, n.l);
	(* mem: o_hiword(mem) *)

    |BurgNT.Rule{ 109 }:
	  R.MemCast (n, n.l);
	(* mem: o_cast(mem) *)

    |BurgNT.Rule{ 110 }:
	  R.MemCast (n, n.l);
	(* mem: o_val(mem) *)

    |BurgNT.Rule{ 111 }:
	  R.UnOp (n, n.l); 
	(* reg: o_cast(mrc) *)

    |BurgNT.Rule{ 112 }:
	  R.CallProcFunc (n, n.l,  NTreg); 
	(* reg: o_call(mrc) *)

    |BurgNT.Rule{ 113 }:
	  R.CallProcFunc (n, n.l,  NTlocal); 
	(* local: o_call(mrc) *)

    |BurgNT.Rule{ 114 }:
	  R.AdjustSP; R.Alloca (n, n.l); 
	(* reg: o_alloca(mrc) *)

    |BurgNT.Rule{ 115 }:
	  R.AdjustSP; R.Alloca (n, n.l); 
	(* local: o_alloca(mrc) *)

    |BurgNT.Rule{ 116 }:
	  R.InclExcl (n, n.l, n.r); 
	(* reg: o_incl(reg,reg) *)

    |BurgNT.Rule{ 117 }:
	  R.InclExcl (n, n.l, n.r); 
	(* local: o_incl(mem,reg) *)

    |BurgNT.Rule{ 118 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.InclExcl (n, n.r.l, n.r.r); 
	(* stm: o_storer(addr,o_incl(mem,reg)) *)

    |BurgNT.Rule{ 119 }:
	  R.MoveInclExcl (n, n.r); 
	(* reg: o_incl(o_par,reg) *)

    |BurgNT.Rule{ 120 }:
	  R.InclExcl (n, n.l, n.r); 
	(* reg: o_excl(reg,reg) *)

    |BurgNT.Rule{ 121 }:
	  R.InclExcl (n, n.l, n.r); 
	(* local: o_excl(mem,reg) *)

    |BurgNT.Rule{ 122 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.InclExcl (n, n.r.l, n.r.r); 
	(* stm: o_storer(addr,o_excl(mem,reg)) *)

    |BurgNT.Rule{ 123 }:
	  R.MoveInclExcl (n, n.r); 
	(* reg: o_excl(o_par,reg) *)

    |BurgNT.Rule{ 124 }:
	  R.LoHiSet (n, n.l); 
	(* reg: o_loset(reg) *)

    |BurgNT.Rule{ 125 }:
	  R.InclExcl (n, n.l, n.r.l); 
	(* reg: o_logical(reg,o_loset(reg)) *)

    |BurgNT.Rule{ 126 }:
	  R.InclExcl (n, n.r, n.l.l); 
	(* reg: o_logical(o_loset(reg),reg) *)

    |BurgNT.Rule{ 127 }:
	  R.LoHiSet (n, n.l); 
	(* reg: o_hiset(reg) *)

	(* stm: o_storer(addr,mem) *)

    |BurgNT.Rule{ 129 }:
	  n.l^.a.bv := n^.tr^.Write; R.RCToMem (n.l, n.r, n^.tr^.ResSize); 
	(* stm: o_storer(addr,rc) *)

    |BurgNT.Rule{ 130 }:
	  R.CompareR_MRC (n, n.l, n.r, FALSE);
          R.SkipTrap (n, D.JGE, D.JAE); 
	(* stm: o_checklo(reg,mrc) *)

    |BurgNT.Rule{ 131 }:
	  R.CompareR_MRC (n, n.r, n.l, FALSE);
          R.SkipTrap (n, D.JLE, D.JBE); 
	(* stm: o_checklo(mrc,reg) *)

    |BurgNT.Rule{ 132 }:
	  R.CompareM_RC (n, n.l, n.r, FALSE);
          R.SkipTrap (n, D.JGE, D.JAE); 
	(* stm: o_checklo(mem,rc) *)

    |BurgNT.Rule{ 133 }:
	  R.CompareM_RC (n, n.r, n.l, FALSE);
          R.SkipTrap (n, D.JLE, D.JBE); 
	(* stm: o_checklo(rc,mem) *)

    |BurgNT.Rule{ 134 }:
	  R.CompareR_MRC (n, n.l, n.r, FALSE);
          R.SkipTrap (n, D.JL, D.JB); 
	(* stm: o_checkhi(reg,mrc) *)

    |BurgNT.Rule{ 135 }:
	  R.CompareR_MRC (n, n.r, n.l, FALSE);
          R.SkipTrap (n, D.JG, D.JA); 
	(* stm: o_checkhi(mrc,reg) *)

    |BurgNT.Rule{ 136 }:
	  R.CompareM_RC (n, n.l, n.r, FALSE);
          R.SkipTrap (n, D.JL, D.JB); 
	(* stm: o_checkhi(mem,rc) *)

    |BurgNT.Rule{ 137 }:
	  R.CompareM_RC (n, n.r, n.l, FALSE);
          R.SkipTrap (n, D.JG, D.JA); 
	(* stm: o_checkhi(rc,mem) *)

    |BurgNT.Rule{ 138 }:
	  R.CompareR_NIL (n, n.l);
          Emit.work.GenSkipTrap (D.JNE, R.TrapNo (n^.tr), n^.tr^.Position); 
	(* stm: o_checknil(reg) *)

    |BurgNT.Rule{ 139 }:
	  R.FCompareTOS_NIL;
          Emit.work.GenSkipTrap (D.JE, Emit.DivideTrap, n^.tr^.Position); 
	(* stm: o_checknil(tos) *)

    |BurgNT.Rule{ 140 }:
	  R.PutPar (n, n.l); 
	(* stm: o_putpar(mrc) *)

    |BurgNT.Rule{ 141 }:
	  R.DoRaise (n, n.l.l, n.l.r, n.r); 
	(* stm: o_error(o_comma(rc,rc),rc) *)

    |BurgNT.Rule{ 142 }:
	  R.DoHalt (n.l); 
	(* stm: o_stop(rc) *)

    |BurgNT.Rule{ 143 }:
	  R.CallProcFunc (n, n.l,  NTstm); 
	(* stm: o_call(mrc) *)

    |BurgNT.Rule{ 144 }:
	  R.GenCopy (n, n.l.l, n.l.r, n.r); 
    |BurgNT.Rule{ -144 }:
	  reg.TryAssignReg (n.l.l, D.ESI);
          reg.TryAssignReg (n.l.r, D.EDI);
          reg.TryAssignReg (n.r, D.ECX); 
	(* stm: o_copy(o_comma(rc,rc),rc) *)

    |BurgNT.Rule{ 145 }:
	  n.l.l^.nt :=  NTconst; R.GenCopy (n, n.l.l, n.l.r, n.r); 
    |BurgNT.Rule{ -145 }:
	  reg.TryAssignReg (n.l.r, D.EDI); reg.TryAssignReg (n.r, D.ECX); 
	(* stm: o_copy(o_comma(o_par,rc),rc) *)

    |BurgNT.Rule{ 146 }:
	  n.l.r^.nt :=  NTconst; R.GenCopy (n, n.l.l, n.l.r, n.r); 
    |BurgNT.Rule{ -146 }:
	  reg.TryAssignReg (n.l.l, D.ESI); reg.TryAssignReg (n.r, D.ECX); 
	(* stm: o_copy(o_comma(rc,o_par),rc) *)

    |BurgNT.Rule{ 147 }:
	  n.l.l^.nt :=  NTconst; n.l.r^.nt :=  NTconst;
          R.GenCopy (n, n.l.l, n.l.r, n.r); 
    |BurgNT.Rule{ -147 }:
	  reg.TryAssignReg (n.r, D.ECX); 
	(* stm: o_copy(o_comma(o_par,o_par),rc) *)

    |BurgNT.Rule{ 148 }:
	  R.GenCopy4 (n, n.l.l, n.l.r, n.r.l); 
    |BurgNT.Rule{ -148 }:
	  reg.TryAssignReg (n.l.l, D.ESI);
          reg.TryAssignReg (n.l.r, D.EDI);
          reg.TryAssignReg (n.r.l, D.ECX); 
	(* stm: o_copy(o_comma(rc,rc),o_mul(rc,o_par)) *)

    |BurgNT.Rule{ 149 }:
	  R.GenCopy4 (n, n.l.l, n.l.r, n.r.r); 
    |BurgNT.Rule{ -149 }:
	  reg.TryAssignReg (n.l.l, D.ESI);
          reg.TryAssignReg (n.l.r, D.EDI);
          reg.TryAssignReg (n.r.r, D.ECX); 
	(* stm: o_copy(o_comma(rc,rc),o_mul(o_par,rc)) *)

    |BurgNT.Rule{ 150 }:
	  R.AdjustSP; RD.NodeInfo^[n^.tr^.NodeNo].j := D.UnJ; 
	(* stm: o_goto *)

    |BurgNT.Rule{ 151 }:
	  R.AdjustSP; R.CompareR_MRC (n, n.l, n.r, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JLE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JBE;
          END; 
	(* stm: o_le(reg,mrc) *)

    |BurgNT.Rule{ 152 }:
	  R.AdjustSP; R.CompareR_MRC (n, n.r, n.l, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JGE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JAE;
          END; 
	(* stm: o_le(mrc,reg) *)

    |BurgNT.Rule{ 153 }:
	  R.AdjustSP; R.CompareM_RC (n, n.l, n.r, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JLE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JBE;
          END; 
	(* stm: o_le(mem,rc) *)

    |BurgNT.Rule{ 154 }:
	  R.AdjustSP; R.CompareM_RC (n, n.r, n.l, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JGE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JAE;
          END; 
	(* stm: o_le(rc,mem) *)

    |BurgNT.Rule{ 155 }:
	  R.AdjustSP; R.CompareR_MRC (n, n.l, n.r, TRUE);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE; 
	(* stm: o_eq(reg,mrc) *)

    |BurgNT.Rule{ 156 }:
	  R.AdjustSP; R.CompareR_MRC (n, n.r, n.l, TRUE);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE; 
	(* stm: o_eq(mrc,reg) *)

    |BurgNT.Rule{ 157 }:
	  R.AdjustSP; R.CompareM_RC (n, n.l, n.r, TRUE);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE; 
	(* stm: o_eq(mem,rc) *)

    |BurgNT.Rule{ 158 }:
	  R.AdjustSP; R.CompareM_RC (n, n.r, n.l, TRUE);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE; 
	(* stm: o_eq(rc,mem) *)

    |BurgNT.Rule{ 159 }:
	  R.AdjustSP; R.Test_MR (n, n.l.l, n.l.r);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE; 
	(* stm: o_eq(o_logical(reg,o_par),o_par) *)

    |BurgNT.Rule{ 160 }:
	  R.AdjustSP; R.Test_MR (n, n.l.l, n.l.r);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE; 
	(* stm: o_eq(o_logical(mem,o_par),o_par) *)

    |BurgNT.Rule{ 161 }:
	  R.AdjustSP; R.InSet (n, n.l, n.r); 
	(* stm: o_in(reg,reg) *)

    |BurgNT.Rule{ 162 }:
	  R.AdjustSP; R.InSetConst (n, n.l, n.r);
        
	(* stm: o_in(reg,o_par) *)

    |BurgNT.Rule{ 163 }:
	  R.AdjustSP; R.InSet (n, n.l, n.r);
	(* stm: o_in(reg,mem) *)

    |BurgNT.Rule{ 164 }:
	  R.AdjustSP; R.InSet (n, n.l, n.r);
	(* stm: o_in(reg,addr) *)

    |BurgNT.Rule{ 165 }:
	  R.AdjustSP; R.DoCase (n, n.l); 
	(* stm: o_case(reg) *)

    |BurgNT.Rule{ 166 }:
	  R.RetProc (n^.tr); 
	(* stm: o_ret *)

    |BurgNT.Rule{ 167 }:
	  R.RetFunc (n.l, n^.tr); 
    |BurgNT.Rule{ -167 }:
	  IF (n^.tr^.ResType <> ir.t_float) THEN
              reg.TryAssignReg (n.l, D.EAX);
          END; 
	(* stm: o_retfun(mrc) *)

	(* stm: local *)

    |BurgNT.Rule{ 169 }:
	  n^.nt :=  NTtos; 
	(* tos: o_par *)

    |BurgNT.Rule{ 170 }:
	  n.l^.a.bv := n^.tr^.Read; R.MemToTOS (n, n.l, n^.tr^.ResSize); 
	(* tos: o_loadr(addr) *)

    |BurgNT.Rule{ 171 }:
	  R.TryAddPosition (n); R.MemToTOS (n, n, n^.sz); 
	(* tos: mem *)

    |BurgNT.Rule{ 172 }:
	  R.TryAddPosition (n); R.MemToTOS (n, n, n^.sz); 
	(* tos: mem *)

    |BurgNT.Rule{ 173 }:
	  R.TryAddPosition (n); R.ImmToTOS (n, P.SmallestSize (n)); 
	(* tos: o_par *)

    |BurgNT.Rule{ 174 }:
	  R.TryAddPosition (n); R.ImmToTOS (n, n^.sz); 
	(* tos: o_par *)

    |BurgNT.Rule{ 175 }:
	  R.TOS_FBinary (n, n.l, n.r,  NTtos,
                         P.TosVar (n.r), n^.tr^.ResSize); 
	(* tos: o_fbin(tos,tos) *)

    |BurgNT.Rule{ 176 }:
	  R.TOS_FBinary (n, n.l, n.r,  NTmem, FALSE, n^.tr^.ResSize); 
	(* tos: o_fbin(tos,mem) *)

    |BurgNT.Rule{ 177 }:
	  R.TOS_FBinary (n, n.r, n.l,  NTmem, TRUE, n^.tr^.ResSize); 
	(* tos: o_fbin(mem,tos) *)

    |BurgNT.Rule{ 178 }:
	  R.TOS_FBinary (n, n.l, n.r.l,  NTmem, FALSE, n.r^.tr^.OpSize); 
	(* tos: o_fbin(tos,o_val(mem)) *)

    |BurgNT.Rule{ 179 }:
	  R.TOS_FBinary (n, n.r, n.l.l,  NTmem, TRUE, n.l^.tr^.OpSize); 
	(* tos: o_fbin(o_val(mem),tos) *)

    |BurgNT.Rule{ 180 }:
	  R.TOS_FBinary (n, n.l, n.r.l,  NTimem, FALSE, n.r^.tr^.OpSize); 
	(* tos: o_fbin(tos,o_val(mem)) *)

    |BurgNT.Rule{ 181 }:
	  R.TOS_FBinary (n, n.r, n.l.l,  NTimem, TRUE, n.l^.tr^.OpSize); 
	(* tos: o_fbin(o_val(mem),tos) *)

    |BurgNT.Rule{ 182 }:
	  R.TOS_FBinary (n, n.l, n.r,  NTconst, FALSE, P.SmallestSize (n.r)); 
	(* tos: o_fbin(tos,o_par) *)

    |BurgNT.Rule{ 183 }:
	  R.TOS_FBinary (n, n.r, n.l,  NTconst, TRUE, P.SmallestSize (n.l)); 
	(* tos: o_fbin(o_par,tos) *)

    |BurgNT.Rule{ 184 }:
	  R.FUnary (n); 
	(* tos: o_funary(tos) *)

    |BurgNT.Rule{ 185 }:
	  R.UnAbsNeg (n.l, n^.tr); n^.nt :=  NTlocal; 
	(* local: o_funary(mem) *)

    |BurgNT.Rule{ 186 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          reg.MarkUseAddrMode (n.l.a); R.UnAbsNeg (n.r.l, n.r^.tr); 
	(* stm: o_storer(addr,o_funary(mem)) *)

    |BurgNT.Rule{ 187 }:
	  R.TryAddPosition (n);
          R.TOSToLocal (n, n^.tr^.ResSize); n^.nt :=  NTlocal; 
	(* local: tos *)

    |BurgNT.Rule{ 188 }:
	  R.GetParToTOS (n); 
	(* tos: o_getpar *)

    |BurgNT.Rule{ 189 }:
	  n^.nt :=  NTtos; 
	(* tos: o_val(tos) *)

    |BurgNT.Rule{ 190 }:
	  n^.nt :=  NTtos; 
	(* tos: o_cast(tos) *)

    |BurgNT.Rule{ 191 }:
	  R.ToReal (n, n.l, n^.tr^.OpType, n^.tr^.OpSize); 
	(* tos: o_val(reg) *)

    |BurgNT.Rule{ 192 }:
	  R.ToReal (n, n.l, n^.tr^.OpType, n^.tr^.OpSize); 
	(* tos: o_val(mem) *)

    |BurgNT.Rule{ 193 }:
	  R.ToOrdinal (n, n.l, n^.tr^.ResType, n^.tr^.ResSize); 
	(* reg: o_val(tos) *)

    |BurgNT.Rule{ 194 }:
	  R.ToOrdinal (n, n.l, n^.tr^.ResType, n^.tr^.ResSize); 
	(* local: o_val(tos) *)

    |BurgNT.Rule{ 195 }:
	  R.ToOrdinal (n, n.l, n^.tr^.ResType, n^.tr^.ResSize); 
	(* reg: o_val(mem) *)

    |BurgNT.Rule{ 196 }:
	  R.ToOrdinal (n, n.l, n^.tr^.ResType, n^.tr^.ResSize); 
	(* local: o_val(mem) *)

    |BurgNT.Rule{ 197 }:
	  R.TryAddPosition (n); R.SYSTEMVAL_ToTos (n, n, n^.sz); 
	(* tos: reg *)

    |BurgNT.Rule{ 198 }:
	  R.SYSTEMVAL_ToTos (n, n.l, n^.sz); 
	(* tos: o_cast(reg) *)

    |BurgNT.Rule{ 199 }:
	  R.TryAddPosition (n); R.SYSTEMVALTosToReg (n, n^.sz); 
	(* reg: tos *)

    |BurgNT.Rule{ 200 }:
	  R.SYSTEMVALTosToReg (n, n.l^.sz); 
	(* reg: o_cast(tos) *)

    |BurgNT.Rule{ 201 }:
	  R.CallProcFunc (n, n.l,  NTtos); 
	(* tos: o_call(mrc) *)

    |BurgNT.Rule{ 202 }:
	  n.l^.a.bv := n^.tr^.Write; R.TOSToMem (n.l, n^.tr^.ResSize); 
	(* stm: o_storer(addr,tos) *)

    |BurgNT.Rule{ 203 }:
	  n.l^.a.bv := n^.tr^.Write; R.FConstToMem (n.l, n.r, n^.tr^.ResSize); 
	(* stm: o_storer(addr,o_par) *)

    |BurgNT.Rule{ 204 }:
	  R.PutPar (n, n.l); 
	(* stm: o_putpar(mem) *)

    |BurgNT.Rule{ 205 }:
	  R.PutParRealConst (n, n.l); 
	(* stm: o_putpar(o_par) *)

    |BurgNT.Rule{ 206 }:
	  R.PutPar (n, n.l); 
	(* stm: o_putpar(tos) *)

    |BurgNT.Rule{ 207 }:
	  n.l^.nt :=  NTmem; R.PutAggregate (n, n.l); 
	(* stm: o_putpar(addr) *)

    |BurgNT.Rule{ 208 }:
	  IF (P.TosVar (n.r)) THEN
            R.FCompare (n.r,  NTtos, n^.tr^.ResSize, 41H);
            IF (R.UseSAHF ()) THEN
                RD.NodeInfo^[n^.tr^.NodeNo].j := D.JBE;
            ELSE
                RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
            END;
          ELSE
            R.FCompare (n.r,  NTtos, n^.tr^.ResSize, 01H);
            IF (R.UseSAHF ()) THEN
                RD.NodeInfo^[n^.tr^.NodeNo].j := D.JAE;
            ELSE
                RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
            END;
          END; 
	(* stm: o_fle(tos,tos) *)

    |BurgNT.Rule{ 209 }:
	  R.FCompare (n.r,  NTmem, n^.tr^.ResSize, 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_fle(tos,mem) *)

    |BurgNT.Rule{ 210 }:
	  R.FCompare (n.l,  NTmem, n^.tr^.ResSize, 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          END; 
	(* stm: o_fle(mem,tos) *)

    |BurgNT.Rule{ 211 }:
	  R.FCompare (n.r.l,  NTmem, n.r^.tr^.OpSize, 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_fle(tos,o_val(mem)) *)

    |BurgNT.Rule{ 212 }:
	  R.FCompare (n.l.l,  NTmem, n.l^.tr^.OpSize, 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          END; 
	(* stm: o_fle(o_val(mem),tos) *)

    |BurgNT.Rule{ 213 }:
	  R.FCompare (n.r.l,  NTimem, n.r^.tr^.OpSize, 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_fle(tos,o_val(mem)) *)

    |BurgNT.Rule{ 214 }:
	  R.FCompare (n.l.l,  NTimem, n.l^.tr^.OpSize, 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          END; 
	(* stm: o_fle(o_val(mem),tos) *)

    |BurgNT.Rule{ 215 }:
	  R.FCompare (n.r,  NTconst, P.SmallestSize (n.r), 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_fle(tos,o_par) *)

    |BurgNT.Rule{ 216 }:
	  R.FCompare (n.l,  NTconst, P.SmallestSize (n.l), 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          END; 
	(* stm: o_fle(o_par,tos) *)

    |BurgNT.Rule{ 217 }:
	  R.FCompare (n.r,  NTtos, n^.tr^.ResSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(tos,tos) *)

    |BurgNT.Rule{ 218 }:
	  R.FCompare (n.r,  NTmem, n^.tr^.ResSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(tos,mem) *)

    |BurgNT.Rule{ 219 }:
	  R.FCompare (n.l,  NTmem, n^.tr^.ResSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(mem,tos) *)

    |BurgNT.Rule{ 220 }:
	  R.FCompare (n.r.l,  NTmem, n.r^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(tos,o_val(mem)) *)

    |BurgNT.Rule{ 221 }:
	  R.FCompare (n.l.l,  NTmem, n.l^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(o_val(mem),tos) *)

    |BurgNT.Rule{ 222 }:
	  R.FCompare (n.r.l,  NTimem, n.r^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(tos,o_val(mem)) *)

    |BurgNT.Rule{ 223 }:
	  R.FCompare (n.l.l,  NTimem, n.l^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(o_val(mem),tos) *)

    |BurgNT.Rule{ 224 }:
	  R.FCompare (n.r,  NTconst, P.SmallestSize (n.r), 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[n^.tr^.NodeNo].j := D.JNE;
          END; 
	(* stm: o_feq(tos,o_par) *)

    |BurgNT.Rule{ 225 }:
	  R.FEqZero (n.l, n^.tr^.ResSize);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JE; 
	(* stm: o_feq(mem,o_par) *)

    |BurgNT.Rule{ 226 }:
	  R.FMemLeZero (n.l, n^.tr^.ResSize);
          RD.NodeInfo^[n^.tr^.NodeNo].j := D.JLE; 
	(* stm: o_fle(mem,o_par) *)

    |BurgNT.Rule{ 227 }:
	  R.RetFunc (n.l, n^.tr); 
	(* stm: o_retfun(tos) *)

    |BurgNT.Rule{ 228 }:
	  R.CallProcFunc (n.l, n.l.l,  NTstm); R.RetProc (n^.tr); 
	(* stm: o_retfun(o_call(mrc)) *)

    |BurgNT.Rule{ 229 }:
	  R.CompareR_MRC (n, n.l, n.r, TRUE); R.CondSetR (n, D.JE); 
	(* reg: o_move_eq(reg,mrc) *)

    |BurgNT.Rule{ 230 }:
	  R.CompareR_MRC (n, n.r, n.l, TRUE); R.CondSetR (n, D.JE); 
	(* reg: o_move_eq(mrc,reg) *)

    |BurgNT.Rule{ 231 }:
	  R.CompareM_RC (n, n.l, n.r, TRUE); R.CondSetR (n, D.JE); 
	(* reg: o_move_eq(mem,rc) *)

    |BurgNT.Rule{ 232 }:
	  R.CompareM_RC (n, n.r, n.l, TRUE); R.CondSetR (n, D.JE); 
	(* reg: o_move_eq(rc,mem) *)

    |BurgNT.Rule{ 233 }:
	  R.Test_MR (n, n.l.l, n.l.r); R.CondSetR (n, D.JE); 
	(* reg: o_move_eq(o_logical(reg,o_par),o_par) *)

    |BurgNT.Rule{ 234 }:
	  R.Test_MR (n, n.l.l, n.l.r); R.CondSetR (n, D.JE); 
	(* reg: o_move_eq(o_logical(mem,o_par),o_par) *)

    |BurgNT.Rule{ 235 }:
	  R.BitTest (n.l.r, n.l.l.r); R.CondSetR (n, D.JNC); 
	(* reg: o_move_eq(o_logical(o_incl(o_par,reg),reg),o_par) *)

    |BurgNT.Rule{ 236 }:
	  R.CompareR_MRC (n, n.l, n.r, TRUE);
          R.FormMem (n^.a, n^.tr^.Name);
          R.CondSetM (n, n^.a, D.JE);
          n^.nt :=  NTlocal; 
	(* local: o_move_eq(reg,mrc) *)

    |BurgNT.Rule{ 237 }:
	  R.CompareR_MRC (n, n.r, n.l, TRUE);
          R.FormMem (n^.a, n^.tr^.Name);
          R.CondSetM (n, n^.a, D.JE);
          n^.nt :=  NTlocal; 
	(* local: o_move_eq(mrc,reg) *)

    |BurgNT.Rule{ 238 }:
	  R.CompareM_RC (n, n.l, n.r, TRUE);
          R.FormMem (n^.a, n^.tr^.Name);
          R.CondSetM (n, n^.a, D.JE);
          n^.nt :=  NTlocal; 
	(* local: o_move_eq(mem,rc) *)

    |BurgNT.Rule{ 239 }:
	  R.CompareM_RC (n, n.r, n.l, TRUE);
          R.FormMem (n^.a, n^.tr^.Name);
          R.CondSetM (n, n^.a, D.JE);
          n^.nt :=  NTlocal; 
	(* local: o_move_eq(rc,mem) *)

    |BurgNT.Rule{ 240 }:
	  R.Test_MR (n, n.l.l, n.l.r);
          R.FormMem (n^.a, n^.tr^.Name);
          R.CondSetM (n, n^.a, D.JE);
          n^.nt :=  NTlocal; 
	(* local: o_move_eq(o_logical(reg,o_par),o_par) *)

    |BurgNT.Rule{ 241 }:
	  R.Test_MR (n, n.l.l, n.l.r);
          R.FormMem (n^.a, n^.tr^.Name);
          R.CondSetM (n, n^.a, D.JE);
          n^.nt :=  NTlocal; 
	(* local: o_move_eq(o_logical(mem,o_par),o_par) *)

    |BurgNT.Rule{ 242 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareR_MRC (n.r, n.r.l, n.r.r, TRUE);
          R.CondSetM (n.r, n.l^.a, D.JE); 
	(* stm: o_storer(addr,o_move_eq(reg,mrc)) *)

    |BurgNT.Rule{ 243 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareR_MRC (n.r, n.r.r, n.r.l, TRUE);
          R.CondSetM (n.r, n.l^.a, D.JE); 
	(* stm: o_storer(addr,o_move_eq(mrc,reg)) *)

    |BurgNT.Rule{ 244 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareM_RC (n.r, n.r.l, n.r.r, TRUE);
          R.CondSetM (n.r, n.l^.a, D.JE); 
	(* stm: o_storer(addr,o_move_eq(mem,rc)) *)

    |BurgNT.Rule{ 245 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareM_RC (n.r, n.r.r, n.r.l, TRUE);
          R.CondSetM (n.r, n.l^.a, D.JE); 
	(* stm: o_storer(addr,o_move_eq(rc,mem)) *)

    |BurgNT.Rule{ 246 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.Test_MR (n.r, n.r.l.l, n.r.l.r);
          R.CondSetM (n.r, n.l^.a, D.JE); 
	(* stm: o_storer(addr,o_move_eq(o_logical(reg,o_par),o_par)) *)

    |BurgNT.Rule{ 247 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.Test_MR (n.r, n.r.l.l, n.r.l.r);
          R.CondSetM (n.r, n.l^.a, D.JE); 
	(* stm: o_storer(addr,o_move_eq(o_logical(mem,o_par),o_par)) *)

    |BurgNT.Rule{ 248 }:
	  R.CompareR_MRC (n, n.l, n.r, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (n, D.JLE);
          ELSE
            R.CondSetR (n, D.JBE);
          END; 
	(* reg: o_move_le(reg,mrc) *)

    |BurgNT.Rule{ 249 }:
	  R.CompareR_MRC (n, n.r, n.l, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (n, D.JGE);
          ELSE
            R.CondSetR (n, D.JAE);
          END; 
	(* reg: o_move_le(mrc,reg) *)

    |BurgNT.Rule{ 250 }:
	  R.CompareM_RC (n, n.l, n.r, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (n, D.JLE);
          ELSE
            R.CondSetR (n, D.JBE);
          END; 
	(* reg: o_move_le(mem,rc) *)

    |BurgNT.Rule{ 251 }:
	  R.CompareM_RC (n, n.r, n.l, FALSE);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (n, D.JGE);
          ELSE
            R.CondSetR (n, D.JAE);
          END; 
	(* reg: o_move_le(rc,mem) *)

    |BurgNT.Rule{ 252 }:
	  R.CompareR_MRC (n, n.l, n.r, TRUE);
          R.FormMem (n^.a, n^.tr^.Name);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n, n^.a, D.JLE);
          ELSE
            R.CondSetM (n, n^.a, D.JBE);
          END;
          n^.nt :=  NTlocal; 
	(* local: o_move_le(reg,mrc) *)

    |BurgNT.Rule{ 253 }:
	  R.CompareR_MRC (n, n.r, n.l, TRUE);
          R.FormMem (n^.a, n^.tr^.Name);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n, n^.a, D.JGE);
          ELSE
            R.CondSetM (n, n^.a, D.JAE);
          END;
          n^.nt :=  NTlocal; 
	(* local: o_move_le(mrc,reg) *)

    |BurgNT.Rule{ 254 }:
	  R.CompareM_RC (n, n.l, n.r, FALSE);
          R.FormMem (n^.a, n^.tr^.Name);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n, n^.a, D.JLE);
          ELSE
            R.CondSetM (n, n^.a, D.JBE);
          END;
          n^.nt :=  NTlocal; 
	(* local: o_move_le(mem,rc) *)

    |BurgNT.Rule{ 255 }:
	  R.CompareM_RC (n, n.r, n.l, FALSE);
          R.FormMem (n^.a, n^.tr^.Name);
          IF (n^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n, n^.a, D.JGE);
          ELSE
            R.CondSetM (n, n^.a, D.JAE);
          END;
          n^.nt :=  NTlocal; 
	(* local: o_move_le(rc,mem) *)

    |BurgNT.Rule{ 256 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareR_MRC (n.r, n.r.l, n.r.r, FALSE);
          IF (n.r^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n.r, n.l^.a, D.JLE);
          ELSE
            R.CondSetM (n.r, n.l^.a, D.JBE);
          END; 
	(* stm: o_storer(addr,o_move_le(reg,mrc)) *)

    |BurgNT.Rule{ 257 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareR_MRC (n.r, n.r.r, n.r.l, FALSE);
          IF (n.r^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n.r, n.l^.a, D.JGE);
          ELSE
            R.CondSetM (n.r, n.l^.a, D.JAE);
          END; 
	(* stm: o_storer(addr,o_move_le(mrc,reg)) *)

    |BurgNT.Rule{ 258 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareM_RC (n.r, n.r.l, n.r.r, FALSE);
          IF (n.r^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n.r, n.l^.a, D.JLE);
          ELSE
            R.CondSetM (n.r, n.l^.a, D.JBE);
          END; 
	(* stm: o_storer(addr,o_move_le(mem,rc)) *)

    |BurgNT.Rule{ 259 }:
	  Emit.TryAddPosition (n.r^.tr); n.l^.a.bv := n^.tr^.Write;
          R.CompareM_RC (n.r, n.r.r, n.r.l, FALSE);
          IF (n.r^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (n.r, n.l^.a, D.JGE);
          ELSE
            R.CondSetM (n.r, n.l^.a, D.JAE);
          END; 
	(* stm: o_storer(addr,o_move_le(rc,mem)) *)

    |BurgNT.Rule{ 260 }:
	  n.l^.a.bv := n^.tr^.Write; R.Mem8ToMem (n.l, n.r); 
	(* stm: o_storer(addr,mem) *)

    |BurgNT.Rule{ 261 }:
	  R.UnOp (n, n.l); n^.nt :=  NTreg; 
	(* reg: o_assign(mrc) *)

    |BurgNT.Rule{ 262 }:
	  R.FormMem (n^.a,  n^.tr^.Name);
          R.RCToMem (n, n.l, n^.tr^.ResSize);
          n^.nt :=  NTlocal; 
	(* local: o_assign(rc) *)

    |BurgNT.Rule{ 263 }:
	  n^.nt :=  NTtos; 
	(* tos: o_assign(tos) *)

  ELSE
  END;
END NTactions;

PROCEDURE ^ NTclosure_reg (n: RD.DAGNODE; ic0: LONGINT);
PROCEDURE ^ NTclosure_rc (n: RD.DAGNODE; ic0: LONGINT);
PROCEDURE ^ NTclosure_mem (n: RD.DAGNODE; ic0: LONGINT);
PROCEDURE ^ NTclosure_based (n: RD.DAGNODE; ic0: LONGINT);
PROCEDURE ^ NTclosure_scaled (n: RD.DAGNODE; ic0: LONGINT);
PROCEDURE ^ NTclosure_addr (n: RD.DAGNODE; ic0: LONGINT);
PROCEDURE ^ NTclosure_local (n: RD.DAGNODE; ic0: LONGINT);
PROCEDURE ^ NTclosure_tos (n: RD.DAGNODE; ic0: LONGINT);

PROCEDURE NTclosure_reg* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
	c0 := ic0 + 201;
	IF (c0 < n.cost[NTtos]) THEN
		n.cost[NTtos] := VAL (INTEGER, c0);
		n.rule[NTtos] := BurgNT.Rule { 197 };
		NTclosure_tos(n, c0);
	END;
IF P.InLocal (n)  THEN
	c0 := ic0 + 11;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 47 };
		NTclosure_local(n, c0);
	END;
	END;
IF P.NotWhole8Node(n) (*& P.NotDeprecCompoundVar(n)*) THEN
	c0 := ic0;
	IF (c0 < n.cost[NTbased]) THEN
		n.cost[NTbased] := VAL (INTEGER, c0);
		n.rule[NTbased] := BurgNT.Rule { 5 };
		NTclosure_based(n, c0);
	END;
	END;
	c0 := ic0;
	IF (c0 < n.cost[NTrc]) THEN
		n.cost[NTrc] := VAL (INTEGER, c0);
		n.rule[NTrc] := BurgNT.Rule { 1 };
		NTclosure_rc(n, c0);
	END;
END NTclosure_reg;

PROCEDURE NTclosure_rc* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
	c0 := ic0;
	IF (c0 < n.cost[NTmrc]) THEN
		n.cost[NTmrc] := VAL (INTEGER, c0);
		n.rule[NTmrc] := BurgNT.Rule { 3 };
	END;
END NTclosure_rc;

PROCEDURE NTclosure_mem* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
IF P.ResultOfPriorTriade (n) & P.IsFloatNode(n)&(n^.sz >= 4)  THEN
	c0 := ic0 + 30;
	IF (c0 < n.cost[NTtos]) THEN
		n.cost[NTtos] := VAL (INTEGER, c0);
		n.rule[NTtos] := BurgNT.Rule { 172 };
		NTclosure_tos(n, c0);
	END;
	END;
IF P.IsFloatNode(n)&(n^.sz >= 4)  THEN
	c0 := ic0 + 31;
	IF (c0 < n.cost[NTtos]) THEN
		n.cost[NTtos] := VAL (INTEGER, c0);
		n.rule[NTtos] := BurgNT.Rule { 171 };
		NTclosure_tos(n, c0);
	END;
	END;
IF (n^.sz <= 4)OR(P.Whole8Node(n))  THEN
	c0 := ic0 + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 43 };
		NTclosure_reg(n, c0);
	END;
	END;
	c0 := ic0 + 10;
	IF (c0 < n.cost[NTmrc]) THEN
		n.cost[NTmrc] := VAL (INTEGER, c0);
		n.rule[NTmrc] := BurgNT.Rule { 4 };
	END;
END NTclosure_mem;

PROCEDURE NTclosure_based* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
IF P.NotWhole8Node(n)  THEN
	c0 := ic0 + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 28 };
		NTclosure_reg(n, c0);
	END;
	END;
	c0 := ic0;
	IF (c0 < n.cost[NTaddr]) THEN
		n.cost[NTaddr] := VAL (INTEGER, c0);
		n.rule[NTaddr] := BurgNT.Rule { 15 };
		NTclosure_addr(n, c0);
	END;
END NTclosure_based;

PROCEDURE NTclosure_scaled* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
IF P.NotWhole8Node(n)  THEN
	c0 := ic0 + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 31 };
		NTclosure_reg(n, c0);
	END;
	END;
	c0 := ic0;
	IF (c0 < n.cost[NTaddr]) THEN
		n.cost[NTaddr] := VAL (INTEGER, c0);
		n.rule[NTaddr] := BurgNT.Rule { 17 };
		NTclosure_addr(n, c0);
	END;
END NTclosure_scaled;

PROCEDURE NTclosure_addr* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
IF P.NotWhole8Node(n)  THEN
	c0 := ic0 + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 34 };
		NTclosure_reg(n, c0);
	END;
	END;
END NTclosure_addr;

PROCEDURE NTclosure_local* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
	c0 := ic0;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 168 };
	END;
END NTclosure_local;

PROCEDURE NTclosure_tos* (n: RD.DAGNODE; ic0: LONGINT);
VAR c0: LONGINT;
BEGIN
	c0 := ic0 + 201;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 199 };
		NTclosure_reg(n, c0);
	END;
IF P.RootNode (n)  THEN
	c0 := ic0 + 71;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 187 };
		NTclosure_local(n, c0);
	END;
	END;
END NTclosure_tos;


TYPE newstate_proc *= PROCEDURE (n: RD.DAGNODE);

CONST MaxCost = BurgNT.CostArray {
	0,
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER),
	MAX (INTEGER)
};

PROCEDURE newstate_o_assign* (n: RD.DAGNODE); (* 2 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* tos: o_assign(tos)  263 *)
	IF (n^.tr^.ResType = ir.t_float) &
           (n^.tr^.OpType = ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + 0;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 263 };
			NTclosure_tos(n, c0);
		END;
	END;

(* local: o_assign(rc)  262 *)
	IF n^.tr^.ResSize <= 4  THEN
		c0 := VAL (LONGINT, l.cost [NTrc]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 262 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_assign(mrc)  261 *)
	IF (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType <> ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + 11;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 261 };
			NTclosure_reg(n, c0);
		END;
	END;
END newstate_o_assign;

PROCEDURE newstate_o_copy* (n: RD.DAGNODE); (* 3 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_copy(o_comma(rc,rc),o_mul(o_par,rc))  149 *)
	IF (l.op = ir.o_comma (* 63 *)) &
           (r.op = ir.o_mul (* 12 *)) &
           (r.l.op = ir.o_par (* 64 *))  
	THEN
		IF P.Const4 (n.r.l)  THEN
			c0 := VAL (LONGINT, l.l.cost [NTrc]) + VAL (LONGINT, l.r.cost [NTrc]) + VAL (LONGINT, r.r.cost [NTrc]) + 1;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 149 };
			END;
		END;
	END;

(* stm: o_copy(o_comma(rc,rc),o_mul(rc,o_par))  148 *)
	IF (l.op = ir.o_comma (* 63 *)) &
           (r.op = ir.o_mul (* 12 *)) &
           (r.r.op = ir.o_par (* 64 *))  
	THEN
		IF P.Const4 (n.r.r)  THEN
			c0 := VAL (LONGINT, l.l.cost [NTrc]) + VAL (LONGINT, l.r.cost [NTrc]) + VAL (LONGINT, r.l.cost [NTrc]) + 1;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 148 };
			END;
		END;
	END;

(* stm: o_copy(o_comma(o_par,o_par),rc)  147 *)
	IF (l.op = ir.o_comma (* 63 *)) &
           (l.l.op = ir.o_par (* 64 *)) &
           (l.r.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstStackAddr (l.l) & P.ConstStackAddr (l.r)  THEN
			c0 := VAL (LONGINT, r.cost [NTrc]) + 11;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 147 };
			END;
		END;
	END;

(* stm: o_copy(o_comma(rc,o_par),rc)  146 *)
	IF (l.op = ir.o_comma (* 63 *)) &
           (l.r.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstStackAddr (l.r)  THEN
			c0 := VAL (LONGINT, l.l.cost [NTrc]) + VAL (LONGINT, r.cost [NTrc]) + 11;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 146 };
			END;
		END;
	END;

(* stm: o_copy(o_comma(o_par,rc),rc)  145 *)
	IF (l.op = ir.o_comma (* 63 *)) &
           (l.l.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstStackAddr (l.l)  THEN
			c0 := VAL (LONGINT, l.r.cost [NTrc]) + VAL (LONGINT, r.cost [NTrc]) + 11;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 145 };
			END;
		END;
	END;

(* stm: o_copy(o_comma(rc,rc),rc)  144 *)
	IF (l.op = ir.o_comma (* 63 *))  
	THEN
		c0 := VAL (LONGINT, l.l.cost [NTrc]) + VAL (LONGINT, l.r.cost [NTrc]) + VAL (LONGINT, r.cost [NTrc]) + 21;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 144 };
		END;
	END;
END newstate_o_copy;

PROCEDURE newstate_o_val* (n: RD.DAGNODE); (* 4 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_val(mem)  196 *)
	IF (l^.sz = 8) &
           (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType = ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 101;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 196 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_val(mem)  195 *)
	IF (l^.sz = 8) &
           (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType = ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 101;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 195 };
			NTclosure_reg(n, c0);
		END;
	END;

(* local: o_val(tos)  194 *)
	IF (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType = ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + 101;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 194 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_val(tos)  193 *)
	IF (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType = ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + 101;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 193 };
			NTclosure_reg(n, c0);
		END;
	END;

(* tos: o_val(mem)  192 *)
	IF (n^.tr^.ResType = ir.t_float) &
           (n^.tr^.OpType <> ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 101;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 192 };
			NTclosure_tos(n, c0);
		END;
	END;

(* tos: o_val(reg)  191 *)
	IF (n^.tr^.ResType = ir.t_float) &
           (n^.tr^.OpType <> ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTreg]) + 101;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 191 };
			NTclosure_tos(n, c0);
		END;
	END;

(* tos: o_val(tos)  189 *)
	IF (n^.tr^.ResType = ir.t_float) &
           (n^.tr^.OpType = ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + 0;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 189 };
			NTclosure_tos(n, c0);
		END;
	END;

(* mem: o_val(mem)  110 *)
	IF  (n^.tr^.Name >= Color.NNonTempVars) & H.NobodyWrites (n) &
           (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType <> ir.t_float) &
           (n.tr.ResSize <= n.tr.OpSize)
	THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 0;
		IF (c0 < n.cost[NTmem]) THEN
			n.cost[NTmem] := VAL (INTEGER, c0);
			n.rule[NTmem] := BurgNT.Rule { 110 };
			NTclosure_mem(n, c0);
		END;
	END;

(* reg: o_val(mrc)  106 *)
	IF (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType <> ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + 31;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 106 };
			NTclosure_reg(n, c0);
		END;
	END;
END newstate_o_val;

PROCEDURE newstate_o_cast* (n: RD.DAGNODE); (* 5 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_cast(tos)  200 *)
	c0 := VAL (LONGINT, l.cost [NTtos]) + 201;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 200 };
		NTclosure_reg(n, c0);
	END;

(* tos: o_cast(reg)  198 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + 201;
	IF (c0 < n.cost[NTtos]) THEN
		n.cost[NTtos] := VAL (INTEGER, c0);
		n.rule[NTtos] := BurgNT.Rule { 198 };
		NTclosure_tos(n, c0);
	END;

(* tos: o_cast(tos)  190 *)
	IF (n^.tr^.ResType = ir.t_float) &
           (n^.tr^.OpType = ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + 0;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 190 };
			NTclosure_tos(n, c0);
		END;
	END;

(* reg: o_cast(mrc)  111 *)
	IF (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType <> ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + 31;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 111 };
			NTclosure_reg(n, c0);
		END;
	END;

(* mem: o_cast(mem)  109 *)
	IF  (n^.tr^.Name >= Color.NNonTempVars) & H.NobodyWrites (n) &
           (n^.tr^.ResType <> ir.t_float) &
           (n^.tr^.OpType <> ir.t_float) &
           (n.tr.ResSize <= n.tr.OpSize)
	THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 0;
		IF (c0 < n.cost[NTmem]) THEN
			n.cost[NTmem] := VAL (INTEGER, c0);
			n.rule[NTmem] := BurgNT.Rule { 109 };
			NTclosure_mem(n, c0);
		END;
	END;
END newstate_o_cast;

PROCEDURE newstate_o_cap* (n: RD.DAGNODE); (* 6 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_cap(mrc)  79 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 79 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_cap;

PROCEDURE newstate_o_add* (n: RD.DAGNODE); (* 11 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_add(mem,o_par)  56 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & P.SameAlloc (n, l) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.IsOne (n.r) & P.NotWhole8Node(n)  THEN
			c0 := VAL (LONGINT, l.cost [NTmem]) + 30;
			IF (c0 < n.cost[NTlocal]) THEN
				n.cost[NTlocal] := VAL (INTEGER, c0);
				n.rule[NTlocal] := BurgNT.Rule { 56 };
				NTclosure_local(n, c0);
			END;
		END;
	END;

(* local: o_add(rc,mem)  55 *)
	IF R.MemOps & P.SameAlloc (n, n.r)  THEN
		c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 55 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_add(mem,rc)  54 *)
	IF R.MemOps & P.SameAlloc (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 54 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_add(mrc,o_par)  53 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.IsOne (n.r) & P.NotWhole8Node(n)  THEN
			c0 := VAL (LONGINT, l.cost [NTmrc]) + 21;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 53 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_add(mrc,reg)  52 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 52 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_add(reg,mrc)  51 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 51 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_add(scaled,based)  39 *)
	IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{})  THEN
		c0 := VAL (LONGINT, l.cost [NTscaled]) + VAL (LONGINT, r.cost [NTbased]) + 11;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 39 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_add(based,scaled)  38 *)
	IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{})  THEN
		c0 := VAL (LONGINT, l.cost [NTbased]) + VAL (LONGINT, r.cost [NTscaled]) + 11;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 38 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_add(based,based)  37 *)
	IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{})  THEN
		c0 := VAL (LONGINT, l.cost [NTbased]) + VAL (LONGINT, r.cost [NTbased]) + 21;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 37 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_add(addr,o_par)  35 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotStackAddr (n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 35 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_add(scaled,o_par)  32 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotStackAddr (n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTscaled]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 32 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_add(based,o_par)  29 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotStackAddr (n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTbased]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 29 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* addr: o_add(addr,o_par)  22 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotStackAddr (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   (l^.cost [ NTaddr] < MAX (INTEGER)) &
                   H.LiveAddrMode (l,  NTaddr, n^.tr^.Name)
		THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + 0;
			IF (c0 < n.cost[NTaddr]) THEN
				n.cost[NTaddr] := VAL (INTEGER, c0);
				n.rule[NTaddr] := BurgNT.Rule { 22 };
				NTclosure_addr(n, c0);
			END;
		END;
	END;

(* addr: o_add(scaled,based)  19 *)
	IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) &  (n^.tr^.Name >= Color.NNonTempVars) &
           (n.r^.cost [ NTbased] < MAX (INTEGER)) &
           (l^.cost [ NTscaled] < MAX (INTEGER)) &
           H.LiveAddrMode (n.r,  NTbased, n^.tr^.Name) &
           H.LiveAddrMode (l,  NTscaled, n^.tr^.Name) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTscaled]) + VAL (LONGINT, r.cost [NTbased]) + 0;
		IF (c0 < n.cost[NTaddr]) THEN
			n.cost[NTaddr] := VAL (INTEGER, c0);
			n.rule[NTaddr] := BurgNT.Rule { 19 };
			NTclosure_addr(n, c0);
		END;
	END;

(* addr: o_add(based,scaled)  18 *)
	IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) &  (n^.tr^.Name >= Color.NNonTempVars) &
           (l^.cost [ NTbased] < MAX (INTEGER)) &
           (n.r^.cost [ NTscaled] < MAX (INTEGER)) &
           H.LiveAddrMode (l,  NTbased, n^.tr^.Name) &
           H.LiveAddrMode (n.r,  NTscaled, n^.tr^.Name) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTbased]) + VAL (LONGINT, r.cost [NTscaled]) + 0;
		IF (c0 < n.cost[NTaddr]) THEN
			n.cost[NTaddr] := VAL (INTEGER, c0);
			n.rule[NTaddr] := BurgNT.Rule { 18 };
			NTclosure_addr(n, c0);
		END;
	END;

(* addr: o_add(based,based)  16 *)
	IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) &  (n^.tr^.Name >= Color.NNonTempVars) &
           (l^.cost [ NTbased] < MAX (INTEGER)) &
           (n.r^.cost [ NTbased] < MAX (INTEGER)) &
           H.LiveAddrMode (l,  NTbased, n^.tr^.Name) &
           H.LiveAddrMode (n.r,  NTbased, n^.tr^.Name) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTbased]) + VAL (LONGINT, r.cost [NTbased]) + 10;
		IF (c0 < n.cost[NTaddr]) THEN
			n.cost[NTaddr] := VAL (INTEGER, c0);
			n.rule[NTaddr] := BurgNT.Rule { 16 };
			NTclosure_addr(n, c0);
		END;
	END;

(* scaled: o_add(scaled,o_par)  13 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotStackAddr (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   (l^.cost [ NTscaled] < MAX (INTEGER)) &
                   H.LiveAddrMode (l,  NTscaled, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTscaled]) + 0;
			IF (c0 < n.cost[NTscaled]) THEN
				n.cost[NTscaled] := VAL (INTEGER, c0);
				n.rule[NTscaled] := BurgNT.Rule { 13 };
				NTclosure_scaled(n, c0);
			END;
		END;
	END;

(* based: o_add(based,o_par)  8 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotStackAddr (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   (l^.cost [ NTbased] < MAX (INTEGER)) &
                   H.LiveAddrMode (l,  NTbased, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTbased]) + 0;
			IF (c0 < n.cost[NTbased]) THEN
				n.cost[NTbased] := VAL (INTEGER, c0);
				n.rule[NTbased] := BurgNT.Rule { 8 };
				NTclosure_based(n, c0);
			END;
		END;
	END;
END newstate_o_add;

PROCEDURE newstate_o_mul* (n: RD.DAGNODE); (* 12 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_mul(reg,o_par)  96 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF (n^.tr^.ResSize <> 1) &
                   ((n^.tr^.ResType = ir.t_int) OR  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{})) &
                   (n.r^.op = ir.o_par) & (n.r^.par^.tag = ir.y_NumConst) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 301;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 96 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_mul(mem,o_par)  95 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF (n^.tr^.ResSize <> 1) &
                   ((n^.tr^.ResType = ir.t_int) OR  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{})) &
                   (n.r^.op = ir.o_par) & (n.r^.par^.tag = ir.y_NumConst) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTmem]) + 301;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 95 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* local: o_mul(mrc,rc)  90 *)
	IF (n^.tr^.ResSize = 1) OR (n^.tr^.ResType <> ir.t_int)  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTrc]) + 401;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 90 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_mul(rc,mrc)  89 *)
	IF (n^.tr^.ResSize = 1) OR (n^.tr^.ResType <> ir.t_int)  THEN
		c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 89 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_mul(mrc,rc)  88 *)
	IF (n^.tr^.ResSize = 1) OR (n^.tr^.ResType <> ir.t_int)  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTrc]) + 401;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 88 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_mul(rc,mrc)  87 *)
	IF (n^.tr^.ResSize = 1) OR (n^.tr^.ResType <> ir.t_int)  THEN
		c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 87 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_mul(mrc,reg)  86 *)
	IF (n^.tr^.ResSize <> 1) & ((n^.tr^.ResType = ir.t_int) OR  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}))  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 311;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 86 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_mul(reg,mrc)  85 *)
	IF (n^.tr^.ResSize <> 1) & ((n^.tr^.ResType = ir.t_int) OR  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}))  THEN
		c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 311;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 85 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_mul(reg,o_par)  27 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.Const359 (n.r) & (n.tr.ResSize <= 4) THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 21;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 27 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_mul(reg,o_par)  25 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.Const248 (n.r) & (n.tr.ResSize <= 4)  THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 21;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 25 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* addr: o_mul(reg,o_par)  24 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.Const359 (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   H.LiveReg (l, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 10;
			IF (c0 < n.cost[NTaddr]) THEN
				n.cost[NTaddr] := VAL (INTEGER, c0);
				n.rule[NTaddr] := BurgNT.Rule { 24 };
				NTclosure_addr(n, c0);
			END;
		END;
	END;

(* scaled: o_mul(reg,o_par)  11 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.Const248 (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   H.LiveReg (l, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 10;
			IF (c0 < n.cost[NTscaled]) THEN
				n.cost[NTscaled] := VAL (INTEGER, c0);
				n.rule[NTscaled] := BurgNT.Rule { 11 };
				NTclosure_scaled(n, c0);
			END;
		END;
	END;
END newstate_o_mul;

PROCEDURE newstate_o_mulh* (n: RD.DAGNODE); (* 13 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_mulh(mrc,rc)  94 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTrc]) + 401;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 94 };
		NTclosure_local(n, c0);
	END;

(* local: o_mulh(rc,mrc)  93 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 93 };
		NTclosure_local(n, c0);
	END;

(* reg: o_mulh(mrc,rc)  92 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTrc]) + 401;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 92 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_mulh(rc,mrc)  91 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 91 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_mulh;

PROCEDURE newstate_o_div* (n: RD.DAGNODE); (* 14 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_div(rc,mrc)  98 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 98 };
		NTclosure_local(n, c0);
	END;

(* reg: o_div(rc,mrc)  97 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 97 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_div;

PROCEDURE newstate_o_dvd* (n: RD.DAGNODE); (* 15 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_dvd(rc,mrc)  100 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 100 };
		NTclosure_local(n, c0);
	END;

(* reg: o_dvd(rc,mrc)  99 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 99 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_dvd;

PROCEDURE newstate_o_mod* (n: RD.DAGNODE); (* 16 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_mod(rc,mrc)  104 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 104 };
		NTclosure_local(n, c0);
	END;

(* reg: o_mod(rc,mrc)  103 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 103 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_mod;

PROCEDURE newstate_o_rem* (n: RD.DAGNODE); (* 17 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_rem(rc,mrc)  102 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 102 };
		NTclosure_local(n, c0);
	END;

(* reg: o_rem(rc,mrc)  101 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmrc]) + 401;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 101 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_rem;

PROCEDURE newstate_o_not* (n: RD.DAGNODE); (* 23 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_not(mem)  80 *)
	IF R.MemOps & P.SameAlloc (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 80 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_not(mrc)  78 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 78 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_not;

PROCEDURE newstate_o_incl* (n: RD.DAGNODE); (* 24 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_incl(o_par,reg)  119 *)
	IF (l.op = ir.o_par (* 64 *))  
	THEN
		IF P.IsZero (n^.tr^.ResSize, l)  THEN
			c0 := VAL (LONGINT, r.cost [NTreg]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 119 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* local: o_incl(mem,reg)  117 *)
	IF R.MemOps & P.SameAlloc (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTreg]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 117 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_incl(reg,reg)  116 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTreg]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 116 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_incl;

PROCEDURE newstate_o_excl* (n: RD.DAGNODE); (* 25 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_excl(o_par,reg)  123 *)
	IF (l.op = ir.o_par (* 64 *))  
	THEN
		IF P.IsAllOnes (l, n^.tr^.ResType)  THEN
			c0 := VAL (LONGINT, r.cost [NTreg]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 123 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* local: o_excl(mem,reg)  121 *)
	IF R.MemOps & P.SameAlloc (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTreg]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 121 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_excl(reg,reg)  120 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTreg]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 120 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_excl;

PROCEDURE newstate_o_loset* (n: RD.DAGNODE); (* 26 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_loset(reg)  124 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 124 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_loset;

PROCEDURE newstate_o_hiset* (n: RD.DAGNODE); (* 27 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_hiset(reg)  127 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 127 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_hiset;

PROCEDURE newstate_o_sgnext* (n: RD.DAGNODE); (* 33 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_sgnext(mrc)  105 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 32;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 105 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_sgnext;

PROCEDURE newstate_o_call* (n: RD.DAGNODE); (* 36 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* tos: o_call(mrc)  201 *)
	IF (n^.tr^.ResType = ir.t_float)  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 201 };
			NTclosure_tos(n, c0);
		END;
	END;

(* stm: o_call(mrc)  143 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 143 };
	END;

(* local: o_call(mrc)  113 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 113 };
		NTclosure_local(n, c0);
	END;

(* reg: o_call(mrc)  112 *)
	IF n^.tr^.ResType <> ir.t_float  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 112 };
			NTclosure_reg(n, c0);
		END;
	END;
END newstate_o_call;

PROCEDURE newstate_o_ret* (n: RD.DAGNODE); (* 37 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_ret  166 *)
	c0 :=11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 166 };
	END;
END newstate_o_ret;

PROCEDURE newstate_o_checknil* (n: RD.DAGNODE); (* 38 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_checknil(tos)  139 *)
	c0 := VAL (LONGINT, l.cost [NTtos]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 139 };
	END;

(* stm: o_checknil(reg)  138 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 138 };
	END;
END newstate_o_checknil;

PROCEDURE newstate_o_checklo* (n: RD.DAGNODE); (* 39 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_checklo(rc,mem)  133 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 133 };
	END;

(* stm: o_checklo(mem,rc)  132 *)
	c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 132 };
	END;

(* stm: o_checklo(mrc,reg)  131 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 131 };
	END;

(* stm: o_checklo(reg,mrc)  130 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 130 };
	END;
END newstate_o_checklo;

PROCEDURE newstate_o_checkhi* (n: RD.DAGNODE); (* 40 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_checkhi(rc,mem)  137 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 137 };
	END;

(* stm: o_checkhi(mem,rc)  136 *)
	c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 136 };
	END;

(* stm: o_checkhi(mrc,reg)  135 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 135 };
	END;

(* stm: o_checkhi(reg,mrc)  134 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 134 };
	END;
END newstate_o_checkhi;

PROCEDURE newstate_o_stop* (n: RD.DAGNODE); (* 41 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_stop(rc)  142 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 142 };
	END;
END newstate_o_stop;

PROCEDURE newstate_o_error* (n: RD.DAGNODE); (* 42 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_error(o_comma(rc,rc),rc)  141 *)
	IF (l.op = ir.o_comma (* 63 *))  
	THEN
		c0 := VAL (LONGINT, l.l.cost [NTrc]) + VAL (LONGINT, l.r.cost [NTrc]) + VAL (LONGINT, r.cost [NTrc]) + 11;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 141 };
		END;
	END;
END newstate_o_error;

PROCEDURE newstate_o_loadr* (n: RD.DAGNODE); (* 45 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* tos: o_loadr(addr)  170 *)
	IF (n^.tr^.ResType = ir.t_float) & (n^.tr^.ResSize >= 4)  THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + 31;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 170 };
			NTclosure_tos(n, c0);
		END;
	END;

(* local: o_loadr(addr)  46 *)
	IF P.SameMemLoadr (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + 0;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 46 };
			NTclosure_local(n, c0);
		END;
	END;

(* mem: o_loadr(addr)  44 *)
	IF (~ (n^.tr^.Name >= Color.NNonTempVars)& P.SameMemLoadr(n, l) OR
           (  (n^.tr^.Name >= Color.NNonTempVars)& H.NobodyWrites (n) &
           (l^.cost [ NTaddr] < MAX (INTEGER)) &
           H.LiveAddrMode (l,  NTaddr, n^.tr^.Name)) )
	THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + 0;
		IF (c0 < n.cost[NTmem]) THEN
			n.cost[NTmem] := VAL (INTEGER, c0);
			n.rule[NTmem] := BurgNT.Rule { 44 };
			NTclosure_mem(n, c0);
		END;
	END;

(* reg: o_loadr(addr)  40 *)
	IF (n^.tr^.ResType # ir.t_float )&(n^.tr^.ResType # ir.t_arr ) & (n^.tr^.ResSize <= 8)  THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + 11;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 40 };
			NTclosure_reg(n, c0);
		END;
	END;
END newstate_o_loadr;

PROCEDURE newstate_o_storer* (n: RD.DAGNODE); (* 46 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_storer(addr,mem)  260 *)
	IF (n^.tr^.ResSize = 8) &
           ((n.r^.op <> ir.o_par) OR (n.r^.par^.tag = ir.y_Variable) &
           (P.RD_LocIN_MEM(n.r^.par^.name))) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.cost [NTmem]) + 70;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 260 };
		END;
	END;

(* stm: o_storer(addr,o_move_le(rc,mem))  259 *)
	IF (r.op = ir.o_move_le (* 58 *))  
	THEN
		IF n^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTrc]) + VAL (LONGINT, r.r.cost [NTmem]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 259 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_le(mem,rc))  258 *)
	IF (r.op = ir.o_move_le (* 58 *))  
	THEN
		IF n.r^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + VAL (LONGINT, r.r.cost [NTrc]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 258 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_le(mrc,reg))  257 *)
	IF (r.op = ir.o_move_le (* 58 *))  
	THEN
		IF n.r^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmrc]) + VAL (LONGINT, r.r.cost [NTreg]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 257 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_le(reg,mrc))  256 *)
	IF (r.op = ir.o_move_le (* 58 *))  
	THEN
		IF n.r^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTreg]) + VAL (LONGINT, r.r.cost [NTmrc]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 256 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_eq(o_logical(mem,o_par),o_par))  247 *)
	IF (r.op = ir.o_move_eq (* 57 *)) &
           (r.l.op = ir.o_logical (* 67 *)) &
           (r.l.r.op = ir.o_par (* 64 *)) &
           (r.r.op = ir.o_par (* 64 *))  
	THEN
		IF (n.r.l^.tr^.Op = ir.o_and) & P.IsZero (n.r^.tr^.OpSize, n.r.r) &
                   (n.r^.tr^.OpSize = n.r.l^.tr^.ResSize) & P.ConstNotAddr (n.r.l.r) &
                   (n.r^.tr^.ResSize = 1) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.l.cost [NTmem]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 247 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_eq(o_logical(reg,o_par),o_par))  246 *)
	IF (r.op = ir.o_move_eq (* 57 *)) &
           (r.l.op = ir.o_logical (* 67 *)) &
           (r.l.r.op = ir.o_par (* 64 *)) &
           (r.r.op = ir.o_par (* 64 *))  
	THEN
		IF (n.r.l^.tr^.Op = ir.o_and) & P.IsZero (n.r^.tr^.OpSize, n.r.r) &
                   (n.r^.tr^.OpSize = n.r.l^.tr^.ResSize) & P.ConstNotAddr (n.r.l.r) &
                   (n.r^.tr^.ResSize = 1) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.l.cost [NTreg]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 246 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_eq(rc,mem))  245 *)
	IF (r.op = ir.o_move_eq (* 57 *))  
	THEN
		IF n.r^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTrc]) + VAL (LONGINT, r.r.cost [NTmem]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 245 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_eq(mem,rc))  244 *)
	IF (r.op = ir.o_move_eq (* 57 *))  
	THEN
		IF n.r^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + VAL (LONGINT, r.r.cost [NTrc]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 244 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_eq(mrc,reg))  243 *)
	IF (r.op = ir.o_move_eq (* 57 *))  
	THEN
		IF n.r^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmrc]) + VAL (LONGINT, r.r.cost [NTreg]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 243 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_move_eq(reg,mrc))  242 *)
	IF (r.op = ir.o_move_eq (* 57 *))  
	THEN
		IF n.r^.tr^.ResSize = 1  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTreg]) + VAL (LONGINT, r.r.cost [NTmrc]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 242 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_par)  203 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstReal (n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + 44;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 203 };
			END;
		END;
	END;

(* stm: o_storer(addr,tos)  202 *)
	c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.cost [NTtos]) + 71;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 202 };
	END;

(* stm: o_storer(addr,o_funary(mem))  186 *)
	IF (r.op = ir.o_funary (* 69 *))  
	THEN
		IF ((n.r^.tr^.Op = ir.o_abs) OR (n.r^.tr^.Op = ir.o_neg)) &
                   P.SameAddrs (n, n.r.l) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + 30;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 186 };
			END;
		END;
	END;

(* stm: o_storer(addr,rc)  129 *)
	IF (n^.tr^.ResSize <= 8)OR(n^.tr^.ResType = ir.t_int)  THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.cost [NTrc]) + 11;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 129 };
		END;
	END;

(* stm: o_storer(addr,mem)  128 *)
	IF P.SameMemStorer (l, n.r)  THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.cost [NTmem]) + 0;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 128 };
		END;
	END;

(* stm: o_storer(addr,o_excl(mem,reg))  122 *)
	IF (r.op = ir.o_excl (* 25 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + VAL (LONGINT, r.r.cost [NTreg]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 122 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_incl(mem,reg))  118 *)
	IF (r.op = ir.o_incl (* 24 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + VAL (LONGINT, r.r.cost [NTreg]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 118 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_shift(mem,o_par))  84 *)
	IF (r.op = ir.o_shift (* 66 *)) &
           (r.r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l) & P.ConstNotAddr (n.r.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 84 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_not(mem))  81 *)
	IF (r.op = ir.o_not (* 23 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 81 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_logical(rc,mem))  77 *)
	IF (r.op = ir.o_logical (* 67 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTrc]) + VAL (LONGINT, r.r.cost [NTmem]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 77 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_logical(mem,rc))  76 *)
	IF (r.op = ir.o_logical (* 67 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + VAL (LONGINT, r.r.cost [NTrc]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 76 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_logical(mem,o_par))  71 *)
	IF (r.op = ir.o_logical (* 67 *)) &
           (r.r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & (n.r^.tr^.Op = ir.o_xor) &
                   P.IsAllOnes (n.r.r, n.r^.tr^.ResType) & P.SameAddrs (n, n.r.l) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + 30;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 71 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_neg(mem))  68 *)
	IF (r.op = ir.o_neg (* 60 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 68 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_sub(mem,o_par))  65 *)
	IF (r.op = ir.o_sub (* 61 *)) &
           (r.r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l) &  (n.r^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.IsOne ( n.r.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + 30;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 65 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_sub(mem,rc))  64 *)
	IF (r.op = ir.o_sub (* 61 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + VAL (LONGINT, r.r.cost [NTrc]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 64 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_add(mem,o_par))  59 *)
	IF (r.op = ir.o_add (* 11 *)) &
           (r.r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l) &  (n.r^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.IsOne (n.r.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + 30;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 59 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_add(rc,mem))  58 *)
	IF (r.op = ir.o_add (* 11 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTrc]) + VAL (LONGINT, r.r.cost [NTmem]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 58 };
			END;
		END;
	END;

(* stm: o_storer(addr,o_add(mem,rc))  57 *)
	IF (r.op = ir.o_add (* 11 *))  
	THEN
		IF R.MemOps & P.SameAddrs (n, n.r.l)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + VAL (LONGINT, r.l.cost [NTmem]) + VAL (LONGINT, r.r.cost [NTrc]) + 31;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 57 };
			END;
		END;
	END;
END newstate_o_storer;

PROCEDURE newstate_o_eq* (n: RD.DAGNODE); (* 49 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_eq(o_logical(mem,o_par),o_par)  160 *)
	IF (l.op = ir.o_logical (* 67 *)) &
           (l.r.op = ir.o_par (* 64 *)) &
           (r.op = ir.o_par (* 64 *))  
	THEN
		IF (l^.tr^.Op = ir.o_and) & P.IsZero (n^.tr^.ResSize, n.r) &
                   (n^.tr^.ResSize = l^.tr^.ResSize) & P.ConstNotAddr (l.r) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + 21;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 160 };
			END;
		END;
	END;

(* stm: o_eq(o_logical(reg,o_par),o_par)  159 *)
	IF (l.op = ir.o_logical (* 67 *)) &
           (l.r.op = ir.o_par (* 64 *)) &
           (r.op = ir.o_par (* 64 *))  
	THEN
		IF (l^.tr^.Op = ir.o_and) & P.IsZero (n^.tr^.ResSize, n.r) &
                   (n^.tr^.ResSize = l^.tr^.ResSize) & P.ConstNotAddr (l.r) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTreg]) + 11;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 159 };
			END;
		END;
	END;

(* stm: o_eq(rc,mem)  158 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 158 };
	END;

(* stm: o_eq(mem,rc)  157 *)
	c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 157 };
	END;

(* stm: o_eq(mrc,reg)  156 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 156 };
	END;

(* stm: o_eq(reg,mrc)  155 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 155 };
	END;
END newstate_o_eq;

PROCEDURE newstate_o_le* (n: RD.DAGNODE); (* 50 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_le(rc,mem)  154 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 154 };
	END;

(* stm: o_le(mem,rc)  153 *)
	c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 153 };
	END;

(* stm: o_le(mrc,reg)  152 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 152 };
	END;

(* stm: o_le(reg,mrc)  151 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 151 };
	END;
END newstate_o_le;

PROCEDURE newstate_o_in* (n: RD.DAGNODE); (* 52 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_in(reg,addr)  164 *)
	IF n.tr.ResType=ir.t_ref THEN
		c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTaddr]) + 11;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 164 };
		END;
	END;

(* stm: o_in(reg,mem)  163 *)
	IF n.tr.ResType#ir.t_ref THEN
		c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmem]) + 11;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 163 };
		END;
	END;

(* stm: o_in(reg,o_par)  162 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF FALSE(*(n.r.par.tag = ir.y_NumConst)&(n.r.sz<=tune.BITSET_LEN_scalar)*) THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 20;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 162 };
			END;
		END;
	END;

(* stm: o_in(reg,reg)  161 *)
	IF (n.tr.ResSize<=8)&(n.tr.ResType#ir.t_ref) THEN
		c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTreg]) + 21;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 161 };
		END;
	END;
END newstate_o_in;

PROCEDURE newstate_o_case* (n: RD.DAGNODE); (* 54 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_case(reg)  165 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 165 };
	END;
END newstate_o_case;

PROCEDURE newstate_o_goto* (n: RD.DAGNODE); (* 55 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_goto  150 *)
	c0 :=0;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 150 };
	END;
END newstate_o_goto;

PROCEDURE newstate_o_getpar* (n: RD.DAGNODE); (* 56 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* tos: o_getpar  188 *)
	IF n.tr.ResType=ir.t_float THEN
		c0 :=31;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 188 };
			NTclosure_tos(n, c0);
		END;
	END;

(* local: o_getpar  50 *)
	IF R.GetParInLocal (n^.tr)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 50 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_getpar  49 *)
	IF n^.tr^.ResSize <= 8  THEN
		c0 :=11;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 49 };
			NTclosure_reg(n, c0);
		END;
	END;
END newstate_o_getpar;

PROCEDURE newstate_o_move_eq* (n: RD.DAGNODE); (* 57 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_move_eq(o_logical(mem,o_par),o_par)  241 *)
	IF (l.op = ir.o_logical (* 67 *)) &
           (l.r.op = ir.o_par (* 64 *)) &
           (r.op = ir.o_par (* 64 *))  
	THEN
		IF (l^.tr^.Op = ir.o_and) & P.IsZero (n^.tr^.OpSize, n.r) &
                   (n^.tr^.OpSize = l^.tr^.ResSize) & P.ConstNotAddr (l.r) &
                   (n^.tr^.ResSize = 1) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + 21;
			IF (c0 < n.cost[NTlocal]) THEN
				n.cost[NTlocal] := VAL (INTEGER, c0);
				n.rule[NTlocal] := BurgNT.Rule { 241 };
				NTclosure_local(n, c0);
			END;
		END;
	END;

(* local: o_move_eq(o_logical(reg,o_par),o_par)  240 *)
	IF (l.op = ir.o_logical (* 67 *)) &
           (l.r.op = ir.o_par (* 64 *)) &
           (r.op = ir.o_par (* 64 *))  
	THEN
		IF (l^.tr^.Op = ir.o_and) & P.IsZero (n^.tr^.OpSize, n.r) &
                   (n^.tr^.OpSize = l^.tr^.ResSize) & P.ConstNotAddr (l.r) &
                   (n^.tr^.ResSize = 1) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTreg]) + 11;
			IF (c0 < n.cost[NTlocal]) THEN
				n.cost[NTlocal] := VAL (INTEGER, c0);
				n.rule[NTlocal] := BurgNT.Rule { 240 };
				NTclosure_local(n, c0);
			END;
		END;
	END;

(* local: o_move_eq(rc,mem)  239 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 239 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_move_eq(mem,rc)  238 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 238 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_move_eq(mrc,reg)  237 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 237 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_move_eq(reg,mrc)  236 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 236 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_move_eq(o_logical(o_incl(o_par,reg),reg),o_par)  235 *)
	IF (l.op = ir.o_logical (* 67 *)) &
           (l.l.op = ir.o_incl (* 24 *)) &
           (l.l.l.op = ir.o_par (* 64 *)) &
           (r.op = ir.o_par (* 64 *))  
	THEN
		IF (l^.tr^.Op = ir.o_and)
                   & P.IsZero (n^.tr^.OpSize, l.l.l) & P.IsZero (n^.tr^.OpSize, n.r)

		THEN
			c0 := VAL (LONGINT, l.l.r.cost [NTreg]) + VAL (LONGINT, l.r.cost [NTreg]) + 10;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 235 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_move_eq(o_logical(mem,o_par),o_par)  234 *)
	IF (l.op = ir.o_logical (* 67 *)) &
           (l.r.op = ir.o_par (* 64 *)) &
           (r.op = ir.o_par (* 64 *))  
	THEN
		IF (l^.tr^.Op = ir.o_and) & P.IsZero (n^.tr^.OpSize, n.r) &
                   (n^.tr^.OpSize = l^.tr^.ResSize) & P.ConstNotAddr (l.r) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + 21;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 234 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_move_eq(o_logical(reg,o_par),o_par)  233 *)
	IF (l.op = ir.o_logical (* 67 *)) &
           (l.r.op = ir.o_par (* 64 *)) &
           (r.op = ir.o_par (* 64 *))  
	THEN
		IF (l^.tr^.Op = ir.o_and) & P.IsZero (n^.tr^.OpSize, n.r) &
                   (n^.tr^.OpSize = l^.tr^.ResSize) & P.ConstNotAddr (l.r) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTreg]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 233 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_move_eq(rc,mem)  232 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 232 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_move_eq(mem,rc)  231 *)
	c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 231 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_move_eq(mrc,reg)  230 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 230 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_move_eq(reg,mrc)  229 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 229 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_move_eq;

PROCEDURE newstate_o_move_le* (n: RD.DAGNODE); (* 58 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_move_le(rc,mem)  255 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 255 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_move_le(mem,rc)  254 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 254 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_move_le(mrc,reg)  253 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 253 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_move_le(reg,mrc)  252 *)
	IF n^.tr^.ResSize = 1  THEN
		c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 252 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_move_le(rc,mem)  251 *)
	c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 251 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_move_le(mem,rc)  250 *)
	c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 250 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_move_le(mrc,reg)  249 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 249 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_move_le(reg,mrc)  248 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 11;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 248 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_move_le;

PROCEDURE newstate_o_alloca* (n: RD.DAGNODE); (* 59 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_alloca(mrc)  115 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTlocal]) THEN
		n.cost[NTlocal] := VAL (INTEGER, c0);
		n.rule[NTlocal] := BurgNT.Rule { 115 };
		NTclosure_local(n, c0);
	END;

(* reg: o_alloca(mrc)  114 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 114 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_alloca;

PROCEDURE newstate_o_neg* (n: RD.DAGNODE); (* 60 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_neg(mem)  67 *)
	IF R.MemOps & P.SameAlloc (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 67 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_neg(mrc)  66 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 66 };
		NTclosure_reg(n, c0);
	END;
END newstate_o_neg;

PROCEDURE newstate_o_sub* (n: RD.DAGNODE); (* 61 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_sub(mem,o_par)  63 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & P.SameAlloc (n, l) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.IsOne (n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTmem]) + 30;
			IF (c0 < n.cost[NTlocal]) THEN
				n.cost[NTlocal] := VAL (INTEGER, c0);
				n.rule[NTlocal] := BurgNT.Rule { 63 };
				NTclosure_local(n, c0);
			END;
		END;
	END;

(* local: o_sub(mem,rc)  62 *)
	IF R.MemOps & P.SameAlloc (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 62 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_sub(mrc,o_par)  61 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.IsOne (n.r) & P.NotWhole8Node(n)  THEN
			c0 := VAL (LONGINT, l.cost [NTmrc]) + 21;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 61 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_sub(reg,mrc)  60 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 60 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_sub(addr,o_par)  36 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotAddr (n.r) & (n.tr.ResSize <= 4)  THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 36 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_sub(scaled,o_par)  33 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotAddr (n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTscaled]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 33 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_sub(based,o_par)  30 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotAddr (n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTbased]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 30 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* addr: o_sub(addr,o_par)  23 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotAddr (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   (l^.cost [ NTaddr] < MAX (INTEGER)) &
                   H.LiveAddrMode (l,  NTaddr, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTaddr]) + 0;
			IF (c0 < n.cost[NTaddr]) THEN
				n.cost[NTaddr] := VAL (INTEGER, c0);
				n.rule[NTaddr] := BurgNT.Rule { 23 };
				NTclosure_addr(n, c0);
			END;
		END;
	END;

(* scaled: o_sub(scaled,o_par)  14 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotAddr (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   (l^.cost [ NTscaled] < MAX (INTEGER)) &
                   H.LiveAddrMode (l,  NTscaled, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTscaled]) + 0;
			IF (c0 < n.cost[NTscaled]) THEN
				n.cost[NTscaled] := VAL (INTEGER, c0);
				n.rule[NTscaled] := BurgNT.Rule { 14 };
				NTclosure_scaled(n, c0);
			END;
		END;
	END;

(* based: o_sub(based,o_par)  9 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.ConstNotAddr (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   (l^.cost [ NTbased] < MAX (INTEGER)) &
                   H.LiveAddrMode (l,  NTbased, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTbased]) + 0;
			IF (c0 < n.cost[NTbased]) THEN
				n.cost[NTbased] := VAL (INTEGER, c0);
				n.rule[NTbased] := BurgNT.Rule { 9 };
				NTclosure_based(n, c0);
			END;
		END;
	END;
END newstate_o_sub;

PROCEDURE newstate_o_putpar* (n: RD.DAGNODE); (* 62 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_putpar(addr)  207 *)
	IF NOT  (NOT (P.IsAggregateByValue (n^.tr)))  THEN
		c0 := VAL (LONGINT, l.cost [NTaddr]) + 22;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 207 };
		END;
	END;

(* stm: o_putpar(tos)  206 *)
	IF  (NOT (P.IsAggregateByValue (n^.tr)))  THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + 22;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 206 };
		END;
	END;

(* stm: o_putpar(o_par)  205 *)
	IF (l.op = ir.o_par (* 64 *))  
	THEN
		IF  (NOT (P.IsAggregateByValue (n^.tr))) & P.ConstReal (l)  THEN
			c0 := 22;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 205 };
			END;
		END;
	END;

(* stm: o_putpar(mem)  204 *)
	IF  (NOT (P.IsAggregateByValue (n^.tr))) & (n^.tr^.OpSize >= 4)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 22;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 204 };
		END;
	END;

(* stm: o_putpar(mrc)  140 *)
	IF  (NOT (P.IsAggregateByValue (n^.tr))) & ((n^.tr^.OpSize <= 4)OR(n^.tr^.OpType=ir.t_int)OR(n^.tr^.OpType=ir.t_unsign))  THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + 11;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 140 };
		END;
	END;
END newstate_o_putpar;

PROCEDURE newstate_o_comma* (n: RD.DAGNODE); (* 63 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;
END newstate_o_comma;

PROCEDURE newstate_o_par* (n: RD.DAGNODE); (* 64 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* tos: o_par  174 *)
	IF P.GoodFloatConst (n)  THEN
		c0 :=21;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 174 };
			NTclosure_tos(n, c0);
		END;
	END;

(* tos: o_par  173 *)
	IF P.ConstNotVar (n)  THEN
		c0 :=31;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 173 };
			NTclosure_tos(n, c0);
		END;
	END;

(* tos: o_par  169 *)
	IF P.TosVar (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 169 };
			NTclosure_tos(n, c0);
		END;
	END;

(* mem: o_par  48 *)
	IF P.LocalVar (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTmem]) THEN
			n.cost[NTmem] := VAL (INTEGER, c0);
			n.rule[NTmem] := BurgNT.Rule { 48 };
			NTclosure_mem(n, c0);
		END;
	END;

(* mem: o_par  45 *)
	IF P.MemVar (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTmem]) THEN
			n.cost[NTmem] := VAL (INTEGER, c0);
			n.rule[NTmem] := BurgNT.Rule { 45 };
			NTclosure_mem(n, c0);
		END;
	END;

(* reg: o_par  42 *)
	IF P.ConstNotVar (n)  THEN
		c0 :=11;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 42 };
			NTclosure_reg(n, c0);
		END;
	END;

(* reg: o_par  41 *)
	IF P.RegVar (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 41 };
			NTclosure_reg(n, c0);
		END;
	END;

(* addr: o_par  21 *)
	IF P.ConstNotStackAddr (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTaddr]) THEN
			n.cost[NTaddr] := VAL (INTEGER, c0);
			n.rule[NTaddr] := BurgNT.Rule { 21 };
			NTclosure_addr(n, c0);
		END;
	END;

(* addr: o_par  20 *)
	IF P.AddrVar(n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTaddr]) THEN
			n.cost[NTaddr] := VAL (INTEGER, c0);
			n.rule[NTaddr] := BurgNT.Rule { 20 };
			NTclosure_addr(n, c0);
		END;
	END;

(* scaled: o_par  10 *)
	IF P.ScaledVar (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTscaled]) THEN
			n.cost[NTscaled] := VAL (INTEGER, c0);
			n.rule[NTscaled] := BurgNT.Rule { 10 };
			NTclosure_scaled(n, c0);
		END;
	END;

(* based: o_par  7 *)
	IF P.ConstStackAddr (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTbased]) THEN
			n.cost[NTbased] := VAL (INTEGER, c0);
			n.rule[NTbased] := BurgNT.Rule { 7 };
			NTclosure_based(n, c0);
		END;
	END;

(* based: o_par  6 *)
	IF P.BasedVar (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTbased]) THEN
			n.cost[NTbased] := VAL (INTEGER, c0);
			n.rule[NTbased] := BurgNT.Rule { 6 };
			NTclosure_based(n, c0);
		END;
	END;

(* rc: o_par  2 *)
	IF P.ConstNotAddr (n) OR (n^.sz = 4) & P.ConstNotStackAddr (n)  THEN
		c0 :=0;
		IF (c0 < n.cost[NTrc]) THEN
			n.cost[NTrc] := VAL (INTEGER, c0);
			n.rule[NTrc] := BurgNT.Rule { 2 };
			NTclosure_rc(n, c0);
		END;
	END;
END newstate_o_par;

PROCEDURE newstate_o_retfun* (n: RD.DAGNODE); (* 65 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_retfun(o_call(mrc))  228 *)
	IF (l.op = ir.o_call (* 36 *))  
	THEN
		IF (n^.tr^.ResSize = l^.tr^.ResSize) &
                   (P.ProcResultInTOS(at.curr_procno) = P.CallResultInTOS (l^.tr)) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmrc]) + 11;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 228 };
			END;
		END;
	END;

(* stm: o_retfun(tos)  227 *)
	c0 := VAL (LONGINT, l.cost [NTtos]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 227 };
	END;

(* stm: o_retfun(mrc)  167 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + 11;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 167 };
	END;
END newstate_o_retfun;

PROCEDURE newstate_o_shift* (n: RD.DAGNODE); (* 66 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_shift(mem,o_par)  83 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & P.SameAlloc (n, l) & P.ConstNotAddr (n.r) THEN
			c0 := VAL (LONGINT, l.cost [NTmem]) + 31;
			IF (c0 < n.cost[NTlocal]) THEN
				n.cost[NTlocal] := VAL (INTEGER, c0);
				n.rule[NTlocal] := BurgNT.Rule { 83 };
				NTclosure_local(n, c0);
			END;
		END;
	END;

(* reg: o_shift(reg,mrc)  82 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 32;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 82 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_shift(reg,o_par)  26 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF (n^.tr^.Op = ir.o_shl) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.Const123 (n.r) & (n.tr.ResSize <= 4)  THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 21;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 26 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* scaled: o_shift(reg,o_par)  12 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF (n^.tr^.Op = ir.o_shl) &  (n^.tr^.ResType <> ir.t_float) & (n^.tr^.ResSize = 4) &  (n^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) & P.Const123 (n.r) &  (n^.tr^.Name >= Color.NNonTempVars) &
                   H.LiveReg (l, n^.tr^.Name) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + 10;
			IF (c0 < n.cost[NTscaled]) THEN
				n.cost[NTscaled] := VAL (INTEGER, c0);
				n.rule[NTscaled] := BurgNT.Rule { 12 };
				NTclosure_scaled(n, c0);
			END;
		END;
	END;
END newstate_o_shift;

PROCEDURE newstate_o_logical* (n: RD.DAGNODE); (* 67 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* reg: o_logical(o_loset(reg),reg)  126 *)
	IF (l.op = ir.o_loset (* 26 *))  
	THEN
		IF (n^.tr^.Op = ir.o_and) & (n^.tr^.ResSize = l^.tr^.ResSize)  THEN
			c0 := VAL (LONGINT, l.l.cost [NTreg]) + VAL (LONGINT, r.cost [NTreg]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 126 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* reg: o_logical(reg,o_loset(reg))  125 *)
	IF (r.op = ir.o_loset (* 26 *))  
	THEN
		IF (n^.tr^.Op = ir.o_and) & (n^.tr^.ResSize = n.r^.tr^.ResSize)  THEN
			c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.l.cost [NTreg]) + 11;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 125 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;

(* local: o_logical(rc,mem)  75 *)
	IF R.MemOps & P.SameAlloc (n, n.r)  THEN
		c0 := VAL (LONGINT, l.cost [NTrc]) + VAL (LONGINT, r.cost [NTmem]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 75 };
			NTclosure_local(n, c0);
		END;
	END;

(* local: o_logical(mem,rc)  74 *)
	IF R.MemOps & P.SameAlloc (n, l)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTrc]) + 31;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 74 };
			NTclosure_local(n, c0);
		END;
	END;

(* reg: o_logical(mrc,reg)  73 *)
	c0 := VAL (LONGINT, l.cost [NTmrc]) + VAL (LONGINT, r.cost [NTreg]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 73 };
		NTclosure_reg(n, c0);
	END;

(* reg: o_logical(reg,mrc)  72 *)
	c0 := VAL (LONGINT, l.cost [NTreg]) + VAL (LONGINT, r.cost [NTmrc]) + 22;
	IF (c0 < n.cost[NTreg]) THEN
		n.cost[NTreg] := VAL (INTEGER, c0);
		n.rule[NTreg] := BurgNT.Rule { 72 };
		NTclosure_reg(n, c0);
	END;

(* local: o_logical(mem,o_par)  70 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF R.MemOps & (n^.tr^.Op = ir.o_xor) &
                   P.IsAllOnes (n.r, n^.tr^.ResType) & P.SameAlloc (n, l) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTmem]) + 30;
			IF (c0 < n.cost[NTlocal]) THEN
				n.cost[NTlocal] := VAL (INTEGER, c0);
				n.rule[NTlocal] := BurgNT.Rule { 70 };
				NTclosure_local(n, c0);
			END;
		END;
	END;

(* reg: o_logical(mrc,o_par)  69 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF (n^.tr^.Op = ir.o_xor) & P.IsAllOnes (n.r, n^.tr^.ResType)  THEN
			c0 := VAL (LONGINT, l.cost [NTmrc]) + 21;
			IF (c0 < n.cost[NTreg]) THEN
				n.cost[NTreg] := VAL (INTEGER, c0);
				n.rule[NTreg] := BurgNT.Rule { 69 };
				NTclosure_reg(n, c0);
			END;
		END;
	END;
END newstate_o_logical;

PROCEDURE newstate_o_fbin* (n: RD.DAGNODE); (* 68 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* tos: o_fbin(o_par,tos)  183 *)
	IF (l.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstNotAddr (l) & ( (n^.tr^.ResSize <= 8) OR (P.SmallestSize (l) <= 8))  THEN
			c0 := VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTtos]) THEN
				n.cost[NTtos] := VAL (INTEGER, c0);
				n.rule[NTtos] := BurgNT.Rule { 183 };
				NTclosure_tos(n, c0);
			END;
		END;
	END;

(* tos: o_fbin(tos,o_par)  182 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstNotAddr (n.r) & ( (n^.tr^.ResSize <= 8) OR (P.SmallestSize (n.r) <= 8))  THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTtos]) THEN
				n.cost[NTtos] := VAL (INTEGER, c0);
				n.rule[NTtos] := BurgNT.Rule { 182 };
				NTclosure_tos(n, c0);
			END;
		END;
	END;

(* tos: o_fbin(o_val(mem),tos)  181 *)
	IF (l.op = ir.o_val (* 4 *))  
	THEN
		IF (l^.tr^.ResType = ir.t_float) &
                   (l^.tr^.OpType <> ir.t_float) &
                   ((l^.tr^.OpSize = 2)OR(l^.tr^.OpSize = 4)) &
                   (l^.tr^.OpType <> ir.t_unsign) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTtos]) THEN
				n.cost[NTtos] := VAL (INTEGER, c0);
				n.rule[NTtos] := BurgNT.Rule { 181 };
				NTclosure_tos(n, c0);
			END;
		END;
	END;

(* tos: o_fbin(tos,o_val(mem))  180 *)
	IF (r.op = ir.o_val (* 4 *))  
	THEN
		IF (n.r^.tr^.ResType = ir.t_float) &
                   (n.r^.tr^.OpType <> ir.t_float) &
                   ((n.r^.tr^.OpSize = 2)OR(n.r^.tr^.OpSize = 4)) &
                   (n.r^.tr^.OpType <> ir.t_unsign) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.l.cost [NTmem]) + 101;
			IF (c0 < n.cost[NTtos]) THEN
				n.cost[NTtos] := VAL (INTEGER, c0);
				n.rule[NTtos] := BurgNT.Rule { 180 };
				NTclosure_tos(n, c0);
			END;
		END;
	END;

(* tos: o_fbin(o_val(mem),tos)  179 *)
	IF (l.op = ir.o_val (* 4 *))  
	THEN
		IF (l^.tr^.ResType = ir.t_float) &
                   (l^.tr^.OpType = ir.t_float) &
                   (l^.tr^.OpSize <= 8) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTtos]) THEN
				n.cost[NTtos] := VAL (INTEGER, c0);
				n.rule[NTtos] := BurgNT.Rule { 179 };
				NTclosure_tos(n, c0);
			END;
		END;
	END;

(* tos: o_fbin(tos,o_val(mem))  178 *)
	IF (r.op = ir.o_val (* 4 *))  
	THEN
		IF (n.r^.tr^.ResType = ir.t_float) &
                   (n.r^.tr^.OpType = ir.t_float) &
                   (n.r^.tr^.OpSize <= 8) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.l.cost [NTmem]) + 101;
			IF (c0 < n.cost[NTtos]) THEN
				n.cost[NTtos] := VAL (INTEGER, c0);
				n.rule[NTtos] := BurgNT.Rule { 178 };
				NTclosure_tos(n, c0);
			END;
		END;
	END;

(* tos: o_fbin(mem,tos)  177 *)
	IF  (n^.tr^.ResSize <= 8)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 177 };
			NTclosure_tos(n, c0);
		END;
	END;

(* tos: o_fbin(tos,mem)  176 *)
	IF  (n^.tr^.ResSize <= 8)  THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.cost [NTmem]) + 101;
		IF (c0 < n.cost[NTtos]) THEN
			n.cost[NTtos] := VAL (INTEGER, c0);
			n.rule[NTtos] := BurgNT.Rule { 176 };
			NTclosure_tos(n, c0);
		END;
	END;

(* tos: o_fbin(tos,tos)  175 *)
	c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.cost [NTtos]) + 101;
	IF (c0 < n.cost[NTtos]) THEN
		n.cost[NTtos] := VAL (INTEGER, c0);
		n.rule[NTtos] := BurgNT.Rule { 175 };
		NTclosure_tos(n, c0);
	END;
END newstate_o_fbin;

PROCEDURE newstate_o_funary* (n: RD.DAGNODE); (* 69 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* local: o_funary(mem)  185 *)
	IF ((n^.tr^.Op = ir.o_abs) OR (n^.tr^.Op = ir.o_neg)) &
           P.SameAlloc (n, l) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 30;
		IF (c0 < n.cost[NTlocal]) THEN
			n.cost[NTlocal] := VAL (INTEGER, c0);
			n.rule[NTlocal] := BurgNT.Rule { 185 };
			NTclosure_local(n, c0);
		END;
	END;

(* tos: o_funary(tos)  184 *)
	c0 := VAL (LONGINT, l.cost [NTtos]) + 31;
	IF (c0 < n.cost[NTtos]) THEN
		n.cost[NTtos] := VAL (INTEGER, c0);
		n.rule[NTtos] := BurgNT.Rule { 184 };
		NTclosure_tos(n, c0);
	END;
END newstate_o_funary;

PROCEDURE newstate_o_fle* (n: RD.DAGNODE); (* 70 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_fle(mem,o_par)  226 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResSize <= 8) & P.IsRealZero (n^.tr^.ResSize, n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTmem]) + 91;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 226 };
			END;
		END;
	END;

(* stm: o_fle(o_par,tos)  216 *)
	IF (l.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstNotAddr (l) & ( (n^.tr^.ResSize <= 8) OR (P.SmallestSize (l) <= 8))  THEN
			c0 := VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 216 };
			END;
		END;
	END;

(* stm: o_fle(tos,o_par)  215 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstNotAddr (n.r) & ( (n^.tr^.ResSize <= 8) OR (P.SmallestSize (n.r) <= 8))  THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 215 };
			END;
		END;
	END;

(* stm: o_fle(o_val(mem),tos)  214 *)
	IF (l.op = ir.o_val (* 4 *))  
	THEN
		IF (l^.tr^.ResType = ir.t_float) &
                   (l^.tr^.OpType <> ir.t_float) &
                   ((l^.tr^.OpSize = 2)OR(l^.tr^.OpSize = 4)) &
                   (l^.tr^.OpType <> ir.t_unsign) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 214 };
			END;
		END;
	END;

(* stm: o_fle(tos,o_val(mem))  213 *)
	IF (r.op = ir.o_val (* 4 *))  
	THEN
		IF (n.r^.tr^.ResType = ir.t_float) &
                   (n.r^.tr^.OpType <> ir.t_float) &
                   ((n.r^.tr^.OpSize = 2)OR(n.r^.tr^.OpSize = 4)) &
                   (n.r^.tr^.OpType <> ir.t_unsign) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.l.cost [NTmem]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 213 };
			END;
		END;
	END;

(* stm: o_fle(o_val(mem),tos)  212 *)
	IF (l.op = ir.o_val (* 4 *))  
	THEN
		IF (l^.tr^.ResType = ir.t_float) &
                   (l^.tr^.OpType = ir.t_float) &
                   (l^.tr^.OpSize <= 8) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 212 };
			END;
		END;
	END;

(* stm: o_fle(tos,o_val(mem))  211 *)
	IF (r.op = ir.o_val (* 4 *))  
	THEN
		IF (n.r^.tr^.ResType = ir.t_float) &
                   (n.r^.tr^.OpType = ir.t_float) &
                   (n.r^.tr^.OpSize <= 8) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.l.cost [NTmem]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 211 };
			END;
		END;
	END;

(* stm: o_fle(mem,tos)  210 *)
	IF  (n^.tr^.ResSize <= 8)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 210 };
		END;
	END;

(* stm: o_fle(tos,mem)  209 *)
	IF  (n^.tr^.ResSize <= 8)  THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.cost [NTmem]) + 101;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 209 };
		END;
	END;

(* stm: o_fle(tos,tos)  208 *)
	c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.cost [NTtos]) + 101;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 208 };
	END;
END newstate_o_fle;

PROCEDURE newstate_o_feq* (n: RD.DAGNODE); (* 71 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* stm: o_feq(mem,o_par)  225 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF  (n^.tr^.ResSize <= 8) & P.IsRealZero (n^.tr^.ResSize, n.r)  THEN
			c0 := VAL (LONGINT, l.cost [NTmem]) + 91;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 225 };
			END;
		END;
	END;

(* stm: o_feq(tos,o_par)  224 *)
	IF (r.op = ir.o_par (* 64 *))  
	THEN
		IF P.ConstNotAddr (n.r) & ( (n^.tr^.ResSize <= 8) OR (P.SmallestSize (n.r) <= 8))  THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 224 };
			END;
		END;
	END;

(* stm: o_feq(o_val(mem),tos)  223 *)
	IF (l.op = ir.o_val (* 4 *))  
	THEN
		IF (l^.tr^.ResType = ir.t_float) &
                   (l^.tr^.OpType <> ir.t_float) &
                   ((l^.tr^.OpSize = 2)OR(l^.tr^.OpSize = 4)) &
                   (l^.tr^.OpType <> ir.t_unsign) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 223 };
			END;
		END;
	END;

(* stm: o_feq(tos,o_val(mem))  222 *)
	IF (r.op = ir.o_val (* 4 *))  
	THEN
		IF (n.r^.tr^.ResType = ir.t_float) &
                   (n.r^.tr^.OpType <> ir.t_float) &
                   ((n.r^.tr^.OpSize = 2)OR(n.r^.tr^.OpSize = 4)) &
                   (n.r^.tr^.OpType <> ir.t_unsign) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.l.cost [NTmem]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 222 };
			END;
		END;
	END;

(* stm: o_feq(o_val(mem),tos)  221 *)
	IF (l.op = ir.o_val (* 4 *))  
	THEN
		IF (l^.tr^.ResType = ir.t_float) &
                   (l^.tr^.OpType = ir.t_float) &
                   (l^.tr^.OpSize <= 8) 
		THEN
			c0 := VAL (LONGINT, l.l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 221 };
			END;
		END;
	END;

(* stm: o_feq(tos,o_val(mem))  220 *)
	IF (r.op = ir.o_val (* 4 *))  
	THEN
		IF (n.r^.tr^.ResType = ir.t_float) &
                   (n.r^.tr^.OpType = ir.t_float) &
                   (n.r^.tr^.OpSize <= 8) 
		THEN
			c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.l.cost [NTmem]) + 101;
			IF (c0 < n.cost[NTstm]) THEN
				n.cost[NTstm] := VAL (INTEGER, c0);
				n.rule[NTstm] := BurgNT.Rule { 220 };
			END;
		END;
	END;

(* stm: o_feq(mem,tos)  219 *)
	IF  (n^.tr^.ResSize <= 8)  THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + VAL (LONGINT, r.cost [NTtos]) + 101;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 219 };
		END;
	END;

(* stm: o_feq(tos,mem)  218 *)
	IF  (n^.tr^.ResSize <= 8)  THEN
		c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.cost [NTmem]) + 101;
		IF (c0 < n.cost[NTstm]) THEN
			n.cost[NTstm] := VAL (INTEGER, c0);
			n.rule[NTstm] := BurgNT.Rule { 218 };
		END;
	END;

(* stm: o_feq(tos,tos)  217 *)
	c0 := VAL (LONGINT, l.cost [NTtos]) + VAL (LONGINT, r.cost [NTtos]) + 101;
	IF (c0 < n.cost[NTstm]) THEN
		n.cost[NTstm] := VAL (INTEGER, c0);
		n.rule[NTstm] := BurgNT.Rule { 217 };
	END;
END newstate_o_feq;

PROCEDURE newstate_o_hiword* (n: RD.DAGNODE); (* 84 *)
VAR     c0:   LONGINT;
        p:    ir.TriadePtr;
        l, r: RD.DAGNODE;
BEGIN
	n.cost := MaxCost;
	l := n.l;
	r := n.r;
	p := n.tr;

(* mem: o_hiword(mem)  108 *)
	IF  (n^.tr^.Name >= Color.NNonTempVars) & H.NobodyWrites (n) THEN
		c0 := VAL (LONGINT, l.cost [NTmem]) + 0;
		IF (c0 < n.cost[NTmem]) THEN
			n.cost[NTmem] := VAL (INTEGER, c0);
			n.rule[NTmem] := BurgNT.Rule { 108 };
			NTclosure_mem(n, c0);
		END;
	END;

(* reg: o_hiword(mrc)  107 *)
	IF (n^.tr^.ResType # ir.t_float) &
           (n^.tr^.OpType # ir.t_float) 
	THEN
		c0 := VAL (LONGINT, l.cost [NTmrc]) + 31;
		IF (c0 < n.cost[NTreg]) THEN
			n.cost[NTreg] := VAL (INTEGER, c0);
			n.rule[NTreg] := BurgNT.Rule { 107 };
			NTclosure_reg(n, c0);
		END;
	END;
END newstate_o_hiword;
TYPE NewstatesType = ARRAY BurgNT.OpRange OF newstate_proc;
CONST newstates *= NewstatesType {
	 NIL
	,NIL
	,newstate_o_assign
	,newstate_o_copy
	,newstate_o_val
	,newstate_o_cast
	,newstate_o_cap
	,NIL
	,NIL
	,NIL
	,NIL
	,newstate_o_add
	,newstate_o_mul
	,newstate_o_mulh
	,newstate_o_div
	,newstate_o_dvd
	,newstate_o_mod
	,newstate_o_rem
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,newstate_o_not
	,newstate_o_incl
	,newstate_o_excl
	,newstate_o_loset
	,newstate_o_hiset
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,newstate_o_sgnext
	,NIL
	,NIL
	,newstate_o_call
	,newstate_o_ret
	,newstate_o_checknil
	,newstate_o_checklo
	,newstate_o_checkhi
	,newstate_o_stop
	,newstate_o_error
	,NIL
	,NIL
	,newstate_o_loadr
	,newstate_o_storer
	,NIL
	,NIL
	,newstate_o_eq
	,newstate_o_le
	,NIL
	,newstate_o_in
	,NIL
	,newstate_o_case
	,newstate_o_goto
	,newstate_o_getpar
	,newstate_o_move_eq
	,newstate_o_move_le
	,newstate_o_alloca
	,newstate_o_neg
	,newstate_o_sub
	,newstate_o_putpar
	,newstate_o_comma
	,newstate_o_par
	,newstate_o_retfun
	,newstate_o_shift
	,newstate_o_logical
	,newstate_o_fbin
	,newstate_o_funary
	,newstate_o_fle
	,newstate_o_feq
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,NIL
	,newstate_o_hiword
};


END Burg.
