%{
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
%}

%nonterm stm
%nonterm reg
%nonterm rc
%nonterm mrc
%nonterm mem
%nonterm based
%nonterm scaled
%nonterm addr
%nonterm local
%nonterm tos
%nonterm const
%nonterm imem

//                    1 - FI
%term   o_assign   =  2
// ert
%term   o_copy     =  3
%term   o_val      =  4
%term   o_cast     =  5
%term   o_cap      =  6
//                    7..10 - ABS, COMPLEX, RE, IM
%term   o_add      = 11
%term   o_mul      = 12
%term   o_mulh     = 13
%term   o_div      = 14
%term   o_dvd      = 15
%term   o_mod      = 16
%term   o_rem      = 17
//                   18 - POWER, 19..22 - AND, OR, ANDNOT, XOR
%term   o_not      = 23
%term   o_incl     = 24
%term   o_excl     = 25
%term   o_loset    = 26
%term   o_hiset    = 27
//                   28..32 - SHL, SHR, SAR, ROL, ROR
%term   o_sgnext   = 33
//                   34 - BF_GET, 35 - BF_PUT
%term   o_call     = 36
%term   o_ret      = 37
%term   o_checknil = 38
%term   o_checklo  = 39
%term   o_checkhi  = 40
%term   o_stop     = 41
%term   o_error    = 42
//                   43..44 - LOAD, STORE
%term   o_loadr    = 45
%term   o_storer   = 46
//                   47 - FORSTART, 48 - FORCONT
%term   o_eq       = 49
%term   o_le       = 50
//                   51 - LESET
%term   o_in       = 52
//                   53 - ODD
%term   o_case     = 54
%term   o_goto     = 55
%term   o_getpar   = 56
%term   o_move_eq  = 57
%term   o_move_le  = 58
%term   o_alloca   = 59

%term   o_neg      = 60
%term   o_sub      = 61
%term   o_putpar   = 62
%term   o_comma    = 63
%term   o_par      = 64
%term   o_retfun   = 65
%term   o_shift    = 66
%term   o_logical  = 67
%term   o_fbin     = 68
%term   o_funary   = 69
%term   o_fle      = 70
%term   o_feq      = 71

%term   o_hiword   = 84


/-------------------------------------------------------------------------------

%define  8 (@^.tr^.ResSize <= 8)
%define 14 (NOT (P.IsAggregateByValue (@^.tr)))
%define 15 (@^.tr^.Name >= Color.NNonTempVars)
%define 16 (@^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{})
%define 17 (@^.tr^.ResType <> ir.t_float) & (@^.tr^.ResSize = 4)
%define 18 (@4^.tr^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{})
%define 19 (~RD.Loc[@^.tr^.Name].hasLongLifeTime)

/-------------------------------------------------------------------------------

%start  stm                     // Стартовый нетерминал грамматики
%cost   1                       // Длина вектора стоимости выбора правила

%%

/-------------------------------------------------------------------------------


rc:     reg                                                             // 1
        = (0)
        { @^.nt := %reg; }:
        { };

rc:     o_par                                                           // 2
        { P.ConstNotAddr (@) OR (@^.sz = 4) & P.ConstNotStackAddr (@) }
        = (0)
        { @^.nt := %const; }:
        { };

mrc:    rc                                                              // 3
        = (0)
        { }:
        { };

mrc:    mem                                                             // 4
        = (10)
        { @^.nt := %mem; }:
        { };

based:  reg
        {P.NotWhole8Node(@) (*& P.NotDeprecCompoundVar(@)*)}                                                       // 5
        = (0)
        { R.RegToBased (@); }:
        { };

based:  o_par                                                           // 6
        { P.BasedVar (@) }
        = (0)
        { @.a := RD.Trees^[@.par^.name]^.a; @^.nt := %based; }:
        { };

based:  o_par                                                           // 7
        { P.ConstStackAddr (@) }
        = (0)
        { R.StackAddrToBased (@^.a, @^.par); @^.nt := %based; }:
        { };

based:  o_add (based<1>, o_par<2>)                                      // 8
        { $17 & $16 & P.ConstNotStackAddr (@2) & $15 &
          (@1^.cost [%based] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %based, @^.tr^.Name) }
        = (0)
        { R.AddAddrParToAddr (@, @1, @2); @^.nt := %based; }:
        { };

based:  o_sub (based<1>, o_par<2>)                                      // 9
        { $17 & $16 & P.ConstNotAddr (@2) & $15 &
         (@1^.cost [%based] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %based, @^.tr^.Name) }
        = (0)
        { R.SubAddrParToAddr (@, @1, @2); @^.nt := %based; }:
        { };

scaled: o_par                                                           // 10
        { P.ScaledVar (@) }
        = (0)
        { @.a := RD.Trees^[@.par^.name]^.a; @^.nt := %scaled; }:
        { };

scaled: o_mul (reg<1>, o_par<2>)                                        // 11
        { $17 & $16 & P.Const248 (@2) & $15 &
          H.LiveReg (@1, @^.tr^.Name) }
        = (10)
        { R.MultToScaled (@, @1, @2); }:
        { };

scaled: o_shift (reg<1>, o_par<2>)                                      // 12
        { (@^.tr^.Op = ir.o_shl) & $17 & $16 & P.Const123 (@2) & $15 &
          H.LiveReg (@1, @^.tr^.Name) }
        = (10)
        { R.ShiftToScaled (@, @1, @2); }:
        { };

scaled: o_add (scaled<1>, o_par<2>)                                     // 13
        { $17 & $16 & P.ConstNotStackAddr (@2) & $15 &
          (@1^.cost [%scaled] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %scaled, @^.tr^.Name) }
        = (0)
        { R.AddAddrParToAddr (@, @1, @2); @^.nt := %scaled; }:
        { };

scaled: o_sub (scaled<1>, o_par<2>)                                     // 14
        { $17 & $16 & P.ConstNotAddr (@2) & $15 &
          (@1^.cost [%scaled] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %scaled, @^.tr^.Name) }
        = (0)
        { R.SubAddrParToAddr (@, @1, @2); @^.nt := %scaled; }:
        { };

addr:   based                                                           // 15
        = (0)
        { @^.nt := %addr; }:
        { };

addr:   o_add (based<1>, based<2>)                                      // 16
        { $17 & $16 & $15 &
          (@1^.cost [%based] < MAX (INTEGER)) &
          (@2^.cost [%based] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %based, @^.tr^.Name) &
          H.LiveAddrMode (@2, %based, @^.tr^.Name) }
        = (10)
        { R.AddAddrAddrToAddr (@, @1, @2, @1^.a.place1, @2^.a.place1, D.x1); }:
        { };

addr:   scaled                                                          // 17
        = (0)
        { @^.nt := %addr; }:
        { };

addr:   o_add (based<1>, scaled<2>)                                     // 18
        { $17 & $16 & $15 &
         (@1^.cost [%based] < MAX (INTEGER)) &
         (@2^.cost [%scaled] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %based,  @^.tr^.Name) &
          H.LiveAddrMode (@2, %scaled, @^.tr^.Name) }
        = (0)
        { R.AddAddrAddrToAddr (@, @1, @2, @1^.a.place1, @2^.a.place2, @2^.a.scale ); }:
        { };

addr:   o_add (scaled<2>, based<1>)                                     // 19
        { $17 & $16 & $15 &
          (@1^.cost [%based] < MAX (INTEGER)) &
         (@2^.cost [%scaled] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %based, @^.tr^.Name) &
          H.LiveAddrMode (@2, %scaled, @^.tr^.Name) }
        = (0)
        { R.AddAddrAddrToAddr (@, @1, @2, @1^.a.place1, @2^.a.place2, @2^.a.scale ); }:
        { };

addr:   o_par                                                           // 20
        { P.AddrVar(@) }
        = (0)
        { @.a := RD.Trees^[@.par^.name]^.a; @^.nt := %addr; }:
        { };

addr:   o_par                                                           // 21
        { P.ConstNotStackAddr (@) }
        = (0)
        { R.ParToAddr (@^.a, @^.par); @^.nt := %addr; }:
        { };

addr:   o_add (addr<1>, o_par<2>)                                       // 22
        { $17 & $16 & P.ConstNotStackAddr (@2) & $15 &
          (@1^.cost [%addr] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %addr, @^.tr^.Name)}
        = (0)
        { R.AddAddrParToAddr (@, @1, @2); @^.nt := %addr; }:
        { };

addr:   o_sub (addr<1>, o_par<2>)                                       // 23
        { $17 & $16 & P.ConstNotAddr (@2) & $15 &
          (@1^.cost [%addr] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %addr, @^.tr^.Name) }
        = (0)
        { R.SubAddrParToAddr (@, @1, @2); @^.nt := %addr; }:
        { };

addr:   o_mul (reg<1>, o_par<2>)                                        // 24
        { $17 & $16 & P.Const359 (@2) & $15 &
          H.LiveReg (@1, @^.tr^.Name) }
        = (10)
        { R.MultToAddr (@, @1, @2); }:
        { };

/-------------------------------------------------------------------------------

reg:    o_mul (reg<1>, o_par<2>)                                        // 25
        { $16 & P.Const248 (@2) & (@.tr.ResSize <= 4) }
        = (21)
        { R.MultToScaled (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    o_shift (reg<1>, o_par<2>)                                      // 26
        { (@^.tr^.Op = ir.o_shl) & $16 & P.Const123 (@2) & (@.tr.ResSize <= 4) }
        = (21)
        { R.ShiftToScaled (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    o_mul (reg<1>, o_par<2>)                                        // 27
        { $16 & P.Const359 (@2) & (@.tr.ResSize <= 4)}
        = (21)
        { R.MultToAddr (@, @1, @2); R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    based                                                  // 28
        { P.NotWhole8Node(@) }
        = (11)
        { R.TryAddPosition (@); R.LEA (@, 4); }:
        { };

reg:    o_add (based<1>, o_par<2>)                                      // 29
        { $16 & P.ConstNotStackAddr (@2)  }
        = (11)
        { R.AddAddrParToAddr (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    o_sub (based<1>, o_par<2>)                                      // 30
        { $16 & P.ConstNotAddr (@2)  }
        = (11)
        { R.SubAddrParToAddr (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize) }:
        { };

reg:    scaled
        {P.NotWhole8Node(@) }                              // 31
        = (11)
        { R.TryAddPosition (@); R.LEA (@, 4); }:
        { };

reg:    o_add (scaled<1>, o_par<2>)                                     // 32
        { $16 & P.ConstNotStackAddr (@2)  }
        = (11)
        { R.AddAddrParToAddr (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    o_sub (scaled<1>, o_par<2>)                                     // 33
        { $16 & P.ConstNotAddr (@2)  }
        = (11)
        { R.SubAddrParToAddr (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize) }:
        { };

reg:    addr                                                                // 34
        { P.NotWhole8Node(@) }
        = (11)
        { R.TryAddPosition (@); R.LEA (@, 4); }:
        { };

reg:    o_add (addr<1>, o_par<2>)                                       // 35
        { $16 & P.ConstNotStackAddr (@2)  }
        = (11)
        { R.AddAddrParToAddr (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    o_sub (addr<1>, o_par<2>)                                       // 36
        { $16 & P.ConstNotAddr (@2) & (@.tr.ResSize <= 4) }
        = (11)
        { R.SubAddrParToAddr (@, @1, @2);
          R.LEA (@, @^.tr^.ResSize) }:
        { };

reg:    o_add (based<1>, based<2>)                                      // 37
        { $16  }
        = (21)
        { R.AddAddrAddrToAddr (@, @1, @2, @1^.a.place1, @2^.a.place1, D.x1);
          R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    o_add (based<1>, scaled<2>)                                     // 38
        { $16  }
        = (11)
        { R.AddAddrAddrToAddr (@, @1, @2, @1^.a.place1, @2^.a.place2, @2^.a.scale );
          R.LEA (@, @^.tr^.ResSize); }:
        { };

reg:    o_add (scaled<2>, based<1>)                                     // 39
        { $16  }
        = (11)
        { R.AddAddrAddrToAddr (@, @1, @2, @1^.a.place1, @2^.a.place2, @2^.a.scale );
          R.LEA (@, @^.tr^.ResSize); }:
        { };

/-------------------------------------------------------------------------------

reg:    o_loadr (addr<1>)                                                  // 40
        { (@^.tr^.ResType # ir.t_float )&(@^.tr^.ResType # ir.t_arr ) & (@^.tr^.ResSize <= 8) }
        = (11)
        { R.LoadrToReg (@, @1, @^.tr^.ResSize); }:
        { };

reg:    o_par                                                           // 41
        { P.RegVar (@)  }
        = (0)
        { @^.place.r := RD.Loc^[@^.par.name].reg;
          @^.place.v := @^.par.name;
          @^.nt := %reg; }:
        { };

reg:    o_par                                                           // 42
        { P.ConstNotVar (@)  }
        = (11)
        { R.TryAddPosition (@); R.ImmToReg (@, @^.sz); }:
        { };

reg:    mem                                                             // 43
        { (@^.sz <= 4)OR(P.Whole8Node(@)) }
        = (11)
        { R.TryAddPosition (@);
          R.MemToReg (@, @^.sz); }:
        { };

mem:    o_loadr (addr<1>)                                               // 44
        { (~$15& P.SameMemLoadr(@, @1) OR
          ( $15& H.NobodyWrites (@) &
          (@1^.cost [%addr] < MAX (INTEGER)) &
          H.LiveAddrMode (@1, %addr, @^.tr^.Name)) )}
        = (0)
        { @^.a := @1^.a; @^.a.bv := @^.tr^.Read; @^.nt := %mem; }:
        { };

mem:    o_par                                                           // 45
        { P.MemVar (@) }
        = (0)
        { @^.a := RD.Trees^[@^.par.name]^.a; @^.nt := %mem; }:
        { };

local:  o_loadr (addr<1>)                                               // 46
        { P.SameMemLoadr (@, @1) }
        = (0)
        { @^.nt := %local; }:
        { };

local:  reg                                                             // 47
        { P.InLocal (@) }
        = (11)
        { R.TryAddPosition (@);
          R.RegToLocal (@, @^.tr^.ResSize); @^.nt := %local; }:
        { };

mem:    o_par                                                           // 48
        { P.LocalVar (@) }
        = (0)
        { R.FormMem (@^.a, @^.par^.name); @^.nt := %mem; }:
        { };

/-------------------------------------------------------------------------------

reg:    o_getpar                                                        // 49
        { @^.tr^.ResSize <= 8 }
        = (11)
        { R.GetParToReg (@); }:
        { };

local:  o_getpar                                                        // 50
        { R.GetParInLocal (@^.tr) }
        = (0)
        { @^.nt := %local; }:
        { };

/-------------------------------------------------------------------------------

reg:    o_add (reg<1>, mrc<2>)                                          // 51
        = (22)
        { R.BinOp (@, @1, @2); }:
        { };

reg:    o_add (mrc<2>, reg<1>)                                          // 52
        = (22)
        { R.BinOp (@, @1, @2); }:
        { };

reg:    o_add (mrc<1>, o_par<2>)                                        // 53
        { $16 & P.IsOne (@2) & P.NotWhole8Node(@) }                                       // INC R
        = (21)
        { R.UnOp (@, @1); }:
        { };

local:  o_add (mem<1>, rc<2>)                                           // 54
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.BinOpM (@1, @2, @^.tr); @^.nt := %local; }:
        { };

local:  o_add (rc<2>, mem<1>)                                           // 55
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.BinOpM (@1, @2, @^.tr); @^.nt := %local; }:
        { };

local:  o_add (mem<1>, o_par<2>)                                        // 56
        { R.MemOps & P.SameAlloc (@, @1) & $16 & P.IsOne (@2) & P.NotWhole8Node(@) }                 // INC M
        = (30)
        { R.UnOpM (@1, @^.tr); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_add<4> (mem<2>, rc<3>))                    // 57
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.BinOpM (@1, @3, @4^.tr); }:
        { };

stm:    o_storer (addr<1>, o_add<4> (rc<3>, mem<2>))                    // 58
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.BinOpM (@1, @3, @4^.tr); }:
        { };

stm:    o_storer (addr<1>, o_add<4> (mem<2>, o_par<3>))                 // 59
        { R.MemOps & P.SameAddrs (@, @2) & $18 & P.IsOne (@3) }                 // INC M
        = (30)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.UnOpM (@1, @4^.tr); }:
        { };

reg:    o_sub (reg<1>, mrc<2>)                                          // 60
        = (22)
        { R.BinOp (@, @1, @2); }:
        { };

reg:    o_sub (mrc<1>, o_par<2>)                                        // 61
        { $16 & P.IsOne (@2) & P.NotWhole8Node(@) }                                       // DEC R
        = (21)
        { R.UnOp (@, @1); }:
        { };

local:  o_sub (mem<1>, rc<2>)                                           // 62
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.BinOpM (@1, @2, @^.tr); @^.nt := %local; }:
        { };

local:  o_sub (mem<1>, o_par<2>)                                        // 63
        { R.MemOps & P.SameAlloc (@, @1) & $16 & P.IsOne (@2) }                 // DEC M
        = (30)
        { R.UnOpM (@1, @^.tr); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_sub<4> (mem<2>, rc<3>))                    // 64
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.BinOpM (@1, @3, @4^.tr); }:
        { };

stm:    o_storer (addr<1>, o_sub<4> (mem<2>, o_par<3>))                 // 65
        { R.MemOps & P.SameAddrs (@, @2) & $18 & P.IsOne ( @3) }                 // DEC M
        = (30)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.UnOpM (@1, @4^.tr); }:
        { };

reg:    o_neg (mrc<1>)                                                  // 66
        = (22)
        { R.UnOp (@, @1); }:
        { };

local:  o_neg (mem<1>)                                                  // 67
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.UnOpM (@1, @^.tr); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_neg<3> (mem<2>))                           // 68
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@3^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.UnOpM (@1, @3^.tr); }:
        { };

reg:    o_logical (mrc<1>, o_par<2>)                                    // 69
        { (@^.tr^.Op = ir.o_xor) & P.IsAllOnes (@2, @^.tr^.ResType)  }
        = (21)
        { R.UnOp (@, @1); }:
        { };

local:  o_logical (mem<1>, o_par<2>)                                    // 70
        { R.MemOps & (@^.tr^.Op = ir.o_xor) &
          P.IsAllOnes (@2, @^.tr^.ResType) & P.SameAlloc (@, @1) }
        = (30)
        { R.UnOpM (@1, @^.tr); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_logical<3> (mem<2>, o_par<4>))             // 71
        { R.MemOps & (@3^.tr^.Op = ir.o_xor) &
          P.IsAllOnes (@4, @3^.tr^.ResType) & P.SameAddrs (@, @2) }
        = (30)
        { Emit.TryAddPosition (@3^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.UnOpM (@1, @3^.tr); }:
        { };

reg:    o_logical (reg<1>, mrc<2>)                                      // 72
        = (22)
        { R.BinOp (@, @1, @2); }:
        { };

reg:    o_logical (mrc<2>, reg<1>)                                      // 73
        = (22)
        { R.BinOp (@, @1, @2); }:
        { };

local:  o_logical (mem<1>, rc<2>)                                       // 74
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.BinOpM (@1, @2, @^.tr); @^.nt := %local; }:
        { };

local:  o_logical (rc<2>, mem<1>)                                       // 75
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.BinOpM (@1, @2, @^.tr); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_logical<4> (mem<2>, rc<3>))                // 76
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.BinOpM (@1, @3, @4^.tr); }:
        { };

stm:    o_storer (addr<1>, o_logical<4> (rc<3>, mem<2>))                // 77
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.BinOpM (@1, @3, @4^.tr); }:
        { };

reg:    o_not (mrc<1>)                                                  // 78
        = (22)
        { R.UnOp (@, @1); }:
        { };

reg:    o_cap (mrc<1>)                                                  // 79
        = (22)
        { R.UnOp (@, @1); }:
        { };

local:  o_not (mem<1>)                                                  // 80
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.UnOpM (@1, @^.tr); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_not<3> (mem<2>))                           // 81
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@3^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.UnOpM (@1, @3^.tr); }:
        { };

reg:    o_shift (reg<1>, mrc<2>)                                        // 82
        = (32)
        { R.ShiftOp (@, @1, @2); }:
        { reg.TryAssignReg (@2, D.ECX); };

local:  o_shift (mem<1>, o_par<2>)                                      // 83
        { R.MemOps & P.SameAlloc (@, @1) & P.ConstNotAddr (@2)}
        = (31)
        { R.ShiftOpM (@, @1, @2); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_shift<4> (mem<2>, o_par<3>))               // 84
        { R.MemOps & P.SameAddrs (@, @2) & P.ConstNotAddr (@3) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.ShiftOpM (@4, @1, @3); }:
        { };

reg:    o_mul (reg<1>, mrc<2>)                                          // 85
        { (@^.tr^.ResSize <> 1) & ((@^.tr^.ResType = ir.t_int) OR $16)  }
        = (311)
        { R.BinOp (@, @1, @2); }:
        { };

reg:    o_mul (mrc<2>, reg<1>)                                          // 86
        { (@^.tr^.ResSize <> 1) & ((@^.tr^.ResType = ir.t_int) OR $16)  }
        = (311)
        { R.BinOp (@, @1, @2); }:
        { };

reg:    o_mul (rc<1>, mrc<2>)                                           // 87
        { (@^.tr^.ResSize = 1) OR (@^.tr^.ResType <> ir.t_int)  }
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EAXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

reg:    o_mul (mrc<2>, rc<1>)                                           // 88
        { (@^.tr^.ResSize = 1) OR (@^.tr^.ResType <> ir.t_int)  }
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EAXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

local:  o_mul (rc<1>, mrc<2>)                                           // 89
        { (@^.tr^.ResSize = 1) OR (@^.tr^.ResType <> ir.t_int) }
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EAXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

local:  o_mul (mrc<2>, rc<1>)                                           // 90
        { (@^.tr^.ResSize = 1) OR (@^.tr^.ResType <> ir.t_int) }
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EAXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

reg:    o_mulh (rc<1>, mrc<2>)                                          // 91
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EDXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

reg:    o_mulh (mrc<2>, rc<1>)                                          // 92
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EDXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

local:  o_mulh (rc<1>, mrc<2>)                                          // 93
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EDXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

local:  o_mulh (mrc<2>, rc<1>)                                          // 94
        = (401)
        { R.Mul (@, @1, @2, D.RegTable[D.EDXp,@^.tr^.ResSize]); }:
        { reg.TryAssignReg (@1, D.RegTable[D.EAXp,@1.sz]);
          reg.TryAssignReg (@2, D.RegTable[D.EAXp,@2.sz]); };

reg:    o_mul (mem<1>, o_par<2>)                                        // 95
        { (@^.tr^.ResSize <> 1) &
          ((@^.tr^.ResType = ir.t_int) OR $16) &
          (@2^.op = ir.o_par) & (@2^.par^.tag = ir.y_NumConst)  }
        = (301)
        { R.IMulByConst (@, @1, @2); }:
        { };

reg:    o_mul (reg<1>, o_par<2>)                                        // 96
        { (@^.tr^.ResSize <> 1) &
          ((@^.tr^.ResType = ir.t_int) OR $16) &
          (@2^.op = ir.o_par) & (@2^.par^.tag = ir.y_NumConst)  }
        = (301)
        { R.IMulByConst (@, @1, @2); }:
        { };

reg:    o_div (rc<1>, mrc<2>)                                           // 97
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

local:  o_div (rc<1>, mrc<2>)                                           // 98
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

reg:    o_dvd (rc<1>, mrc<2>)                                           // 99
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

local:  o_dvd (rc<1>, mrc<2>)                                           // 100
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

reg:    o_rem (rc<1>, mrc<2>)                                           // 101
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

local:  o_rem (rc<1>, mrc<2>)                                           // 102
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

reg:    o_mod (rc<1>, mrc<2>)                                           // 103
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

local:  o_mod (rc<1>, mrc<2>)                                           // 104
        = (401)
        { R.Div (@, @1, @2); }:
        { reg.TryAssignReg (@1, D.EAX); };

reg:    o_sgnext (mrc<1>)                                               // 105
        = (32)
        { R.UnOp (@, @1); }:
        { };

reg:    o_val (mrc<1>)                                                  // 106
        { (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType  <> ir.t_float) }
        = (31)
        { R.UnOp (@, @1); }:
        { };

reg:    o_hiword (mrc<1>)                                               // 107
        { (@^.tr^.ResType # ir.t_float) &
          (@^.tr^.OpType  # ir.t_float) }
        = (31)
        { R.UnOp (@, @1);}:
        { };

mem:    o_hiword (mem<1>)                                               // 108
        { $15 & H.NobodyWrites (@)}
        = (0)
        { R.MemCast (@, @1);}:
        { };

mem:    o_cast (mem<1>)                                               // 109
        { $15 & H.NobodyWrites (@) &
          (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType  <> ir.t_float) &
          (@.tr.ResSize <= @.tr.OpSize)}
        = (0)
        { R.MemCast (@, @1);}:
        { };

mem:    o_val (mem<1>)                                               // 110
        { $15 & H.NobodyWrites (@) &
          (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType  <> ir.t_float) &
          (@.tr.ResSize <= @.tr.OpSize)}
        = (0)
        { R.MemCast (@, @1);}:
        { };

reg:    o_cast (mrc<1>)                                                 // 111
        { (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType  <> ir.t_float) }
        = (31)
        { R.UnOp (@, @1); }:
        { };

reg:    o_call (mrc<1>)                                                 // 112
        { @^.tr^.ResType <> ir.t_float }
        = (22)
        { R.CallProcFunc (@, @1, %reg); }:
        { };

local:  o_call (mrc<1>)                                                 // 113
        = (22)
        { R.CallProcFunc (@, @1, %local); }:
        { };

reg:    o_alloca (mrc<1>)                                               // 114
        = (22)
        { R.AdjustSP; R.Alloca (@, @1); }:
        { };

local:  o_alloca (mrc<1>)                                               // 115
        = (22)
        { R.AdjustSP; R.Alloca (@, @1); }:
        { };

reg:    o_incl (reg<1>, reg<2>)                                         // 116
        = (22)
        { R.InclExcl (@, @1, @2); }:
        { };

local:  o_incl (mem<1>, reg<2>)                                         // 117
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.InclExcl (@, @1, @2); }:
        { };

stm:    o_storer (addr<1>, o_incl<4> (mem<2>, reg<3>))                  // 118
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.InclExcl (@, @2, @3); }:
        { };

reg:    o_incl (o_par<1>, reg<2>)                                       // 119
        { P.IsZero (@^.tr^.ResSize, @1) }
        = (11)
        { R.MoveInclExcl (@, @2); }:
        { };

reg:    o_excl (reg<1>, reg<2>)                                         // 120
        = (22)
        { R.InclExcl (@, @1, @2); }:
        { };

local:  o_excl (mem<1>, reg<2>)                                         // 121
        { R.MemOps & P.SameAlloc (@, @1) }
        = (31)
        { R.InclExcl (@, @1, @2); }:
        { };

stm:    o_storer (addr<1>, o_excl<4> (mem<2>, reg<3>))                  // 122
        { R.MemOps & P.SameAddrs (@, @2) }
        = (31)
        { Emit.TryAddPosition (@4^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.InclExcl (@, @2, @3); }:
        { };

reg:    o_excl (o_par<1>, reg<2>)                                       // 123
        { P.IsAllOnes (@1, @^.tr^.ResType) }
        = (11)
        { R.MoveInclExcl (@, @2); }:
        { };

reg:    o_loset (reg<1>)                                                // 124
        = (11)
        { R.LoHiSet (@, @1); }:
        { };

reg:    o_logical (reg<1>, o_loset<3> (reg<2>))                         // 125
        { (@^.tr^.Op = ir.o_and) & (@^.tr^.ResSize = @3^.tr^.ResSize) }
        = (11)
        { R.InclExcl (@, @1, @2); }:
        { };

reg:    o_logical (o_loset<3> (reg<2>), reg<1>)                         // 126
        { (@^.tr^.Op = ir.o_and) & (@^.tr^.ResSize = @3^.tr^.ResSize) }
        = (11)
        { R.InclExcl (@, @1, @2); }:
        { };

reg:    o_hiset (reg<1>)                                                // 127
        = (11)
        { R.LoHiSet (@, @1); }:
        { };

/-------------------------------------------------------------------------------

stm:    o_storer (addr<1>, mem<2>)                                      // 128
        { P.SameMemStorer (@1, @2) }
        = (0)
        { }:
        { };

stm:    o_storer (addr<1>, rc<2>)                                       // 129
        { (@^.tr^.ResSize <= 8)OR(@^.tr^.ResType = ir.t_int) }
        = (11)
        { @1^.a.bv := @^.tr^.Write; R.RCToMem (@1, @2, @^.tr^.ResSize); }:
        { };

stm:    o_checklo (reg<1>, mrc<2>)                                      // 130
        = (11)
        { R.CompareR_MRC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JGE, D.JAE); }:
        { };

stm:    o_checklo (mrc<2>, reg<1>)                                      // 131
        = (11)
        { R.CompareR_MRC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JLE, D.JBE); }:
        { };

stm:    o_checklo (mem<1>, rc<2>)                                       // 132
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JGE, D.JAE); }:
        { };

stm:    o_checklo (rc<2>, mem<1>)                                       // 133
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JLE, D.JBE); }:
        { };

stm:    o_checkhi (reg<1>, mrc<2>)                                      // 134
        = (11)
        { R.CompareR_MRC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JL, D.JB); }:
        { };

stm:    o_checkhi (mrc<2>, reg<1>)                                      // 135
        = (11)
        { R.CompareR_MRC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JG, D.JA); }:
        { };

stm:    o_checkhi (mem<1>, rc<2>)                                       // 136
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JL, D.JB); }:
        { };

stm:    o_checkhi (rc<2>, mem<1>)                                       // 137
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          R.SkipTrap (@, D.JG, D.JA); }:
        { };

stm:    o_checknil (reg<1>)                                             // 138
        = (11)
        { R.CompareR_NIL (@, @1);
          Emit.work.GenSkipTrap (D.JNE, R.TrapNo (@^.tr), @^.tr^.Position); }:
        { };

stm:    o_checknil (tos<1>)                                             // 139
        = (11)
        { R.FCompareTOS_NIL;
          Emit.work.GenSkipTrap (D.JE, Emit.DivideTrap, @^.tr^.Position); }:
        { };

stm:    o_putpar (mrc<1>)                                               // 140
        { $14 & ((@^.tr^.OpSize <= 4)OR(@^.tr^.OpType=ir.t_int)OR(@^.tr^.OpType=ir.t_unsign)) }
        = (11)
        { R.PutPar (@, @1); }:
        { };

stm:    o_error (o_comma (rc<1>, rc<2>), rc<3>)                   // 141
        = (11)
        { R.DoRaise (@, @1, @2, @3); }:
        { };

stm:    o_stop (rc<1>)                                                  // 142
        = (11)
        { R.DoHalt (@1); }:
        { };

stm:    o_call (mrc<1>)                                                 // 143
        = (22)
        { R.CallProcFunc (@, @1, %stm); }:
        { };

stm:    o_copy (o_comma (rc<1>, rc<2>), rc<3>)                          // 144
        = (21)
        { R.GenCopy (@, @1, @2, @3); }:
        { reg.TryAssignReg (@1, D.ESI);
          reg.TryAssignReg (@2, D.EDI);
          reg.TryAssignReg (@3, D.ECX); };

stm:    o_copy (o_comma (o_par<1>, rc<2>), rc<3>)                       // 145
        { P.ConstStackAddr (@1) }
        = (11)
        { @1^.nt := %const; R.GenCopy (@, @1, @2, @3); }:
        { reg.TryAssignReg (@2, D.EDI); reg.TryAssignReg (@3, D.ECX); };

stm:    o_copy (o_comma (rc<1>, o_par<2>), rc<3>)                       // 146
        { P.ConstStackAddr (@2) }
        = (11)
        { @2^.nt := %const; R.GenCopy (@, @1, @2, @3); }:
        { reg.TryAssignReg (@1, D.ESI); reg.TryAssignReg (@3, D.ECX); };

stm:    o_copy (o_comma (o_par<1>, o_par<2>), rc<3>)                    // 147
        { P.ConstStackAddr (@1) & P.ConstStackAddr (@2) }
        = (11)
        { @1^.nt := %const; @2^.nt := %const;
          R.GenCopy (@, @1, @2, @3); }:
        { reg.TryAssignReg (@3, D.ECX); };

stm:    o_copy (o_comma (rc<1>, rc<2>), o_mul(rc<3>, o_par<4>))         // 148
        { P.Const4 (@4) }
        = (1)
        { R.GenCopy4 (@, @1, @2, @3); }:
        { reg.TryAssignReg (@1, D.ESI);
          reg.TryAssignReg (@2, D.EDI);
          reg.TryAssignReg (@3, D.ECX); };

stm:    o_copy (o_comma (rc<1>, rc<2>), o_mul(o_par<4>,rc<3>))         // 149
        { P.Const4 (@4) }
        = (1)
        { R.GenCopy4 (@, @1, @2, @3); }:
        { reg.TryAssignReg (@1, D.ESI);
          reg.TryAssignReg (@2, D.EDI);
          reg.TryAssignReg (@3, D.ECX); };

/-------------------------------------------------------------------------------

stm:    o_goto                                                          // 150
        = (0)
        { R.AdjustSP; RD.NodeInfo^[@^.tr^.NodeNo].j := D.UnJ; }:
        { };

stm:    o_le (reg<1>, mrc<2>)                                           // 151
        = (11)
        { R.AdjustSP; R.CompareR_MRC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JLE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JBE;
          END; }:
        { };

stm:    o_le (mrc<2>, reg<1>)                                           // 152
        = (11)
        { R.AdjustSP; R.CompareR_MRC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JGE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JAE;
          END; }:
        { };

stm:    o_le (mem<1>, rc<2>)                                            // 153
        = (11)
        { R.AdjustSP; R.CompareM_RC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JLE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JBE;
          END; }:
        { };

stm:    o_le (rc<2>, mem<1>)                                            // 154
        = (11)
        { R.AdjustSP; R.CompareM_RC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JGE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JAE;
          END; }:
        { };

stm:    o_eq (reg<1>, mrc<2>)                                           // 155
        = (11)
        { R.AdjustSP; R.CompareR_MRC (@, @1, @2, TRUE);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE; }:
        { };

stm:    o_eq (mrc<2>, reg<1>)                                           // 156
        = (11)
        { R.AdjustSP; R.CompareR_MRC (@, @1, @2, TRUE);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE; }:
        { };

stm:    o_eq (mem<1>, rc<2>)                                            // 157
        = (11)
        { R.AdjustSP; R.CompareM_RC (@, @1, @2, TRUE);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE; }:
        { };

stm:    o_eq (rc<2>, mem<1>)                                            // 158
        = (11)
        { R.AdjustSP; R.CompareM_RC (@, @1, @2, TRUE);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE; }:
        { };

stm:    o_eq (o_logical<3> (reg<1>, o_par<2>), o_par<4>)                // 159
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@^.tr^.ResSize, @4) &
          (@^.tr^.ResSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) }
        = (11)
        { R.AdjustSP; R.Test_MR (@, @1, @2);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE; }:
        { };

stm:    o_eq (o_logical<3> (mem<1>, o_par<2>), o_par<4>)                // 160
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@^.tr^.ResSize, @4) &
          (@^.tr^.ResSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) }
        = (21)
        { R.AdjustSP; R.Test_MR (@, @1, @2);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE; }:
        { };

stm:    o_in (reg<1>, reg<2>)                                           // 161
        {(@.tr.ResSize<=8)&(@.tr.ResType#ir.t_ref)}
        = (21)
        { R.AdjustSP; R.InSet (@, @1, @2); }:
        { };

stm:    o_in (reg<1>, o_par<2>)                                           // 162
        {FALSE(*(@2.par.tag = ir.y_NumConst)&(@2.sz<=tune.BITSET_LEN_scalar)*)}
        = (20)
        { R.AdjustSP; R.InSetConst (@, @1, @2);
        }:
        { };

stm:    o_in (reg<1>, mem<2>)                                           // 163
        {@.tr.ResType#ir.t_ref}
        = (11)
        { R.AdjustSP; R.InSet (@, @1, @2);}:
        { };

stm:    o_in (reg<1>, addr<2>)                                           // 164
        {@.tr.ResType=ir.t_ref}
        = (11)
        { R.AdjustSP; R.InSet (@, @1, @2);}:
        { };

stm:    o_case (reg<1>)                                                 // 165
        = (11)
        { R.AdjustSP; R.DoCase (@, @1); }:
        { };

stm:    o_ret                                                           // 166
        = (11)
        { R.RetProc (@^.tr); }:
        { };

stm:    o_retfun (mrc<1>)                                               // 167
        = (11)
        { R.RetFunc (@1, @^.tr); }:
        { IF (@^.tr^.ResType <> ir.t_float) THEN
              reg.TryAssignReg (@1, D.EAX);
          END; };

stm:    local  // Фиктивное правило - чтобы из stm была дорога в local  // 168
        = (0)
        { }:
        { };

/-------------------------------------------------------------------------------

tos:    o_par                                                           // 169
        { P.TosVar (@) }
        = (0)
        { @^.nt := %tos; }:
        { };

tos:    o_loadr (addr)                                                  // 170
        { (@^.tr^.ResType = ir.t_float) & (@^.tr^.ResSize >= 4) }
        = (31)
        { @1^.a.bv := @^.tr^.Read; R.MemToTOS (@, @1, @^.tr^.ResSize); }:
        { };

tos:    mem                                                             // 171
        { P.IsFloatNode(@)&(@^.sz >= 4) }
        = (31)
        { R.TryAddPosition (@); R.MemToTOS (@, @, @^.sz); }:
        { };

tos:    mem                                                             // 172
        { P.ResultOfPriorTriade (@) & P.IsFloatNode(@)&(@^.sz >= 4) }
        = (30)
        { R.TryAddPosition (@); R.MemToTOS (@, @, @^.sz); }:
        { };

tos:    o_par                                                           // 173
        { P.ConstNotVar (@) }
        = (31)
        { R.TryAddPosition (@); R.ImmToTOS (@, P.SmallestSize (@)); }:
        { };

tos:    o_par                                                           // 174
        { P.GoodFloatConst (@) }
        = (21)
        { R.TryAddPosition (@); R.ImmToTOS (@, @^.sz); }:
        { };

tos:    o_fbin (tos<1>, tos<2>)                                         // 175
        = (101)
        { R.TOS_FBinary (@, @1, @2, %tos,
                         P.TosVar (@2), @^.tr^.ResSize); }:
        { };

tos:    o_fbin (tos<1>, mem<2>)                                         // 176
        { $8 }
        = (101)
        { R.TOS_FBinary (@, @1, @2, %mem, FALSE, @^.tr^.ResSize); }:
        { };

tos:    o_fbin (mem<1>, tos<2>)                                         // 177
        { $8 }
        = (101)
        { R.TOS_FBinary (@, @2, @1, %mem, TRUE, @^.tr^.ResSize); }:
        { };

tos:    o_fbin (tos<1>, o_val<3> (mem<2>))                              // 178
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType  = ir.t_float) &
          (@3^.tr^.OpSize <= 8) }
        = (101)
        { R.TOS_FBinary (@, @1, @2, %mem, FALSE, @3^.tr^.OpSize); }:
        { };

tos:    o_fbin (o_val<3> (mem<1>), tos<2>)                              // 179
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType  = ir.t_float) &
          (@3^.tr^.OpSize <= 8) }
        = (101)
        { R.TOS_FBinary (@, @2, @1, %mem, TRUE, @3^.tr^.OpSize); }:
        { };

tos:    o_fbin (tos<1>, o_val<3> (mem<2>))                              // 180
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType <> ir.t_float) &
          ((@3^.tr^.OpSize = 2)OR(@3^.tr^.OpSize = 4)) &
          (@3^.tr^.OpType <> ir.t_unsign) }
        = (101)
        { R.TOS_FBinary (@, @1, @2, %imem, FALSE, @3^.tr^.OpSize); }:
        { };

tos:    o_fbin (o_val<3> (mem<1>), tos<2>)                              // 181
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType <> ir.t_float) &
          ((@3^.tr^.OpSize = 2)OR(@3^.tr^.OpSize = 4)) &
          (@3^.tr^.OpType <> ir.t_unsign) }
        = (101)
        { R.TOS_FBinary (@, @2, @1, %imem, TRUE, @3^.tr^.OpSize); }:
        { };

tos:    o_fbin (tos<1>, o_par<2>)                                       // 182
        { P.ConstNotAddr (@2) & ($8 OR (P.SmallestSize (@2) <= 8)) }
        = (101)
        { R.TOS_FBinary (@, @1, @2, %const, FALSE, P.SmallestSize (@2)); }:
        { };

tos:    o_fbin (o_par<1>, tos<2>)                                       // 183
        { P.ConstNotAddr (@1) & ($8 OR (P.SmallestSize (@1) <= 8)) }
        = (101)
        { R.TOS_FBinary (@, @2, @1, %const, TRUE, P.SmallestSize (@1)); }:
        { };

tos:    o_funary (tos<1>)                                               // 184
        = (31)
        { R.FUnary (@); }:
        { };

local:  o_funary (mem<1>)                                               // 185
        { ((@^.tr^.Op = ir.o_abs) OR (@^.tr^.Op = ir.o_neg)) &
          P.SameAlloc (@, @1) }
        = (30)
        { R.UnAbsNeg (@1, @^.tr); @^.nt := %local; }:
        { };

stm:    o_storer (addr<1>, o_funary<3> (mem<2>))                        // 186
        { ((@3^.tr^.Op = ir.o_abs) OR (@3^.tr^.Op = ir.o_neg)) &
          P.SameAddrs (@, @2) }
        = (30)
        { Emit.TryAddPosition (@3^.tr); @1^.a.bv := @^.tr^.Write;
          reg.MarkUseAddrMode (@1.a); R.UnAbsNeg (@2, @3^.tr); }:
        { };

local:  tos                                                             // 187
        { P.RootNode (@) }
        = (71)
        { R.TryAddPosition (@);
          R.TOSToLocal (@, @^.tr^.ResSize); @^.nt := %local; }:
        { };

tos:    o_getpar                                                        // 188
        {@.tr.ResType=ir.t_float}
        = (31)
        { R.GetParToTOS (@); }:
        { };

tos:    o_val (tos<1>)                                                  // 189
        { (@^.tr^.ResType = ir.t_float) &
          (@^.tr^.OpType  = ir.t_float) }
        = (0)
        { @^.nt := %tos; }:
        { };

tos:    o_cast (tos<1>)                                                 // 190
        { (@^.tr^.ResType = ir.t_float) &
          (@^.tr^.OpType  = ir.t_float) }
        = (0)
        { @^.nt := %tos; }:
        { };

tos:    o_val (reg<1>)                                                  // 191
        { (@^.tr^.ResType = ir.t_float) &
          (@^.tr^.OpType <> ir.t_float) }
        = (101)
        { R.ToReal (@, @1, @^.tr^.OpType, @^.tr^.OpSize); }:
        { };

tos:    o_val (mem<1>)                                                  // 192
        { (@^.tr^.ResType = ir.t_float) &
          (@^.tr^.OpType <> ir.t_float) }
        = (101)
        { R.ToReal (@, @1, @^.tr^.OpType, @^.tr^.OpSize); }:
        { };

reg:    o_val (tos<1>)                                                  // 193
        { (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType = ir.t_float) }
        = (101)
        { R.ToOrdinal (@, @1, @^.tr^.ResType, @^.tr^.ResSize); }:
        { };

local:  o_val (tos<1>)                                                  // 194
        { (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType = ir.t_float) }
        = (101)
        { R.ToOrdinal (@, @1, @^.tr^.ResType, @^.tr^.ResSize); }:
        { };

reg:    o_val (mem<1>)                                                  // 195
        { (@1^.sz = 8) &
          (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType = ir.t_float) }
        = (101)
        { R.ToOrdinal (@, @1, @^.tr^.ResType, @^.tr^.ResSize); }:
        { };

local:  o_val (mem<1>)                                                  // 196
        { (@1^.sz = 8) &
          (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType = ir.t_float) }
        = (101)
        { R.ToOrdinal (@, @1, @^.tr^.ResType, @^.tr^.ResSize); }:
        { };

tos:    reg                                                             // 197
        = (201)
        { R.TryAddPosition (@); R.SYSTEMVAL_ToTos (@, @, @^.sz); }:
        { };

tos:    o_cast (reg<1>)                                                 // 198
        = (201)
        { R.SYSTEMVAL_ToTos (@, @1, @^.sz); }:
        { };

reg:    tos                                                             // 199
        = (201)
        { R.TryAddPosition (@); R.SYSTEMVALTosToReg (@, @^.sz); }:
        { };

reg:    o_cast (tos<1>)                                                 // 200
        = (201)
        { R.SYSTEMVALTosToReg (@, @1^.sz); }:
        { };

tos:    o_call (mrc<1>)                                                 // 201
        { (@^.tr^.ResType = ir.t_float) }
        = (22)
        { R.CallProcFunc (@, @1, %tos); }:
        { };

stm:    o_storer (addr<1>, tos<2>)                                      // 202
        = (71)
        { @1^.a.bv := @^.tr^.Write; R.TOSToMem (@1, @^.tr^.ResSize); }:
        { };

stm:    o_storer (addr<1>, o_par<2>)                                    // 203
        { P.ConstReal (@2) }
        = (44)
        { @1^.a.bv := @^.tr^.Write; R.FConstToMem (@1, @2, @^.tr^.ResSize); }:
        { };

stm:    o_putpar (mem<1>)                                               // 204
        { $14 & (@^.tr^.OpSize >= 4) }
        = (22)
        { R.PutPar (@, @1); }:
        { };

stm:    o_putpar (o_par<1>)                                             // 205
        { $14 & P.ConstReal (@1) }
        = (22)
        { R.PutParRealConst (@, @1); }:
        { };

stm:    o_putpar (tos<1>)                                               // 206
        { $14 }
        = (22)
        { R.PutPar (@, @1); }:
        { };

stm:    o_putpar (addr<1>)                                              // 207
        { NOT $14 }
        = (22)
        { @1^.nt := %mem; R.PutAggregate (@, @1); }:
        { };

stm:    o_fle (tos<1>, tos<2>)                                          // 208
        = (101)
        { IF (P.TosVar (@2)) THEN
            R.FCompare (@2, %tos, @^.tr^.ResSize, 41H);
            IF (R.UseSAHF ()) THEN
                RD.NodeInfo^[@^.tr^.NodeNo].j := D.JBE;
            ELSE
                RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
            END;
          ELSE
            R.FCompare (@2, %tos, @^.tr^.ResSize, 01H);
            IF (R.UseSAHF ()) THEN
                RD.NodeInfo^[@^.tr^.NodeNo].j := D.JAE;
            ELSE
                RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
            END;
          END; }:
        { };

stm:    o_fle (tos<1>, mem<2>)                                          // 209
        { $8 }
        = (101)
        { R.FCompare (@2, %mem, @^.tr^.ResSize, 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_fle (mem<1>, tos<2>)                                          // 210
        { $8 }
        = (101)
        { R.FCompare (@1, %mem, @^.tr^.ResSize, 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          END; }:
        { };

stm:    o_fle (tos<1>, o_val<3> (mem<2>))                               // 211
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType  = ir.t_float) &
          (@3^.tr^.OpSize <= 8) }
        = (101)
        { R.FCompare (@2, %mem, @3^.tr^.OpSize, 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_fle (o_val<3> (mem<1>), tos<2>)                               // 212
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType  = ir.t_float) &
          (@3^.tr^.OpSize <= 8) }
        = (101)
        { R.FCompare (@1, %mem, @3^.tr^.OpSize, 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          END; }:
        { };

stm:    o_fle (tos<1>, o_val<3> (mem<2>))                               // 213
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType <> ir.t_float) &
          ((@3^.tr^.OpSize = 2)OR(@3^.tr^.OpSize = 4)) &
          (@3^.tr^.OpType <> ir.t_unsign) }
        = (101)
        { R.FCompare (@2, %imem, @3^.tr^.OpSize, 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_fle (o_val<3> (mem<1>), tos<2>)                               // 214
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType <> ir.t_float) &
          ((@3^.tr^.OpSize = 2)OR(@3^.tr^.OpSize = 4)) &
          (@3^.tr^.OpType <> ir.t_unsign) }
        = (101)
        { R.FCompare (@1, %imem, @3^.tr^.OpSize, 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          END; }:
        { };

stm:    o_fle (tos<1>, o_par<2>)                                        // 215
        { P.ConstNotAddr (@2) & ($8 OR (P.SmallestSize (@2) <= 8)) }
        = (101)
        { R.FCompare (@2, %const, P.SmallestSize (@2), 41H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JBE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_fle (o_par<1>, tos<2>)                                        // 216
        { P.ConstNotAddr (@1) & ($8 OR (P.SmallestSize (@1) <= 8)) }
        = (101)
        { R.FCompare (@1, %const, P.SmallestSize (@1), 01H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JAE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          END; }:
        { };

stm:    o_feq (tos<1>, tos<2>)                                          // 217
        = (101)
        { R.FCompare (@2, %tos, @^.tr^.ResSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (tos<1>, mem<2>)                                          // 218
        { $8 }
        = (101)
        { R.FCompare (@2, %mem, @^.tr^.ResSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (mem<1>, tos<2>)                                          // 219
        { $8 }
        = (101)
        { R.FCompare (@1, %mem, @^.tr^.ResSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (tos<1>, o_val<3> (mem<2>))                               // 220
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType  = ir.t_float) &
          (@3^.tr^.OpSize <= 8) }
        = (101)
        { R.FCompare (@2, %mem, @3^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (o_val<3> (mem<1>), tos<2>)                               // 221
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType  = ir.t_float) &
          (@3^.tr^.OpSize <= 8) }
        = (101)
        { R.FCompare (@1, %mem, @3^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (tos<1>, o_val<3> (mem<2>))                               // 222
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType <> ir.t_float) &
          ((@3^.tr^.OpSize = 2)OR(@3^.tr^.OpSize = 4)) &
          (@3^.tr^.OpType <> ir.t_unsign) }
        = (101)
        { R.FCompare (@2, %imem, @3^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (o_val<3> (mem<1>), tos<2>)                               // 223
        { (@3^.tr^.ResType = ir.t_float) &
          (@3^.tr^.OpType <> ir.t_float) &
          ((@3^.tr^.OpSize = 2)OR(@3^.tr^.OpSize = 4)) &
          (@3^.tr^.OpType <> ir.t_unsign) }
        = (101)
        { R.FCompare (@1, %imem, @3^.tr^.OpSize, 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (tos<1>, o_par<2>)                                        // 224
        { P.ConstNotAddr (@2) & ($8 OR (P.SmallestSize (@2) <= 8)) }
        = (101)
        { R.FCompare (@2, %const, P.SmallestSize (@2), 40H);
          IF (R.UseSAHF ()) THEN
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE;
          ELSE
            RD.NodeInfo^[@^.tr^.NodeNo].j := D.JNE;
          END; }:
        { };

stm:    o_feq (mem<1>, o_par<2>)                                        // 225
        { $8 & P.IsRealZero (@^.tr^.ResSize, @2) }
        = (91)
        { R.FEqZero (@1, @^.tr^.ResSize);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JE; }:
        { };

stm:    o_fle (mem<1>, o_par<2>)                                        // 226
        { $8 & P.IsRealZero (@^.tr^.ResSize, @2) }
        = (91)
        { R.FMemLeZero (@1, @^.tr^.ResSize);
          RD.NodeInfo^[@^.tr^.NodeNo].j := D.JLE; }:
        { };

stm:    o_retfun (tos<1>)                                               // 227
        = (11)
        { R.RetFunc (@1, @^.tr); }:
        { };

stm:    o_retfun (o_call<2> (mrc<1>))                                   // 228
        { (@^.tr^.ResSize = @2^.tr^.ResSize) &
          (P.ProcResultInTOS(at.curr_procno) = P.CallResultInTOS (@2^.tr)) }
        = (11)
        { R.CallProcFunc (@2, @1, %stm); R.RetProc (@^.tr); }:
        { };

/-------------------------------------------------------------------------------

reg:    o_move_eq (reg<1>, mrc<2>)                                      // 229
        = (11)
        { R.CompareR_MRC (@, @1, @2, TRUE); R.CondSetR (@, D.JE); }:
        { };

reg:    o_move_eq (mrc<2>, reg<1>)                                      // 230
        = (11)
        { R.CompareR_MRC (@, @1, @2, TRUE); R.CondSetR (@, D.JE); }:
        { };

reg:    o_move_eq (mem<1>, rc<2>)                                       // 231
        = (11)
        { R.CompareM_RC (@, @1, @2, TRUE); R.CondSetR (@, D.JE); }:
        { };

reg:    o_move_eq (rc<2>, mem<1>)                                       // 232
        = (11)
        { R.CompareM_RC (@, @1, @2, TRUE); R.CondSetR (@, D.JE); }:
        { };

reg:    o_move_eq (o_logical<3> (reg<1>, o_par<2>), o_par<4>)           // 233
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@^.tr^.OpSize, @4) &
          (@^.tr^.OpSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) }
        = (11)
        { R.Test_MR (@, @1, @2); R.CondSetR (@, D.JE); }:
        { };

reg:    o_move_eq (o_logical<3> (mem<1>, o_par<2>), o_par<4>)           // 234
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@^.tr^.OpSize, @4) &
          (@^.tr^.OpSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) }
        = (21)
        { R.Test_MR (@, @1, @2); R.CondSetR (@, D.JE); }:
        { };

reg:    o_move_eq (o_logical<5> (o_incl<3> ( o_par<1>, reg<2> ), reg<4>), o_par<6>)          // 235
        { (@5^.tr^.Op = ir.o_and)
          & P.IsZero (@^.tr^.OpSize, @1) & P.IsZero (@^.tr^.OpSize, @6)
          }
        = (10)
        { R.BitTest (@4, @2); R.CondSetR (@, D.JNC); }:
        { };

local:  o_move_eq (reg<1>, mrc<2>)                                      // 236
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareR_MRC (@, @1, @2, TRUE);
          R.FormMem (@^.a, @^.tr^.Name);
          R.CondSetM (@, @^.a, D.JE);
          @^.nt := %local; }:
        { };

local:  o_move_eq (mrc<2>, reg<1>)                                      // 237
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareR_MRC (@, @1, @2, TRUE);
          R.FormMem (@^.a, @^.tr^.Name);
          R.CondSetM (@, @^.a, D.JE);
          @^.nt := %local; }:
        { };

local:  o_move_eq (mem<1>, rc<2>)                                       // 238
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareM_RC (@, @1, @2, TRUE);
          R.FormMem (@^.a, @^.tr^.Name);
          R.CondSetM (@, @^.a, D.JE);
          @^.nt := %local; }:
        { };

local:  o_move_eq (rc<2>, mem<1>)                                       // 239
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareM_RC (@, @1, @2, TRUE);
          R.FormMem (@^.a, @^.tr^.Name);
          R.CondSetM (@, @^.a, D.JE);
          @^.nt := %local; }:
        { };

local:  o_move_eq (o_logical<3> (reg<1>, o_par<2>), o_par<4>)           // 240
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@^.tr^.OpSize, @4) &
          (@^.tr^.OpSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) &
          (@^.tr^.ResSize = 1) }
        = (11)
        { R.Test_MR (@, @1, @2);
          R.FormMem (@^.a, @^.tr^.Name);
          R.CondSetM (@, @^.a, D.JE);
          @^.nt := %local; }:
        { };

local:  o_move_eq (o_logical<3> (mem<1>, o_par<2>), o_par<4>)           // 241
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@^.tr^.OpSize, @4) &
          (@^.tr^.OpSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) &
          (@^.tr^.ResSize = 1) }
        = (21)
        { R.Test_MR (@, @1, @2);
          R.FormMem (@^.a, @^.tr^.Name);
          R.CondSetM (@, @^.a, D.JE);
          @^.nt := %local; }:
        { };

stm:    o_storer (addr<3>, o_move_eq<4> (reg<1>, mrc<2>))               // 242
        { @4^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareR_MRC (@4, @1, @2, TRUE);
          R.CondSetM (@4, @3^.a, D.JE); }:
        { };

stm:    o_storer (addr<3>, o_move_eq<4> (mrc<2>, reg<1>))               // 243
        { @4^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareR_MRC (@4, @1, @2, TRUE);
          R.CondSetM (@4, @3^.a, D.JE); }:
        { };

stm:    o_storer (addr<3>, o_move_eq<4> (mem<1>, rc<2>))                // 244
        { @4^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareM_RC (@4, @1, @2, TRUE);
          R.CondSetM (@4, @3^.a, D.JE); }:
        { };

stm:    o_storer (addr<3>, o_move_eq<4> (rc<2>, mem<1>))                // 245
        { @4^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareM_RC (@4, @1, @2, TRUE);
          R.CondSetM (@4, @3^.a, D.JE); }:
        { };

stm:    o_storer (addr<6>,                                              // 246
                  o_move_eq<5> (o_logical<3> (reg<1>, o_par<2>),
                                o_par<4>))
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@5^.tr^.OpSize, @4) &
          (@5^.tr^.OpSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) &
          (@5^.tr^.ResSize = 1) }
        = (21)
        { Emit.TryAddPosition (@5^.tr); @6^.a.bv := @^.tr^.Write;
          R.Test_MR (@5, @1, @2);
          R.CondSetM (@5, @6^.a, D.JE); }:
        { };

stm:    o_storer (addr<6>,                                              // 247
                  o_move_eq<5> (o_logical<3> (mem<1>, o_par<2>),
                                o_par<4>))
        { (@3^.tr^.Op = ir.o_and) & P.IsZero (@5^.tr^.OpSize, @4) &
          (@5^.tr^.OpSize = @3^.tr^.ResSize) & P.ConstNotAddr (@2) &
          (@5^.tr^.ResSize = 1) }
        = (21)
        { Emit.TryAddPosition (@5^.tr); @6^.a.bv := @^.tr^.Write;
          R.Test_MR (@5, @1, @2);
          R.CondSetM (@5, @6^.a, D.JE); }:
        { };

reg:    o_move_le (reg<1>, mrc<2>)                                      // 248
        = (11)
        { R.CompareR_MRC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (@, D.JLE);
          ELSE
            R.CondSetR (@, D.JBE);
          END; }:
        { };

reg:    o_move_le (mrc<2>, reg<1>)                                      // 249
        = (11)
        { R.CompareR_MRC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (@, D.JGE);
          ELSE
            R.CondSetR (@, D.JAE);
          END; }:
        { };

reg:    o_move_le (mem<1>, rc<2>)                                       // 250
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (@, D.JLE);
          ELSE
            R.CondSetR (@, D.JBE);
          END; }:
        { };

reg:    o_move_le (rc<2>, mem<1>)                                       // 251
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetR (@, D.JGE);
          ELSE
            R.CondSetR (@, D.JAE);
          END; }:
        { };

local:  o_move_le (reg<1>, mrc<2>)                                      // 252
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareR_MRC (@, @1, @2, TRUE);
          R.FormMem (@^.a, @^.tr^.Name);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@, @^.a, D.JLE);
          ELSE
            R.CondSetM (@, @^.a, D.JBE);
          END;
          @^.nt := %local; }:
        { };

local:  o_move_le (mrc<2>, reg<1>)                                      // 253
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareR_MRC (@, @1, @2, TRUE);
          R.FormMem (@^.a, @^.tr^.Name);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@, @^.a, D.JGE);
          ELSE
            R.CondSetM (@, @^.a, D.JAE);
          END;
          @^.nt := %local; }:
        { };

local:  o_move_le (mem<1>, rc<2>)                                       // 254
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          R.FormMem (@^.a, @^.tr^.Name);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@, @^.a, D.JLE);
          ELSE
            R.CondSetM (@, @^.a, D.JBE);
          END;
          @^.nt := %local; }:
        { };

local:  o_move_le (rc<2>, mem<1>)                                       // 255
        { @^.tr^.ResSize = 1 }
        = (11)
        { R.CompareM_RC (@, @1, @2, FALSE);
          R.FormMem (@^.a, @^.tr^.Name);
          IF (@^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@, @^.a, D.JGE);
          ELSE
            R.CondSetM (@, @^.a, D.JAE);
          END;
          @^.nt := %local; }:
        { };

stm:    o_storer (addr<3>, o_move_le<4> (reg<1>, mrc<2>))               // 256
        { @4^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareR_MRC (@4, @1, @2, FALSE);
          IF (@4^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@4, @3^.a, D.JLE);
          ELSE
            R.CondSetM (@4, @3^.a, D.JBE);
          END; }:
        { };

stm:    o_storer (addr<3>, o_move_le<4> (mrc<2>, reg<1>))               // 257
        { @4^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareR_MRC (@4, @1, @2, FALSE);
          IF (@4^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@4, @3^.a, D.JGE);
          ELSE
            R.CondSetM (@4, @3^.a, D.JAE);
          END; }:
        { };

stm:    o_storer (addr<3>, o_move_le<4> (mem<1>, rc<2>))                // 258
        { @4^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareM_RC (@4, @1, @2, FALSE);
          IF (@4^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@4, @3^.a, D.JLE);
          ELSE
            R.CondSetM (@4, @3^.a, D.JBE);
          END; }:
        { };

stm:    o_storer (addr<3>, o_move_le<4> (rc<2>, mem<1>))                // 259
        { @^.tr^.ResSize = 1 }
        = (21)
        { Emit.TryAddPosition (@4^.tr); @3^.a.bv := @^.tr^.Write;
          R.CompareM_RC (@4, @1, @2, FALSE);
          IF (@4^.tr^.OpType = ir.t_int) THEN
            R.CondSetM (@4, @3^.a, D.JGE);
          ELSE
            R.CondSetM (@4, @3^.a, D.JAE);
          END; }:
        { };

stm:    o_storer (addr<1>, mem<2>)                                      // 260
        { (@^.tr^.ResSize = 8) &
          ((@2^.op <> ir.o_par) OR (@2^.par^.tag = ir.y_Variable) &
                                   (P.RD_LocIN_MEM(@2^.par^.name))) }
        = (70)
        { @1^.a.bv := @^.tr^.Write; R.Mem8ToMem (@1, @2); }:
        { };

/-------------------------------------------------------------------------------

reg:    o_assign (mrc<1>)                                               // 261
        { (@^.tr^.ResType <> ir.t_float) &
          (@^.tr^.OpType  <> ir.t_float) }
        = (11)
        { R.UnOp (@, @1); @^.nt := %reg; }:
        { };

local:  o_assign (rc<1>)                                                // 262
        { @^.tr^.ResSize <= 4 }
        = (11)
        { R.FormMem (@^.a,  @^.tr^.Name);
          R.RCToMem (@, @1, @^.tr^.ResSize);
          @^.nt := %local; }:
        { };

tos:    o_assign (tos<1>)                                               // 263
        { (@^.tr^.ResType = ir.t_float) &
          (@^.tr^.OpType  = ir.t_float) }
        = (0)
        { @^.nt := %tos; }:
        { };

%%

