(* Created by KDV;
   back interface with bin emitter ---
   defines extensions of EmitDef.BinRecipe class,
   every extension corresponds to a method of Emit.work and used
   to create operation by this method and with the same parameters, as
   in r386' call of the method.
*)

MODULE Rrd2Bin;

IMPORT BitVect,
       ir,
       pc := pcK,
       def := OcirDef,
       ocir,
       Emit;
IMPORT D := desc386;

TYPE   AddrMode = Emit.AddrMode;
       SizeType = Emit.SizeType;
VAR    BE* : Emit.SelfPtr;

PROCEDURE ReadFromBadMem(VAR r : AddrMode);
BEGIN
    r.place1.r  := def.UNDEF_REG;
    r.place1.v  := ir.UNDEFINED;
    r.place2.r  := def.UNDEF_REG;
    r.place2.v  := ir.UNDEFINED;
    r.local := def.BAD_MEM;
    r.proc  := ir.ProcUNDEFINED;
    r.offs  := 0;
    r.offslist     := NIL;
    r.proclist     := NIL;
END ReadFromBadMem;

PROCEDURE WriteToBadMem(VAR r : AddrMode);
BEGIN
    r.place1.r  := def.UNDEF_REG;
    r.place1.v  := ir.UNDEFINED;
    r.place2.r  := def.UNDEF_REG;
    r.place2.v  := ir.UNDEFINED;
    r.local := def.BAD_MEM;
    r.proc  := ir.ProcUNDEFINED;
    r.offs  := 0;
    r.offslist     := NIL;
    r.proclist     := NIL;
END WriteToBadMem;

------------------------------------------------------------------------------

TYPE
    CmpXchg     *= POINTER TO CmpXchg_Rec;
    CmpXchg_Rec *= RECORD (def.BinRecipeRec)
                      a*    : AddrMode;
                      r*    : def.Register;
                      sz*   : SizeType;
                      lock* : BOOLEAN;
                      generic*: BOOLEAN;
                   END;

--------------------------------------------------------------------------------

PROCEDURE (self : CmpXchg) Fill*(a-: AddrMode; r: def.Register; sz : SizeType; lock, generic: BOOLEAN);
BEGIN
   self.a := a; self.r := r; self.sz := sz;
   self.lock := lock; self.generic := generic;
END Fill;

PROCEDURE (self : CmpXchg) ToBin*();
BEGIN
   ASSERT(FALSE);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveR_R     *= POINTER TO MoveR_R_Rec;
    MoveR_R_Rec *= RECORD (def.BinRecipeRec)
                      d*, s*: def.Register;
                      sz : SizeType;
                   END;

PROCEDURE (self : MoveR_R) Fill*(d, s: def.Register; sz : SizeType);
BEGIN
   self.d := d; self.s := s; self.sz := sz;
END Fill;

PROCEDURE (self : MoveR_R) ToBin*();
BEGIN
   BE.GenMoveR_R(self.d, self.s, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveR_M     *= POINTER TO MoveR_M_Rec;
    MoveR_M_Rec *= RECORD (def.BinRecipeRRec)
                      r*  : def.Register;
                      a*  : AddrMode;
                      sz: SizeType;
                   END;

PROCEDURE (self : MoveR_M) Fill*(r : def.Register; a- : AddrMode; sz: SizeType);
BEGIN
   self.r := r; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : MoveR_M) ToBin*();
BEGIN
   BE.GenMoveR_M(self.r, self.a, self.sz);
END ToBin;

PROCEDURE (self : MoveR_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : MoveR_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    MoveR_INum     *= POINTER TO MoveR_INum_Rec;
    MoveR_INum_Rec *= RECORD (def.BinRecipeRec)
                        r  : def.Register;
                        v  : LONGINT;
                        sz: SizeType;
                      END;

PROCEDURE (self : MoveR_INum) Fill*(r : def.Register; v : LONGINT; sz: SizeType);
BEGIN
   self.r := r; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : MoveR_INum) ToBin*();
BEGIN
   BE.GenMoveR_INum(self.r, self.v, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveR_Iglobal     *= POINTER TO MoveR_Iglobal_Rec;
    MoveR_Iglobal_Rec *= RECORD (def.BinRecipeRec)
                           r*  : def.Register;
                           a*  : AddrMode;
                           sz: SizeType;
                         END;

PROCEDURE (self : MoveR_Iglobal) Fill*(r : def.Register; a-: AddrMode; sz: SizeType);
BEGIN
   self.r := r; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : MoveR_Iglobal) ToBin*();
BEGIN
   BE.GenMoveR_Iglobal(self.r, self.a, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveM_R           *= POINTER TO MoveM_R_Rec;
    MoveM_R_Rec       *= RECORD (def.BinRecipeWRec)
                           a     : AddrMode;
                           r: def.Register;
                           sz: SizeType;
                         END;

PROCEDURE (self : MoveM_R) Fill*(a-: AddrMode; r: def.Register; sz: SizeType);
BEGIN
   self.a := a; self.r := r; self.sz := sz;
END Fill;

PROCEDURE (self : MoveM_R) ToBin*();
BEGIN
   BE.GenMoveM_R(self.a, self.r, self.sz);
END ToBin;

PROCEDURE (self : MoveM_R) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : MoveM_R) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;


------------------------------------------------------------------------------

TYPE
    MoveM_INum        *= POINTER TO MoveM_INum_Rec;
    MoveM_INum_Rec    *= RECORD (def.BinRecipeWRec)
                           a  : AddrMode;
                           v  : LONGINT;
                           sz: SizeType;
                         END;

PROCEDURE (self : MoveM_INum) Fill*(a-: AddrMode; v : LONGINT; sz: SizeType);
BEGIN
   self.a := a; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : MoveM_INum) ToBin*();
BEGIN
   BE.GenMoveM_INum(self.a, self.v, self.sz);
END ToBin;

PROCEDURE (self : MoveM_INum) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

PROCEDURE (self : MoveM_INum) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

------------------------------------------------------------------------------

TYPE
    MoveM_Iglobal     *= POINTER TO MoveM_Iglobal_Rec;
    MoveM_Iglobal_Rec *= RECORD (def.BinRecipeWRec)
                           a  : AddrMode;
                           ag : AddrMode;
                           sz: SizeType;
                         END;

PROCEDURE (self : MoveM_Iglobal) Fill*(a-, ag- : AddrMode; sz: SizeType);
BEGIN
   self.a := a; self.ag := ag; self.sz := sz;
END Fill;

PROCEDURE (self : MoveM_Iglobal) ToBin*();
BEGIN
   BE.GenMoveM_Iglobal(self.a, self.ag, self.sz);
END ToBin;

PROCEDURE (self : MoveM_Iglobal) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

PROCEDURE (self : MoveM_Iglobal) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

------------------------------------------------------------------------------

TYPE
    XchgR_R           *= POINTER TO XchgR_R_Rec;
    XchgR_R_Rec       *= RECORD (def.BinRecipeRec)
                           d, s: def.Register;
                           sz: SizeType;
                         END;

PROCEDURE (self : XchgR_R) Fill*(d, s: def.Register; sz: SizeType);
BEGIN
   self.d := d; self.s := s; self.sz := sz;
END Fill;


PROCEDURE (self : XchgR_R) ToBin*();
BEGIN
   BE.GenXchgR_R(self.d, self.s, self.sz);
END ToBin;


------------------------------------------------------------------------------

TYPE
    XchgR_M           *= POINTER TO XchgR_M_Rec;
    XchgR_M_Rec       *= RECORD (def.BinRecipeRWRec)
                           r  : def.Register;
                           a  : AddrMode;
                           sz: SizeType;
                         END;

PROCEDURE (self : XchgR_M) Fill*(r : def.Register; a- : AddrMode; sz: SizeType);
BEGIN
   self.r := r; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : XchgR_M) ToBin*();
BEGIN
   BE.GenXchgR_M(self.r, self.a, self.sz);
END ToBin;

PROCEDURE (self : XchgR_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : XchgR_M) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : XchgR_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

PROCEDURE (self : XchgR_M) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    Push_R            *= POINTER TO Push_R_Rec;
    Push_R_Rec        *= RECORD (def.BinRecipeRec)
                           r : def.Register;
                         END;

PROCEDURE (self : Push_R) Fill*(r : def.Register);
BEGIN
   self.r := r;
END Fill;

PROCEDURE (self : Push_R) ToBin*();
BEGIN
   BE.GenPush_R(self.r);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Push_M            *= POINTER TO Push_M_Rec;
    Push_M_Rec        *= RECORD (def.BinRecipeRRec)
                           a : AddrMode;
                           sz: SizeType;
                         END;

PROCEDURE (self : Push_M) Fill*(a- : AddrMode; sz: SizeType);
BEGIN
   self.a := a;
   self.sz := sz;
END Fill;

PROCEDURE (self : Push_M) ToBin*();
BEGIN
   BE.GenPush_M(self.a,self.sz);
END ToBin;

PROCEDURE (self : Push_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : Push_M) GetReadSz*() : SizeType;
BEGIN
    RETURN 4;
END GetReadSz;
------------------------------------------------------------------------------

TYPE
    Push_INum         *= POINTER TO Push_INum_Rec;
    Push_INum_Rec     *= RECORD (def.BinRecipeRec)
                           v  : LONGINT;
                           sz: SizeType;
                         END;

PROCEDURE (self : Push_INum) Fill*(v : LONGINT; sz: SizeType);
BEGIN
   self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : Push_INum) ToBin*();
BEGIN
   BE.GenPush_INum(self.v, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Push_Iglobal      *= POINTER TO Push_Iglobal_Rec;
    Push_Iglobal_Rec  *= RECORD (def.BinRecipeRec)
                           a  : AddrMode;
                           sz: SizeType;
                         END;

PROCEDURE (self : Push_Iglobal) Fill*(a- : AddrMode);
BEGIN
   self.a := a;
END Fill;

PROCEDURE (self : Push_Iglobal) ToBin*();
BEGIN
   BE.GenPush_Iglobal(self.a);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Pop_R             *= POINTER TO Pop_R_Rec;
    Pop_R_Rec         *= RECORD (def.BinRecipeRec)
                           r : def.Register;
                         END;

PROCEDURE (self : Pop_R) Fill*(r : def.Register);
BEGIN
   self.r := r;
END Fill;

PROCEDURE (self : Pop_R) ToBin*();
BEGIN
   BE.GenPop_R(self.r);
END ToBin;

------------------------------------------------------------------------------

TYPE
    OpR_R             *= POINTER TO OpR_R_Rec;
    OpR_R_Rec         *= RECORD (def.BinRecipeRec)
                           ttt       : D.BinaryOp;
                           d, s: def.Register;
                           sz  : SizeType;
                         END;

PROCEDURE (self : OpR_R) Fill*(ttt : D.BinaryOp; d, s: def.Register; sz: SizeType);
BEGIN
   self.ttt := ttt; self.d := d; self.s := s; self.sz := sz;
END Fill;

PROCEDURE (self : OpR_R) ToBin*();
BEGIN
   BE.GenOpR_R(self.ttt, self.d, self.s, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    OpR_M             *= POINTER TO OpR_M_Rec;
    OpR_M_Rec         *= RECORD (def.BinRecipeRRec)
                           ttt : D.BinaryOp;
                           r   : def.Register;
                           a   : AddrMode;
                           sz  : SizeType;
                         END;

PROCEDURE (self : OpR_M) Fill*(ttt : D.BinaryOp; r : def.Register; a- : AddrMode;
                               sz: SizeType);
BEGIN
   self.ttt := ttt; self.r := r; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : OpR_M) ToBin*();
BEGIN
   BE.GenOpR_M(self.ttt, self.r, self.a, self.sz);
END ToBin;

PROCEDURE (self : OpR_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : OpR_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    OpR_INum          *= POINTER TO OpR_INum_Rec;
    OpR_INum_Rec      *= RECORD (def.BinRecipeRec)
                           ttt : D.BinaryOp;
                           r   : def.Register;
                           v   : LONGINT;
                           sz  : SizeType;
                         END;

PROCEDURE (self : OpR_INum) Fill*(ttt : D.BinaryOp; r : def.Register; v : LONGINT;
                                  sz: SizeType);
BEGIN
   self.ttt := ttt; self.r := r; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : OpR_INum) ToBin*();
BEGIN
   BE.GenOpR_INum(self.ttt, self.r, self.v, self.sz);
END ToBin;

PROCEDURE (self : OpR_INum) GetVal*() : LONGINT;
BEGIN
   RETURN self.v;
END GetVal;
------------------------------------------------------------------------------

TYPE
    OpR_Iglobal       *= POINTER TO OpR_Iglobal_Rec;
    OpR_Iglobal_Rec   *= RECORD (def.BinRecipeRec)
                           ttt : D.BinaryOp;
                           r   : def.Register;
                           a   : AddrMode;
                           sz  : SizeType;
                         END;

PROCEDURE (self : OpR_Iglobal) Fill*(ttt : D.BinaryOp; r : def.Register; a- : AddrMode;
                                     sz: SizeType);
BEGIN
   self.ttt := ttt; self.r := r; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : OpR_Iglobal) ToBin*();
BEGIN
   BE.GenOpR_Iglobal(self.ttt, self.r, self.a, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    OpM_R             *= POINTER TO OpM_R_Rec;
    OpM_R_Rec         *= RECORD (def.BinRecipeRWRec)
                           ttt : D.BinaryOp;
                           a   : AddrMode;
                           r   : def.Register;
                           sz  : SizeType;
                         END;

PROCEDURE (self : OpM_R) Fill*(ttt : D.BinaryOp; a- : AddrMode; r : def.Register;
                               sz: SizeType);
BEGIN
   self.ttt := ttt; self.a := a; self.r := r; self.sz := sz;
END Fill;

PROCEDURE (self : OpM_R) ToBin*();
BEGIN
   BE.GenOpM_R(self.ttt, self.a, self.r, self.sz);
END ToBin;

PROCEDURE (self : OpM_R) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : OpM_R) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : OpM_R) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

PROCEDURE (self : OpM_R) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    OpM_INum          *= POINTER TO OpM_INum_Rec;
    OpM_INum_Rec      *= RECORD (def.BinRecipeRWRec)
                           ttt : D.BinaryOp;
                           a   : AddrMode;
                           v   : LONGINT;
                           sz  : SizeType;
                         END;

PROCEDURE (self : OpM_INum) Fill*(ttt : D.BinaryOp; a- : AddrMode; v : LONGINT;
                                  sz: SizeType);
BEGIN
   self.ttt := ttt; self.a := a; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : OpM_INum) ToBin*();
BEGIN
   BE.GenOpM_INum(self.ttt, self.a, self.v, self.sz);
END ToBin;

PROCEDURE (self : OpM_INum) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : OpM_INum) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : OpM_INum) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

PROCEDURE (self : OpM_INum) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    OpM_Iglobal       *= POINTER TO OpM_Iglobal_Rec;
    OpM_Iglobal_Rec   *= RECORD (def.BinRecipeRWRec)
                           ttt : D.BinaryOp;
                           a   : AddrMode;
                           ag  : AddrMode;
                           sz  : SizeType;
                         END;

PROCEDURE (self : OpM_Iglobal) Fill*(ttt : D.BinaryOp; a-, ag- : AddrMode;
                                     sz: SizeType);
BEGIN
   self.ttt := ttt; self.a := a; self.ag := ag; self.sz := sz;
END Fill;

PROCEDURE (self : OpM_Iglobal) ToBin*();
BEGIN
   BE.GenOpM_Iglobal(self.ttt, self.a, self.ag, self.sz);
END ToBin;

PROCEDURE (self : OpM_Iglobal) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : OpM_Iglobal) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : OpM_Iglobal) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

PROCEDURE (self : OpM_Iglobal) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    Div_R             *= POINTER TO Div_R_Rec;
    Div_R_Rec         *= RECORD (def.BinRecipeRec)
                           r      : def.Register;
                           sz     : SizeType;
                           signed : BOOLEAN;
                         END;

PROCEDURE (self : Div_R) Fill*(r: def.Register; sz: SizeType; signed : BOOLEAN);
BEGIN
   self.r := r; self.sz := sz; self.signed := signed;
END Fill;

PROCEDURE (self : Div_R) ToBin*();
BEGIN
   BE.GenDiv_R(self.r, self.sz, self.signed);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Div_M             *= POINTER TO Div_M_Rec;
    Div_M_Rec         *= RECORD (def.BinRecipeRRec)
                           a      : AddrMode;
                           sz     : SizeType;
                           signed : BOOLEAN;
                         END;

PROCEDURE (self : Div_M) Fill*(a- : AddrMode; sz: SizeType; signed : BOOLEAN);
BEGIN
   self.a := a; self.sz := sz; self.signed := signed;
END Fill;

PROCEDURE (self : Div_M) ToBin*();
BEGIN
   BE.GenDiv_M(self.a, self.sz, self.signed);
END ToBin;

PROCEDURE (self : Div_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : Div_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    Div_INum          *= POINTER TO Div_INum_Rec;
    Div_INum_Rec      *= RECORD (def.BinRecipeRec)
                           v      : LONGINT;
                           sz     : SizeType;
                           signed : BOOLEAN;
                         END;

PROCEDURE (self : Div_INum) Fill*(v : LONGINT; sz: SizeType; signed : BOOLEAN);
BEGIN
   self.v := v; self.sz := sz; self.signed := signed;
END Fill;

PROCEDURE (self : Div_INum) ToBin*();
BEGIN
   BE.GenDiv_INum(self.v, self.sz, self.signed);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Mul_R             *= POINTER TO Mul_R_Rec;
    Mul_R_Rec         *= RECORD (def.BinRecipeRec)
                           r      : def.Register;
                           sz     : SizeType;
                           signed : BOOLEAN;
                         END;

PROCEDURE (self : Mul_R) Fill*(r: def.Register; sz: SizeType; signed : BOOLEAN);
BEGIN
   self.r := r; self.sz := sz; self.signed := signed;
END Fill;

PROCEDURE (self : Mul_R) ToBin*();
BEGIN
   BE.GenMul_R(self.r, self.sz, self.signed);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Mul_M             *= POINTER TO Mul_M_Rec;
    Mul_M_Rec         *= RECORD (def.BinRecipeRRec)
                           a      : AddrMode;
                           sz     : SizeType;
                           signed : BOOLEAN;
                         END;

PROCEDURE (self : Mul_M) Fill*(a- : AddrMode; sz: SizeType; signed : BOOLEAN);
BEGIN
   self.a := a; self.sz := sz; self.signed := signed;
END Fill;

PROCEDURE (self : Mul_M) ToBin*();
BEGIN
   BE.GenMul_M(self.a, self.sz, self.signed);
END ToBin;

PROCEDURE (self : Mul_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : Mul_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    Mul_INum          *= POINTER TO Mul_INum_Rec;
    Mul_INum_Rec      *= RECORD (def.BinRecipeRec)
                           v      : LONGINT;
                           sz     : SizeType;
                           signed : BOOLEAN;
                         END;

PROCEDURE (self : Mul_INum) Fill*(v : LONGINT; sz: SizeType; signed : BOOLEAN);
BEGIN
   self.v := v; self.sz := sz; self.signed := signed;
END Fill;

PROCEDURE (self : Mul_INum) ToBin*();
BEGIN
   BE.GenMul_INum(self.v, self.sz, self.signed);
END ToBin;

------------------------------------------------------------------------------

TYPE
    IMulR_RC          *= POINTER TO IMulR_RC_Rec;
    IMulR_RC_Rec      *= RECORD (def.BinRecipeRec)
                            d, s : def.Register;
                            v    : LONGINT;
                            sz   : SizeType;
                         END;

PROCEDURE (self : IMulR_RC) Fill*(d, s : def.Register; v : LONGINT; sz: SizeType);
BEGIN
   self.d := d; self.s := s; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : IMulR_RC) ToBin*();
BEGIN
   BE.GenIMulR_RC(self.d, self.s, self.v, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    IMulR_MC          *= POINTER TO IMulR_MC_Rec;
    IMulR_MC_Rec      *= RECORD (def.BinRecipeRRec)
                             r  : def.Register;
                             a  : AddrMode;
                             v  : LONGINT;
                             sz: SizeType;
                         END;

PROCEDURE (self : IMulR_MC) Fill*(r : def.Register; a- : AddrMode; v : LONGINT;
                                  sz: SizeType);
BEGIN
   self.r := r; self.a := a; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : IMulR_MC) ToBin*();
BEGIN
   BE.GenIMulR_MC(self.r, self.a, self.v, self.sz);
END ToBin;

PROCEDURE (self : IMulR_MC) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : IMulR_MC) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    TestR_R           *= POINTER TO TestR_R_Rec;
    TestR_R_Rec       *= RECORD (def.BinRecipeRec)
                             r1, r2: def.Register; sz: SizeType;
                         END;

PROCEDURE (self : TestR_R) Fill*(r1, r2: def.Register; sz: SizeType);
BEGIN
   self.r1 := r1; self.r2 := r2; self.sz := sz;
END Fill;

PROCEDURE (self : TestR_R) ToBin*();
BEGIN
   BE.GenTestR_R(self.r1, self.r2, self.sz);
END ToBin;


------------------------------------------------------------------------------

TYPE
    TestR_INum        *= POINTER TO TestR_INum_Rec;
    TestR_INum_Rec    *= RECORD (def.BinRecipeRec)
                             r  : def.Register;
                             v  : LONGINT;
                             sz: SizeType;
                         END;

PROCEDURE (self : TestR_INum) Fill*(r : def.Register; v : LONGINT; sz: SizeType);
BEGIN
   self.r := r; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : TestR_INum) ToBin*();
BEGIN
   BE.GenTestR_INum(self.r, self.v, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    TestM_INum        *= POINTER TO TestM_INum_Rec;
    TestM_INum_Rec    *= RECORD (def.BinRecipeRRec)
                             a  : AddrMode;
                             v  : LONGINT;
                             sz: SizeType;
                         END;

PROCEDURE (self : TestM_INum) Fill*(a- : AddrMode; v : LONGINT; sz: SizeType);
BEGIN
   self.a := a; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : TestM_INum) ToBin*();
BEGIN
   BE.GenTestM_INum(self.a, self.v, self.sz);
END ToBin;

PROCEDURE (self : TestM_INum) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : TestM_INum) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    LEA               *= POINTER TO LEA_Rec;
    LEA_Rec           *= RECORD (def.BinRecipeRec)
                             r : def.Register;
                             a : AddrMode;
                         END;

PROCEDURE (self : LEA) Fill*(r : def.Register; a- : AddrMode);
BEGIN
   self.r := r; self.a := a;
END Fill;

PROCEDURE (self : LEA) ToBin*();
BEGIN
   BE.GenLEA(self.r, self.a);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MovesxR_M         *= POINTER TO MovesxR_M_Rec;
    MovesxR_M_Rec     *= RECORD (def.BinRecipeRRec)
                             r*      : def.Register;
                             a*      : AddrMode;
                             rsz, sz: SizeType;
                         END;

PROCEDURE (self : MovesxR_M) Fill*(r : def.Register; a- : AddrMode;
                                    rsz, sz: SizeType);
BEGIN
   self.r := r; self.a := a; self.rsz := rsz; self.sz := sz;
END Fill;

PROCEDURE (self : MovesxR_M) ToBin*();
BEGIN
   BE.GenMovesxR_M(self.r, self.a, self.rsz, self.sz);
END ToBin;

PROCEDURE (self : MovesxR_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : MovesxR_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    MovesxR_R         *= POINTER TO MovesxR_R_Rec;
    MovesxR_R_Rec     *= RECORD (def.BinRecipeRec)
                             d*, s*  : def.Register;
                             rsz, sz: SizeType;
                         END;

PROCEDURE (self : MovesxR_R) Fill*(d, s : def.Register;
                                   rsz, sz: SizeType);
BEGIN
   self.d := d; self.s := s; self.rsz := rsz; self.sz := sz;
END Fill;

PROCEDURE (self : MovesxR_R) ToBin*();
BEGIN
   BE.GenMovesxR_R(self.d, self.s, self.rsz, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MovezxR_M         *= POINTER TO MovezxR_M_Rec;
    MovezxR_M_Rec     *= RECORD (def.BinRecipeRRec)
                             r       : def.Register;
                             a       : AddrMode;
                             rsz, sz: SizeType;
                         END;

PROCEDURE (self : MovezxR_M) Fill*(r : def.Register; a- : AddrMode;
                                   rsz, sz: SizeType);
BEGIN
   self.r := r; self.a := a; self.rsz := rsz; self.sz := sz;
END Fill;

PROCEDURE (self : MovezxR_M) ToBin*();
BEGIN
   BE.GenMovezxR_M(self.r, self.a, self.rsz, self.sz);
END ToBin;

PROCEDURE (self : MovezxR_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : MovezxR_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    MovezxR_R         *= POINTER TO MovezxR_R_Rec;
    MovezxR_R_Rec     *= RECORD (def.BinRecipeRec)
                             d, s    : def.Register;
                             rsz, sz: SizeType;
                         END;

PROCEDURE (self : MovezxR_R) Fill*(d, s : def.Register;
                                   rsz, sz: SizeType);
BEGIN
   self.d := d; self.s := s; self.rsz := rsz; self.sz := sz;
END Fill;

PROCEDURE (self : MovezxR_R) ToBin*();
BEGIN
   BE.GenMovezxR_R(self.d, self.s, self.rsz, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    OpR               *= POINTER TO OpR_Rec;
    OpR_Rec           *= RECORD (def.BinRecipeRec)
                             ttt : D.UnaryOp;
                             r   : def.Register;
                             sz  : SizeType;
                         END;

PROCEDURE (self : OpR) Fill*(ttt : D.UnaryOp; r : def.Register; sz: SizeType);
BEGIN
   self.ttt := ttt; self.r := r; self.sz := sz;
END Fill;

PROCEDURE (self : OpR) ToBin*();
BEGIN
   BE.GenOpR(self.ttt, self.r, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    OpM               *= POINTER TO OpM_Rec;
    OpM_Rec           *= RECORD (def.BinRecipeRWRec)
                             ttt : D.UnaryOp;
                             a   : AddrMode;
                             sz  : SizeType;
                         END;

PROCEDURE (self : OpM) Fill*(ttt : D.UnaryOp; a- : AddrMode; sz: SizeType);
BEGIN
   self.ttt := ttt; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : OpM) ToBin*();
BEGIN
   BE.GenOpM(self.ttt, self.a, self.sz);
END ToBin;

PROCEDURE (self : OpM) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : OpM) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : OpM) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

PROCEDURE (self : OpM) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    SetC_R            *= POINTER TO SetC_R_Rec;
    SetC_R_Rec        *= RECORD (def.BinRecipeRec)
                             r : def.Register;
                             c : D.Condition;
                         END;

PROCEDURE (self : SetC_R) Fill*(r: def.Register; c : D.Condition);
BEGIN
   self.r := r; self.c := c;
END Fill;

PROCEDURE (self : SetC_R) ToBin*();
BEGIN
   BE.GenSetC_R(self.r, self.c);
END ToBin;

------------------------------------------------------------------------------

TYPE
    SetC_M            *= POINTER TO SetC_M_Rec;
    SetC_M_Rec        *= RECORD (def.BinRecipeWRec)
                             a : AddrMode;
                             c : D.Condition;
                         END;

PROCEDURE (self : SetC_M) Fill*(a- : AddrMode; c : D.Condition);
BEGIN
   self.a := a; self.c := c;
END Fill;

PROCEDURE (self : SetC_M) ToBin*();
BEGIN
   BE.GenSetC_M(self.a, self.c);
END ToBin;

PROCEDURE (self : SetC_M) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : SetC_M) GetWriteSz*() : SizeType;
BEGIN
    RETURN 4;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    ShiftR_R          *= POINTER TO ShiftR_R_Rec;
    ShiftR_R_Rec      *= RECORD (def.BinRecipeRec)
                             ttt : D.ShiftOp;
                             r   : def.Register;
                             sz  : SizeType;
                         END;

PROCEDURE (self : ShiftR_R) Fill*(ttt : D.ShiftOp; r: def.Register; sz: SizeType);
BEGIN
   self.ttt := ttt; self.r := r; self.sz := sz;
END Fill;

PROCEDURE (self : ShiftR_R) ToBin*();
BEGIN
   BE.GenShiftR_R(self.ttt, self.r, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    ShiftR_INum       *= POINTER TO ShiftR_INum_Rec;
    ShiftR_INum_Rec   *= RECORD (def.BinRecipeRec)
                             ttt : D.ShiftOp;
                             r   : def.Register;
                             v   : LONGINT;
                             sz  : SizeType;
                         END;

PROCEDURE (self : ShiftR_INum) Fill*(ttt : D.ShiftOp; r : def.Register;
                                     v : LONGINT; sz: SizeType);
BEGIN
   self.ttt := ttt; self.r := r; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : ShiftR_INum) ToBin*();
BEGIN
   BE.GenShiftR_INum(self.ttt, self.r, self.v, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    ShiftM_INum       *= POINTER TO ShiftM_INum_Rec;
    ShiftM_INum_Rec   *= RECORD (def.BinRecipeRWRec)
                             ttt : D.ShiftOp;
                             a   : AddrMode;
                             v   : LONGINT;
                             sz  : SizeType;
                         END;

PROCEDURE (self : ShiftM_INum) Fill*(ttt : D.ShiftOp; a- : AddrMode;
                                     v : LONGINT; sz: SizeType);
BEGIN
   self.ttt := ttt; self.a := a; self.v := v; self.sz := sz;
END Fill;

PROCEDURE (self : ShiftM_INum) ToBin*();
BEGIN
   BE.GenShiftM_INum(self.ttt, self.a, self.v, self.sz);
END ToBin;

PROCEDURE (self : ShiftM_INum) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : ShiftM_INum) WriteMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : ShiftM_INum) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

PROCEDURE (self : ShiftM_INum) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    CBW               *= POINTER TO CBW_Rec;
    CBW_Rec           *= RECORD (def.BinRecipeRec)
                         END;

PROCEDURE (self : CBW) Fill*();
END Fill;

PROCEDURE (self : CBW) ToBin*();
BEGIN
   BE.GenCBW();
END ToBin;

------------------------------------------------------------------------------

TYPE
    CDQ               *= POINTER TO CDQ_Rec;
    CDQ_Rec           *= RECORD (def.BinRecipeRec)
                            sz: SizeType;
                         END;

PROCEDURE (self : CDQ) Fill*(sz: SizeType);
BEGIN
    self.sz := sz;
END Fill;

PROCEDURE (self : CDQ) ToBin*();
BEGIN
   BE.GenCDQ(self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    RepStoSD          *= POINTER TO RepStoSD_Rec;
    RepStoSD_Rec      *= RECORD (def.BinRecipeRWRec)
                         END;

PROCEDURE (self : RepStoSD) Fill*();
END Fill;

PROCEDURE (self : RepStoSD) ToBin*();
BEGIN
   ASSERT(FALSE);
END ToBin;

PROCEDURE (self : RepStoSD) ReadMem*(VAR r : AddrMode);
BEGIN
    ReadFromBadMem(r);
END ReadMem;

PROCEDURE (self : RepStoSD) WriteMem*(VAR r : AddrMode);
BEGIN
    WriteToBadMem(r);
END WriteMem;

------------------------------------------------------------------------------
TYPE
    RepMovSD          *= POINTER TO RepMovSD_Rec;
    RepMovSD_Rec      *= RECORD (def.BinRecipeRWRec)
                         END;

PROCEDURE (self : RepMovSD) Fill*();
END Fill;

PROCEDURE (self : RepMovSD) ToBin*();
BEGIN
   BE.GenRepMovSD();
END ToBin;

PROCEDURE (self : RepMovSD) ReadMem*(VAR r : AddrMode);
BEGIN
    ReadFromBadMem(r);
END ReadMem;

PROCEDURE (self : RepMovSD) WriteMem*(VAR r : AddrMode);
BEGIN
    WriteToBadMem(r);
END WriteMem;

------------------------------------------------------------------------------

TYPE
    MovSD             *= POINTER TO MovSD_Rec;
    MovSD_Rec         *= RECORD (def.BinRecipeRWRec)
                         END;

PROCEDURE (self : MovSD) Fill*();
END Fill;

PROCEDURE (self : MovSD) ToBin*();
BEGIN
   BE.GenMovSD();
END ToBin;

PROCEDURE (self : MovSD) ReadMem*(VAR r : AddrMode);
BEGIN
    ReadFromBadMem(r);
END ReadMem;

PROCEDURE (self : MovSD) WriteMem*(VAR r : AddrMode);
BEGIN
    WriteToBadMem(r);
END WriteMem;

------------------------------------------------------------------------------

TYPE
    RepMovSB          *= POINTER TO RepMovSB_Rec;
    RepMovSB_Rec      *= RECORD (def.BinRecipeRWRec)
                         END;

PROCEDURE (self : RepMovSB) Fill*();
END Fill;

PROCEDURE (self : RepMovSB) ToBin*();
BEGIN
   BE.GenRepMovSB();
END ToBin;

PROCEDURE (self : RepMovSB) ReadMem*(VAR r : AddrMode);
BEGIN
    ReadFromBadMem(r);
END ReadMem;

PROCEDURE (self : RepMovSB) WriteMem*(VAR r : AddrMode);
BEGIN
    WriteToBadMem(r);
END WriteMem;

------------------------------------------------------------------------------

TYPE
    MovSB             *= POINTER TO MovSB_Rec;
    MovSB_Rec         *= RECORD (def.BinRecipeRWRec)
                         END;

PROCEDURE (self : MovSB) Fill*();
END Fill;

PROCEDURE (self : MovSB) ToBin*();
BEGIN
   BE.GenMovSB();
END ToBin;

PROCEDURE (self : MovSB) ReadMem*(VAR r : AddrMode);
BEGIN
    ReadFromBadMem(r);
END ReadMem;

PROCEDURE (self : MovSB) WriteMem*(VAR r : AddrMode);
BEGIN
    WriteToBadMem(r);
END WriteMem;

------------------------------------------------------------------------------

TYPE
    MovSW             *= POINTER TO MovSW_Rec;
    MovSW_Rec         *= RECORD (def.BinRecipeRWRec)
                         END;

PROCEDURE (self : MovSW) Fill*();
END Fill;

PROCEDURE (self : MovSW) ToBin*();
BEGIN
   BE.GenMovSW();
END ToBin;

PROCEDURE (self : MovSW) ReadMem*(VAR r : AddrMode);
BEGIN
    ReadFromBadMem(r);
END ReadMem;

PROCEDURE (self : MovSW) WriteMem*(VAR r : AddrMode);
BEGIN
    WriteToBadMem(r);
END WriteMem;

------------------------------------------------------------------------------

TYPE
    Sahf              *= POINTER TO Sahf_Rec;
    Sahf_Rec          *= RECORD (def.BinRecipeRec)
                         END;

PROCEDURE (self : Sahf) Fill*();
END Fill;

PROCEDURE (self : Sahf) ToBin*();
BEGIN
   BE.GenSahf();
END ToBin;

------------------------------------------------------------------------------

TYPE
    J                 *= POINTER TO J_Rec;
    J_Rec             *= RECORD (def.BinRecipeRec)
                             j    : D.Condition;
                             o    : LONGINT;
                             long : BOOLEAN;
                         END;

PROCEDURE (self : J) Fill*(j : D.Condition; o : LONGINT; long : BOOLEAN);
BEGIN
   self.j := j; self.o := o; self.long := long;
END Fill;

PROCEDURE (self : J) ToBin*();
BEGIN
   BE.GenJ(self.j, self.o, self.long);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Call_M            *= POINTER TO Call_M_Rec;
    Call_M_Rec        *= RECORD (def.BinRecipeRRec)
                             a    : AddrMode;
                             u, m : def.Regs;
                             r, w : BitVect.BitVector;
                         END;

PROCEDURE (self : Call_M) Fill*(a- : AddrMode; u, m : def.Regs;
                                r, w: BitVect.BitVector);
BEGIN
   self.a := a; self.u := u; self.m := m; self.r := r; self.w := w;
END Fill;

PROCEDURE (self : Call_M) ToBin*();
BEGIN
   BE.GenCall_M(self.a, self.u, self.m, self.r, self.w);
END ToBin;

PROCEDURE (self : Call_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : Call_M) GetReadSz*() : SizeType;
BEGIN
    RETURN 4;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    Call_R            *= POINTER TO Call_R_Rec;
    Call_R_Rec        *= RECORD (def.BinRecipeRec)
                             r     : def.Register;
                             u, m  : def.Regs;
                             rd, w : BitVect.BitVector;
                         END;

PROCEDURE (self : Call_R) Fill*(r : def.Register; u, m : def.Regs;
                                rd, w: BitVect.BitVector);
BEGIN
   self.r := r; self.u := u; self.m := m; self.rd := rd; self.w := w;
END Fill;
PROCEDURE (self : Call_R) ToBin*();
BEGIN
   BE.GenCall_R(self.r, self.u, self.m, self.rd, self.w);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Call_INum         *= POINTER TO Call_INum_Rec;
    Call_INum_Rec     *= RECORD (def.BinRecipeRec)
                             v    : LONGINT;
                             u, m : def.Regs;
                             r, w : BitVect.BitVector;
                         END;

PROCEDURE (self : Call_INum) Fill*(v : LONGINT; u, m : def.Regs;
                                   r, w: BitVect.BitVector);
BEGIN
   self.v := v; self.u := u; self.m := m; self.r := r; self.w := w;
END Fill;

PROCEDURE (self : Call_INum) ToBin*();
BEGIN
   BE.GenCall_INum(self.v, self.u, self.m, self.r, self.w);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Call_Iglobal      *= POINTER TO Call_Iglobal_Rec;
    Call_Iglobal_Rec  *= RECORD (def.BinRecipeRec)
                             a    : AddrMode;
                             u, m : def.Regs;
                             r, w : BitVect.BitVector;
                         END;

PROCEDURE (self : Call_Iglobal) Fill*(a- : AddrMode; u, m : def.Regs;
                                   r, w: BitVect.BitVector);
BEGIN
   self.a := a; self.u := u; self.m := m; self.r := r; self.w := w;
END Fill;

PROCEDURE (self : Call_Iglobal) ToBin*();
BEGIN
   BE.GenCall_Iglobal(self.a, self.u, self.m, self.r, self.w);
END ToBin;


------------------------------------------------------------------------------

TYPE
    Ret               *= POINTER TO Ret_Rec;
    Ret_Rec           *= RECORD (def.BinRecipeRec)
                             n    : ir.INT;
                         END;

PROCEDURE (self : Ret) Fill*(n : ir.INT);
BEGIN
   self.n := n;
END Fill;

PROCEDURE (self : Ret) ToBin*();
BEGIN
   BE.GenRet(self.n);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Case              *= POINTER TO Case_Rec;
    Case_Rec          *= RECORD (def.BinRecipeRec)
                             r : def.Register;
                             v : LONGINT;
                             lb: Emit.LABEL;
                         END;

PROCEDURE (self : Case) Fill*(r : def.Register; v : LONGINT; lb: Emit.LABEL);
BEGIN
   self.r := r; self.v := v;
END Fill;

PROCEDURE (self : Case) ToBin*();
BEGIN
   BE.GenCase(self.r, self.v, self.lb);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveR_Table       *= POINTER TO MoveR_Table_Rec;
    MoveR_Table_Rec   *= RECORD (def.BinRecipeRec)
                             r, i : def.Register;
                             t    : pc.OBJECT;
                             sz   : SizeType;
                         END;

PROCEDURE (self : MoveR_Table) Fill*(r, i : def.Register;
                                     t    : pc.OBJECT;
                                     sz   : SizeType);
BEGIN
   self.r := r; self.i := i; self.t := t; self.sz := sz;
END Fill;

PROCEDURE (self : MoveR_Table) ToBin*();
BEGIN
   BE.GenMoveR_Table(self.r, self.i, self.t, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    OpR_Table         *= POINTER TO OpR_Table_Rec;
    OpR_Table_Rec     *= RECORD (def.BinRecipeRec)
                             ttt  : D.BinaryOp;
                             r, i : def.Register;
                             t    : pc.OBJECT;
                             sz   : SizeType;
                         END;

PROCEDURE (self : OpR_Table) Fill*(ttt  : D.BinaryOp;
                                   r, i : def.Register;
                                   t    : pc.OBJECT;
                                   sz   : SizeType);
BEGIN
   self.ttt := ttt; self.r := r; self.i := i; self.t := t; self.sz := sz;
END Fill;

PROCEDURE (self : OpR_Table) ToBin*();
BEGIN
   BE.GenOpR_Table(self.ttt, self.r, self.i, self.t, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    TestR_Table       *= POINTER TO TestR_Table_Rec;
    TestR_Table_Rec   *= RECORD (def.BinRecipeRec)
                             r, i : def.Register;
                             t    : pc.OBJECT;
                             sz   : SizeType;
                         END;

PROCEDURE (self : TestR_Table) Fill*(r, i : def.Register;
                                     t    : pc.OBJECT;
                                     sz   : SizeType);
BEGIN
   self.r := r; self.i := i; self.t := t; self.sz := sz;
END Fill;

PROCEDURE (self : TestR_Table) ToBin*();
BEGIN
   BE.GenTestR_Table(self.r, self.i, self.t, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Halt              *= POINTER TO Halt_Rec;
    Halt_Rec          *= RECORD (def.BinRecipeRec)
                         END;

PROCEDURE (self : Halt) Fill*();
END Fill;

PROCEDURE (self : Halt) ToBin*();
BEGIN
   BE.GenHalt();
END ToBin;


------------------------------------------------------------------------------

TYPE
    Raise             *= POINTER TO Raise_Rec;
    Raise_Rec         *= RECORD (def.BinRecipeRec)
                            is_assert: BOOLEAN;
                         END;

PROCEDURE (self : Raise) Fill*(is_assert, is_trapnil: BOOLEAN);
BEGIN
   self.is_assert := is_assert;
END Fill;

PROCEDURE (self : Raise) ToBin*();
BEGIN
   BE.GenRaise(self.is_assert);
END ToBin;

------------------------------------------------------------------------------

TYPE
    SkipTrap          *= POINTER TO SkipTrap_Rec;
    SkipTrap_Rec      *= RECORD (def.BinRecipeRec)
                             j: D.Condition;
                             intno : Emit.Trap;
                             _pos     : ir.TPOS;
                         END;

PROCEDURE (self : SkipTrap) Fill*(j:D.Condition; intno : Emit.Trap; _pos : ir.TPOS);
BEGIN
   self.j := j; self.intno := intno; self._pos := _pos;
END Fill;

PROCEDURE (self : SkipTrap) ToBin*();
BEGIN
   BE.GenSkipTrap(self.j, self.intno, self._pos);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveTOS_M         *= POINTER TO MoveTOS_M_Rec;
    MoveTOS_M_Rec     *= RECORD (def.BinRecipeFloatRRec)
                             a : AddrMode;
                             sz: SizeType;
                         END;

PROCEDURE (self : MoveTOS_M) Fill*(a- : AddrMode; sz: SizeType);
BEGIN
   IF ocir.InLoop THEN ocir.WasFloatMemInLoop := TRUE; END;
   self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : MoveTOS_M) ToBin*();
BEGIN
   BE.GenMoveTOS_M(self.a, self.sz);
END ToBin;

PROCEDURE (self : MoveTOS_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : MoveTOS_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    MoveTOS_STi       *= POINTER TO MoveTOS_STi_Rec;
    MoveTOS_STi_Rec   *= RECORD (def.BinRecipeRec)
                             i* : LONGINT;
                         END;

PROCEDURE (self :MoveTOS_STi) Fill*(i : LONGINT);
BEGIN
   self.i := i;
END Fill;

PROCEDURE (self : MoveTOS_STi) ToBin*();
BEGIN
   BE.GenMoveTOS_STi(self.i);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveTOS_INum      *= POINTER TO MoveTOS_INum_Rec;
    MoveTOS_INum_Rec  *= RECORD (def.BinRecipeRec)
                             w  : ir.VALUE;
                             sz: SizeType;
                         END;

PROCEDURE (self :MoveTOS_INum) Fill*(w : ir.VALUE; sz: SizeType);
BEGIN
   self.w := w; self.sz := sz;
END Fill;

PROCEDURE (self : MoveTOS_INum) ToBin*();
BEGIN
   BE.GenMoveTOS_INum(self.w, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FILD              *= POINTER TO FILD_Rec;
    FILD_Rec          *= RECORD (def.BinRecipeRRec)
                             a  : AddrMode;
                             sz: SizeType;
                         END;

PROCEDURE (self : FILD) Fill*(a- : AddrMode; sz: SizeType);
BEGIN
   self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : FILD) ToBin*();
BEGIN
   BE.GenFILD(self.a, self.sz);
END ToBin;

PROCEDURE (self : FILD) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : FILD) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    MoveM_TOS         *= POINTER TO MoveM_TOS_Rec;
    MoveM_TOS_Rec     *= RECORD (def.BinRecipeFloatWRec)
                             a*  : AddrMode;
                             sz* : SizeType;
                         END;

PROCEDURE (self : MoveM_TOS) Fill*(a- : AddrMode; sz: SizeType);
BEGIN
   IF ocir.InLoop THEN ocir.WasFloatMemInLoop := TRUE; END;
   self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : MoveM_TOS) ToBin*();
BEGIN
   BE.GenMoveM_TOS(self.a, self.sz);
END ToBin;

PROCEDURE (self : MoveM_TOS) WriteMem*(VAR r : AddrMode) ;
BEGIN
    r := self.a;
END WriteMem;

PROCEDURE (self : MoveM_TOS) GetWriteSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetWriteSz;

------------------------------------------------------------------------------

TYPE
    MoveSTi_TOS       *= POINTER TO MoveSTi_TOS_Rec;
    MoveSTi_TOS_Rec   *= RECORD (def.BinRecipeRec)
                             i* : LONGINT;
                         END;

PROCEDURE (self : MoveSTi_TOS) Fill*(i : LONGINT);
BEGIN
   self.i := i;
END Fill;

PROCEDURE (self : MoveSTi_TOS) ToBin*();
BEGIN
   BE.GenMoveSTi_TOS(self.i);
END ToBin;

------------------------------------------------------------------------------

TYPE
    MoveSTi_ST0       *= POINTER TO MoveSTi_ST0_Rec;
    MoveSTi_ST0_Rec   *= RECORD (def.BinRecipeRec)
                             i* : LONGINT;
                         END;

PROCEDURE (self : MoveSTi_ST0) Fill*(i : LONGINT);
BEGIN
   self.i := i;
END Fill;

PROCEDURE (self : MoveSTi_ST0) ToBin*();
BEGIN
   BE.GenMoveSTi_ST0(self.i);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FOpSTi_TOS        *= POINTER TO FOpSTi_TOS_Rec;
    FOpSTi_TOS_Rec    *= RECORD (def.BinRecipeRec)
                             op* : D.FloatOp;
                             r*  : SHORTINT;
                         END;

PROCEDURE (self : FOpSTi_TOS) Fill*(op : D.FloatOp; r: SHORTINT);
BEGIN
   self.op := op; self.r := r;
END Fill;

PROCEDURE (self : FOpSTi_TOS) ToBin*();
BEGIN
   BE.GenFOpSTi_TOS(self.op, self.r);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FOpSTi_ST0        *= POINTER TO FOpSTi_ST0_Rec;
    FOpSTi_ST0_Rec    *= RECORD (def.BinRecipeRec)
                             op* : D.FloatOp;
                             r*  : SHORTINT;
                         END;

PROCEDURE (self : FOpSTi_ST0) Fill*(op : D.FloatOp; r: SHORTINT);
BEGIN
   self.op := op; self.r := r;
END Fill;

PROCEDURE (self : FOpSTi_ST0) ToBin*();
BEGIN
   BE.GenFOpSTi_ST0(self.op, self.r);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FOpST0_STi        *= POINTER TO FOpST0_STi_Rec;
    FOpST0_STi_Rec    *= RECORD (def.BinRecipeRec)
                             op* : D.FloatOp;
                             r*  : SHORTINT;
                         END;

PROCEDURE (self : FOpST0_STi) Fill*(op : D.FloatOp; r : SHORTINT);
BEGIN
   self.op := op; self.r := r;
END Fill;

PROCEDURE (self : FOpST0_STi) ToBin*();
BEGIN
   BE.GenFOpST0_STi(self.op, self.r);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FOpST0_M          *= POINTER TO FOpST0_M_Rec;
    FOpST0_M_Rec      *= RECORD (def.BinRecipeFloatRRec)
                             op* : D.FloatOp;
                             a   : AddrMode;
                             sz  : SizeType;
                         END;

PROCEDURE (self : FOpST0_M) Fill*(op : D.FloatOp; a- : AddrMode; sz: SizeType);
BEGIN
   IF ocir.InLoop THEN ocir.WasFloatMemInLoop := TRUE; END;
   self.op := op; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : FOpST0_M) ToBin*();
BEGIN
   BE.GenFOpST0_M(self.op, self.a, self.sz);
END ToBin;

PROCEDURE (self : FOpST0_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : FOpST0_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    FOpST0_IM         *= POINTER TO FOpST0_IM_Rec;
    FOpST0_IM_Rec     *= RECORD (def.BinRecipeRec)
                             op : D.FloatOp;
                             a  : AddrMode;
                             sz: SizeType;
                         END;

PROCEDURE (self : FOpST0_IM) Fill*(op : D.FloatOp; a- : AddrMode; sz: SizeType);
BEGIN
   self.op := op; self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : FOpST0_IM) ToBin*();
BEGIN
   BE.GenFOpST0_IM(self.op, self.a, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FOpST0_INum       *= POINTER TO FOpST0_INum_Rec;
    FOpST0_INum_Rec   *= RECORD (def.BinRecipeRec)
                             op : D.FloatOp;
                             w  : ir.VALUE;
                             sz: SizeType;
                         END;

PROCEDURE (self : FOpST0_INum) Fill*(op : D.FloatOp; w : ir.VALUE; sz: SizeType);
BEGIN
   self.op := op; self.w := w; self.sz := sz;
END Fill;

PROCEDURE (self : FOpST0_INum) ToBin*();
BEGIN
   BE.GenFOpST0_INum(self.op, self.w, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FOp               *= POINTER TO FOp_Rec;
    FOp_Rec           *= RECORD (def.BinRecipeRec)
                             fcode : D.FloatOp;
                         END;

PROCEDURE (self : FOp) Fill*(fcode : D.FloatOp);
BEGIN
   self.fcode := fcode;
END Fill;

PROCEDURE (self : FOp) ToBin*();
BEGIN
   BE.GenFOp(self.fcode);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FComTOS_TOS       *= POINTER TO FComTOS_TOS_Rec;
    FComTOS_TOS_Rec   *= RECORD (def.BinRecipeRec)
                         END;

PROCEDURE (self : FComTOS_TOS) Fill*();
END Fill;

PROCEDURE (self : FComTOS_TOS) ToBin*();
BEGIN
   BE.GenFComTOS_TOS();
END ToBin;

------------------------------------------------------------------------------

TYPE
    FComTOS_INum      *= POINTER TO FComTOS_INum_Rec;
    FComTOS_INum_Rec  *= RECORD (def.BinRecipeRec)
                             w  : ir.VALUE;
                             sz: SizeType;
                         END;

PROCEDURE (self : FComTOS_INum) Fill*(w : ir.VALUE; sz: SizeType);
BEGIN
   self.w := w; self.sz := sz;
END Fill;

PROCEDURE (self : FComTOS_INum) ToBin*();
BEGIN
   BE.GenFComTOS_INum(self.w, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FComTOS_M         *= POINTER TO FComTOS_M_Rec;
    FComTOS_M_Rec     *= RECORD (def.BinRecipeFloatRRec)
                             a  : AddrMode;
                             sz: SizeType;
                         END;

PROCEDURE (self : FComTOS_M) Fill*(a- : AddrMode; sz: SizeType);
BEGIN
   IF ocir.InLoop THEN ocir.WasFloatMemInLoop := TRUE; END;
   self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : FComTOS_M) ToBin*();
BEGIN
   BE.GenFComTOS_M(self.a, self.sz);
END ToBin;

PROCEDURE (self : FComTOS_M) ReadMem*(VAR r : AddrMode);
BEGIN
    r := self.a;
END ReadMem;

PROCEDURE (self : FComTOS_M) GetReadSz*() : SizeType;
BEGIN
    RETURN self.sz;
END GetReadSz;

------------------------------------------------------------------------------

TYPE
    FComTOS_STi       *= POINTER TO FComTOS_STi_Rec;
    FComTOS_STi_Rec   *= RECORD (def.BinRecipeRec)
                             r* : SHORTINT;
                         END;

PROCEDURE (self : FComTOS_STi) Fill*(r : SHORTINT);
BEGIN
   self.r := r;
END Fill;

PROCEDURE (self : FComTOS_STi) ToBin*();
BEGIN
   BE.GenFComTOS_STi(self.r);
END ToBin;

------------------------------------------------------------------------------

TYPE
    FComTOS_IM        *= POINTER TO FComTOS_IM_Rec;
    FComTOS_IM_Rec    *= RECORD (def.BinRecipeRec)
                             a  : AddrMode;
                             sz: SizeType;
                         END;

PROCEDURE (self : FComTOS_IM) Fill*(a- : AddrMode; sz: SizeType);
BEGIN
   self.a := a; self.sz := sz;
END Fill;

PROCEDURE (self : FComTOS_IM) ToBin*();
BEGIN
   BE.GenFComTOS_IM(self.a, self.sz);
END ToBin;

------------------------------------------------------------------------------

TYPE
    Fstsw             *= POINTER TO Fstsw_Rec;
    Fstsw_Rec         *= RECORD (def.BinRecipeRec)
                         END;

PROCEDURE (self : Fstsw) Fill*();
END Fill;

PROCEDURE (self : Fstsw) ToBin*();
BEGIN
   BE.GenFstsw();
END ToBin;

------------------------------------------------------------------------------

TYPE
    CallToOrdinal     *= POINTER TO CallToOrdinal_Rec;
    CallToOrdinal_Rec *= RECORD (def.BinRecipeRec)
                             card : BOOLEAN;
                             u, m : def.Regs;
                             f : BOOLEAN;
                             sz: SizeType;
                         END;

PROCEDURE (self : CallToOrdinal) Fill*(card : BOOLEAN; u, m : def.Regs; f: BOOLEAN; sz: SizeType);
BEGIN
   self.card := card; self.u := u; self.m := m; self.f := f; self.sz := sz;
END Fill;

PROCEDURE (self : CallToOrdinal) ToBin*();
BEGIN
   BE.GenCallToOrdinal(self.card, self.u, self.m, self.f, self.sz);
END ToBin;

END Rrd2Bin.
