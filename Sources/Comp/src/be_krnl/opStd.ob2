MODULE opStd;
IMPORT pc := pcK,
             ir,
       pr := opProcs,
     tune := opTune,
       at := opAttrs,
      env := xiEnv;

TYPE INT = ir.INT;

(* ---------- C o n s t a n t   r e d e f i n i t i o n --------------------- *)
CONST
  by_val        = ORD(pr.by_val);         (* parameter by value *)
  by_val_struct = ORD(pr.by_val_struct);  (* structured parameter by val (SL-1) *)
  by_ref        = ORD(pr.by_ref);         (* parameter by ref (VAR) *)
  by_ROref      = ORD(pr.by_ROref);       (* parameter by read-only ref *)

(* -------------- M o d u l e   p r o t o t y p e s ------------------------- *)
VAR
  ModuleProtoNum*  : pr.ProtoNum;
  MainProtoNum*    : pr.ProtoNum;
  DLLMainProtoNum* : pr.ProtoNum;
<* IF TARGET_386 THEN *>
  AsmCodeProtoNum* : pr.ProtoNum;   -- prototype for a piece of embedded code
<* END *>
(* ----------------- I n i t i a t i o n ------------------------ *)

VAR x2c* : pc.STRUCT;

PROCEDURE make_X2C;
  VAR nm: pc.STRING;
BEGIN
--  IF x2c = NIL THEN
    NEW(x2c);
    x2c.mode := pc.ty_module;
    x2c.flag := pc.flag_c;
    x2c.mno  := tune.x2c_mno;
    x2c.tags := pc.TTAG_SET{};
    x2c.mem := NIL;
    x2c.prof:= NIL;
    x2c.base:= NIL;
    x2c.inx := NIL;
    x2c.len := 0;               (* module - version tag *)
    x2c.pos := env.null_pos;
    x2c.end := env.null_pos;
    x2c.use := NIL;             (* import               *)
    x2c.ext := NIL;             (* for back-end only    *)
    nm := at.make_name(tune.rts_name);
    x2c.obj := at.new_work_object(nm, x2c, NIL, pc.ob_module, FALSE);
    x2c.obj.mno := tune.x2c_mno;
--  END;
END make_X2C;

(* ----------- r t s - p r o c e d u r e s ---------- *)
CONST
--  X2C_NATIVE_BEGIN* = 0;
  X2C_BEGIN*        = 0;
  X2C_MODULE*       = 1;
  X2C_NEW_OPEN*     = 2;
  X2C_DYNALLOCATE*  = 3;
  X2C_DISPOSE*      = 4;
  X2C_DYNDEALLOCATE*= 5;
  X2C_NEW*          = 6;
  X2C_ALLOCATE*     = 7;
  X2C_DEALLOCATE*   = 8;
  X2C_PROTECT*      = 9;
  X2C_XRETRY*       = 10;
  X2C_XREMOVE*      = 11;
  X2C_XOFF*         = 12;
  X2C_XON*          = 13;
  X2C_FINALLY*      = 14;
  X2C_XInitHandler* = 15;
  X2C_setjmp*       = 16;
  X2C_HALT*         = 17;
  X2C_AND*          = 18;
  X2C_XOR*          = 19;
  X2C_OR*           = 20;
  X2C_BIC*          = 21;
  X2C_COMPLEMENT*   = 22;
  X2C_LENGTH*       = 23;
  X2C_ENTIER*       = 24;
  X2C_CAP*          = 25;
  X2C_STRCMP_PROC*  = 26;
  X2C_SET_EQU*      = 27;
  X2C_SET_LEQ*      = 28;
  X2C_INCL*         = 29;
  X2C_EXCL*         = 30;
  X2C_LONGSET*      = 31;
  X2C_ROT*          = 32;
  X2C_LSH*          = 33;
  X2C_ROTL*         = 34;
  X2C_LSHL*         = 35;
  MEM_CMP*          = 36;
  X2C_EXPCI*        = 37;
  X2C_EXPCR*        = 38;
  X2C_EXPLI*        = 39;
  X2C_EXPLR*        = 40;
  X2C_EXPRI*        = 41;
  X2C_EXPRR*        = 42;
  X2C_COPY*         = 43;
  X2C_MEMSET*       = 44;
  X2C_TRUNCI*       = 45;
  X2C_TRUNCC*       = 46;
  X2C_TRAP_NIL*     = 47;
  X2C_TRAP_RANGE*   = 48;
  X2C_TRAP_INDEX*   = 49;
  X2C_TRAP_DIV*     = 50;
  X2C_TRAP_OVERFL*  = 51;
  X2C_TRAP_F*       = 52;
  X2C_ASSERT_F*     = 53;
  X2C_InitFPP*      = 54;
  X2C_PROC_IN*      = 55;
  X2C_PROC_OUT*     = 56;
  X2C_BEGINDLL*     = 57;
  X2C_INIT_HISTORY* = 58;
  X2J_TRUNCI64*     = 59;
  X2C_PROFILE_PROC_START  *= 60;
  X2C_PROFILE_PROC_START_C*= 61;
  X2C_PROFILE_PROC_END    *= 62;
  X2C_PROFILE_NEST_START  *= 63;
  X2C_PROFILE_NEST_END    *= 64;
  X2C_PROFILE_BEGIN_MODULE*= 65;
  X2C_INIT_PROFILER       *= 66;
  X2J_MUL64               *= 67;
  X2J_DIV64*         = 68;
  X2J_REM64*         = 69;
  X2J_SHL64*         = 70;
  X2J_SHR64*         = 71;
  X2J_USHR64*        = 72;
  X2C_DYNCALLOCATE*  = 73;
  X2C_DYNCDEALLOCATE*= 74;
  X2J_UDIV64*        = 75;
  X2J_UREM64*        = 76;
<* IF (TARGET_RISC OR TARGET_SPARC) THEN *>
  X2C_EXIT                *= 77;
  LAST_PROC = X2C_EXIT;
<* ELSE *>
  LAST_PROC = 76;
<* END *>

VAR
  procs : ARRAY (LAST_PROC+1) OF pc.OBJECT;

PROCEDURE std_func_proto(npar: INTEGER;
                           ty: ir.TypeType;
                           sz: ir.SizeType;
                           RtoL: BOOLEAN): pr.ProtoNum;
  VAR pt: pr.ProtoNum; P: pr.Proto;
BEGIN
  pr.NewProto(pt, P);
  P.ret_type := ty;
  P.ret_size := sz;
  P.right_to_left := RtoL;  (* порядок вычисления аргументов *)
  P.lang_flag := pc.flag_c;       (* -- временно !! *)
  P.rtn      := FALSE;
  P.ext_rtn  := FALSE;
  P.seq      := FALSE;
  P.nbase    := 0;
  P.bases    := pr.Bases{};
  P.npar     := npar;
  IF npar # 0 THEN NEW(P.par, npar) END;
  RETURN pt
END std_func_proto;

PROCEDURE std_proc_proto(npar: INTEGER): pr.ProtoNum;
BEGIN
  RETURN std_func_proto(npar, ir.t_void, 0, TRUE);
END std_proc_proto;

PROCEDURE std_param(pt   : pr.ProtoNum;
                    n    : INTEGER;
                    mode : pr.ParamMode; (* вид параметра: pm_*, см. CONST *)
                    ind  : SHORTINT;     (* признак REF или номер размерности *)
                    type : ir.TypeType;  (* тип параметра ?? *)
                    size : ir.SizeType;  (* размер параметра ?? *)
                    where: pr.MemType;   (* где находится параметр *)
                    offs : LONGINT       (* смещение на стеке *)
                   );
BEGIN
  IF mode = pr.pm_return THEN
    pr.ProtoList[pt].rtn := TRUE;
    pr.ProtoList[pt].ext_rtn := TRUE;
  END;
  pr.ProtoList[pt].par[n-1].mode := mode; (* вид параметра: pm_*, см. CONST *)
  pr.ProtoList[pt].par[n-1].ind  := ind;  (* для param: (ind # 0) - нужен адрес *)
                                          (* для len:    ind - номер измерения *)
  pr.ProtoList[pt].par[n-1].type := type;      (* размер параметра ??    *)
  pr.ProtoList[pt].par[n-1].size := size;      (* размер параметра ??    *)
  pr.ProtoList[pt].par[n-1].where:= where;     (* где находится параметр *)
  pr.ProtoList[pt].par[n-1].offs := offs;      (* смещение на стеке      *)
END std_param;

VAR (* ----------- r t s - v a r i a b l e s ---------- *)
  x2c_td_null*  : pc.OBJECT;
  x2c_td_ptr*   : pc.OBJECT;

PROCEDURE new_std_var(name-: ARRAY OF CHAR;
                       type: pc.STRUCT;
                       mode: pc.OB_MODE): pc.OBJECT;
  VAR o: pc.OBJECT;
     nm: pc.STRING;
BEGIN
  nm := at.make_name(name);
  o := at.new_work_object(nm, type, x2c, mode, TRUE);
  o.flag := pc.flag_c;
  RETURN o
END new_std_var;

PROCEDURE make_std_proc(N: INT): pc.OBJECT;
 VAR nm: pc.STRING;
   proto: pr.ProtoNum;
   o: pc.OBJECT;
   p: pr.ProcNum;
BEGIN
  CASE N OF
(*
  | X2C_NATIVE_BEGIN: (* --------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_NATIVE_BEGIN");
*)

  | X2C_BEGIN: (* ---------------------------------------------------------- *)
     proto := std_proc_proto(5);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+12);
     std_param(proto, 5, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+16);
     nm := at.make_name("X2C_BEGIN");

  | X2C_BEGINDLL: (* ------------------------------------------------------- *)
     proto := std_func_proto(3, tune.bool_ty, tune.bool_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     nm := at.make_name("X2C_BEGINDLL");

  | X2C_MODULE: (* --------------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_MODULE");

  | X2C_NEW_OPEN: (* ------------------------------------------------------- *)
     proto := std_proc_proto(6);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START+12);
     std_param(proto, 5, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+16);
     std_param(proto, 6, pr.pm_param, by_val, tune.bool_ty, tune.bool_sz,
                                                 pr.STACK, tune.PARAM_START+20);
     nm := at.make_name("X2C_NEW_OPEN");

  | X2C_DYNALLOCATE: (* ---------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_DYNALLOCATE");

  | X2C_DISPOSE: (* -------------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_DISPOSE");

  | X2C_DYNDEALLOCATE: (* -------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_DYNDEALLOCATE");

  | X2C_NEW: (* ------------------------------------------------------------ *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, tune.bool_ty, tune.bool_sz,
                                                 pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_NEW");

  | X2C_ALLOCATE: (* ------------------------------------------------------- *)
     proto := std_proc_proto(2);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     nm := at.make_name("X2C_ALLOCATE");

  | X2C_DEALLOCATE: (* ----------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_DEALLOCATE");

  | X2C_PROTECT: (* -------------------------------------------------------- *)
     proto := std_proc_proto(2);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.protect_ty, tune.protect_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     nm := at.make_name("X2C_PROTECT");

  | X2C_XRETRY: (* --------------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_XRETRY");

  | X2C_XREMOVE: (* -------------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_XREMOVE");

  | X2C_XOFF: (* ----------------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_XOFF");

  | X2C_XON: (* ------------------------------------------------------------ *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_XON");

  | X2C_FINALLY: (* -------------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, tune.proc_ty, tune.proc_sz,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_FINALLY");

  | X2C_XInitHandler: (* --------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_XInitHandler");

  | X2C_setjmp: (* --------------------------------------------------------- *)
     proto := std_func_proto(1, tune.index_ty, tune.index_sz, TRUE);
     pr.place_result(proto, pr.EAX, 0);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_setjmp");

  | X2C_HALT: (* ----------------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_HALT");

<* IF (TARGET_RISC OR TARGET_SPARC) THEN *>
  | X2C_EXIT: (* ----------------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_EXIT");
<* END *>

  | X2C_AND: (* ------------------------------------------------------------ *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param,  by_val, ir.t_unsign,  2,
                                                  pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_AND");

  | X2C_XOR: (* ------------------------------------------------------------ *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param,  by_val, ir.t_unsign,  2,
                                                  pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_XOR");

  | X2C_OR: (* ------------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param,  by_val, ir.t_unsign,  2,
                                                  pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_OR");

  | X2C_BIC: (* ------------------------------------------------------------ *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param,  by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param,  by_val, ir.t_unsign,  2,
                                                  pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_BIC");

  | X2C_COMPLEMENT: (* ----------------------------------------------------- *)
     proto := std_proc_proto(3);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_unsign,  2,
                                                 pr.STACK, tune.PARAM_START+8);
     nm := at.make_name("X2C_COMPLEMENT");

  | X2C_LENGTH: (* --------------------------------------------------------- *)
     proto := std_func_proto(2, tune.index_ty, tune.index_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_LENGTH");

  | X2C_ENTIER: (* --------------------------------------------------------- *)
     proto := std_func_proto(1, ir.t_int, 4, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                 pr.STACK, tune.PARAM_START);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_ENTIER");

  | X2C_CAP: (* ------------------------------------------------------------ *)
     proto := std_func_proto(1, tune.char_ty, tune.char_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.char_ty, tune.char_sz,
                                                 pr.STACK, tune.PARAM_START);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_CAP");

  | X2C_STRCMP_PROC: (* ---------------------------------------------------- *)
     proto := std_func_proto(4, ir.t_int, tune.index_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+12);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_STRCMP_PROC");

  | X2C_SET_EQU: (* -------------------------------------------------------- *)
     proto := std_func_proto(3, tune.bool_ty, tune.bool_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_unsign, 2,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_SET_EQU");

  | X2C_SET_LEQ: (* -------------------------------------------------------- *)
     proto := std_func_proto(3, tune.bool_ty, tune.bool_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_unsign, 2,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_SET_LEQ");

  | X2C_INCL: (* ----------------------------------------------------------- *)
     proto := std_func_proto(3, tune.addr_ty, tune.addr_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK,  tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_unsign, 2,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_INCL");

  | X2C_EXCL: (* ----------------------------------------------------------- *)
     proto := std_func_proto(3, tune.addr_ty, tune.addr_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK,  tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_unsign, 2,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_EXCL");

  | X2C_LONGSET: (* -------------------------------------------------------- *)
     proto := std_func_proto(4, tune.addr_ty, tune.addr_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK,  tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.index_ty,tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, ir.t_unsign, 2,
                                                 pr.STACK, tune.PARAM_START+12);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_LONGSET");

  | X2C_ROT: (* ------------------------------------------------------------ *)
     proto := std_func_proto(3, ir.t_unsign, tune.BITSET_LEN DIV 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_unsign, tune.BITSET_LEN DIV 8,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_ROT");

  | X2C_LSH: (* ------------------------------------------------------------ *)
     proto := std_func_proto(3, ir.t_unsign, tune.BITSET_LEN DIV 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_unsign, tune.BITSET_LEN DIV 8,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_LSH");

  | X2C_ROTL: (* ----------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_ROTL");

  | X2C_LSHL: (* ----------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_LSHL");

  | MEM_CMP: (* ------------------------------------------------------------ *)
     proto := std_func_proto(3, tune.index_ty, tune.index_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("memcmp");

  | X2C_EXPCI: (* ---------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_re, by_val, ir.t_float, tune.real_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_im, by_val, ir.t_float, tune.real_sz,
                                                  pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                  pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C__EXPCI");

  | X2C_EXPCR: (* ---------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_re, by_val, ir.t_float, tune.real_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_im, by_val, ir.t_float, tune.real_sz,
                                                  pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                  pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C__EXPCR");

  | X2C_EXPLI: (* ---------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_re, by_val, ir.t_float, tune.longreal_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_im, by_val, ir.t_float, tune.longreal_sz,
                                                  pr.STACK, tune.PARAM_START+12);
     std_param(proto, 4, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                  pr.STACK, tune.PARAM_START+20);
     nm := at.make_name("X2C__EXPLI");

  | X2C_EXPLR: (* ---------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_return, by_val, tune.addr_ty, tune.addr_sz,
                                                  pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_re, by_val, ir.t_float, tune.longreal_sz,
                                                  pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_im, by_val, ir.t_float, tune.longreal_sz,
                                                  pr.STACK, tune.PARAM_START+12);
     std_param(proto, 4, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                  pr.STACK, tune.PARAM_START+20);
     nm := at.make_name("X2C__EXPLR");

  | X2C_EXPRI: (* ---------------------------------------------------------- *)
     proto := std_func_proto(2, ir.t_float, tune.longreal_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.ST0, 0);
     nm := at.make_name("X2C_EXPRI");

  | X2C_EXPRR: (* ---------------------------------------------------------- *)
     proto := std_func_proto(2, ir.t_float, tune.longreal_sz, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.ST0, 0);
     IF at.MC = at.NATIVE THEN nm := at.make_name("X2C_EXPRR");
     ELSE                      nm := at.make_name("pow");
     END;

  | X2C_COPY: (* ----------------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, tune.index_ty, tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_COPY");

  | X2C_MEMSET: (* --------------------------------------------------------- *)
     proto := std_proc_proto(3);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, tune.char_ty, tune.char_sz,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_val, tune.index_ty,tune.index_sz,
                                                 pr.STACK, tune.PARAM_START+8);
     nm := at.make_name("X2C_MEMSET");

  | X2C_TRUNCI: (* --------------------------------------------------------- *)
     proto := std_func_proto(3, ir.t_int, 4, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+12);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_TRUNCI");

  | X2C_TRUNCC: (* --------------------------------------------------------- *)
     proto := std_func_proto(3, ir.t_unsign, 4, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_unsign, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_unsign, 4,
                                                 pr.STACK, tune.PARAM_START+12);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2C_TRUNCC");

  | X2C_TRAP_NIL: (* ------------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_TRAP_NIL_C");

  | X2C_TRAP_RANGE: (* ----------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_TRAP_RANGE_C");

  | X2C_TRAP_INDEX: (* ----------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_TRAP_INDEX_C");

  | X2C_TRAP_DIV: (* ------------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_TRAP_DIV_C");

  | X2C_TRAP_OVERFL: (* ---------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_TRAP_OVERFL_C");

  | X2C_TRAP_F: (* --------------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_TRAP_FC");

  | X2C_ASSERT_F: (* ------------------------------------------------------- *)
     proto := std_proc_proto(3);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_ref, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 3, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_ASSERT_FC");

  | X2C_InitFPP: (* -------------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_InitFPP");

  | X2C_PROC_IN: (* -------------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROC_IN");

  | X2C_PROC_OUT: (* ------------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, tune.addr_ty, tune.addr_sz,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROC_OUT");

  | X2C_INIT_HISTORY: (* --------------------------------------------------- *)
     proto := std_proc_proto(0);
     nm := at.make_name("X2C_INIT_HISTORY");
  | X2J_TRUNCI64:
     proto := std_func_proto(1, ir.t_int, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_float, tune.longreal_sz,
                                                 pr.STACK, tune.PARAM_START);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_TRUNCI64");
  | X2C_PROFILE_PROC_START:
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROFILE_PROC_START");
  | X2C_PROFILE_PROC_START_C:
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROFILE_PROC_START_C");
  | X2C_PROFILE_PROC_END:
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROFILE_PROC_END");
  | X2C_PROFILE_NEST_START:
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROFILE_NEST_START");
  | X2C_PROFILE_NEST_END:
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROFILE_NEST_END");
  | X2C_PROFILE_BEGIN_MODULE:
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_PROFILE_BEGIN_MODULE");
  | X2C_INIT_PROFILER:
     proto := std_proc_proto(1);
     nm := at.make_name("X2C_INIT_PROFILER");
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START);
  | X2J_MUL64:
     proto := std_func_proto(2, ir.t_int, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_MUL64");
  | X2J_DIV64:
     proto := std_func_proto(2, ir.t_int, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_DIV64");

  | X2J_UDIV64:
     proto := std_func_proto(2, ir.t_unsign, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_unsign, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_unsign, 8, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_UDIV64");

  | X2J_REM64:
     proto := std_func_proto(2, ir.t_int, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_REM64");

  | X2J_UREM64:
     proto := std_func_proto(2, ir.t_unsign, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_unsign, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_unsign, 8, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_UREM64");

  | X2J_SHL64:
     proto := std_func_proto(2, ir.t_int, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_SHL64");

  | X2J_SHR64:
     proto := std_func_proto(2, ir.t_int, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_SHR64");

  | X2J_USHR64:
     proto := std_func_proto(2, ir.t_int, 8, TRUE);
     std_param(proto, 1, pr.pm_param, by_val, ir.t_int, 8, pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4, pr.STACK, tune.PARAM_START+8);
     pr.place_result(proto, pr.EAX, 0);
     nm := at.make_name("X2J_USHR64");

  | X2C_DYNCALLOCATE: (* ---------------------------------------------------- *)
     proto := std_proc_proto(4);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     std_param(proto, 2, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+4);
     std_param(proto, 3, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START+8);
     std_param(proto, 4, pr.pm_param, by_val, ir.t_int, 4,
                                                 pr.STACK, tune.PARAM_START+12);
     nm := at.make_name("X2C_DYNCALLOCATE");

  | X2C_DYNCDEALLOCATE: (* -------------------------------------------------- *)
     proto := std_proc_proto(1);
     std_param(proto, 1, pr.pm_param, by_ref, ir.t_ref, 4,
                                                 pr.STACK, tune.PARAM_START);
     nm := at.make_name("X2C_DYNCDEALLOCATE");
  END;
 <* IF TARGET_RISC OR TARGET_SPARC THEN *>
  pr.EvalProto(proto);
 <* END *>
  o := at.new_work_object(nm, NIL, x2c, pc.ob_xproc, TRUE);
  p := pr.NewProc(o);
  pr.ProcList[p].tags := {pr.external};
  pr.ProcList[p].proto_no := proto;
  at.app_info(o, at.a_self, ir.y_ProcConst, VAL(at.InfExtName, p), NIL, 0);
  o.flag := pc.flag_c;
  RETURN o
END make_std_proc;

PROCEDURE Proc*(N: INT): pc.OBJECT;
  VAR o: pc.OBJECT;
BEGIN
  o := procs[N];
  IF o = NIL THEN
    o := make_std_proc(N);
    procs[N] := o;
  END;
  RETURN o
END Proc;

PROCEDURE Ini*(addr_type, set_table_type: pc.STRUCT);
  VAR
    i: INT;
BEGIN
  at.Ini;
  pr.Ini;

  ModuleProtoNum := std_proc_proto(0);
  pr.ProtoList[ModuleProtoNum].lang_flag := pc.flag_m2;
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  pr.EvalProto(ModuleProtoNum);
<* END *>

  MainProtoNum := std_proc_proto(2);
  std_param(MainProtoNum, 1, pr.pm_param, by_val, ir.t_int, 4, pr.STACK, tune.PARAM_START);
  std_param(MainProtoNum, 2, pr.pm_param, by_val, ir.t_ref, 4, pr.STACK, tune.PARAM_START+4);
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  pr.EvalProto(MainProtoNum);
<* END *>

  DLLMainProtoNum := std_proc_proto(0);
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  pr.EvalProto(DLLMainProtoNum);
<* END *>

<* IF TARGET_386 THEN *>
  AsmCodeProtoNum := std_proc_proto(0);
  pr.ProtoList[AsmCodeProtoNum].lang_flag := pc.flag_m2;
<* END *>

  make_X2C;

 (* -------------- v a r i a b l e s --------------------- *)
  x2c_td_null := new_std_var("x2c_td_null", addr_type, pc.ob_var);
  x2c_td_ptr  := new_std_var("x2c_td_ptr", addr_type, pc.ob_var);
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  at.GlobTOC := new_std_var(".toc", addr_type, pc.ob_var);
<* END *>

  at.incl_table  := new_std_var("X2C_INCLs", set_table_type, pc.ob_cons);
  at.excl_table  := new_std_var("X2C_EXCLs", set_table_type, pc.ob_cons);
  at.incl_table_hi  := new_std_var("X2C_INCLs_HI", set_table_type, pc.ob_cons);
  at.excl_table_hi  := new_std_var("X2C_EXCLs_HI", set_table_type, pc.ob_cons);
  at.loset_table := new_std_var("X2C_LOSETs", set_table_type, pc.ob_cons);
  at.hiset_table := new_std_var("X2C_HISETs", set_table_type, pc.ob_cons);

 (* -------------- p r o c e d u r e s --------------------- *)
  FOR i := 0 TO LAST_PROC DO procs[i] := NIL END;
END Ini;

PROCEDURE Exi*;
  VAR i : INT;
BEGIN
  FOR i := 0 TO LAST_PROC DO procs[i] := NIL END;
  x2c         := NIL;
  x2c_td_null := NIL;
  x2c_td_ptr  := NIL;
  pr.Exi;
  at.Exi;
END Exi;

END opStd.
