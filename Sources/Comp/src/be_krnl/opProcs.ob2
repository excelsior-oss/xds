<* NEW db_procs- *>
MODULE opProcs;
IMPORT
<* IF db_procs THEN *> io := opIO, <* END *>
       ir,
       pc := pcK,
     tune := opTune,
      cd := CodeDef,
<* IF TARGET_SPARC THEN *>
    SPARCDefs,
<* ELSIF TARGET_RISC THEN *>
       ArgRes,
       TOC,
<* ELSIF TARGET_386 THEN *>
       AsmX86,
       desc386,
<* END *>
       env := xiEnv,
       at := opAttrs;
IMPORT SYSTEM;
<* IF TARGET_RISC THEN *>
TYPE RegMask = ArgRes.RegMask;
         Reg *= [0..31];
<* ELSIF TARGET_SPARC THEN *>
TYPE RegMask = SPARCDefs.RegMask;
         Reg *= [0..31];
<* ELSIF TARGET_386 THEN *>
TYPE RegMask *= desc386.RegSet;
     Reg *= desc386.Reg;
<* ELSE *>
    TYPE RegMask *= SET;
         Reg *= [0..31];
<* END *>
CONST EmptyRegMask = RegMask {};

TYPE INT = ir.INT;

TYPE Language = pc.Lang;
CONST
  Oberon*    = pc.flag_o2;
  Modula*    = pc.flag_m2;
  C_call*    = pc.flag_c;
  Pascal*    = pc.flag_p;
  Sys_call*  = pc.flag_syscall;
  Std_call*  = pc.flag_stdcall;
  NativeLangs* = pc.LangSet{pc.flag_o2, pc.flag_m2, pc.flag_java};

VAR MustFillFrame* : BOOLEAN;

TYPE MemType* = SHORTINT;  (* _¤_ Ї_а_¤ _вбп Ї а ┐_ва *)
CONST
  (* ----- -  бв_Є_ бR б┐_й_-Ё_┐ --------- *)
  STACK* = MemType{ -1 };
  (* ----- ў а__Ёбва е ЇаRж_ббRа  -------- *)
  AL* = MemType{ 0 };       AX* = MemType{  8 };       EAX* = MemType{ 16 };
  CL* = MemType{ 1 };       CX* = MemType{  9 };       ECX* = MemType{ 17 };
  DL* = MemType{ 2 };       DX* = MemType{ 10 };       EDX* = MemType{ 18 };
  BL* = MemType{ 3 };       BX* = MemType{ 11 };       EBX* = MemType{ 19 };
  AH* = MemType{ 4 };       SP* = MemType{ 12 };       ESP* = MemType{ 20 };
  CH* = MemType{ 5 };       BP* = MemType{ 13 };       EBP* = MemType{ 21 };
  DH* = MemType{ 6 };       SI* = MemType{ 14 };       ESI* = MemType{ 22 };
  BH* = MemType{ 7 };       DI* = MemType{ 15 };       EDI* = MemType{ 23 };
  (* ----- ў а__Ёбва е бRЇаRж_ббRа  ------ *)
  ST0* = MemType{ 24 };
  ST1* = MemType{ 25 };
  ST2* = MemType{ 26 };
  ST3* = MemType{ 27 };
  ST4* = MemType{ 28 };
  ST5* = MemType{ 29 };
  ST6* = MemType{ 30 };
  ST7* = MemType{ 31 };

TYPE
    ParamMode* = (  (* виды параметров *)
        pm_return,                          (* адрес переменной для возвращаемого значения *)       
        pm_base,                            (* база одной из охват. процедур *)                     
        pm_param,                           (* параметр *)                                          
        _pm_op_array,                       (* гибкий массив - было закомментарено. Оставлено kevin'om только для того, чтобы не поплыли *)
        pm_len,                             (* длина параметра-гибкого массива по одному измер.*)   
        pm_formrec,                         (* VAR параметр - обероновская запись *)                
        pm_type,                            (* тип этой записи *)                                   
        pm_re,                              (* RE-часть комплексного параметра *)                   
        pm_im,                              (* IM-часть комплексного параметра *)                   
        pm_seq                              (* C-последовательность параметров *)                   
    );
    ParamModeSet *= PACKEDSET OF ParamMode;

TYPE (* field "ind" values for "pm_param" case *)
    By_* = (
        by_val,        (* parameter by value *)
        by_val_struct, (* structured parameter by val (SL-1) *)
        by_ref,        (* parameter by ref (VAR) *)
        by_ROref        (* parameter by read-only ref *)
    );

TYPE
  param* = RECORD
             mode* : ParamMode;   (* вид параметра: pm_*, см. CONST *)
             ind*  : SHORTINT;    (* для param: (ind # 0) - нужен адрес *)
                                  (* для len:    ind - номер измерения *)
                                  (* для base:   ind - число уровней вверх *)
             type* : ir.TypeType; (* тип параметра *)
             size* : ir.SizeType; (* размер параметра ?? *)
             where*: MemType;     (* где находится параметр *)
             offs* : LONGINT;     (* смещение на стеке *)
           END;

  params* = POINTER TO ARRAY OF param;

(* ------ l i s t   o f   p r o t o t y p e s -------- *)
TYPE
  ProcNum*  = at.ProcNum;
  ProtoNum* = ir.ProtoNum;  (* идея: хранить информацию о типе, размере и расположе-
                           нии результата процедуры в 0-ом параметре списка *)
                        (* еще одна - сделать в прототипе ссылку на соответ-
                           ствующий ему STRUCT                              *)
  Bases *= SET;

  Proto_rec =
     RECORD
       ret_type*      : ir.TypeType;  (* тип для триады call *)
       ret_size*      : ir.SizeType;
       where_res*     : MemType;  (* где находится результат *)
       offs_res*      : LONGINT;  (* смещение результата на стеке *)
       right_to_left* : BOOLEAN;  (* порядок вычисления аргументов *)
       lang_flag*     : Language; (* соглашения о связях -- временно !! *)
       rtn*           : BOOLEAN;  (* возврат результата через доп. параметр *)
       ext_rtn*       : BOOLEAN;  (* TRUE if ret 4/8/16 whould appear at the end of the function *)
       seq*           : BOOLEAN;  (* есть Си-шный SEQ-параметр *)
       nbase*         : SHORTINT; (* число дополнительных параметров - баз *)
(*??*) bases*         : Bases;      (* базы каких процедур следует передавать *)
       npar*          : INTEGER;  (* число параметров *)
       par*           : params;   (* параметры *)
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
       par_regs*      : RegMask;  (* множество регистров,
                                     используемых для передачи параметров
                                  *)
<* END *>
       obj*: pc.OBJECT;        -- это для запоминания имени виртуального метода
     END;

  Proto* = POINTER TO Proto_rec;

  ProtoListRef = POINTER TO ARRAY OF Proto;

VAR
  ProtoList* : ProtoListRef;
  NProto*    : ProtoNum;

<* IF TARGET_RISC OR TARGET_SPARC THEN *>

VAR
  EvalProto* : PROCEDURE (p: ProtoNum);  (* target-dependent processing *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE DummyEvalProto (p : ProtoNum);
BEGIN
END DummyEvalProto;
<* POP *>

VAR IsParInReg* : PROCEDURE (p : ProtoNum; NPar : INTEGER) : BOOLEAN;
                  (* target depending predicat,
                     defines location of NPar-th parameter*)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE DummyIsParInReg(p : ProtoNum; NPar : INTEGER) : BOOLEAN;
BEGIN
    RETURN FALSE;
END DummyIsParInReg;
<* POP *>

<* END *>

(* ------ l i s t   o f   p r o c e d u r e s -------- *)
CONST
  external* = 0;
  public*   = 1;
  has_info  = 2;    (* у процедуры есть back-end информация *)
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  has_calls = 3;    (* процедура содержит вызовы *)
<* END *>

TYPE
  proc_info* = RECORD
                 tags*     : SET;
                 BE_info   : RegMask;
                 name*     : ir.NameType;
                 obj*      : pc.OBJECT;
               <* IF TARGET_RISC THEN *>
                 desc*     : pc.OBJECT;    (* Procedure descriptor *)
               <* END *>
                 proto_no* : ProtoNum;
               END;

  ProcListRef = POINTER TO ARRAY OF proc_info;

VAR
  ProcList*    : ProcListRef;
  NProc*       : ProcNum;

PROCEDURE NewProc*(o: pc.OBJECT): ProcNum;
  VAR i: ProcNum; new: ProcListRef;
BEGIN
  IF NProc = LEN(ProcList^) THEN
    NEW(new, NProc+ProcNum{ 20 });
    FOR i := ir.ZEROProcNum TO SYSTEM.PRED(NProc) DO new[i] := ProcList[i] END;
    ProcList := new;
  END;
  ProcList[NProc].tags := {};
  ProcList[NProc].BE_info := EmptyRegMask;
  IF o # NIL THEN ProcList[NProc].name := o.name;
  ELSE            ProcList[NProc].name := NIL;
  END;
  ProcList[NProc].obj :=  o;
 <* IF TARGET_RISC THEN *>
  ProcList[NProc].desc := NIL;    (* Procedure descriptor *)
 <* END *>
  ProcList[NProc].proto_no := ir.INVProtoNum;
  INC(NProc);
  RETURN SYSTEM.PRED(NProc)
END NewProc;

PROCEDURE ProcNumByObj* (o: pc.OBJECT): ProcNum;
  VAR a: at.ATTR_EXT;
BEGIN
  ASSERT (o.mode IN (pc.PROCs + pc.OB_SET{pc.ob_module}));
  a := at.attr(o.ext, at.a_self);
  ASSERT (a(at.INFO_EXT).e_tag = ir.y_ProcConst);
  RETURN VAL(ProcNum, a(at.INFO_EXT).name);
END ProcNumByObj;

PROCEDURE ProcProtoNum* (p: ProcNum): ProtoNum;
BEGIN
  RETURN ProcList[p].proto_no
END ProcProtoNum;

PROCEDURE ProcName* (p: ProcNum): ir.NameType;
BEGIN
  RETURN ProcList[p].name
END ProcName;

PROCEDURE ProcObj* (num: ProcNum): pc.OBJECT;
BEGIN
  RETURN ProcList[num].obj
END ProcObj;

PROCEDURE NumParams*(p: ProcNum): INTEGER;
  VAR prot: Proto;
BEGIN
  prot := ProtoList[ProcList[p].proto_no];
  RETURN prot.npar
END NumParams;

PROCEDURE NumParamsByProto*(p: ProtoNum): INTEGER;
BEGIN
  RETURN ProtoList[p].npar
END NumParamsByProto;

PROCEDURE WhereParam*(p : ProtoNum; ParNum : LONGINT) : MemType;
BEGIN
  IF (ParNum >= ProtoList[p].npar) THEN
    RETURN STACK;
  ELSE
    RETURN ProtoList[p].par[ParNum].where;
  END;
END WhereParam;

PROCEDURE ResProc*(p: ProcNum;
               VAR where: MemType;
               VAR offs: LONGINT);
  VAR prot: Proto;
BEGIN
  prot := ProtoList[ProcList[p].proto_no];
  ASSERT(prot.ret_type # ir.t_void);
  where := prot.where_res;
  offs := prot.offs_res;
(*
  IF npar # 0 THEN
    ofs := prot.par[prot.npar-1];
  ELSE
    ofs := tune.PARAM_START;
  END;
  WHILE (ofs MOD 4) # 0 DO INC(ofs) END:
*)
END ResProc;

PROCEDURE LenParams*(proto_no: ir.ProtoNum): LONGINT;
  VAR i: INT;
    of, of1: LONGINT;
    prot: Proto; par : params;
BEGIN
  prot := ProtoList[proto_no];
  par := prot.par;
  of := tune.PARAM_START;
  FOR i := 0 TO prot.npar-1 DO
    IF (par[i].where = STACK) THEN
      of1 := par[i].offs + ORD(par[i].size);
      IF of < of1 THEN of := of1 END;
    END;
  END;
  WHILE (of MOD 4) # 0 DO INC(of) END;
<* IF db_procs THEN *>
  io.print("LenParam (%d) = %d\n", proto_no, of-tune.PARAM_START);
<* END *>
  RETURN (of - tune.PARAM_START)
END LenParams;

(* --------- с в о й с т в а   п р о ц е д у р ы ------------------ *)

(*
PROCEDURE get_loc_offs* (p: pc.OBJECT): LONGINT;
BEGIN
  IF at.omark_locals_used IN p.marks THEN

  ELSE RETURN tune.LOCAL_START
  END;
END get_loc_offs;

PROCEDURE set_loc_offs* (p: pc.OBJECT; offs: LONGINT);
BEGIN
  IF at.omark_locals_used IN p.marks THEN

  ELSE
  END;
END set_loc_offs;
*)

(* --------- B a c k - e n d   i n f o r m a t i o n -------------- *)

PROCEDURE IsBackEndInfo* (n: ProcNum): BOOLEAN;
BEGIN
  RETURN (has_info IN ProcList[n].tags);
END IsBackEndInfo;

PROCEDURE SetBackEndInfo* (n: ProcNum; x: RegMask);
BEGIN
  INCL(ProcList[n].tags, has_info);
  ProcList[n].BE_info := x;
END SetBackEndInfo;

PROCEDURE GetBackEndInfo* (n: ProcNum; VAR x: RegMask);
BEGIN
  ASSERT(has_info IN ProcList[n].tags);
  x := ProcList[n].BE_info;
END GetBackEndInfo;

<* IF TARGET_RISC OR TARGET_SPARC THEN *>

PROCEDURE HasCalls* (n: ProcNum): BOOLEAN;
BEGIN
  RETURN (has_calls IN ProcList[n].tags);
END HasCalls;

PROCEDURE SetHasCalls* (n: ProcNum);
BEGIN
  INCL(ProcList[n].tags, has_calls);
END SetHasCalls;

<* END *>

(* ------- Локал служит для обращения к константе -------------------------- *)

PROCEDURE LocalIsConstant* (l: ir.Local): BOOLEAN;
  VAR o: pc.OBJECT;
BEGIN
  o := ir.Locals[l].Obj;
  RETURN (o # NIL) & (o.mode = pc.ob_cons)
END LocalIsConstant;

(* ------- Кодовые процедуры ----------------------------------------------- *)

PROCEDURE IsCodeProc* (p: ProcNum): BOOLEAN;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  RETURN (o.mode = pc.ob_cproc)
END IsCodeProc;

<* IF TARGET_386 THEN *>

VAR locals_offset : LONGINT;

PROCEDURE GetOffs(o: pc.OBJECT): LONGINT;
  VAR a: at.ATTR_EXT;
   offs: LONGINT;
BEGIN
  a := at.attr(o.ext, at.a_self);
  IF o.mode = pc.ob_field THEN
    offs := a(at.INFO_EXT).offs;
  ELSE
    ASSERT ((o.lev>0) & (o.host = at.curr_proc.type));
    offs := ir.Locals[VAL(ir.VarNum, a(at.INFO_EXT).name)].Offset + locals_offset;
  END;
  RETURN offs
END GetOffs;

PROCEDURE GetCodeProc* (p: ProcNum; loc_offs: LONGINT): cd.CODE_SEGM;
  VAR o: pc.OBJECT;
    sg : cd.CODE_SEGM;
BEGIN
  o := ProcObj(p);
  IF (o = NIL) OR (o.val = NIL) OR (o.val.mode = pc.nd_aggregate) THEN
    env.errors.Error(o.val.pos, 922); -- code procedures not supported
    cd.new_segm(sg); -- return empty segment
    RETURN sg;
  ELSE
    locals_offset := loc_offs;
    sg := AsmX86.Gen (o.val.val, GetOffs);
    RETURN sg
  END;
END GetCodeProc;

<* ELSE *>

<* PUSH *>
<* WOFF301+ *>
PROCEDURE GetCodeProc* (p: ProcNum; loc_offs: LONGINT): cd.CODE_SEGM;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  RETURN cd.get_ready(o)
END GetCodeProc;
<* POP *>

<* END *>

(* ------- Процедура является вложенной в другую --------------------------- *)

PROCEDURE IsNested* (p: ProcNum): BOOLEAN;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  RETURN o.lev > at.curr_proc.lev
END IsNested;

(* ------- Процедура доступна только внутри модуля ------------------------- *)

PROCEDURE IsInternal* (p: ProcNum): BOOLEAN;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  RETURN (o.mode = pc.ob_proc)
END IsInternal;

(* ------- Процедура из другого модуля ------------------------- *)

PROCEDURE IsExternal* (p: ProcNum): BOOLEAN;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  RETURN (o.mno # at.curr_mno) OR (o.mode = pc.ob_eproc)
END IsExternal;

(* ------- Процедура генерируется нашим back-end'ом ------------------------ *)

PROCEDURE IsOurProc* (p: ProcNum): BOOLEAN;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  RETURN (o.mode <> pc.ob_eproc) & (o.mno = at.curr_mno);
END IsOurProc;

(* ------- generate special header for SL1 procedure ------------------------ *)

(* ------- get procedure descriptor (it will be created if nothing) --------- *)

<* IF TARGET_RISC THEN *>

PROCEDURE ProcDesc * (prc: pc.OBJECT) : pc.OBJECT;
  VAR pn: ProcNum; dsc: pc.OBJECT; old, sg: cd.CODE_SEGM;
BEGIN
  IF at.ABI # at.PowerOpen THEN RETURN prc END;
  pn := ProcNumByObj (prc);
  dsc := ProcList[pn].desc;
  IF dsc = NIL THEN
    dsc := at.new_work_object(prc.name, NIL, pc.mods[prc.mno].type, pc.ob_cons, TRUE);
    dsc.flag := prc.flag;
    dsc.tags := prc.tags;
    INCL(dsc.marks,at.omark_procdesc);
    IF prc.mode = pc.ob_eproc THEN
      dsc.mno := tune.x2c_mno;
    ELSIF prc.mno = at.curr_mno THEN
      cd.get_segm(old);
      cd.new_segm(sg); cd.set_segm(sg);
      cd.gen_fixup(prc, 0, cd.fx_obj32);
      cd.gen_fixup(at.GlobTOC, 0, cd.fx_obj32);
      cd.set_ready(dsc, sg);
      cd.set_segm(old);
    END;
    ProcList[pn].desc := dsc;
    TOC.Add(dsc);
  END;
  RETURN dsc
END ProcDesc;

PROCEDURE FindProcByDesc (dsc: pc.OBJECT) : pc.OBJECT;
  VAR pn: ProcNum;
BEGIN
  IF at.ABI # at.PowerOpen THEN
    ASSERT (dsc.mode IN pc.PROCs);
    RETURN dsc
  ELSE
    pn := ir.ZEROProcNum;
    LOOP
      ASSERT(pn < NProc);
      IF ProcList[pn].desc = dsc THEN
        RETURN ProcList[pn].obj
      END;
      INC(pn);
    END;
  END;
END FindProcByDesc;

<* END *> -- TARGET_RISC

(* ------- Язык процедуры --------------------------------------------------- *)

PROCEDURE ProcNameLang* (p: ProcNum): Language;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  RETURN o.flag
END ProcNameLang;

PROCEDURE ProcCallLang* (p: ProcNum): Language;
  VAR o: pc.OBJECT;
BEGIN
  o := ProcObj(p);
  IF (o = at.curr_mod) & at.main THEN
    RETURN pc.flag_c    (* to make a main module entry point C-RTS compatible *)
  ELSE
    RETURN o.type.flag
  END;
END ProcCallLang;

PROCEDURE HasAccess* (p: ProcNum; l: ir.Local): BOOLEAN;
  VAR o,v: pc.OBJECT; u: pc.USAGE;
BEGIN
  v := ir.Locals[l].Obj;
  o := ProcObj(p);
  u := o.type.use;
  LOOP
    IF u = NIL THEN RETURN FALSE END;
    IF (NOT (pc.utag_extended IN u.tags)) AND (u.obj = v) THEN RETURN TRUE END;
    u := u.next;
  END;
END HasAccess;

PROCEDURE NestedUseLocals* (): BOOLEAN;
BEGIN
  RETURN at.curr_proc.marks * pc.OMARK_SET{at.omark_nested_read, at.omark_nested_write} # pc.OMARK_SET{}
END NestedUseLocals;

PROCEDURE ParamLoc*(n: INT): ir.Local;  (* local's number corresponding *)
  VAR l: ir.Local;                      (* to the number of a parameter *)
    offs: LONGINT;
    ptno: ProtoNum;
BEGIN
  ptno := ProcList[at.curr_procno].proto_no;
  offs := ProtoList[ptno].par[n].offs;
  l := ir.ZEROVarNum;
  LOOP
    IF l >= ir.NLocals THEN RETURN ir.UNDEFINED END;
    IF ir.Locals[l].Offset = offs THEN RETURN l END;
    INC(l);
  END;
END ParamLoc;

PROCEDURE ParamOffs*(n: INT): LONGINT;    (* parameter's offset *)
  VAR ptno: ProtoNum;
BEGIN
--  io.print("ParamOffs(%d) %d - %s\n", n, at.curr_procno, at.curr_proc.name^);
  ptno := ProcList[at.curr_procno].proto_no;
  RETURN ProtoList[ptno].par[n].offs;
END ParamOffs;

PROCEDURE ProcParamType * (proc_prot: ir.ProtoNum;
                           parm_no:   INT
                          ) : ir.TypeType;    (* parameter's type *)
BEGIN
  RETURN ProtoList[proc_prot].par[parm_no].type
END ProcParamType;

PROCEDURE ProcParamRO * (proc_prot: ir.ProtoNum; parm_no: INT) : BOOLEAN;
  VAR mode : ParamMode; ind : SHORTINT;
BEGIN
  mode := ProtoList[proc_prot].par[parm_no].mode;
  IF mode IN ParamModeSet{pm_type, pm_len, pm_re, pm_im} THEN
    RETURN TRUE
  ELSIF mode # pm_param THEN
    RETURN FALSE
  END;
  ind := ProtoList[proc_prot].par[parm_no].ind;
  RETURN (ind = ORD(by_ROref)) OR (ind = ORD(by_val_struct))
END ProcParamRO;

<* IF TARGET_SPARC OR TARGET_RISC OR TARGET_68K THEN *>
PROCEDURE ProcParamStructVal * (proc_prot: INTEGER; parm_no: INT) : BOOLEAN;
BEGIN
  RETURN FALSE;
END ProcParamStructVal;
<* END *>
(* ------- Complex parameters -------------------------------------- *)

PROCEDURE IsComplexParam * (l: ir.Local) : BOOLEAN;
  VAR v : pc.OBJECT;
BEGIN
  v := ir.Locals[l].Obj;
  RETURN (v <> NIL) & (v.type.mode IN pc.CPLXs)
                    & (v.lev > at.curr_proc.lev)    (* value parameters only *)
                    & (v.mode = pc.ob_var)
                    & (pc.otag_valpar IN v.tags)
END IsComplexParam;

PROCEDURE ImPart * (l: ir.Local) : ir.Local;
BEGIN
  ASSERT (IsComplexParam (l));
  RETURN SYSTEM.SUCC(l)                                  (* see opCode.get_param *)
END ImPart;

(* --------- з а в е д е н и е  п р о т о т и п а  ---------------- *)

PROCEDURE NewProto*(VAR pno: ProtoNum; VAR prt: Proto);
  VAR i: ProtoNum; new: ProtoListRef;
BEGIN
  IF NProto = LEN(ProtoList^) THEN
    NEW(new, NProto+ir.ProtoNum{20});
    FOR i := ir.ZEROProtoNum TO SYSTEM.PRED(NProto) DO new[i] := ProtoList[i] END;
    ProtoList := new;
  END;
  NEW(prt);
  (* commented by htayod                                 *)
  (* these flags must be set in every prototype manually *)
  (* and it's really done this way                       *)
  (*
  prt.right_to_left := TRUE;                         
  prt.lang_flag := pc.flag_m2;                       
  *)
  prt.obj := NIL;
  pno := NProto;
  ProtoList[NProto] := prt;
  INC(NProto);
END NewProto;

PROCEDURE place_result*(pt   : ProtoNum;
                        where: MemType;    (* где возвращается результат *)
                        offs : LONGINT);   (* его смещение, если на стеке *)
  VAR P: Proto;
BEGIN
  P := ProtoList[pt];
  ASSERT(P.ret_type # ir.t_void);
  P.where_res := where;
  P.offs_res := offs;
END place_result;

PROCEDURE LangByProto*(pt: ProtoNum): Language;
BEGIN
<* IF db_procs THEN *> io.print("LangbyProto(%d)\n", pt); <* END *>
  RETURN ProtoList[pt].lang_flag
END LangByProto;

PROCEDURE SeqProto*(pt: ProtoNum) : BOOLEAN;
BEGIN
<* IF db_procs THEN *> io.print("SeqProto(%d)\n", pt); <* END *>
  RETURN ProtoList[pt].seq
END SeqProto;

PROCEDURE SeqProc*(p: ProcNum) : BOOLEAN;
BEGIN
<* IF db_procs THEN *> io.print("SeqProto(%d)\n", p); <* END *>
  RETURN SeqProto(ProcList[p].proto_no)
END SeqProc;

(* ---------  и н и ц и а л и з а ц и я  ---------------- *)
PROCEDURE Ini*;
BEGIN
  NProto := ir.ZEROProtoNum;  NEW(ProtoList, 30);
  NProc := ir.ZEROProcNum;   NEW(ProcList, 30);
END Ini;

PROCEDURE Exi*;
BEGIN
  ProtoList := NIL;
  ProcList   := NIL;
END Exi;

(* ---------- в и з у а л и з а ц и я --------------- *)
<* IF db_procs THEN *>

PROCEDURE WrType (t: ir.TypeType; s: ir.SizeType);
BEGIN
  CASE t OF
  | ir.t_void:   io.print("void(%d)",s);
  | ir.t_int:    io.print("int(%d)",s);
  | ir.t_unsign: io.print("unsign(%d)",s);
  | ir.t_float:  io.print("float(%d)",s);
  | ir.t_complex:io.print("complex(%d)",s);
  | ir.t_ref:    io.print("ref(%d)",s);
  | ir.t_arr:    io.print("arr(%d)",s);
  | ir.t_rec:    io.print("rec(%d)",s);
  | ir.t_flxarr: io.print("flxarr(%d)",s);
  ELSE           io.print("?%d(%d)",t,s);
  END;
END WrType;

PROCEDURE WrPMode(md: ParamMode);
BEGIN
  CASE md OF
  | pm_return  :  io.print("_return");
  | pm_base    :  io.print("_base");
  | pm_param   :  io.print("_param");
  | pm_len     :  io.print("_len");
  | pm_formrec :  io.print("_formrec");
  | pm_type    :  io.print("_type");
  | pm_re      :  io.print("_re");
  | pm_im      :  io.print("_im");
  | pm_seq     :  io.print("_seq");
  ELSE            io.print("_??(%d)", md);
  END;
END WrPMode;

PROCEDURE WrPWhere(wh: MemType; ofs: LONGINT);
BEGIN
  CASE wh OF
  | STACK: io.print("STACK(%d)", ofs);
  | AL:    io.print("AL");
  | CL:    io.print("CL");
  | DL:    io.print("DL");
  | BL:    io.print("BL");
  | AH:    io.print("AH");
  | CH:    io.print("CH");
  | DH:    io.print("DH");
  | BH:    io.print("BH");
  | AX:    io.print("AX");
  | CX:    io.print("CX");
  | DX:    io.print("DX");
  | BX:    io.print("BX");
  | SP:    io.print("SP");
  | BP:    io.print("BP");
  | SI:    io.print("SI");
  | DI:    io.print("DI");
  | EAX:   io.print("EAX");
  | ECX:   io.print("ECX");
  | EDX:   io.print("EDX");
  | EBX:   io.print("EBX");
  | ESP:   io.print("ESP");
  | EBP:   io.print("EBP");
  | ESI:   io.print("ESI");
  | EDI:   io.print("EDI");
  | ST0:   io.print("ST0");
  | ST1:   io.print("ST1");
  | ST2:   io.print("ST2");
  | ST3:   io.print("ST3");
  | ST4:   io.print("ST4");
  | ST5:   io.print("ST5");
  | ST6:   io.print("ST6");
  | ST7:   io.print("ST7");
  ELSE     io.print("?%d(%d)", wh, ofs);
  END;
END WrPWhere;

PROCEDURE WrParam*(p-: param);
BEGIN
  WrPMode(p.mode);
  io.print(", ind = %d, ", p.ind);
  WrType(p.type, p.size);
  WrPWhere(p.where, p.offs);
  io.print("\n");
END WrParam;

PROCEDURE WrProto*(n: ProtoNum);
  VAR prt: Proto; i: INTEGER;
BEGIN
  io.print("прототип %d: ", n);
  prt := ProtoList[n];
  io.print("ret_type/size = "); WrType(prt.ret_type, prt.ret_size);
  IF prt.right_to_left THEN io.print(", RtoL") END;
  CASE prt.lang_flag OF
  | pc.flag_o2       : io.print(", lang = O2");
  | pc.flag_m2       : io.print(", lang = M2");
  | pc.flag_c        : io.print(", lang = C");
  | pc.flag_sl1      : io.print(", lang = SL1");
  | pc.flag_p        : io.print(", lang = Pascal");
  | pc.flag_bnrp     : io.print(", lang = BNR Pascal");
  | pc.flag_syscall  : io.print(", lang = SysCall");
  | pc.flag_stdcall  : io.print(", lang = StdCall");
  | pc.flag_vmcall   : io.print(", lang = VmCall");
  | pc.flag_lightcall: io.print(", lang = LightCall");
  | pc.flag_javacall : io.print(", lang = JavaCall");
  END;
  IF prt.rtn THEN io.print(", rtn") END;
  IF prt.seq THEN io.print(", seq") END;
  IF prt.nbase # 0 THEN
    io.print(", nbase = %d, bases = %x", prt.nbase, prt.bases);
  END;
  io.print(", npar = %d\n", prt.npar);
  FOR i := 0 TO prt.npar-1 DO
    WrParam(prt.par[i])
  END;
END WrProto;

PROCEDURE WrProc(n: INT);
  VAR tags, BE_info: RegMask;
    name: ir.NameType;
    obj:  pc.OBJECT;
    proto_no: INTEGER;
    first : BOOLEAN; i : INT;
BEGIN
  tags := ProcList[n].tags;
  BE_info := ProcList[n].BE_info;
  name := ProcList[n].name;
  obj := ProcList[n].obj;
  proto_no := ProcList[n].proto_no;
  io.print("Proc(%d) ", n);
  IF name # NIL THEN io.print("'%s'", name^)
  ELSE               io.print("''");
  END;
  IF obj # NIL THEN
    io.print("(");
    IF obj.name # NIL THEN io.print("'%s'", obj.name^) END;
    io.print(")");
  END;
  io.print(" proto=%d; tags=%X, BE_info={", proto_no, tags);
  first := TRUE;
  FOR i := 0 TO MAX(RegMask) DO
    IF i IN BE_info THEN
      IF first THEN first := FALSE; io.print("%d", i);
      ELSE io.print(",%d", i);
      END;
    END;
  END;
  io.print("}\n");
END WrProc;

PROCEDURE show_prototype_list*;
  VAR i : INTEGER;
BEGIN
  io.print(" ========== prototype_list ==============\n");
  FOR i := 0 TO NProto-1 DO WrProto(i) END;
  io.print(" ========================================\n");
END show_prototype_list;

PROCEDURE show_proc_list*;
  VAR i : INT;
BEGIN
  io.print(" ========== procedure_list ==============\n");
  FOR i := 0 TO NProc-1 DO WrProc(i) END;
  io.print(" ========================================\n");
END show_proc_list;

<* END *>

PROCEDURE CallParamType(p : ir.TriadePtr; i : ir.INT) : ir.TypeType;
VAR q : ir.ParamPtr;
BEGIN
  IF i = 0 THEN RETURN ir.t_ref; END;
  q := p.Params^[i];
  IF (i-1 >= ProtoList[p^.Prototype].npar) OR
     (ProtoList[p^.Prototype].par[i-1].mode = pm_seq)
  THEN
    IF (q^.tag = ir.y_RealConst) OR
       (q^.tag = ir.y_Variable) &
       (ir.Vars^[q^.name].Def^.ResType = ir.t_float)
    THEN
      RETURN ir.t_float;
    ELSE
      RETURN tune.seq_item_type;
    END;
  ELSE
    RETURN ProtoList[p^.Prototype].par[i-1].type;
  END;
END CallParamType;

PROCEDURE CallParamSize(p : ir.TriadePtr; i : ir.INT) : ir.SizeType;
VAR q : ir.ParamPtr;
BEGIN
  IF i = 0 THEN RETURN tune.addr_sz; END;

  q := p.Params^[i];
  IF (i-1 >= ProtoList[p^.Prototype].npar) OR
     (ProtoList[p^.Prototype].par[i-1].mode = pm_seq)
  THEN
    IF (q^.tag = ir.y_RealConst) OR
       (q^.tag = ir.y_Variable) &
       (ir.Vars^[q^.name].Def^.ResType = ir.t_float)
    THEN
      RETURN tune.longreal_sz;;
    ELSIF ir.Vars^[q^.name].Def^.ResSize = 8 THEN
      RETURN 8;
    ELSE
      RETURN tune.seq_item_sz;;
    END;
  ELSE
    RETURN ProtoList[p^.Prototype].par[i-1].size;
  END;
END CallParamSize;

BEGIN
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  EvalProto := DummyEvalProto;
  IsParInReg := DummyIsParInReg;
<* END *>
<* IF TARGET_RISC THEN *>
  at.FindProcByDesc := FindProcByDesc;
<* END *>
  ir.CallParamType := CallParamType;
  ir.CallParamSize := CallParamSize;
END opProcs.
