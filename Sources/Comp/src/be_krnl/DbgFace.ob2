-- Замечание: см. open_write_array
-- все делается в предположении что после вызова TypeEmitter
-- write_type_cnt-1 указывает на последний записанный тип
-- Кстати, должно быть write_type_cnt = type_cnt!
-- Если TypeEmittr (ty_pointer, write, NIL) то в tindex_transfer
-- содержится индекс базы, а для open_array ВСЕГДА в tindex_transfer
-- содержится индекс базы этого массива

-- Метки:
--    at.tmark_db_index : на тип уже повешена метка  (pc.STRUCT)
--    at.a_dbg          : объект содержит отладочную информацию (pc.OBJECT)

MODULE DbgFace;

IMPORT sys := SYSTEM;

IMPORT cmd := CodeDef;
IMPORT pc  := pcK;
IMPORT at  := opAttrs;
IMPORT def := opDef;
IMPORT nms := ObjNames;
IMPORT str := Strings;
IMPORT env := xiEnv;
IMPORT opt := Options;
IMPORT reg := Registry;
IMPORT pcVisIR;
IMPORT pcO;
IMPORT DStrings;
IMPORT WholeStr;
IMPORT prc := opProcs;

TYPE
  INT8  = sys.INT8;
  INT16 = sys.INT16;
  INT32 = sys.INT32;


-- INTERFACE --

TYPE
  TYPE_TAG *= ( -- Сложные типы (опираются на другие), для них вызывается TypeEmitter.
               ty_start          -- Начало генерации отладочной информации.
             , ty_end            -- Конец генерации отладочной информации.
             , ty_range
             , ty_enum
             , ty_pointer
             , ty_reference      -- Искуственный тип, есть только в Dbg* модулях
                                 -- используется для генерации VAR параметров и
                                 -- открытых массивов.
             , ty_opaque         -- Скрытый тип.
             , ty_set
             , ty_proctype
             , ty_array
             , ty_array_of
             , ty_open_array     -- Используется только для параметров процедур,
                                 -- для каждого такого параметра генерируется свой
                                 -- объект этого типа (т.к. дескрипторы могут лежать
                                 -- в разных местах); вообщето это не тип, а свойство;
                                 -- всегда генерируется как ty_reference.
             , ty_SS             -- Строковый литерал.
             , ty_record
             , ty_module );

  SYMB_TAG *= ( -- Символы
               sy_start          -- Начало генерации отладочной информации.
             , sy_end            -- Конец генерации отладочной информации.
             , sy_var
             , sy_proc
             , sy_scope_open     -- Открытие области видимости
             , sy_scope_close    -- Закрытие области видимости
             , sy_local_var
             , sy_param );

  ACTION *= ( act_set, act_write );
  -- Процесс генерации отладочной информации имеет 2 стадии: разметка типов (act_set)
  -- и запись типов (act_write).

  TYPE_INDEX *= INT32;

VAR
  type_info *: cmd.CODE_SEGM;
  symb_info *: cmd.CODE_SEGM;

  type_cnt        *: TYPE_INDEX;     -- Метка последнего помеченого типа.
  write_type_cnt  *: TYPE_INDEX;     -- Метка последнего записанного типа.
  tindex_transfer *: TYPE_INDEX;     -- Метки для ty_reference и ty_open_array, у них
                                -- своя нумерация
  tname_transfer  *: ARRAY 256 OF CHAR;

  objname *: ARRAY 256 OF CHAR;

TYPE
  ONE_FIELD_PROC = PROCEDURE (f: pc.OBJECT);

  FIELD_FILTER_PROC = PROCEDURE (f: pc.OBJECT): BOOLEAN;

  Reg2NumTable * = ARRAY prc.Reg OF INTEGER;

PROCEDURE ^ write_obj_name *(o: pc.OBJECT;  VAR name: ARRAY OF CHAR);
-- Конструирует имя объекта и вызывает write_name.

PROCEDURE ^ write_type_name *(t: pc.STRUCT; VAR name: ARRAY OF CHAR);
-- Конструирует имя типа и вызывает write_name.

PROCEDURE ^ emit_type *(action: ACTION; t: pc.STRUCT);
-- Эмитит заданный тип.

PROCEDURE ^ type_index *(type: pc.STRUCT): TYPE_INDEX;
-- Возвращает индекс заданого типа
-- При необходимости "разворачивает" вложенные типы

PROCEDURE ^ proc_len *(o: pc.OBJECT): INT32;
-- Воэвращает длину процедуры в byt'ах.

PROCEDURE ^ start_proc_debug *(o : pc.OBJECT): INT32;
-- Смещение пролога процедуры от начала самой процедуры.

PROCEDURE ^ end_proc_debug *(o: pc.OBJECT): INT32;
-- Смещение эпилога процедуры от начала самой процедуры.

PROCEDURE ^ has_frame *(o: pc.OBJECT): BOOLEAN;
-- Имеет ли процедура фрейм.

PROCEDURE ^ proc_frame_size *(o: pc.OBJECT): INT32;
-- Начальное значение кадра процедуры

PROCEDURE ^ get_index *(t: pc.STRUCT): TYPE_INDEX;
-- Возвращает метку типа, если ее нет то < 0.

PROCEDURE ^ put_index *(t: pc.STRUCT);
-- Записывает метку типу и продвигает type_cnt

PROCEDURE ^ put_index_val *(t: pc.STRUCT; inx: TYPE_INDEX);
-- Записывает метку типу и не продвигает type_cnt

PROCEDURE ^ get_ref_index *(t: pc.STRUCT) : TYPE_INDEX;
-- Возвращает метку reference типа, если ее нет то 0

PROCEDURE ^ put_ref_index_val *(t: pc.STRUCT; inx: TYPE_INDEX);

PROCEDURE ^ get_second_index *(t: pc.STRUCT) : TYPE_INDEX;

PROCEDURE ^ put_second_index_val *(t: pc.STRUCT; inx: TYPE_INDEX);

PROCEDURE ^ field_list *(f: pc.OBJECT; filter: FIELD_FILTER_PROC; one_field: ONE_FIELD_PROC);
-- Обходит поля процедуры и для каждого из них вызывает one_field

PROCEDURE ^ count_fields *(t: pc.STRUCT; filter: FIELD_FILTER_PROC): INT16;
-- Возвращает количество полей в структуре

PROCEDURE ^ whole_word_field *(f: pc.OBJECT; VAR offset: INT32): BOOLEAN;
-- Вычисляет смещение поля в структуре

PROCEDURE ^ generate *(name-, ext-: ARRAY OF CHAR; binary: BOOLEAN; nested_local_procs: BOOLEAN): BOOLEAN;
-- Основная процедура генерации отладочной информации;
-- name, ext - имя и расширение obj-файла; binary - dbg info для obj|asm файла;
-- Возвращает TRUE если генерация прошла успешно.


TYPE

  EMIT_DEBUG *= POINTER TO emit_rec;
  emit_rec   *= RECORD (reg.item_rec)
                  Binary            *: BOOLEAN;
                  CheckMarks        *: BOOLEAN;
                  IterateExternals  *: BOOLEAN;
                  NestedLocalProcs  *: BOOLEAN;
                END;

PROCEDURE (emit: EMIT_DEBUG) PrimitiveTypeNo* (type: pc.STRUCT): TYPE_INDEX;
BEGIN
  ASSERT(FALSE);
END PrimitiveTypeNo;

PROCEDURE (emit: EMIT_DEBUG) TypeEmitter* ( ttag: TYPE_TAG; act: ACTION; type: pc.STRUCT );
BEGIN
  ASSERT(FALSE);
END TypeEmitter;

PROCEDURE (emit: EMIT_DEBUG) SymbEmitter* ( stag: SYMB_TAG; o: pc.OBJECT; tind: TYPE_INDEX );
BEGIN
  ASSERT(FALSE);
END SymbEmitter;


-- IMPLEMENTATION --
VAR
  emit: EMIT_DEBUG;

VAR dbg_only_procs: BOOLEAN;

<* WOFF304+ *>

PROCEDURE adjust_type(t: pc.STRUCT): pc.STRUCT;
BEGIN
  IF (t.mode = pc.ty_record) & dbg_only_procs THEN
    t := pc.void_type;
  END;
  RETURN t;
END adjust_type;

PROCEDURE emit_type *(action: ACTION; t: pc.STRUCT);
VAR
  inx: TYPE_INDEX;
BEGIN
  t := adjust_type(t);
  inx := get_index (t);
  CASE action OF
  | act_set:
    IF inx > 0 THEN RETURN; END;
  | act_write:
    IF inx < write_type_cnt THEN RETURN; END;
  END;
  CASE t.mode OF
  | pc.ty_range      : emit.TypeEmitter (ty_range,    action, t);
  | pc.ty_enum       : emit.TypeEmitter (ty_enum,     action, t);
  | pc.ty_pointer    : emit.TypeEmitter (ty_pointer,  action, t);
  | pc.ty_opaque     : emit.TypeEmitter (ty_opaque,   action, t);
  | pc.ty_set        : emit.TypeEmitter (ty_set,      action, t);
  | pc.ty_proctype   : emit.TypeEmitter (ty_proctype, action, t);
  | pc.ty_array      : emit.TypeEmitter (ty_array,    action, t);
  | pc.ty_array_of   : emit.TypeEmitter (ty_array_of, action, t);
  | pc.ty_SS         : emit.TypeEmitter (ty_SS,       action, t);
  | pc.ty_record     : emit.TypeEmitter (ty_record,   action, t);
  | pc.ty_module     : IF t.flag IN opt.LangsWithModuleConstructors THEN
                         -- только у таких модулей есть процедура
                         emit.TypeEmitter (ty_module, action, t);
                       END;
  ELSE
  END;
END emit_type;


PROCEDURE type_index *(type: pc.STRUCT) : TYPE_INDEX;
-- генерирует тип и возвращает его метку
VAR
  old: cmd.CODE_SEGM;
BEGIN
  old := NIL;
  IF type # NIL THEN
    ASSERT(write_type_cnt=type_cnt);
    emit_type (act_set, type);
    IF emit.Binary THEN
      cmd.get_segm (old);
      cmd.set_segm (type_info);
    END;
    emit_type (act_write, type);
    IF emit.Binary THEN
      cmd.set_segm (old);
    END;
    ASSERT(write_type_cnt=type_cnt);
    RETURN get_index (type);
  ELSE
    RETURN 0;
  END;
END type_index;


PROCEDURE write_open_array (t: pc.STRUCT; o: pc.OBJECT; dim: INT16);
VAR
  el: pc.STRUCT;
BEGIN
  ASSERT(t.mode = pc.ty_array_of);
  el := t.base;
  IF el.mode = pc.ty_array_of THEN
    write_open_array (el, o, dim+1);
    tindex_transfer := write_type_cnt - 1;
    tname_transfer := "";
  ELSE
    emit_type (act_set, el);
    emit_type (act_write, el);
    tindex_transfer := get_index (el);
    tname_transfer := "";
  END;
  ASSERT (type_cnt = write_type_cnt);
  INC (type_cnt); -- потому что нет соответсвующего set_open_array
  emit.TypeEmitter (ty_open_array, act_write, t);
END write_open_array;


PROCEDURE has_limits (t: pc.STRUCT): BOOLEAN;
BEGIN
  ASSERT(t.mode = pc.ty_array_of);
  RETURN t.flag IN opt.LangsWithOpenArrays;
END has_limits;


(*
  We have not "set_open_array" because every open array type
  is referenced only once, and a following procedure "write_
  open_array" combines both 'set' and 'write' functions
*)

PROCEDURE open_array_index (type: pc.STRUCT; o: pc.OBJECT): TYPE_INDEX;
-- для каждого открытого массива генерируется свой тип          
VAR
  tind: TYPE_INDEX;
  old : cmd.CODE_SEGM;
BEGIN
  old := NIL;
  IF emit.Binary THEN
    cmd.get_segm(old);
    cmd.set_segm(type_info);
  END;
  IF has_limits(type) THEN
    write_open_array (type, o, 0);
    tindex_transfer := type_cnt-1;
    tname_transfer := "";
    INC (type_cnt);
    emit.TypeEmitter (ty_reference, act_write, NIL);
    tind := type_cnt-1;
  ELSE --- we write C-like open array as a pointer to its element type
    tind := get_index(type);
    IF tind < 0 THEN -- had not been written
      emit.TypeEmitter (ty_pointer, act_set, type);
      emit.TypeEmitter (ty_pointer, act_write, type);
      tind := get_index(type);
    END;
  END;
  IF emit.Binary THEN
    cmd.set_segm(old);
  END;
  RETURN tind
END open_array_index;


PROCEDURE put_index_val *(t: pc.STRUCT; inx: TYPE_INDEX);
VAR
  se: at.SIZE_EXT;
BEGIN
  ASSERT(NOT (at.tmark_db_index IN t.marks));
  NEW(se);
  se.size := inx;
  at.app_struct_attr(t, se, at.a_index);
  INCL(t.marks, at.tmark_db_index);
END put_index_val;


PROCEDURE get_index *(t: pc.STRUCT): TYPE_INDEX;
VAR
  a  : at.ATTR_EXT;
  res: TYPE_INDEX;
BEGIN
  t := adjust_type(t);
  IF at.tmark_db_index IN t.marks THEN
    a := at.attr(t.ext, at.a_index);
    res := VAL(TYPE_INDEX, a(at.SIZE_EXT).size);
  ELSE
    res := emit.PrimitiveTypeNo (t);
  END;
  RETURN res;
END get_index;


PROCEDURE put_index *(t: pc.STRUCT);
BEGIN
  put_index_val(t, type_cnt);
  INC(type_cnt);
END put_index;


PROCEDURE get_ref_index *(t: pc.STRUCT) : TYPE_INDEX;
VAR
  a: at.ATTR_EXT;
BEGIN
  IF at.tmark_db_index IN t.marks THEN;
    a := at.attr(t.ext, at.a_index_ref);
    IF a # NIL THEN
      RETURN VAL(TYPE_INDEX, a(at.SIZE_EXT).size);
    END;
  END;
  RETURN 0;
END get_ref_index;


PROCEDURE put_ref_index_val *(t: pc.STRUCT; inx: TYPE_INDEX);
  VAR se: at.SIZE_EXT;
BEGIN
  IF at.tmark_db_index IN t.marks THEN
    NEW(se);
    se.size := inx;
    at.app_struct_attr(t, se, at.a_index_ref);
  END;
END put_ref_index_val;


PROCEDURE get_second_index *(t: pc.STRUCT): TYPE_INDEX;
VAR
  a  : at.ATTR_EXT;
  res: TYPE_INDEX;
BEGIN
  ASSERT(at.tmark_db_index IN t.marks);
  a := at.attr(t.ext, at.a_index2);
  res := VAL(TYPE_INDEX, a(at.SIZE_EXT).size);
  RETURN res;
END get_second_index;


PROCEDURE put_second_index_val *(t: pc.STRUCT; inx: TYPE_INDEX);
VAR
  se: at.SIZE_EXT;
BEGIN
  ASSERT(at.tmark_db_index IN t.marks);
  NEW(se);
  se.size := inx;
  at.app_struct_attr(t, se, at.a_index2);
END put_second_index_val;


PROCEDURE field_list *(f: pc.OBJECT; filter: FIELD_FILTER_PROC; one_field: ONE_FIELD_PROC);
VAR
  n: pc.NODE;
BEGIN
  WHILE f # NIL DO
    IF (f.flag # pc.flag_java) OR (f.host.base # NIL) THEN
      IF f.mode IN pc.FIELDs THEN
        IF filter (f) THEN
          one_field (f);
        END;
      ELSE
        ASSERT(f.mode = pc.ob_header);
        IF f.val.obj # NIL THEN one_field(f.val.obj) END;
        n := f.val.l;
        WHILE n # NIL DO
          ASSERT(n.mode = pc.nd_node);
          field_list (n.obj, filter, one_field);
          n := n.next;
        END;
      END;
    END;
    f := f.next;
  END;
END field_list;

VAR
  CountFields: INT16;

PROCEDURE count_field (f: pc.OBJECT);
BEGIN
  ASSERT(f # NIL);
  INC(CountFields);
END count_field;


PROCEDURE count_fields *(t: pc.STRUCT; filter: FIELD_FILTER_PROC): INT16;
BEGIN
  ASSERT(t.mode = pc.ty_record);
  IF t.base = NIL THEN
    CountFields := 0;
  ELSE
    CountFields := 1;
  END;
  field_list (t.prof, filter, count_field);
  RETURN CountFields;
END count_fields;


PROCEDURE whole_word_field *(f: pc.OBJECT; VAR offset: INT32): BOOLEAN;
VAR
  bit_offset, bit_width: INT8;
BEGIN
  at.bit_field_info(f, offset, bit_offset, bit_width);
  IF bit_width > at.BITS_PER_VIRTUAL_WORD THEN
    ASSERT (bit_width = at.BITS_PER_REAL_WORD);
    RETURN TRUE;
  ELSE
    offset := -1;
    RETURN FALSE;
  END;
END whole_word_field;



PROCEDURE gen_var_par_type (t: pc.STRUCT): TYPE_INDEX;
VAR
  tind: TYPE_INDEX;
  old : cmd.CODE_SEGM;
BEGIN
  old := NIL;
  tind := get_ref_index(t);
  IF tind # 0 THEN RETURN tind; END;
  INC(type_cnt);
  IF emit.Binary THEN
    cmd.get_segm(old);
    cmd.set_segm(type_info);
  END;
  tindex_transfer := get_index(t);
  tname_transfer := "";
  emit.TypeEmitter (ty_reference, act_write, NIL);
  IF emit.Binary THEN
    cmd.set_segm(old);
  END;
  tind := write_type_cnt-1;
  put_ref_index_val (t, tind);
  RETURN tind;
END gen_var_par_type;


PROCEDURE emit_symbol (o: pc.OBJECT): BOOLEAN;
BEGIN
  IF (pc.omark_no_debug IN o.marks) THEN
    RETURN FALSE;
  ELSIF dbg_only_procs & ~(o.mode IN pc.PROCs) THEN
    RETURN FALSE;
  ELSE
    RETURN ((o.lev # 0) OR NOT emit.CheckMarks OR (at.omark_gen_marked IN o.marks));
  END;
END emit_symbol;


PROCEDURE write_var (stag: SYMB_TAG; o: pc.OBJECT);
VAR
  type: pc.STRUCT;
  tind: TYPE_INDEX;
  a   : at.ATTR_EXT;
BEGIN
  IF NOT nms.valid_name (o.name) THEN
    RETURN;
  END;
  CASE stag OF
  | sy_var:
    ASSERT(o.lev = 0);
    IF emit_symbol(o) THEN
      tind := type_index (o.type);
      IF (tind >= 0) THEN
        emit.SymbEmitter (stag, o, tind);
      END;
    END;

  | sy_param, sy_local_var:
    IF emit_symbol(o) THEN
      a := at.attr (o.ext, at.a_dbg);
      IF a # NIL THEN
        type := o.type;
        IF type.mode = pc.ty_array_of THEN
          tind := open_array_index (type, o);
        ELSE
          tind := type_index (type);
        END;
        IF tind > 0 THEN
          IF (o.mode = pc.ob_varpar)
            OR ((pc.otag_RO IN o.tags) & ~def.is_scalar(type))
          THEN
            IF type.mode # pc.ty_array_of THEN
              -- we have to make a reference to that type
              tind := gen_var_par_type (type);
            END;
          END;
          emit.SymbEmitter (stag, o, tind);
        END;
      END;
    END;
  END;
END write_var;



PROCEDURE write_obj_name *(o: pc.OBJECT;  VAR name: ARRAY OF CHAR);

  PROCEDURE modname (o: pc.OBJECT; VAR name: ARRAY OF CHAR);
  BEGIN
    ASSERT(o = at.curr_mod);
    IF at.main THEN
      IF at.GENDLL IN at.COMP_MODE THEN
        COPY(nms.DLL_ENTR, name);
      ELSE
        COPY(nms.MAIN_PROG_ENTR, name);
      END;
    ELSE
      COPY(nms.MOD_ENTR, name);
    END;
  END modname;

  PROCEDURE replace_non_alpha (VAR name: ARRAY OF CHAR);
  VAR
    i, l: INTEGER;
  BEGIN
    l := SHORT(LENGTH(name));
    i := 0;
    LOOP
      IF i = l THEN EXIT; END;
      CASE name [i] OF
      | 0C : EXIT;
      | "#": name[i] := '_';
      | "/": name[i] := '_';
      | "\": name[i] := '_';
      ELSE
      END;
      INC (i);
    END;
  END replace_non_alpha;

BEGIN
  COPY('', name);
  IF o.mode = pc.ob_module THEN
    modname(o, name);
  ELSIF o.name # NIL THEN
    IF (o.mode IN pc.PROCs) THEN
      IF (o.host.mode = pc.ty_record)  THEN
        IF nms.valid_name(o.host.obj.name) THEN     (* Если метод описан от указателя на запись, то *)
          COPY(o.host.obj.name^, name);             (* пытается взяться имя типа этого указателя    *)
        ELSE
          COPY(o.type.prof.type.obj.name^, name);
        END;
        str.Append('::', name);
      ELSIF (o.lev > 0) AND NOT (at.DbgNestedProc IN at.COMP_MODE) THEN
        write_obj_name(o.host.obj, name);
        str.Append('$', name);
      END;
    ELSIF (o.mode = pc.ob_type) AND (o.mno > pc.ZEROMno) AND (o.mno # at.curr_mno) THEN
      COPY (pc.mods[o.mno].name^, name);
      str.Append("_", name);
    ELSIF env.config.Option (opt.OPT_DBG_QUALIDS) AND (o.mode = pc.ob_var)
          AND (o.lev = 0) AND (o.mno >= pc.ZEROMno) AND (o.flag IN pc.OA_langs) THEN
      COPY (pc.mods[o.mno].name^, name);
      str.Append("_", name);
    END;
    str.Append (o.name^, name);
    replace_non_alpha (name);
  END;
END write_obj_name;


PROCEDURE write_type_name *(t: pc.STRUCT; VAR name: ARRAY OF CHAR);
BEGIN
  IF (t.obj = NIL) OR NOT nms.valid_name(t.obj.name) THEN
    COPY('', name);
  ELSE
    write_obj_name(t.obj, name);
  END;
END write_type_name;


PROCEDURE proc_len *(o: pc.OBJECT): INT32;
VAR
  sg: cmd.CODE_SEGM;
BEGIN
  sg := cmd.get_ready(o);
  IF sg # NIL THEN
    RETURN sg.code_len;
  ELSE
    RETURN 0;
  END;
END proc_len;


PROCEDURE start_proc_debug *(o : pc.OBJECT): INT32;
VAR
  sg: cmd.CODE_SEGM;
BEGIN
  sg := cmd.get_ready(o);
  IF sg # NIL THEN
    RETURN sg.start;
  ELSE
    RETURN 0;
  END;
END start_proc_debug;


PROCEDURE end_proc_debug *(o: pc.OBJECT): INT32;
VAR
  sg: cmd.CODE_SEGM;
BEGIN
  sg := cmd.get_ready(o);
  IF sg # NIL THEN
    RETURN sg.fin;
  ELSE
    RETURN 0;
  END;
END end_proc_debug;


PROCEDURE has_frame *(o: pc.OBJECT): BOOLEAN;
BEGIN
  ASSERT(o#NIL);
  RETURN (at.use_frame_ptr IN at.COMP_MODE);     (* ?? *)
END has_frame;

PROCEDURE proc_frame_size *(o: pc.OBJECT): INT32;
-- Начальное значение кадра процедуры
VAR
  sg: cmd.CODE_SEGM;
BEGIN
  sg := cmd.get_ready(o);
  IF sg # NIL THEN
    RETURN sg.frame_size;
  ELSE
    RETURN 0;
  END;
END proc_frame_size;

VAR
  PROCs: pc.OB_SET;

PROCEDURE iter_objects (nested_local_procs: BOOLEAN);

VAR
  Level: INTEGER;

  PROCEDURE obj_list(o: pc.OBJECT; prof, ignore_proc, ignore_var: BOOLEAN);
  VAR
    tind: TYPE_INDEX;
  BEGIN
    WHILE o # NIL DO
      IF o.mno # at.curr_mno THEN RETURN; END;
      IF (o.mode IN PROCs) AND (o.val # NIL) AND NOT ignore_proc THEN
        IF emit_symbol(o) THEN
          tind := type_index(o.type);
          IF tind >= 0 THEN
            emit.SymbEmitter (sy_proc, o, tind);
            emit.SymbEmitter (sy_scope_open, o, -1);
            INC(Level);
            IF NOT nested_local_procs THEN
              obj_list(o.type.prof, TRUE, TRUE, FALSE);
              obj_list(o.type.mem, FALSE, TRUE, FALSE);
              emit.SymbEmitter (sy_scope_close, o, -1);
              DEC(Level);
            END;
            obj_list(o.type.prof, TRUE, FALSE, NOT nested_local_procs);
            obj_list(o.type.mem, FALSE, FALSE, NOT nested_local_procs);
            IF nested_local_procs THEN
              emit.SymbEmitter (sy_scope_close, o, -1);
              DEC(Level);
            END;
          END;
        END;
      ELSIF (o.mode = pc.ob_type) THEN
--        IF pc.otag_public IN o.tags THEN -- exported type only
        IF ~dbg_only_procs THEN
          IF nms.valid_name(o.name) THEN
            tind := type_index(o.type);
          END;
          IF (o.type.mode=pc.ty_record) & (o.type.flag IN pc.OOP_langs) THEN
            obj_list(o.type.mem, FALSE, FALSE, FALSE);
          END;
        END;
      ELSIF (o.mode IN pc.VARs) AND NOT ignore_var THEN
        IF emit_symbol(o) THEN
          IF o.lev = 0 THEN
            write_var(sy_var, o);
          ELSIF prof THEN
            ASSERT(Level#0);
            write_var(sy_param, o);
          ELSE
            ASSERT(Level#0);
            write_var(sy_local_var, o);
          END;
        END;
      END;
      o := o.next;
    END;
  END obj_list;

VAR
  t: pc.STRUCT;

  PROCEDURE write_var_FROM;
  VAR
    u: pc.USAGE;
    o: pc.OBJECT;
(*
    tind: TYPE_INDEX;
*)
  BEGIN
    u := t.use;
    WHILE u # NIL DO
      o := u.obj;
      IF o.mode IN pc.VARs THEN
        -- FROM variables IMPORT
        IF emit_symbol(o) THEN
          ASSERT(o.lev = 0);
          write_var(sy_var, o);
        END;
      END;
(*
      IF o.mode IN PROCs THEN
        -- FROM procedures IMPORT
        IF emit_symbol(o) THEN
          tind := type_index(o.type);
          ASSERT(tind > 0);
          emit.SymbEmitter (sy_proc, o, tind);
          emit.SymbEmitter (sy_scope_open, o, -1);
          emit.SymbEmitter (sy_scope_close, o, -1);
        END;
      END;
*)
      u := u.next;
    END;
  END write_var_FROM;


  PROCEDURE write_module;
  VAR
    tind: TYPE_INDEX;

  BEGIN
    IF emit_symbol (at.curr_mod) THEN
      tind := type_index (at.curr_mod.type);
      IF tind > 0 THEN
        emit.SymbEmitter (sy_proc,        at.curr_mod, tind);
        emit.SymbEmitter (sy_scope_open,  at.curr_mod, -1);
        emit.SymbEmitter (sy_scope_close, at.curr_mod, -1);
      END;
    END;
  END write_module;


BEGIN
  t := at.curr_mod.type;
  Level := 0;
  obj_list (t.prof, TRUE, FALSE, FALSE);
  obj_list (t.mem, FALSE, FALSE, FALSE);
  write_var_FROM;
  write_module;
END iter_objects;



PROCEDURE (emit: EMIT_DEBUG) generate* (): BOOLEAN;
VAR
  old: cmd.CODE_SEGM;
BEGIN
  old := NIL;
  IF emit.IterateExternals THEN
    PROCs := pc.PROCs - pc.OB_SET{pc.ob_cproc};
  ELSE
    PROCs := pc.PROCs - pc.OB_SET{pc.ob_eproc, pc.ob_cproc};
  END;

  IF emit.Binary THEN
    cmd.get_segm (old);
    cmd.new_segm (type_info);
    cmd.set_segm (type_info);
    emit.TypeEmitter (ty_start, act_write, NIL);
    cmd.new_segm (symb_info);
    cmd.set_segm (symb_info);
    emit.SymbEmitter (sy_start, NIL, -1);
  ELSE
    emit.TypeEmitter (ty_start, act_write, NIL);
    emit.SymbEmitter (sy_start, NIL, -1);
    type_info := NIL;
    symb_info := NIL;
  END;
  iter_objects (emit.NestedLocalProcs);
  IF emit.Binary THEN
    cmd.set_segm (type_info);
    emit.TypeEmitter  (ty_end, act_write, NIL);
    cmd.set_segm (symb_info);
    emit.SymbEmitter  (sy_end, NIL, -1);
    cmd.set_segm (old);
  ELSE
    emit.TypeEmitter (ty_end, act_write, NIL);
    emit.SymbEmitter (sy_end, NIL, -1);
  END;

  RETURN TRUE;
END generate;


PROCEDURE generate*(name-, ext-: ARRAY OF CHAR; binary: BOOLEAN; nested_local_procs: BOOLEAN): BOOLEAN;
VAR
  item: reg.ITEM;
BEGIN
  type_info := NIL;
  symb_info := NIL;

  emit := NIL;
  item := reg.GetActive(opt.dbgFormat);
  IF item = NIL THEN
    env.errors.Warning(at.curr_mod.pos, 500);
    RETURN FALSE;
  END;
  WITH item: EMIT_DEBUG DO
    emit := item;
  ELSE
    ASSERT(FALSE);
  END;

  emit.Binary := binary;
  emit.CheckMarks := TRUE;
  emit.IterateExternals := FALSE;
  emit.NestedLocalProcs := nested_local_procs;
  str.Concat(name, ext, objname);
  dbg_only_procs := env.config.Option("dbg_only_procs");

  <* IF db_trace THEN *>
   IF env.config.Option ("BR_IRVIS") THEN
      pcVisIR.Ini;
      pcVisIR.VisIR (at.curr_mod.val, "\n======= IR before replace =======", pc.INVMno (*at.curr_mod.mno*) );
    END;
  <* END *>
  <* IF db_trace THEN *>
   IF env.config.Option ("AR_IRVIS") THEN
      pcVisIR.Ini;
      pcVisIR.VisIR (at.curr_mod.val, "\n======= IR after replace =======", pc.INVMno (*at.curr_mod.mno*) );
    END;
  <* END *>
  RETURN emit.generate();
END generate;


BEGIN
  type_info := NIL;
  symb_info := NIL;

  reg.NewList(opt.dbgFormat);
END DbgFace.
