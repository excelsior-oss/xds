MODULE opTune;
(* информация для настройки транслятора на машину *)
IMPORT ir_def, SYSTEM, pcK;

VAR
    nil_val* : ir_def.VALUE;

TYPE
    opTune_IDB     *= POINTER TO opTune_IDB_Rec;
    opTune_IDB_Rec *= RECORD
                        ld_real_sz *: ir_def.SizeType;
                      END;
VAR
  IDB *: opTune_IDB;

  BIG_END*   : BOOLEAN;    (* code for bigendian machine *)

CONST  (* -- информация о реализации некоторых типов на конкретной машине -- *)

  jmp_buf_size = 180;       (* размер буфера для set_jmp *)

  BITSET_LEN*  = ir_def.SizeType{32};       (* длина максимального "скалярного" множества *)
VAR
  BITSET_LEN_InInclExcl*  : LONGINT;       (* длина максимального множества для операций In Incl Excl*)
  BITSET_LEN_scalar*      : LONGINT;       (* длина максимального множества для остальных операций *)
  lset_sz_InInclExcl*     : LONGINT;
CONST
  (* тип и размер одной порции длинного множества *)
  lset_ty_InInclExcl* = ir_def.t_unsign;
  lset_ty* = ir_def.t_unsign;
  lset_sz* = BITSET_LEN DIV 8;
--<* END *>

  seq_item_type*= ir_def.t_unsign;
  seq_item_sz* = VAL(ir_def.SizeType,4);         (* длина одного элемента SEQ-последовательности *)

  bool_ty* = ir_def.t_unsign;    (* BOOLEAN *)
  bool_sz* = VAL(ir_def.SizeType,1);

  char_ty* = ir_def.t_unsign;    (* CHAR *)
  char_sz* = VAL(ir_def.SizeType,1);

  protect_ty* = ir_def.t_unsign; (* PROTECTION *)
  protect_sz* = VAL(ir_def.SizeType,2);

  index_ty* = ir_def.t_int;      (* стандартный тип индекса *)
  index_sz* = VAL(ir_def.SizeType,4);

  addr_ty* = ir_def.t_ref;       (* адрес в индексациях и т.п. *)
  addr_sz* = VAL(ir_def.SizeType,4);

  proc_ty* = ir_def.t_ref;       (* процедурное значение *)
  proc_sz* = VAL(ir_def.SizeType,4);

  counter_ty* = ir_def.t_int;    (* счетчик в цикле FOR *)
  counter_sz* = VAL(ir_def.SizeType,4);

  element_ty* = ir_def.t_int;    (* элемент множества *)
  element_sz* = VAL(ir_def.SizeType,4);

  process_ty* = ir_def.t_rec;    (* см. тип xmRTS.X2C_XHandler_STR *)
  process_buffer_offs* = 2*2 + 2*ORD(addr_sz);
  process_size* = process_buffer_offs + jmp_buf_size;

  real_sz*      = VAL(ir_def.SizeType,4);        (* вещественные  *)
  longreal_sz*  = VAL(ir_def.SizeType,8);        (* разной        *)
                            (* длины         *)
  word_sz  * = 4;

(* ------------------------------------------------------------------ *)

(* Имеет ли смысл SR умножение на эту целую константу *)
PROCEDURE(idb : opTune_IDB)
         NeedSR* (v: ir_def.VALUE; t: ir_def.TypeType; s: ir_def.SizeType): BOOLEAN;
BEGIN
  ASSERT(FALSE);
END NeedSR;

(* Имеет ли смысл "безопасное" умножение заменять сдвигами и сложениями *)

PROCEDURE(idb : opTune_IDB)
         NeedAdd* (v: ir_def.VALUE; t: ir_def.TypeType; s: ir_def.SizeType): BOOLEAN;
BEGIN
  ASSERT(FALSE);
END NeedAdd;

PROCEDURE(idb : opTune_IDB)
        MinLocalStart* (proc:pcK.OBJECT):LONGINT;(* конец первого локала - далее убывают *)
BEGIN
  ASSERT(FALSE);
END MinLocalStart;

(* ------------------------------------------------------------------ *)

VAR PARAM_START*    (* смещение для первого параметра - далее возрастает *)
    : LONGINT;

(* ------------- t r a p s ------------------------------------------ *)

CONST -- Константы определены в RTS (менять нельзя)
  indexException        *= 0;
  rangeException        *= 1;
  caseSelectException   *= 2;
  invalidLocation       *= 3;
  functionException     *= 4;
  wholeValueException   *= 5;
  wholeDivException     *= 6;
  realValueException    *= 7;
  realDivException      *= 8;
  complexValueException *= 9;
  complexDivException   *= 10;
  protException         *= 11;
  sysException          *= 12;
  coException           *= 13;
  exException           *= 14;
(* Oberon-2 Exceptions ----- *)
  assertException       *= 15;
  guardException        *= 16;
(* RTS Exceptions ---------- *)
  noMemoryException     *= 17;
  internalError         *= 18;
(* дополнительно ----------- *)
  undefinedException    *= 19;

CONST
  ABORT_EXIT_CODE       *= 9;      (* see TERMINATION.X2C_ABORT *)

(* -------- R T S - с т р у к т у р ы ---------------------------------- *)
           ---------------------------
CONST
  x2c_mno* = MAX(pcK.Mno);    (* -- m n o   o f   R T S - l i b r a r y --- *)

CONST
  rts_name*   = "X2C";
  X2C_OFS_END* = 1;                       (* see file X2C.H *)
  X2C_OFS_ARR* = 2;
  X2C_OFS_REC* = 3;
  X2C_BASE*    = 4;

  empty_rec_size* = 4;       (* размер пустой записи *)

  MOD_DESC_size* =  7 * addr_sz;
  TYP_DESC_size* =  index_sz + 3*addr_sz + 2*2 + 16*addr_sz + 6*addr_sz + 4;

  DYNARR_ADDR_offset* =  0;  (* смещение ссылки на "хвост" в дескрипторе гибкого массива *)
  DYNARR_LENs_offset* =  4;  (* смещение таблицы длин по измерениям *)

  TD_PTR_offset*      = -8;  (* смещение указателя на тип в дин. записях *)

  BASE_size*          =  4;  (* размер элемента таблицы базовых типов      *)
  BASEs_offset*       = 20;  (* смещение таблицы базовых типов             *)
  PROC_offset*        = 84;  (* смещение указателя на таблицу методов типа *)
  SELF_offset*        = 104; (* смещение до указателя на сам дескриптор типа *)

(* в данный момент реализация рассчитана на такую структуру дескриптора типа:

X2C_TD_STR = RECORD
 (*  0*)   size   : SYSTEM.size_t;
 (*  4*)   name   : POINTER TO CHAR;    (* type name                    *)
 (*  8*)   module : X2C_MD;             (* host module                  *)
 (* 12*)   next   : X2C_TD;             (* next type in module types list*)
 (* 16*)   methods: SYSTEM.INT16;
 (* 18*)   level  : SYSTEM.INT16;
 (* 20*)   base   : ARRAY [0..15] OF X2C_TD;
 (* 84*)   proc   : POINTER TO PROC;
 (* 88*)   offs   : POINTER TO POINTER TO SYSTEM.void;
 (* 92*)   succ   : X2C_TD;
 (* 96*)   link   : X2C_LINK;
 (*100*)   tail   : X2C_LINK;
 (*104*)   self   : X2C_TD;   (* -- new field: pointer to the type descriptor -- *)
 (*108*)   res    : SYSTEM.CARD32;
END;

*)

BEGIN
    NEW(IDB);
    IDB.ld_real_sz := -1;
<* IF    TARGET_68K  THEN *>  
    BIG_END := TRUE;
<* ELSIF TARGET_RISC OR TARGET_SPARC THEN *>  
    BIG_END := TRUE;
<* ELSE *>                    
    BIG_END := FALSE;
<* END *>
END opTune.

(*  так было бы наверно лучше ...
Смещение  Размер
                                                                 | . . .    |
  -4       -4*n    base[X2C_EXT];  ссылки на базовые типы        | bases    |
   0         4     size;           размер экземпляра             | size     |
   4         4     offs;           ук-тель на таблицу указателей | ptr_offs +-------+
   8         4     name;           ук-тель на строку             | name     |       |
  12         4     module;         ук-тель на дескриптор модуля  | module   |       V
         void *      ext;           ???                          |          |    +--------+
  16         2     methods;        число методов ??              | methods  |    |  ofs0  |
  18         2     level;          уровень расширения записи     | level    |    |  ofs1  |
  20        4*m    proc;           таблица методов               | procs    |    |  . . . |
                                                                 | . . .    |
*)
