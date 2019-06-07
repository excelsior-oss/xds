(** Copyright (c) 1991,98 XDS Ltd, Russia. All Rights Reserved. *)
(** XDS: Syntax tree definition                                   *)
<* IF ~ DEFINED(COMPONENT_TESTCOVERAGE) THEN *> <* NEW COMPONENT_TESTCOVERAGE- *> <* END *>

MODULE pcK; (* Ned 28-Mar-91. *)
            (* Sem 02-Jul-93. *)

(* Modifications:
   05-Oct-98 Vit  Preliminary merging with pre-3.0 version
                  End position is introduced into NODE
   13-May-98 Vit  ORDs, PTRs sets are added
          pcConst options changed:
          - en_cnsexp eliminated
   29-Apr-98 Vit  NODE-VALUE revolution; 2nd pos in NODE
   23-Mar-96 Ned  method "VALUE.cast_ordinal" and constant NUM_LITERALs
                  are added.
   24-Mar-96 Ned  <*IF extvalue*> is deleted.
   24-Mar-96 Ned  field "code_rec.en_cnsexp" is added.
   26-Mar-96 Ned  value_rec.get_cardinal is added.
   27-Mar-96 Ned  PRO0046: Actual LEN for strings(ty_array) may be less then
                  type.len. Use binary(sb_len) to retrieve the len.
                  Comments are added for value.unary/binary.
   16-May-96 Ned  PRO0124: field "marks" is added to OBJECT & STRUCT.

*)

<* +o2addkwd *>
FROM   SYSTEM IMPORT PRED,SUCC, EVAL;
IMPORT
         SYSTEM
, xfs := xiFiles
, env := xiEnv
, fmt := xcStr
, Strings
, DStrings;

CONST
  name_size* = 256;
  max_name*  = name_size-1;

TYPE
  TPOS*    = env.TPOS;

  BYTE*    = SYSTEM.BYTE;
  Mno *    = env.Mno;

--  SUB_MODE *= SHORTINT;

  Range0_31 = SHORTINT[0..31];
  TMARK *= (
    tmark_aux0,
    tmark_aux1,
    tmark_aux2,
    tmark_aux3,
    tmark_aux4,
    tmark_aux5,
    tmark_aux6,
    tmark_aux7,
    tmark_aux8,
    tmark_aux9,
    tmark_aux10,
    tmark_aux11,
    tmark_aux12,
    tmark_aux13,
    tmark_aux14,
    tmark_aux15,
    tmark_aux16,
    tmark_aux17,
    tmark_aux18,
    tmark_aux19,
    tmark_aux20,
    tmark_aux21,
    tmark_aux22,
    tmark_aux23,
    tmark_aux24,
    tmark_aux25,
    tmark_aux26,
    tmark_aux27,
    tmark_aux28,
    tmark_aux29,
    tmark_aux30,
    tmark_aux31
  );
  OMARK *= (
    omark_aux0,
    omark_aux1,
    omark_aux2,
    omark_aux3,
    omark_aux4,
    omark_aux5,
    omark_aux6,
    omark_aux7,
    omark_aux8,
    omark_aux9,
    omark_aux10,
    omark_aux11,
    omark_aux12,
    omark_aux13,
    omark_aux14,
    omark_aux15,
    omark_aux16,
    omark_aux17,
    omark_aux18,
    omark_aux19,
    omark_aux20,
    omark_aux21,
    omark_aux22,
    omark_code,--*         = OMARK{ 23 }; (** procedure body contains SYSTEM.CODE     *)
    omark_used_by_code,--* = OMARK{ 24 }; (** object used in assembler code           *)
    omark_no_debug,    --* = OMARK{ 25 }; (** don't generate debuginfo for this object *)
    omark_retthis,    (* pcConst, pcSafe: function returns its 'this' *)
    omark_retnothis,  (* pcConst, pcSafe: function returns smth another *)
    omark_aux28,
    omark_notchanged, (* pcConst: the formal paramether is not changed in f() - Inline does not duplicates it *)
    omark_knowntype,  (* pcConst, pcSafe: actual type of the object is known (== o.actual.type) *)
    omark_aux31
  );

  UTAG  *= ( (** object usage tags *)
    utag_read,--*        = UTAG{ 0 }; (** object value is read *)
    utag_write,--*       = UTAG{ 1 }; (** object value is changed *)
    utag_call,--*        = UTAG{ 2 }; (** object is called, for procedure only *)

    utag_extended,--*    = UTAG{ 3 }; (** the below tags should be used along with the tag *)
    utag_declared_in,--* = UTAG{ 4 }; (** class in which this one is declared, for module only *)
    utag_declaring,--*   = UTAG{ 5 }; (** class declared in this one, for module only *)
    utag_throws,--*      = UTAG{ 6 }; (** exception declared in "throws", for procedure only *)
    utag_actimport,--*   = UTAG{ 7 }; (** actually imported module, only for modules *)
    utag_aux8,
    utag_aux9,
    utag_aux10,
    utag_aux11,
    utag_aux12,
    utag_aux13,
    utag_aux14,
    utag_aux15,
    utag_aux16,
    utag_aux17,
    utag_aux18,
    utag_aux19,
    utag_aux20,
    utag_aux21,
    utag_aux22,
    utag_aux23,
    utag_aux24,
    utag_aux25,
    utag_aux26,
    utag_aux27,
    utag_aux28,
    utag_aux29,
    utag_aux30,
    utag_aux31 --only for pco.inp_use for len=4 bytes
  );

  NTAG  *= (
(** node tags *)
    ntag_chk_overflow,--*= NTAG{ 0 };
    ntag_chk_range   ,--*= NTAG{ 1 }; (** also =ChkNil for pointers *)
    ntag_no_exit     ,--*= NTAG{ 2 }; (** no control flow to 'n.next' for all stmd nodes *)
    ntag_hex         ,--*= NTAG{ 3 };
    ntag_lbl_used    ,--*= NTAG{ 4 };
    ntag_x_tcf       ,--*= NTAG{ 5 };
    ntag_in_fin      ,--*= NTAG{ 6 };
    ntag_has_sidef   ,--*= NTAG{ 7 };
    ntag_elsif_node  ,--*= NTAG{ 8 };
    ntag_substitute  ,--*= NTAG{ 9 };  (** shows that statement does not have corresponding node
                        --        and another node is used as a  substitute for it.
                        --        (eg 'for' in java or 'with' in oberon-2) *)

    ntag_exit_label  ,--*= NTAG{ 10 }; (** used in opCode, cleared in pcConst *)

    ntag_in_try      ,--*= NTAG{ 11 };

    ntag_incomparable_as_true,-- *= NTAG{ 12 };
    ntag_this,        -- NTAG{13} -- this parameter should be checked for NIL
    ntag_stackalloc,  -- NTAG{14} -- jnew can allocate memory on the stack (for arrays only)
    ntag_safeass,     -- NTAG{15} -- pcConst's tag
    ntag_constrinlined,
    ntag_triaded,
    ntag_aux18,
    ntag_aux19,
    ntag_aux20,
    ntag_aux21,
    ntag_aux22,
    ntag_aux23,
    ntag_aux24,
    ntag_aux25,
    ntag_aux26,
    ntag_aux27,
    ntag_aux28,
    ntag_aux29,
    ntag_unrolled,    -- pcConst : FOR statement unrolled
    ntag_aux31
  );
  CTAG  *= Range0_31;

  TMARK_SET*= PACKEDSET OF TMARK;
  OMARK_SET*= PACKEDSET OF OMARK;
  UTAG_SET *= PACKEDSET OF UTAG;
  NTAG_SET *= PACKEDSET OF NTAG;
  CTAG_SET *= PACKEDSET OF CTAG;

  BEXT*     = POINTER TO bext_rec;
  PARS*     = POINTER TO pars_rec;
  CODE*     = POINTER TO code_rec;
  UNIT*     = POINTER TO unit_rec;
  CONSTS*   = POINTER TO const_rec;
  IROBJECT* = POINTER TO irobject_rec;
  STRUCT*   = POINTER TO struct_rec;
  USAGE*    = POINTER TO usage_rec;
  OBJECT*   = POINTER TO object_rec;
  NODE*     = POINTER TO node_rec;
  VALUE*    = POINTER TO value_rec;
  NAME*     = ARRAY name_size OF CHAR;
  STRING*   = xfs.String;

  OBJECTS*  = POINTER TO ARRAY OF OBJECT;
  MnoOBJECTS*=POINTER TO ARRAY OF OBJECT;
  RIDER* = RECORD
    del* : BOOLEAN; (* object must be deleted *)
  END;

TYPE
    (** language flags *)
  Lang *= env.Lang;
  LangSet *= PACKEDSET OF Lang;

CONST mno_pervasive* = Mno{-1};
      mno_invalid*   = MIN(Mno);

CONST
    flag_o2  *= env.flag_o2  ;
    flag_m2  *= env.flag_m2  ;
    flag_c   *= env.flag_c   ;
    flag_java*= env.flag_java;
    flag_jbc *= env.flag_jbc ;(** used only in project system to distinguish Java FEs *)
    flag_sl1 *= env.flag_sl1 ;
    flag_p   *= env.flag_p   ;   (** Pascal *)
    flag_bnrp*= env.flag_bnrp;  (** BNR Pascal *)
    flag_syscall  *= env.flag_syscall;  (** for system call *)
    flag_stdcall  *= env.flag_stdcall;  (** for Win32 *)
    flag_vmcall   *= env.flag_vmcall;   (** StdCall but save all regs *)
    flag_lightcall*= env.flag_lightcall;(** SdtCall but save no regs *)
    flag_javacall *= env.flag_javacall; (** for Java Methods *)

CONST AllLanguages* = LangSet{ flag_c, flag_p, flag_m2, flag_o2,
                               flag_syscall, flag_javacall,
                               flag_stdcall, flag_vmcall, flag_lightcall
                             };

CONST LangsWithCLikeStrings* = LangSet{ flag_c,
                                        flag_syscall,
                                        flag_stdcall,
                                        flag_vmcall,
                                        flag_lightcall,
                                        flag_javacall };

      LangsWithOpenArrays* = LangSet{ flag_o2, flag_m2 };

      LangsWithTypedRecords* = LangSet{ flag_o2 };

      LangsWithModuleConstructors* = LangSet{ flag_o2, flag_m2 };

      LangsWithPushingParams_RtoL* = AllLanguages - LangSet{ flag_p,flag_javacall };

      LangsWithSpecifiedComputingParamsOrder* = LangSet{ flag_java };

      LangsWithComputingParams_RtoL* = LangSet{};

      LangsWithSEQParams* = LangSet{ flag_c, flag_syscall, flag_javacall,
                                     flag_stdcall, flag_vmcall, flag_lightcall };

      LangsWithGarbageCollector* = LangSet{flag_o2, flag_java};

      LangsWithRTTI* = LangSet{flag_o2, flag_java};

      LangsAllowCommands* = LangSet{flag_m2, flag_o2};

      LangsWithTypeDescriptors* = LangSet{ flag_o2, flag_java };


TYPE
  OB_MODE *= (
      ob_inv,       -- = OB_MODE{ 0 };    (** invalid object *)
      ob_var,       -- = OB_MODE{ 1 };
      ob_varpar,    -- = OB_MODE{ 2 };    (** variable parameter *)
      ob_seq,       -- = OB_MODE{ 3 };    (** seq parameter *)
      ob_label,     -- = OB_MODE{ 4 };
      ob_proc,      -- = OB_MODE{ 5 };
      ob_xproc,     -- = OB_MODE{ 6 };    (** must have procedure value *)
      ob_eproc,     -- = OB_MODE{ 7 };    (** external, proc. body is in separate module *)
      ob_lproc,     -- = OB_MODE{ 8 };    (** public procedure *)
      ob_cproc,     -- = OB_MODE{ 9 };    (** code procedure *)
      ob_cons,      -- = OB_MODE{ 10 };   (** constant *)
      ob_type,      -- = OB_MODE{ 11 };
      ob_sproc,     -- = OB_MODE{ 12 };   (** RTS procedure *)
      ob_field,     -- = OB_MODE{ 13 };
      ob_field_bts, -- = OB_MODE{ 14 };   (** bit field *)
      ob_unused,    -- = OB_MODE{ 15 };
      ob_module,    -- = OB_MODE{ 16 };
      ob_header,    -- = OB_MODE{ 17 };   (** for variant records *)
      ob_interface,  -- = OB_MODE{ 18 };   (** J interface *)
      ob_aux1,
      ob_aux2,
      ob_aux3,
      ob_aux4,
      ob_aux5,
      ob_aux6
  );

  OB_SET* = PACKEDSET OF OB_MODE;

  (** type modes *)
TYPE
  TY_MODE *= (
      ty_shortcard,--*   = TY_MODE{ 0 };
      ty_cardinal,--*    = TY_MODE{ 1 };
      ty_longcard,--*    = TY_MODE{ 2 };
      ty_shortint,--*    = TY_MODE{ 3 }; (** ty_longlongint > ty_longint > ty_integer > ty_shortint *)
      ty_integer,--*     = TY_MODE{ 4 };
      ty_longint,--*     = TY_MODE{ 5 };
      ty_longlongint,--* = TY_MODE{ 6 };
      ty_ZZ,--*          = TY_MODE{ 7 }; (** whole literal *)
      ty_real,--*        = TY_MODE{ 8 }; (** ty_longreal > ty_real, reals > ints or crds *)
      ty_longreal,--*    = TY_MODE{ 9 };
      ty_ld_real,--*     = TY_MODE{ 10 };
      ty_RR,--*          = TY_MODE{ 11 }; (** real literal *)
      ty_complex,--*     = TY_MODE{ 12 };
      ty_lcomplex,--*    = TY_MODE{ 13 }; (** ty_lcomplex > ty_complex > reals, ints or cards *)
      ty_CC,--*          = TY_MODE{ 14 }; (** complex literal *)
      ty_boolean,--*     = TY_MODE{ 15 };
      ty_char,--*        = TY_MODE{ 16 };
      ty_AA,--*          = TY_MODE{ 17 }; (** address literal (type of NIL) *)
      ty_range,--*       = TY_MODE{ 18 };
      ty_enum,--*        = TY_MODE{ 19 };
      ty_opaque,--*      = TY_MODE{ 20 };
      ty_pointer,--*     = TY_MODE{ 21 };
      ty_set,--*         = TY_MODE{ 22 };
      ty_proctype,--*    = TY_MODE{ 23 };
      ty_array,--*       = TY_MODE{ 24 };
      ty_array_of,--*    = TY_MODE{ 25 };
      ty_SS,--*          = TY_MODE{ 26 }; (** string literal *)
      ty_record,--*      = TY_MODE{ 27 };
      ty_protection,--*  = TY_MODE{ 28 };
      ty_uchar,--*       = TY_MODE{ 29 };
      ty_void,--*        = TY_MODE{ 30 };
      ty_loc,--*         = TY_MODE{ 31 };
      ty_module,--*      = TY_MODE{ 32 };
      ty_process,--*     = TY_MODE{ 33 }; (** process context *)

      ty_longlongcard,--*= TY_MODE{ - };
      ty_free,--*        = TY_MODE{ 40 };
      ty_aux1,
      ty_aux2,
      ty_aux3,
      ty_aux4,
      ty_aux5,
      ty_aux6,
      ty_aux7
  );
  TY_SET *= PACKEDSET OF TY_MODE;

TYPE (** type tags *)
  TTAG *= (
     ttag_usage_ok,--*    = TTAG{ 0 };
     ttag_has_o2_ptr,--*  = TTAG{ 1 };
     ttag_packed,--*      = TTAG{ 2 };
     ttag_aux3,
     ttag_c_type,--*      = TTAG{ 4 }; (** C-types: int, unsigned, size_t *)
     ttag_std_type,--*    = TTAG{ 5 }; (** std types: int, void, etc, used in SL only *)
     ttag_except,
     ttag_throws,
     ttag_aux8,
     ttag_aux9,
     ttag_aux10,
     ttag_aux11,
     ttag_aux12,
     ttag_aux13,
     ttag_intrinsic,  (* встроенная фун-ия - реализуется сопроцессором *)
     ttag_aux15,
     ttag_no_struct,--* =VAL(pc.TTAG, 16); (** do not use struct keyword for record type *)  !!!used in xm!!!
     ttag_aux16,
     ttag_aux17,
     ttag_aux18,
     ttag_aux19,
     ttag_aux20,
     ttag_aux21,
     ttag_aux22,
     ttag_aux23,
     ttag_aux24,
     ttag_strictcallconv,
     ttag_alwaysinline,
     ttag_neverinline,
     ttag_volatile,--*    =TTAG{ 28 }; (** volatile type                  *)
     ttag_aux29,
     ttag_aux30,
     ttag_aux31
  );
  TTAG_SET *= PACKEDSET OF TTAG;

TYPE ProcAttr *= (alwaysinline,neverinline,except,throws,strictcallconv);
     ProcAttrSet *= PACKEDSET OF ProcAttr;

TYPE
  bext_rec* = RECORD
  END;

  pars_rec* = RECORD
    lang-: Lang;
    vers-: ARRAY 24 OF CHAR; (** front-end version, e.: "Oberon-2 v1.11.3" *)
    next : PARS;
  END;

  code_rec* = RECORD
    fwd,bck      : CODE;
    name-        : ARRAY 16 OF CHAR;
    (*---------------------------------------------------*)
    vers*        : ARRAY 16 OF CHAR; (** back-end version *)
    sym_ident*   : LONGINT; (** symfile ident. See sym_* constants *)
    valid_idents*: SET;     (** all valid symfile idents for this BE.
                               test: (id-sym_base) IN valid_idents
                             *)
    (** interface with back-end *)
    max_index*  : VALUE;

    int16*      : BOOLEAN; (** size of SYSTEM.int & SYSTEM.unsigned types *)
    index16*    : BOOLEAN;
    address16*  : BOOLEAN; (** DIFADR type is integer or longint *)
    def_storage*: BOOLEAN; (** enable default memory manager *)
    en_preopt*  : BOOLEAN; (** pcConst: enable all optimizations *)
    en_tmpvar*  : BOOLEAN; (** pcConst: enable temp. var. generation *)
    en_f_inline*: BOOLEAN; (** pcConst: enable function inlining in expressions *)
    max_dim*    : LONGINT; (** max array dimensions *)
    max_ext_lev*: LONGINT; (** max record extensions *)

    FRETs*      : TY_SET;     (** may be returned by function *)

    code_ext*   : STRING;  (** code file extension   *)
    head_ext*   : STRING;  (** header file extension *)

    bits_per_loc* : SHORTINT;
    locs_per_word*: SHORTINT;

    max_sysflag*: Lang;
  END;

  unit_rec* = RECORD
  END;

  const_rec* = RECORD
  END;

  irobject_rec *= RECORD
    lref  *: LONGINT;      -- logical reference
  <* IF COMPONENT_TESTCOVERAGE THEN *>
    owner *: IROBJECT;     -- NIL or reference to local module
  <* END *>
  END;


  struct_rec* = RECORD (irobject_rec)
    mode* : TY_MODE;  (** struct mode, see ty_* consts *)
    flag* : Lang;     (** source language *)
    align*: SHORTINT; (** type alignment *)
    mno*  : Mno;  (** host module ident *)
    tags* : TTAG_SET;
    marks*: TMARK_SET;      (** temporary marks - this field is not written to symfile *)
    obj*  : OBJECT;   (** type definition object (ob_type or PROCs *)
    mem*  : OBJECT;   (** record   - methods
                         proctype - local objects
                         module   - not public objects
                       *)
    prof* : OBJECT;    (** record - fields list (tree for variant records)
                         proctype - args list
                         enum     - consts list
                         module   - public objects (to write to symfile)
                       *)
    base*: STRUCT;   (** array, pointer - base type; proc - return type *)
    inx* : STRUCT;   (** array, array_of - index type
                         method  - super method
                         module  - pointer to J instance record
                     *)
    len* : LONGINT;  (** array, enum, set - length
                         module - version tag
                         record - extension level
                     *)
    num* : LONGINT;  (** record - total methods count   *)
    min* : VALUE;  (*3 Turn to NODE -- Vit *)
    max* : VALUE;
    pos* : TPOS;     (** text position (start of type def.) *)
    end* : TPOS;     (** text position (end of type def.)
                         proctype - right parenthesis in parm. list
                     *)
    use* : USAGE;    (** procedure   : used vars
                         module      : import
             J record    : interfaces implemented
                     *)
    ext* : BEXT;     (** for back-end only              *)
<* IF target_idb THEN *>
    eno* : LONGINT;      (* id of corresponding entity *)
<* END *>
  END;

  usage_rec* = RECORD
    obj*  : OBJECT;
    tags* : UTAG_SET;
    next* : USAGE;
  END;

CONST
  OB_All     = -OB_SET{};
  OB_Auxes   = OB_SET{ob_aux1 .. ob_aux3};
  OB_Common* = OB_All - OB_Auxes;

  VARs*      = OB_SET{ob_var, ob_varpar, ob_seq};
  FIELDs*    = OB_SET{ob_field, ob_field_bts};
  PROCs*     = OB_SET{ob_proc, ob_xproc, ob_eproc, ob_lproc, ob_cproc};

TYPE
  OTAG  *= (
    otag_public,    --= OTAG{ 0 }; (** objects is in module.prof list               *)
    otag_valpar,    --= OTAG{ 1 }; (** to distinguish between var & value parameter *)
    otag_public_f,  --= OTAG{ 2 }; (** exported fields and methods                  *)
    otag_RO,        --= OTAG{ 3 }; (** read only var.                               *)
    otag_RO_public, --= OTAG{ 4 }; (** RO while is accessed from external modules   *)
    otag_no_threat, --= OTAG{ 5 }; (** object name is used only in its scope        *)
    otag_no_aliases,--= OTAG{ 6 }; (** no references to object except its name      *)
    otag_with,      --= OTAG{ 7 }; (** variable is used exactly in one WITH stat.   *)
    otag_C_pref, (* добавлять префикс к имени *)
    otag_aux1,      --= OTAG{ 8 };
    otag_aux2,      --= OTAG{ 9 };
    otag_aux3,      --= OTAG{ 10 };
    otag_aux4,      --= OTAG{ 11 };
    otag_aux5,      --= OTAG{ 12 };
    otag_aux6,      --= OTAG{ 13 };
    otag_aux7,      --= OTAG{ 14 };
    otag_aux8,      --= OTAG{ 15 };
-- there is no otag_aux9!!!!!
    otag_aux10,     --= OTAG{ 17 };
    otag_aux11,     --= OTAG{ 18 };
    otag_aux12,     --= OTAG{ 19 };
    otag_aux13,     --= OTAG{ 20 };
    otag_aux14,     --= OTAG{ 21 };
    otag_aux15,     --= OTAG{ 22 };
    otag_aux16,     --= OTAG{ 23 };
    otag_aux17,     --= OTAG{ 24 };
    otag_aux18,     --= OTAG{ 25 };
    otag_haveExceptTable,     --= OTAG{ 26 }; -- module uses SYSTEM.EXCEPTTABLE inside
    otag_aux20,     --= OTAG{ 27 };
    otag_volatile,  --=OTAG{ 28 }; (** volatile variable                            *)
    otag_reexported,--=OTAG{ 29 }; (** reexporeted (non-primitive) constant         *)
    otag_secretvar, --=OTAG{ 30 }; (** auxiliaury var - does not exist in code      *)
      -- in oberon compiler - nameless function parameter
    otag_aux21     --= OTAG{ 31 };
  );

  OTAG_SET *= PACKEDSET OF OTAG;

  (** Java modifiers and special tags *)
  XOTAG *= (
    xot_public,--*       = XOTAG{ 0 };
    xot_private,--*      = XOTAG{ 1 };
    xot_protected,--*    = XOTAG{ 2 };
    xot_static,--*       = XOTAG{ 3 };
    xot_final,--*        = XOTAG{ 4 };
    xot_synchron,--*     = XOTAG{ 5 };
    xot_volatile,--*     = XOTAG{ 6 };  (** very different from ttag_volatile *)
    xot_transient,--*    = XOTAG{ 7 };
    xot_native,--*       = XOTAG{ 8 };
    xot_interface,--*    = XOTAG{ 9 };
    xot_abstract,--*     = XOTAG{ 10 };
    xot_alien,--*        = XOTAG{ 11 }; (** member from inherited class (not from THIS) *)
    xot_secret,--*       = XOTAG{ 12 }; (** to hide additional global vars *)
    xot_entryp,--*       = XOTAG{ 13 }; (** for the "main" procedure *)
    xot_member,--*       = XOTAG{ 14 }; (** must be exported from obj-file *)
    xot_constr,--*       = XOTAG{ 15 }; (** constructor *)
    xot_safedestr,--*    = XOTAG{ 16 }; (** this method has safe finalize() **)
  --------------------------------
 (* the above "xot" constants must correspond to "mdf_*" declared in xjRTS *)

    xot_inlinable,--*    = XOTAG{ 17 }; (** potentially inlinable method *)
    xot_synthetic,--*    = XOTAG{ 18 }; (** object synthesized by compiler *)
    xot_noninlinable,--* = XOTAG{ 19 }; (** this proc will never be inlined *)
    xot_emptydestr,  --  = XOTAG{ 20 };
    xot_imnoninline,--*  = XOTAG{ 21 };
    xot_overloaded,--*   = XOTAG{ 22 }; (** this method is overloaded (not overriden!!) *)
    xot_statini,--*      = XOTAG{ 23 }; (** this static field has non-constant initializer *)
    xot_clinit,--*       = XOTAG{ 24 };
    xot_redundant,--*    = XOTAG{ 25 }; (** this method isn't used actually **)
    xot_allocator,--*    = XOTAG{ 26 }; (** this proc never returns NULL    **)
    xot_auxilary,--*     = XOTAG{ 27 }; ** **)
    xot_has_main,--      = XOTAG{ 28 }; (** class has "public static void main" mathod *)
    xot_string,  --      = XOTAG{ 29 };
    xot_fuzzy,  --       = XOTAG{ 30 };
    xot_schizoid--*      = XOTAG{ 31 }; (** this member is overloaded with return type *)
  );
  XOTAG_SET*= PACKEDSET OF XOTAG;

CONST
  xot_userclass* = xot_constr; (* TD modifier for user classes (which have ClassLoader!=null) *)
                               (* should be set for classes compiled with JDKCLASSES-         *)

CONST
  OTAG_All     = -OTAG_SET{};
  OTAG_Auxes   = OTAG_SET{otag_aux1 .. otag_aux20, otag_aux21};
  OTAG_Common* = OTAG_All - OTAG_Auxes;


TYPE
  object_rec* = RECORD (irobject_rec)
    mode*  : OB_MODE;  (** object mode, see ob_* consts *)
    lev*   : SHORTINT; (** level of scope *)
    flag*  : Lang;     (** source language *)
    mno*   : Mno;  (** module no *)
    sno*   : INTEGER;  (** can be temporary used in front or back end *)
    name*  : STRING;
    next*  : OBJECT;
    actual*: OBJECT;   (** reexported const - actual object *)
    type*  : STRUCT;
    host*  : STRUCT;   (** o^.host^.[prof|mem]{^.next}=o  *)
    tags*  : OTAG_SET;
    xtags* : XOTAG_SET;      (** additional object tags - J modifiers xot_* *)
    marks* : OMARK_SET;      (** temporary marks - this field is not written to symfile *)
    val*   : NODE;     (** for const                      *)
    attr*  : IROBJECT;     (** for aux object attributes      *)
    pos*   : TPOS;     (** text position of declaration
                        usually - object ident.
                       *)
    end*   : TPOS;     (** text position of declaration
                          proc,module - END ident
                         obj.val.pos - BEGIN
                       *)
    ext*   : BEXT;
<* IF target_idb THEN *>
    eno* : LONGINT;     (* id of corresponding entity *)
    type_use*: NODE;
<* END *>
  END;

  value_rec*  = RECORD
    pos*  : TPOS;
    expr* : NODE;      (*3 eliminated -- Vit *)
  END;


  ND_MODE *= (
      nd_inv,      (** invalid node *)
      nd_module,
      nd_var,
      nd_proc,
      nd_sproc,
      nd_method,
      nd_type,
      nd_prot,     (** get current protection *)
      nd_field,
      nd_index,
      nd_deref,
      nd_eguard,
      nd_guard,
      nd_binary,   (** operator in sub *)
      nd_unary,    (** operator in sub *)
      nd_lconv,    (** designator type conversion *)
      nd_value,    (** for literals *)
      nd_aggregate,
      nd_sequence,
      nd_pair,     (** supplementary node [a..b] *)
      nd_node,     (** supplementary node l,r    *)
      nd_replace,  (** eval left, result in right *)

      nd_call,
      nd_assign,
      nd_while,
      nd_repeat,
      nd_loop,
      nd_exit,
      nd_return,
      nd_for,
      nd_with,
      nd_wtrap,    (** TRAP(GUARD_TRAP) *)
      nd_ftrap,    (** TRAP(RETURN_TRAP) *)
      nd_if,
      nd_case,
      nd_caselse,
      nd_casedo,
      nd_null,     (** null statement *)
      nd_eval,     (** evaluate expression and drop result *)
      nd_goto,     (** ^.l must point to nd_label *)
      nd_label,
      nd_block,
      nd_finally,  (** register finalization procedure *)
      nd_except,   (** if exception *)
      nd_reraise,  (** remove or reraise exception *)
      nd_activate, (** exception must be reraised  *)
      nd_retry,    (** RETRY statement (ISO M2) *)
      nd_protect,  (** protected block *)
      nd_synchron, (** J syncronized block *)
      nd_throw,    (** J throw with a param *)
      nd_catch,
      nd_break,    (** J break statement *)
      nd_continue, (** J continue statement *)
      nd_java_for, (** J for statement *)

      nd_last
  );
  NODE_SET *= PACKEDSET OF ND_MODE;
CONST
  nd_free*     = nd_last;  (** for extensions *)

--CONST (** subclass in node *)
TYPE   SUB_MODE *= (
  su_none    ,--=  SUB_MODE{ 0 };          (** SYSTEM.BITS *)
(** unary: *)
  su_bits    ,--=  SUB_MODE{ 1 };          (** SYSTEM.BITS *)
  su_bytes   ,--=  SUB_MODE{ 2 };          (** SYSTEM.BYTES *)
  su_size    ,--=  SUB_MODE{ 3 };          (** in LOCs *)
  su_words   ,--= SUB_MODE{ 26 };
  su_min     ,--=  SUB_MODE{ 4 };
  su_max     ,--=  SUB_MODE{ 5 };
  su_is      ,--=  SUB_MODE{ 6 };
  su_abs     ,--=  SUB_MODE{ 7 };
  su_neg     ,--=  SUB_MODE{ 8 };
  su_cc      ,--=  SUB_MODE{ 9 };          (** SYSTEM.CC  *)
  su_adr     ,--= SUB_MODE{ 10 };          (** SYSTEM.ADR *)
  su_adr_o2  ,--= SUB_MODE{ 11 };          (** SYSTEM.ADR *)
  su_cap     ,--= SUB_MODE{ 12 };
  su_conv    ,--= SUB_MODE{ 13 };          (** type conversion *)
  su_cast    ,--= SUB_MODE{ 14 };          (** type transfer *)
  su_odd     ,--= SUB_MODE{ 15 };
  su_not     ,--= SUB_MODE{ 16 };
  su_compl   ,--= SUB_MODE{ 17 };          (** set complement *)
  su_entier  ,--= SUB_MODE{ 18 };
  su_length  ,--= SUB_MODE{ 19 };          (** of string *)
  su_im      ,--= SUB_MODE{ 20 };
  su_re      ,--= SUB_MODE{ 21 };

  su_bit_offs  ,--= SUB_MODE{ 22 };   (** bit offset from word boundary       *)
  su_byte_offs ,--= SUB_MODE{ 23 };   (** byte offset inside structure        *)
  su_word_offs ,--= SUB_MODE{ 24 };   (** word offset inside structure        *)
  su_width     ,--= SUB_MODE{ 25 };   (** width of structure field in bits    *)
  (* 26 - su_words *)

  su_ptr2vptr  ,--= SUB_MODE{ 27 };   (** POINTER => VIRTUAL POINTER conversion *)
  su_vptr2ptr  ,--= SUB_MODE{ 28 };   (** VIRTUAL POINTER => POINTER conversion *)


(** binary: *)
  sb_high    ,--= SUB_MODE{ 30 };
  sb_len     ,--= SUB_MODE{ 31 };
  sb_equ     ,--= SUB_MODE{ 32 };
  sb_neq     ,--= SUB_MODE{ 33 };
  sb_lss     ,--= SUB_MODE{ 34 };
  sb_leq     ,--= SUB_MODE{ 35 };
  sb_gtr     ,--= SUB_MODE{ 36 };
  sb_geq     ,--= SUB_MODE{ 37 };
  sb_in      ,--= SUB_MODE{ 38 };
  sb_mul     ,--= SUB_MODE{ 39 };
  sb_div     ,--= SUB_MODE{ 40 };
  sb_mod     ,--= SUB_MODE{ 41 };
  sb_rem     ,--= SUB_MODE{ 42 };
  sb_slash   ,--= SUB_MODE{ 43 };
  sb_plus    ,--= SUB_MODE{ 44 };
  sb_minus   ,--= SUB_MODE{ 45 };
  sb_and     ,--= SUB_MODE{ 46 };
  sb_or      ,--= SUB_MODE{ 47 };     -- evaluates 2 arguments anyway
  sb_xor     ,--= SUB_MODE{ 48 };     --
  sb_bic     ,--= SUB_MODE{ 49 };
  sb_cand    ,--= SUB_MODE{ 50 };     -- evaluates either 1 or 2 arguments
  sb_cor     ,--= SUB_MODE{ 51 };     --
  sb_ash     ,--= SUB_MODE{ 52 };                          --
  sb_lsh     ,--= SUB_MODE{ 53 };     (** SYSTEM.LSH *)    -- shifts with a signed param
  sb_rot     ,--= SUB_MODE{ 54 };     (** SYSTEM.ROT *)    --
  sb_addadr  ,--= SUB_MODE{ 55 };     (** SYSTEM.ADDADR *)
  sb_subadr  ,--= SUB_MODE{ 56 };     (** SYSTEM.SUBADR *)
  sb_difadr  ,--= SUB_MODE{ 57 };     (** SYSTEM.DIFADR *)
  sb_pre_inc ,--= SUB_MODE{ 58 };
  sb_pre_dec ,--= SUB_MODE{ 59 };
  sb_post_inc,--= SUB_MODE{ 60 };
  sb_post_dec,--= SUB_MODE{ 61 };
  sb_concat  ,--= SUB_MODE{ 62 };     (** string concatenation *)
  sb_bit     ,--= SUB_MODE{ 63 };     (** SYSTEM.BIT *)
  sb_cmplx   ,--= SUB_MODE{ 64 };
  sb_exp     ,--= SUB_MODE{ 65 };     (** exponentiation operator *)
  sb_shl     ,--= SUB_MODE{ 66 };     (** shift left  *)
  sb_shr     ,--= SUB_MODE{ 67 };     (** shift right *)

  sb_sar     ,--= SUB_MODE{ 68 };     (** arith shift right *)
  sb_asplus  ,--= SUB_MODE{ 69 };
  sb_asminus ,--= SUB_MODE{ 70 };
  sb_asmul   ,--= SUB_MODE{ 71 };
  sb_asdiv   ,--= SUB_MODE{ 72 };
  sb_asmod   ,--= SUB_MODE{ 73 };
  sb_asand   ,--= SUB_MODE{ 74 };
  sb_asor    ,--= SUB_MODE{ 75 };
  sb_asxor   ,--= SUB_MODE{ 76 };
  sb_asar   ,--= SUB_MODE{ 77 };
  sb_ashl    ,--= SUB_MODE{ 78 };
  sb_ashr    ,--= SUB_MODE{ 79 };

  (** RTS procedures *)
  sp_assert  ,--= SUB_MODE{ 100 };
  sp_halt    ,--= SUB_MODE{ 101 }; (** HALT(n) *)
  sp_abort   ,--= SUB_MODE{ 102 }; (** HALT;   *)
  sp_new     ,--= SUB_MODE{ 103 };
  sp_dispose ,--= SUB_MODE{ 104 };
  sp_sysnew  ,--= SUB_MODE{ 105 }; (** dynamic type ,--= ARRAY OF LOC *)
  sp_code    ,--= SUB_MODE{ 106 }; (** SYSTEM.CODE *)
  sp_move    ,--= SUB_MODE{ 107 };
  sp_incl    ,--= SUB_MODE{ 108 };
  sp_excl    ,--= SUB_MODE{ 109 };
  sp_get     ,--= SUB_MODE{ 110 }; (** SYSTEM.GET *)
  sp_getreg  ,--= SUB_MODE{ 111 }; (** SYSTEM.GETREG *)
  sp_put     ,--= SUB_MODE{ 112 }; (** SYSTEM.PUT *)
  sp_putreg  ,--= SUB_MODE{ 113 }; (** SYSTEM.PUTREG *)
  sp_fill    ,--= SUB_MODE{ 114 }; (** SYSTEM.FILL *)
  sp_copy    ,--= SUB_MODE{ 115 };
  sp_jnew    ,--= SUB_MODE{ 116 }; (** Java NEW (with a return value) *)
  sp_getxobj ,--= SUB_MODE{ 117 }; (** X2J_GetObjectThrown()    *)
  sp_icast   ,--= SUB_MODE{ 118 }; (** X2J_2InterfCast *)
  sp_iinstof ,--= SUB_MODE{ 119 }; (** X2J_InterfIS *)
  sp_lockobj ,--= SUB_MODE{ 120 }; (** X2J_LOCKOBJ *)
  sp_freeobj ,--= SUB_MODE{ 121 }; (** X2J_FREEOBJ *)
  sp_arrcast ,--= SUB_MODE{ 122 }; (** X2J_2ArrayCast *)
  sp_ainstof ,--= SUB_MODE{ 123 }; (** X2J_ArrayIS *)
  sp_initstr ,--= SUB_MODE{ 124 }; (** X2J_INIT_STRING *)
  sp_chknull ,--= SUB_MODE{ 125 }; (** X2J_TRAP_NIL *)
  sp_jstacknew ,--= SUB_MODE{ 126 }; (** stack allocation *)
  sp_initarr   ,--= SUB_MODE{ 127 }; (** X2J_INIT_ARRAY *)

  sp_less64  ,--= SUB_MODE{ -1 };  -- <
  sp_leq64   ,--= SUB_MODE{ -2 };  -- <=
  sp_gtr64   ,--= SUB_MODE{ -3 };  -- >
  sp_geq64   ,--= SUB_MODE{ -4 };  -- >=
  sp_eq64    ,--= SUB_MODE{ -5 };  -- ==
  sp_neq64   ,--= SUB_MODE{ -6 };  -- !=
  sp_or64    ,--= SUB_MODE{ -7 };  -- |
  sp_and64   ,--= SUB_MODE{ -8 };  -- &
  sp_xor64   ,--= SUB_MODE{ -9 };  -- ^
  sp_not64   ,--= SUB_MODE{ -10 }; -- ~

  sp_minus64 ,--= SUB_MODE{ -11 }; -- -(un)
  sp_add64   ,--= SUB_MODE{ -12 }; -- +
  sp_sub64   ,--= SUB_MODE{ -13 }; -- -
  sp_mul64   ,--= SUB_MODE{ -14 }; -- *
  sp_div64   ,--= SUB_MODE{ -15 }; -- /
  sp_rem64   ,--= SUB_MODE{ -16 }; -- %
  sp_shl64   ,--= SUB_MODE{ -17 }; -- <<
  sp_shr64   ,--= SUB_MODE{ -18 }; -- >>
  sp_ushr64  ,--= SUB_MODE{ -19 }; -- >>>
  sp_inc64   ,--= SUB_MODE{ -20 }; -- ++
  sp_dec64,   --= SUB_MODE{ -21 }; -- -- ;)

  su_hiword,

  sub_aux0,
  sp_getclass,    (** X2J_GETCLASS *)
  sp_stdout,      (** X2C_STDOUT *)
  sp_trace_proc,  (** X2J_Trace_Proc *)

  su_method_is,   (** 'is' for instance methods, used in 'quasiinline' *)
  sp_lighticast,  (** light InterfCast *)
  sp_illegalcast,  (** throw IllegalCastException *)
  sp_noclassdeffound, (** throw NoClassDefFoundError *)
  sp_nosuchfield,    (** throw NoSuchFieldError *)
  sp_nosuchmethod,    (** throw NoSuchMethodError *)
  sp_callerIPref,
  sp_excepttable,

  sp_casetrap     -- TESTCOVERAGE: trap in CASE::ELSE 
  );
  SUB_SET *= PACKEDSET OF SUB_MODE;

TYPE
  backend_node_info* = RECORD END;

  BEINFO* = POINTER TO backend_node_info;

  node_rec* = RECORD (irobject_rec)
    mode*: ND_MODE;      (** node mode, see nd_* consts *)
    sub* : SUB_MODE;     (** node submode for nd_binary, nd_unary, nd_sproc *)
    tags*: NTAG_SET;
    next*: NODE;
    type*: STRUCT;
    obj* : OBJECT;
    pos* : TPOS;         (** begin text position *)
    end* : TPOS;         (** end   text position *)
    l*,r*: NODE;
    val* : VALUE;
<* IF target_idb THEN *>
    eno* : LONGINT;      (* id of corresponding entity *)
<* END *>

    be_info*: BEINFO;
  END;

  Registrator* = PROCEDURE (o: IROBJECT): BOOLEAN;

  ValuePatternFormat *= (
  (** value pattern formats *)
    vpf_2compl8,
    vpf_2compl16,
    vpf_2compl32,
    vpf_bitset,
    vpf_float32, (** i486 FPU single precision format *)
    vpf_float64, (** i486 FPU double precision format *)
    vpf_float80  (** i486 FPU extended precision format *)
  );

CONST
  (** symfile idents. See code.ident & code.valid_idents *)
  sym_base*   = 334;        (** value is fixed for backward compatibility *)
  sym_C*      = sym_base+0; (** XDS/C symbol file *)
  sym_native* = sym_base+1; (** Native XDS symbol file *)

  OOP_langs*       = LangSet{flag_o2, flag_java, flag_jbc};
  OA_langs*        = LangSet{flag_m2, flag_o2, flag_java, flag_jbc};
  CallResOptional* = LangSet{flag_java, flag_c, flag_p,
                                flag_stdcall, flag_vmcall, flag_lightcall,
                                flag_syscall, flag_javacall};
  ExtProcsAllowed* = LangSet{flag_p, flag_c,
                                flag_stdcall, flag_vmcall, flag_lightcall,
                                flag_syscall, flag_javacall};

  INTs*     = TY_SET{ty_shortint,  ty_integer,  ty_longint, ty_longlongint, ty_ZZ};
  CARDs*    = TY_SET{ty_shortcard, ty_cardinal, ty_longcard, ty_longlongcard, ty_ZZ
               <* IF TARGET_MEGOS THEN *> , ty_loc <* END *> };
  WHOLEs*   = INTs + CARDs;

  SIGNED_WHOLEs*   = TY_SET{ty_shortint, ty_integer, ty_longint, ty_longlongint, ty_ZZ};
  UNSIGNED_WHOLEs* = TY_SET{ty_shortcard,ty_cardinal,ty_longcard,  ty_longlongcard, ty_uchar};

  ORDs*     = WHOLEs + TY_SET{ty_char, ty_uchar, ty_enum, ty_boolean, ty_range};
  REALs*    = TY_SET{ty_real, ty_longreal, ty_ld_real, ty_RR};
  CPLXs*    = TY_SET{ty_complex, ty_lcomplex, ty_CC};
  NUMs*     = WHOLEs + REALs;
  CNUMs*    = WHOLEs + REALs + CPLXs;
  ARRs*     = TY_SET{ty_array, ty_array_of, ty_SS};
  ADRs*     = TY_SET{ty_pointer, ty_AA};
  VPTRs*    = TY_SET{ty_pointer, ty_opaque};        -- valid pointers
  PTRs*     = VPTRs + TY_SET{ty_AA};                -- all pointers
  PADRs*    = PTRs + TY_SET{ty_proctype};           -- all addresses
  SETs*     = TY_SET{ty_set};
  COMPOUNDs*= ARRs + TY_SET{ty_record};
  NUM_LITERALs* = TY_SET{ty_ZZ, ty_RR, ty_CC}; (** size is undefined *)

  SEQ_ARRs* = COMPOUNDs + CPLXs;
  (** passed as one-dimensional array descriptor (in sequence) *)

  SEQ_PTRs* = PADRs;

  JAVA_PRIM_TYPES *= TY_SET{ty_shortint, ty_integer, ty_longint, ty_longlongint, ty_real, ty_longreal, ty_boolean, ty_uchar};

  BOUNDED_TYPES* = ORDs + REALs + TY_SET{ ty_AA };

  LVALUEs*     = NODE_SET{nd_var,nd_field,nd_index,nd_deref,nd_replace,
                          nd_guard,nd_eguard,nd_lconv};

(*----------------------------------------------------------------*)
                                 
(** Bits in tags (OBJECT, STRUCT and NODE):
      0..7  - front/back interface
      8..15 - back
     16..19 - back-end options
     20..27 - front
     28..31 - front/back interface

    Bits in xtags ( STRUCT )
      0..31 - front/back interface

    Bits in marks (OBJECT, STRUCT)
      0..9  - front
     10..19 - back
     20..27 - middle
     28..31 - browser
*)

CONST (** comment tags *)
  ctag_inline*    = CTAG{ 0 };

CONST
  EOL* = 0AX; (** is used in comments *)

TYPE
  Comment * = POINTER TO CommentRec;
  CommentRec * = RECORD
    pos*,end*: TPOS;    (** null_pos if continuation *)
    str*     : STRING;  (** comment body *)
    tags*    : CTAG_SET;
    next*    : Comment; (** comments are linked into ring! *)
  END;

  ImportedObject* = POINTER TO ImportedObject_rec;

  ImportedObject_rec* = RECORD
                          pos*,
                          end* : TPOS;
                          obj* : OBJECT;
                          next*: ImportedObject;
                        END;

  Import* = RECORD
              pos*, end*: TPOS;
              objects*  : ImportedObject;
              tail      : ImportedObject;
            END;

VAR
  comments*: Comment; (** "exported" comments list of last compiled module *)
  import*  : Import;
  value*   : VALUE;   (** used as VALUE generator *)
  pars-    : PARS;    (** front-end *)
  code*    : CODE;    (** back-end *)
  const*   : CONSTS;  (** middle-end (tree transformations) *)

  mods*    : MnoOBJECTS ; (** array of module objects *)
  mod_cnt* : Mno;
  cur_mod* : Mno;

  sys_mods*: MnoOBJECTS; (** array of pseudo modules (SYSTEM & UNIVERSE) *)

CONST
  INVMno  *= env.INVMno;
  ZEROMno *= env.ZEROMno;
  demo_mod_limit * = 9;   (* max number of modules in the DEMO version *)

VAR      void_type*,
         char_type*,
        uchar_type*,
     shortint_type*,
      integer_type*,
      longint_type*,
  longlongint_type*,
  longlongcard_type*,
    shortcard_type*,
     cardinal_type*,
     longcard_type*,
         real_type*,
     longreal_type*,
      ld_real_type*,
      complex_type*,
     lcomplex_type*,
           ZZ_type*,
           RR_type*,
           AA_type*,
           CC_type* : STRUCT;

(* Constructors for OBJECT, STRUCT, NODE objects *)

(*
PROCEDURE new_obj*(o: pc.OBJECT; mode: pc.OB_MODE);
BEGIN
  o.mode:=mode;
  o.lev:=level;
  o.flag:=pc.flag_o2;
  o.mno:=cur_mod;
  o.tags:=SYSTEM.VAL(pc.OTAG_SET, env.config.tags*{16..19});
  o.marks:=pc.OMARK_SET{};
  cur_pos(o.pos);
  o.end:=o.pos;
  o.ref:=0;
  IF module_super#NIL THEN e.name:=module_super.name END;
  o:=e;
END new_obj;

    flag*  : Lang;     (** source language *)
    mno*   : Mno;  (** module no *)
    sno*   : INTEGER;  (** can be temporary used in front or back end *)
    name*  : STRING;
    next*  : OBJECT;
    actual*: OBJECT;   (** reexported const - actual object *)
    type*  : STRUCT;
    host*  : STRUCT;   (** o^.host^.[prof|mem]{^.next}=o  *)
    tags*  : OTAG_SET;
    xtags* : XOTAG_SET;      (** additional object tags - J modifiers xot_* *)
    marks* : OMARK_SET;      (** temporary marks - this field is not written to symfile *)
    val*   : NODE;     (** for const                      *)
    attr*  : NODE;     (** for aux object attributes      *)
    pos*   : TPOS;     (** text position of declaration
                        usually - object ident.
                       *)
    end*   : TPOS;     (** text position of declaration
                          proc,module - END ident
                         obj.val.pos - BEGIN
                       *)
    ext*   : BEXT;
<* IF target_idb THEN *>
    eno* : LONGINT;     (* id of corresponding entity *)
    type_use*: NODE;
<* END *>
*)

-- Comment ---------------------------------------------------------------------
PROCEDURE (VAR c: CommentRec) Clone * (): Comment;
VAR cp: Comment;
BEGIN
  NEW(cp);
  cp.tags := c.tags;
  cp.pos  := c.pos;
  cp.end  := c.end;
  cp.next := NIL;
  IF c.str # NIL THEN
    DStrings.Assign(c.str^, cp.str);
  ELSE
    cp.str := NIL;
  END;
  RETURN cp;
END Clone;

PROCEDURE (VAR c: CommentRec) isBelongToCurrentModule * (): BOOLEAN;
VAR line, pos: LONGINT;
    file: env.String;
    module: STRING;
BEGIN
  c.pos.unpack(file, line, pos);
  IF env.info.file # NIL THEN
    RETURN (file^ = env.info.file^);
  ELSE
    xfs.sys.GetName(file^, module);
    RETURN (module^ = env.info.module^)
  END;
END isBelongToCurrentModule;

PROCEDURE (VAR c: CommentRec) isMarker * (marker: ARRAY OF CHAR): BOOLEAN;
VAR found: BOOLEAN;
    start_pos: SYSTEM.CARD32;
BEGIN
  Strings.FindNext ("(** ", c.str^, 0, found, start_pos);
  IF found THEN
    Strings.FindNext (marker, c.str^, LENGTH("(** "), found, start_pos);
    RETURN found AND (start_pos = LENGTH("(** "))
  ELSE
    RETURN FALSE
  END
END isMarker;

PROCEDURE (c: Comment) Remove * ();
VAR curr: Comment;
BEGIN
  IF c = c.next THEN
    comments := NIL;
  ELSE
    curr := comments;
    WHILE curr.next # c DO
      curr := curr.next;
    END;
    curr.next := c.next;
    IF c = comments THEN
      comments := curr.next;
    END;
  END;
END Remove;

PROCEDURE (c: Comment) Append * ();
BEGIN
  IF comments = NIL THEN
    comments := c;
    c.next   := c;
  ELSE
    c.next        := comments.next;
    comments.next := c;
    comments      := c;
  END;
END Append;


(*----  Import ---------------------------------------------------*)

PROCEDURE (VAR i: Import) add* (o: OBJECT; start_pos-, end_pos-: TPOS);
VAR
  imp: ImportedObject;
BEGIN
  NEW(imp);
  imp.obj := o;
  imp.pos := start_pos;
  imp.end := end_pos;
  imp.next := NIL;
  IF i.objects = NIL THEN
    i.objects := imp;
  ELSE
    i.tail.next := imp;
  END;
  i.tail := imp;
END add;

(*----------------------------------------------------------------*)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ( VAR x: RIDER ) object* ( o: OBJECT );
(*
*)
BEGIN
END object;
<* POP *>
-----------------------------------------------------------------------------

(** Fills logical reference.
    Is called before writing IR object to sym-file.
    Should be called recursively for all objects
    supposed to be written into sym-file.
    Returns FALSE if the object is already registered.
*)
PROCEDURE (o: IROBJECT) Register*(r: Registrator): BOOLEAN;
BEGIN
  RETURN r(o);
END Register;

<* PUSH *>
<* WOFF301+ *>
(** Writes representation of an IR object to sym-file. *)
PROCEDURE (o: IROBJECT) Externalize* (f: xfs.SymFile);
BEGIN ASSERT (FALSE);
END Externalize;

(** Reads IR object representation from sym-file.
    The internalization code should be designed completely
    symmetrical with respect to the externalization code.
*)
PROCEDURE (o: IROBJECT) Internalize* (f: xfs.SymFile);
BEGIN ASSERT (FALSE);
END Internalize;
<* POP *>

-----------------------------------------------------------------------------

PROCEDURE (t: STRUCT) is_equal * (mode: TY_MODE): BOOLEAN;
BEGIN
  WHILE t.mode = ty_range DO 
    t := t.base 
  END;
  RETURN t.mode = mode;
END is_equal;

-----------------------------------------------------------------------------

PROCEDURE ( t: STRUCT ) is_ordinal* ( )  :   BOOLEAN;
(*
*)
BEGIN
  RETURN t^.mode IN ORDs;
  (**   ty_SS is NOT ordinal type, it's only compatible with them.
        All ordinal types have min and max (exept. ZZ),
        and have the same value representation
        i.e. value.is_ordinal (t)=TRUE
  *)
END is_ordinal;

-----------------------------------------------------------------------------

PROCEDURE ( t: STRUCT ) super_type* ( )  :   STRUCT;
(**
    return apropriate whole type, with same size and sign,
    t must be ordinal type.
*)
BEGIN
  IF t^.mode = ty_range  THEN t := t^.base END;
  IF t^.mode = ty_SS     THEN t := t^.base END;
  IF t^.mode IN WHOLEs THEN RETURN t   END;
  t := t^.base;
  ASSERT( t^.mode IN WHOLEs+TY_SET{ty_free} );
  RETURN t;
END super_type;

-----------------------------------------------------------------------------

PROCEDURE ( t: STRUCT ) signed* ( )        :   BOOLEAN;
(*
*)
BEGIN
  t := t^.super_type();
  RETURN t^.mode IN INTs;
END signed;

-----------------------------------------------------------------------------

PROCEDURE ( t: STRUCT ) objects* ( VAR p: RIDER );
(**
    Iterates all objects in type
*)
  -----------------------------------------------

  PROCEDURE list ( VAR n: OBJECT );
  (*
  *)
  VAR
    l,o : OBJECT;
    h   : STRUCT;
    m   : NODE;
  BEGIN
    l := NIL;
    o := n;
    WHILE o # NIL DO
      ASSERT( o.host = t );
      ASSERT( o.mno = t.mno );
      p.del := FALSE;
      p.object (o);
      IF p.del THEN
        CASE o.mode OF
        | ob_eproc: ASSERT( o.val = NIL );
        | ob_cproc: ASSERT( o.val.mode = nd_aggregate );
        | ob_proc, ob_xproc, ob_lproc:
            ASSERT( o.val.mode = nd_proc );
            h := o.host;
            WHILE NOT ((h.mode = ty_proctype) OR (h.mode = ty_module)) DO
              h := h.obj.host
            END;
            m := h.obj.val.l;
            IF m = o.val THEN
              h.obj.val.l := m.next;
            ELSE
              WHILE m.next # o.val DO
                m := m.next
              END;
              m.next := o.val.next;
            END;
        ELSE
        END;

        o.mode := MAX (OB_MODE);
        o := o.next;
        IF l = NIL
         THEN n := o
         ELSE l.next := o
        END;
      ELSE
        IF (o.mode = ob_type)
         & (o.type.obj = o)
         & (o.type.mode IN TY_SET{ty_record, ty_enum})
        THEN
          ASSERT( o.mno = o.type.mno );
          o.type.objects (p);
        ELSIF o.mode IN PROCs THEN
          ASSERT( o.type.obj = o );
          o.type.objects (p);
        ELSIF o.mode = ob_header THEN
          m := o.val;
          ASSERT( m.mode = nd_case );
          IF m.obj # NIL THEN
            p.del := FALSE;
            p.object (m.obj);
          (* !!!! не реализовано удаление *)
          END;
          m := m.l;
          WHILE m # NIL DO
            ASSERT( m.mode = nd_node );
            list (m.obj);
            m := m.next;
          END;
        END;
        l := o;
        o := o.next;
      END;
    END;
  END list;

  -----------------------------------------------

BEGIN
  ASSERT( (t.mode IN TY_SET{ty_proctype, ty_record, ty_enum})
       OR (t.mode = ty_module) ); -- value >31!
  list (t.prof);
  list (t.mem);
END objects;

-----------------------------------------------------------------------------

PROCEDURE ( o: OBJECT ) GetReadableName* ( need_class_name: BOOLEAN): STRING;
VAR
  ss : STRING;
BEGIN
  DStrings.Assign(o^.name^, ss);
  RETURN ss;
END GetReadableName;

-----------------------------------------------------------------------------

PROCEDURE ( o: OBJECT ) is_public* ( )   :   BOOLEAN;
(*
   Object is accessable by name from other modules
*)
BEGIN
  RETURN (otag_public IN o.tags)
      OR (o.mode = ob_lproc)
      OR (o.mode = ob_module)
      OR (o.mode = ob_xproc)
       & (o.host.mode = ty_record)
       & (otag_public IN o.host.obj.tags)
      OR (o.mode IN OB_SET{ob_field, ob_field_bts})
       & (otag_public_f IN o.tags)
      OR (o.mode = ob_cons)
       & (o.host.mode = ty_enum)
       & (otag_public IN o.host.obj.tags);
END is_public;

-----------------------------------------------------------------------------

PROCEDURE ( n: NODE ) dynamic_type* ( )  :  STRUCT;
(**
    Returns the dynamic type of node, if it is known in compile time,
    otherwise returns NIL.
*)
BEGIN
  IF n.type.mode = ty_array_of      THEN RETURN NIL    END;
  IF n.type.mode = ty_pointer       THEN
    IF n.type.base.mode # ty_record THEN RETURN n.type END;
    IF ~ (n.type.base.flag IN OOP_langs)
                                    THEN RETURN n.type END;
    RETURN NIL;
  ELSIF n.type.mode = ty_record     THEN
    IF ~ (n.type.flag IN OOP_langs) THEN RETURN n.type END;
    IF n.mode = nd_guard            THEN RETURN n.l.dynamic_type()  END;
    IF (n.mode = nd_var)
     & (n.obj.mode = ob_varpar)     THEN RETURN NIL END;
    IF n.mode = nd_deref            THEN RETURN NIL END;
  END;
  RETURN n.type;
END dynamic_type;

-----------------------------------------------------------------------------

PROCEDURE ( n: NODE ) dynamic_type_expr* ( )  :   NODE;
(**
    Returns NIL, if dynamic type of node is know in compile time,
    otherwise returns an expression for dynamic type evaluation.
*)
BEGIN
  IF n.type.mode = ty_array_of      THEN RETURN n END;
  IF n.type.mode = ty_pointer       THEN
    IF n.type.base.mode # ty_record THEN RETURN NIL END;
    IF ~ (n.type.base.flag IN OOP_langs)
                                    THEN RETURN NIL END;
    WHILE n.mode = nd_guard DO
      n := n.l
    END;
    RETURN n;
  ELSIF n.type.mode = ty_record THEN
    IF ~ (n.type.flag IN OOP_langs)
                                THEN RETURN NIL END;
    IF n.mode = nd_guard        THEN RETURN n.l.dynamic_type_expr() END;
    IF (n.mode = nd_var)
     & (n.obj.mode = ob_varpar) THEN RETURN n END;
    IF n.mode = nd_deref        THEN RETURN n.l.dynamic_type_expr() END;
  END;
  RETURN NIL;
END dynamic_type_expr;

-----------------------------------------------------------------------------

PROCEDURE ( dst: NODE ) copy* ( src: NODE );
(**
    Copies object attributes dst^ := src^
*)
BEGIN
  dst.mode := src.mode;
  dst.sub  := src.sub;
  dst.tags := src.tags;
  dst.type := src.type;
  dst.obj  := src.obj;
  dst.l    := src.l;
  dst.r    := src.r;
  dst.val  := src.val;
  dst.pos  := src.pos;
  dst.end  := src.end;
END copy;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) correctUnsigned* ();
BEGIN
  RETURN;
END correctUnsigned;
<* PUSH *>
<* WOFF301+ *>
PROCEDURE ( VAR z: value_rec ) is_ordinal* ( t: STRUCT )  :  BOOLEAN;
(**
    Is ordinal type? Literals of all ordinal types have the same
    representation in VALUE.
*)
BEGIN
  RETURN FALSE;
END is_ordinal;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) is_ZZ* ( )  :   BOOLEAN;
(*
   Is ZZ type value?
*)
BEGIN
  RETURN FALSE;
END is_ZZ;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) is_RR* ( )   :   BOOLEAN;
(*
  Is RR type value?
*)
BEGIN
  RETURN FALSE;
END is_RR;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) get_type* ( ) :STRUCT;
(*
  returns value's type that is guaranted to be one of the "pc.const" bundle.
*)
BEGIN
  ASSERT(FALSE);
END get_type;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) unary* ( cop: SUB_MODE
                                      ;   x: VALUE );
(**
    Applies unary operation (cop in su_* ) to the value z := COP (x)
    Currently available:
    operation   result type (z) operand type (x)
    -------------------------------------------
    su_cast     type of x       any type
    su_abs      ordinal         ordinal
                real            real
    su_neg      ordinal         ordinal
                real            real
                complex         complex
    su_conv     ordinal         ordinal, real, set
                real            ordinal, real
                complex         ordinal, real, complex
    su_odd      ordinal         ordinal
    su_not      ordinal         ordinal
    su_cap      ordinal         ordinal
    su_length   ordinal         string
    su_compl    ordinal         ordinal
                set             set
    su_ptr2vptr ordinal         ordinal
    su_vptr2ptr ordinal         ordinal
    su_re       real            complex
    su_im       real            complex

*)
BEGIN ASSERT( FALSE );
END unary;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) binary* (  cop: SUB_MODE
                                       ; x, y: VALUE);
(**
  Applies binary operation (cop in sb_* ) to the value z := COP (x, y)
  Currently available:
    relations, arithmetic
    sb_concat for strings
    sb_len    for strings       - returns actual length.
*)
BEGIN ASSERT( FALSE );
END binary;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) is_neg* ( )  :  BOOLEAN;
(*
*)
BEGIN ASSERT( FALSE );
END is_neg;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) is_zero* ( )  :  BOOLEAN;
(*
*)
BEGIN ASSERT( FALSE );
END is_zero;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) is_unity* ( )  :  BOOLEAN;
(*
*)
BEGIN ASSERT( FALSE );
END is_unity;


-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) get_integer* ( ) : LONGINT;
(*
   Returns integer value, corresponding to the value.
   z should be ordinal and be in the range of LONGINT type.
*)
BEGIN
  ASSERT( FALSE );
END get_integer;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) get_cardinal* ( ) :SYSTEM.CARD32;
(*
   Returns cardinal value, corresponding to the value.
   z should be ordinal and be in the range of LONGCARD type.
*)
BEGIN ASSERT( FALSE );
END get_cardinal;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) get_minBitSize* (signed: BOOLEAN) : INTEGER;
(*
   Returns minimal number of bits (>0 and multiple 8 ) which is sufficient
   to represent the value
   Note : in the case of signed value a num of bits which is sufficient for
          2's complement is meant
*)
BEGIN ASSERT(FALSE);
END get_minBitSize;

-----------------------------------------------------------------------------
(*
   Note: the get_N* methods are used only if there are no numeric types
   which are capable to contain value ( e.g. 64-bit integer )
*)

PROCEDURE (VAR z: value_rec) get_NByte* ( n: INTEGER ) : SYSTEM.CARD8;
(*
   Returns n-th byte of abstract int value (0 for low byte )
*)
BEGIN
  ASSERT( FALSE );
END get_NByte;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) get_NWord*(n :INTEGER): SYSTEM.CARD16;
(*
   Returns n-th word of abstract int value (0 for low word )
*)
BEGIN
  ASSERT(FALSE);
END get_NWord;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) get_NDWord* ( n: INTEGER ) : SYSTEM.CARD32;
(*
   Returns n-th double word of abstract int value (0 for low dword )
*)
BEGIN ASSERT(FALSE);
END get_NDWord;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) set_NDWord*(n :INTEGER; val :SYSTEM.CARD32);
(*
   Set n-th double word of abstract int value (0 for low dword index )
*)
BEGIN
  ASSERT(FALSE);
END set_NDWord;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) get_min* (n :LONGINT; signed :BOOLEAN);
(*
   Returns minimum of abstract int value ( use in BE! )
*)
BEGIN
  ASSERT(FALSE);
END get_min;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) get_max* (n :LONGINT; signed :BOOLEAN);
(*
   Returns maximun of abstract int value ( use in BE! )
*)
BEGIN
  ASSERT(FALSE);
END get_max;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) set_integer* ( x: LONGINT );
(**
   Set value as integer (with sign extesion)
*)
BEGIN ASSERT( FALSE );
END set_integer;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) set_cardinal* ( x: SYSTEM.CARD32 );
(**
   Set value as cardinal (with zero extension)
*)
BEGIN ASSERT( FALSE );
END set_cardinal;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) set_value* (rawdata :SYSTEM.ADDRESS);
(**
    make abstract value from raw data
*)
BEGIN ASSERT (FALSE);
END set_value;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) cast_ordinal* ( to_type: STRUCT );
(**
    Cast ordinal value to signed or unsigned type.
    Used in CARDINAL (-1), INTEGER (MAX (CARDINAL));
*)
BEGIN ASSERT( FALSE );
END cast_ordinal;

-----------------------------------------------------------------------------

PROCEDURE ( VAR x: value_rec ) set_radix* ( radix: SHORTINT );
(*
*)
BEGIN
  ASSERT( FALSE );
END set_radix;

-----------------------------------------------------------------------------

PROCEDURE ( VAR x: value_rec ) get_radix* ( ): SHORTINT;
(*
*)
BEGIN
  ASSERT( FALSE );
END get_radix;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) get_float* ( ) : REAL;
(*
*)
BEGIN
  ASSERT(FALSE);
END get_float;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) set_float* ( x: REAL );
(*
*)
BEGIN
  ASSERT( FALSE );
END set_float;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) get_real* ( ): LONGREAL;
(*
*)
BEGIN ASSERT( FALSE );
END get_real;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) set_real* ( x: LONGREAL );
(*
*)
BEGIN ASSERT( FALSE );
END set_real;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) is_aN* ( ) : BOOLEAN;
(*
   Returns whether this (floating-point) value is an ordinary number
   (neither NaN nor infinity)
*)
BEGIN
  ASSERT( FALSE );
END is_aN;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) get_object* ( ): OBJECT;
(*
   Returns OBJECT, corresponding to the value.
   z should be procedure type.
*)
BEGIN
  ASSERT( FALSE );
END get_object;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) set_object* ( x: OBJECT );
(**
    Set value as OBJECT
*)
BEGIN ASSERT( FALSE );
END set_object;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) str_to_value* ( str-: ARRAY OF CHAR
                                             ;  fmt: Lang );
(**
     Set value from string representation.
     "fmt" is a language flag (flag_* )
*)
BEGIN ASSERT( FALSE );
END str_to_value;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) value_to_str* ( VAR str: ARRAY OF CHAR
                                             ;     fmt: Lang );
(**
    Returns string representation of value.
    "fmt" is a language flag (flag_* )
*)
BEGIN ASSERT( FALSE );
END value_to_str;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) set_string* ( x-: ARRAY OF CHAR );
(**
    Set string value.
*)
BEGIN ASSERT( FALSE );
END set_string;


PROCEDURE ( VAR z: value_rec ) set_ustring* ( x-: ARRAY OF INTEGER );
(**
    Set unicode string value.
*)
BEGIN ASSERT( FALSE );
END set_ustring;


-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) index_set* ( i: LONGINT
                                          ; v: VALUE );
(**
    Set the value of z[i]. z is set or array.
    i must be in range [0..len-1]
*)
BEGIN ASSERT( FALSE );
END index_set;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) index_get* ( i: LONGINT
                                          ; v: VALUE );
(**
    Returns the value of v[i]. v is set or array.
    i must be in range [0..len-1].
    For a string value, an actual LEN may be less then length of type.
    Use z.binary (sb_length, v, value<0>) to retrieve the actual LENgth.
*)
BEGIN ASSERT( FALSE );
END index_get;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) pattern_get* ( VAR v: ARRAY OF SYSTEM.BYTE
                                            ;   fmt: SHORTINT );
(**
    Returns binary value. fmt in vpf_*.
*)
BEGIN ASSERT( FALSE );
END pattern_get;

-----------------------------------------------------------------------------

PROCEDURE (VAR z: value_rec) new* ( ps   :TPOS;
                                    type :STRUCT
                                  ) :VALUE;
(**
    Returns new value for type.
*)
BEGIN ASSERT( FALSE );
END new;

-----------------------------------------------------------------------------

TYPE
  WRO_PROC *= PROCEDURE (o: IROBJECT);
  RDO_PROC *= PROCEDURE (): IROBJECT;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) out* ( file   :xfs.SymFile;
                                      wr_obj :WRO_PROC );
(**
    Writes value to symfile
*)
BEGIN ASSERT( FALSE );
END out;

-----------------------------------------------------------------------------

PROCEDURE ( VAR z: value_rec ) inp* ( file   : xfs.SymFile;
                                      rd_obj :RDO_PROC
                                    ) :VALUE;
(**
    Restores value from symfile
*)
BEGIN ASSERT( FALSE );
END inp;

-----------------------------------------------------------------------------

(*3 eliminate -- Vit *)
PROCEDURE (VAR z: value_rec) node* ( ) : NODE;
(*
*)
BEGIN
  RETURN z.expr;
END node;

(*----------------------- UNIT -------------------------*)

PROCEDURE ( u: UNIT ) Definition* ( name-: ARRAY OF CHAR );
(*
*)
BEGIN
END Definition;

-----------------------------------------------------------------------------

PROCEDURE ( u: UNIT ) Implementation* ( name-: ARRAY OF CHAR );
(*
*)
BEGIN
END Implementation;

-----------------------------------------------------------------------------

PROCEDURE ( u: UNIT ) Module* ( name-: ARRAY OF CHAR );
(*
*)
BEGIN
END Module;

-----------------------------------------------------------------------------

PROCEDURE ( u: UNIT ) Import* ( name-: ARRAY OF CHAR );
(*
*)
BEGIN ASSERT( FALSE );
END Import;

-----------------------------------------------------------------------------

PROCEDURE (u: UNIT) Out* ( name-: ARRAY OF CHAR; main: BOOLEAN );
(*
*)
BEGIN
  ASSERT( FALSE );
END Out;

(*----------------------- PARSER -------------------------*)

(*
    To process a module the following sequence of front-end
    and back-end methods shall be done:
                pars.ini
                code.ini
                set exception trap
                  pars.compile, pars.chk_import or pars.browser
                remove exception trap
                code.exi
                pars.exi
*)

PROCEDURE ( p: PARS ) new* (  lang: Lang
                           ; vers-: ARRAY OF CHAR );
(*
*)
BEGIN
  p.lang := lang;
  COPY (vers, p.vers);
  p.next := pars;
  pars   := p;
END new;

-----------------------------------------------------------------------------

PROCEDURE ( p: PARS ) ini* (      src: xfs.File
                           ; src_time: xfs.Time );
(**
    front-end initialization
*)
BEGIN
END ini;

-----------------------------------------------------------------------------

PROCEDURE ( p: PARS ) exi*;
(**
    front-end finalization
*)
BEGIN
END exi;

-----------------------------------------------------------------------------

PROCEDURE ( p: PARS ) compile*;
(*
*)
BEGIN
  env.errors.Fault (env.null_pos, 11 );
END compile;

-----------------------------------------------------------------------------

PROCEDURE (p: PARS) make_objects*;
(*
*)
BEGIN
  env.errors.Fault (env.null_pos, 11);
END make_objects;

-----------------------------------------------------------------------------

PROCEDURE (p: PARS) finish_compiling*(): BOOLEAN;
(*
*)
BEGIN
  env.errors.Fault (env.null_pos, 11); RETURN FALSE
END finish_compiling;

-----------------------------------------------------------------------------

PROCEDURE ( p: PARS ) browser* ( mod_name: STRING );
(*
*)
BEGIN
END browser;

-----------------------------------------------------------------------------

PROCEDURE ( p: PARS ) chk_import* ( u: UNIT );
(*
*)
BEGIN
END chk_import;

-----------------------------------------------------------------------------

PROCEDURE (p: PARS) chk_out* ( u: UNIT );
(*
*)
BEGIN
END chk_out;

(*----------------------- CODE -------------------------*)

PROCEDURE ( c: CODE ) ini*;
(*
*)
BEGIN ASSERT( FALSE );
END ini;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) exi*;
(*
*)
BEGIN
END exi;

-----------------------------------------------------------------------------

PROCEDURE (c: CODE ) set_min_value*( t: STRUCT; VAR v: VALUE );
BEGIN
END set_min_value;

-----------------------------------------------------------------------------

PROCEDURE (c: CODE ) set_max_value*( t: STRUCT; VAR v: VALUE );
BEGIN
END set_max_value;

-----------------------------------------------------------------------------


PROCEDURE ( c: CODE ) get_size* ( kind: SUB_MODE
                                ; type: STRUCT
                                )     :         LONGINT;
(**
    Returns size of type.
    kind - one of: su_bits, su_bytes, su_size;
    Returns -1 if size is not defined in compile time.
  *)
BEGIN ASSERT( FALSE );
END get_size;

-----------------------------------------------------------------------------

PROCEDURE (c: CODE) get_offs* ( op: SUB_MODE
                              ;  o: OBJECT
                              )   :           LONGINT;
(*
   Returns offset of object.
   kind - one of: su_bits_offs, su_byte_offs, su_word_offs;
   Returns -1 if offs is not defined in compile time.
*)
BEGIN
  ASSERT( FALSE );
END get_offs;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) get_align* ( type: STRUCT ): SHORTINT;
(*
   Returns actual alignment of a type.
*)
BEGIN
  ASSERT( FALSE );
END get_align;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) allocate* (       cu: Mno
                                ;     main: BOOLEAN
                                ; src_time: xfs.Time );
(**
    Allocation of public objects of mods[cu] module;
    is called from front-end before storing symbol file.
*)
BEGIN
END allocate;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) out_object* ( file: xfs.SymFile
                                  ;    o: OBJECT );
(**
    Write platform dependent object attributes to symbol file.
*)
BEGIN ASSERT( FALSE );
END out_object;


PROCEDURE ( c: CODE ) clear_object* (o :OBJECT );
(**
    Clear module-specific platform-dependent object attributes
*)
BEGIN ASSERT( FALSE );
END clear_object;


-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) inp_object* ( file: xfs.SymFile
                                  ;    o: OBJECT
                                  ;   id: LONGINT );
(**
    Reads platform dependent object attributes from symbol file.
*)
BEGIN ASSERT( FALSE );
END inp_object;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) skip_object* ( file: xfs.SymFile
                                   ;   id: LONGINT );
(**
    Skips platform dependent object attributes in symbol file.
*)
BEGIN ASSERT( FALSE );
END skip_object;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) inp_struct* ( file: xfs.SymFile
                                  ;    s: STRUCT
                                  ;   id: LONGINT );
(**
    Reads platform dependent structure attributes from symbol file.
*)
BEGIN ASSERT( FALSE );
END inp_struct;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) skip_struct* ( file: xfs.SymFile
                                   ;   id: LONGINT );
(**
    Skips platform dependent structure attributes in symbol file.
*)
BEGIN ASSERT( FALSE );
END skip_struct;

-----------------------------------------------------------------------------

CONST (* SHORTINT *)
  srcname_id * = 66;   -- this id is to set (full) source file name;
                       -- typically it is used for module's object -- VitVit

  dllname_id * = 078H; -- to obtain a dllname as an object's extended attr -- VitVit *)

PROCEDURE (c: CODE) set_stringAttr* (    o: OBJECT
                                    ; str-: ARRAY OF CHAR
                                    ;   id: SHORTINT );
(*
   Set platform-dependent string attribute (with an unique identifier "id")
   for the "o" object
*)
BEGIN
  ASSERT(FALSE);
END set_stringAttr;

-----------------------------------------------------------------------------
PROCEDURE ( c: CODE ) gen_code* (          cu: Mno
                                ; main_module: BOOLEAN );
(*
*)
BEGIN
END gen_code;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) selected*;
(*
*)
BEGIN
END selected;
<* POP *>
-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) insert* ( nm-: ARRAY OF CHAR );
(**
    insert itself into back-ends list
*)
BEGIN
  IF code = NIL THEN
    code  := c;
    c.fwd := c;
    c.bck := c;
  ELSE
    c.fwd := code;
    c.bck := code.bck;
    c.fwd.bck := c;
    c.bck.fwd := c;
  END;
  COPY (nm, c.name);
END insert;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) finalize*;
BEGIN
END finalize;

-----------------------------------------------------------------------------

PROCEDURE search_back_end*;
(*
*)
VAR
  s     : env.String;
  c, e  : CODE;
BEGIN
  env.config.Equation ("BACKEND", s(*=>*));
  IF s = NIL THEN RETURN END;
  env.config.SetOption ("__GEN_C__", FALSE);
  env.config.SetOption ("__GEN_X86__", FALSE);
  env.config.SetOption ("__GEN_O2__", FALSE);
  c := code;
  e := code;
  REPEAT
    IF c.name = s^ THEN
      code := c;
      code.selected;
      RETURN
    END;
    c := c.fwd;
  UNTIL c = e;
  env.errors.PrintMsg (env.null_pos, 'f', "Can not select back-end %s", s^);
  code.selected;
END search_back_end;

-----------------------------------------------------------------------------

PROCEDURE execute* ( f       : xfs.File
                   ; clr_mods: BOOLEAN
                   ; header  : BOOLEAN
                   ; lang    : Lang
                   ; tm      : xfs.Time
                   ; comp    : env.PROC
                   ; VAR err : BOOLEAN );

(*
*)
VAR
  fault : BOOLEAN;
  c,l   : PARS;
BEGIN
  l := NIL;
  WHILE (pars.lang # lang)
      & (pars.next # NIL)
  DO
    c      := pars;
    pars   := c.next;
    c.next := l;
    l      := c;
  END;
  c := pars;
  WHILE c.next # NIL DO
    c := c.next
  END;
  c.next := l;
  search_back_end;
  IF clr_mods THEN
    env.Clear;
    NEW (mods, demo_mod_limit);
    sys_mods := NIL;
    mod_cnt  := ZEROMno;
    comments := NIL;
  ELSE
    env.errors.Reset;
--    env.info.Reset;
  END;
  import.objects := NIL;
  IF f # NIL
   THEN env.info.file := f.name
   ELSIF ~env.config.Option("PERFECT") THEN   env.info.file := NIL
  END;
  IF header THEN
    env.info.Header (lang)
  END;
  IF f#NIL THEN pars.ini (f, tm) END;
  code.ini;
  env.errors.Execute (comp, fault(*=>*));
  code.exi;
  IF f#NIL THEN pars.exi END;
  IF clr_mods THEN
    mods     := NIL;
    sys_mods := NIL;
    mod_cnt  := ZEROMno;
    comments := NIL;
  END;
  err := FALSE;
  IF f # NIL THEN f.Close END;
  IF header  THEN
    env.info.Report
  END;
  IF fault & ~env.errors.soft_fault THEN
    INCL (env.err_sum, env.fault);
    err := TRUE;
  ELSIF env.errors.err_cnt # 0 THEN
    INCL (env.err_sum, env.error);
    err := TRUE;
  ELSIF env.errors.wrn_cnt # 0 THEN
    INCL (env.err_sum, env.warning);
  ELSIF env.errors.notice_cnt # 0 THEN
    INCL (env.err_sum, env.notice);
  END;
END execute;

-----------------------------------------------------------------------------

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ( b: BEXT ) out* ( file: xfs.SymFile );
(**
    Writes platform dependent attributes to symbol file.
*)
BEGIN ASSERT( FALSE );
END out;

-----------------------------------------------------------------------------
PROCEDURE ( c: CONSTS ) eval_const* ( n: NODE ): VALUE;
(*
*)
BEGIN ASSERT( FALSE );
END eval_const;

-----------------------------------------------------------------------------

PROCEDURE ( c: CONSTS ) eval_value* ( n: NODE );
(*
*)
BEGIN ASSERT( FALSE )
END eval_value;

-----------------------------------------------------------------------------

PROCEDURE ( c: CONSTS ) eval_module* ( n: NODE );
(*
*)
BEGIN ASSERT( FALSE );
END eval_module;

-----------------------------------------------------------------------------
PROCEDURE ( c: CONSTS ) prescan_modules* ( );
(*
*)
BEGIN ASSERT( FALSE );
END prescan_modules;

-----------------------------------------------------------------------------

PROCEDURE (c: CONSTS) mark_inlinable* (module: NODE);
(**
    Marks all procedures from module that fit into necessary
    (but not sufficient!) inlining requirements
*)
BEGIN ASSERT( FALSE );
END mark_inlinable;
<* POP *>

-----------------------------------------------------------------------------

PROCEDURE create_minmax_value*( type: STRUCT ) : VALUE;
VAR v: VALUE;
    t: STRUCT;
BEGIN
  IF type.mode IN TY_SET{ ty_enum, ty_array_of, ty_SS } THEN
    t := longint_type;
  ELSIF NOT( type.mode IN BOUNDED_TYPES ) THEN
    RETURN NIL;
  ELSE
    -- in M2/O2 min/max values of the type have type ZZ_type or RR_type
    IF type.mode IN REALs THEN
      IF RR_type # NIL THEN
        t := RR_type;
      ELSE
        ASSERT(type.mode = ty_RR);
        t := type;
      END;
    ELSE
      IF ZZ_type # NIL THEN
        t := ZZ_type;
      ELSE
        ASSERT(type.mode = ty_ZZ);
        t := type;
      END;
    END;
  END;
  v := value.new( env.null_pos, t );
  RETURN v;
END create_minmax_value;

PROCEDURE ini_type* (t: STRUCT; mode: TY_MODE);
BEGIN
  t.mode  := mode;
  t.flag  := flag_m2;
  t.marks := TMARK_SET{};
  t.len   := 0;
  t.num   := 0;
  t.pos   := env.null_pos;
  t.end   := env.null_pos;
  t.min   := create_minmax_value(t);
  IF t.min # NIL THEN code.set_min_value( t, t.min ); END;
  t.max   := create_minmax_value(t);
  IF t.max # NIL THEN code.set_max_value( t, t.max ); END;
  t.mno   := cur_mod;
--  t.tags  := SYSTEM.VAL(TTAG_SET, env.config.tags*{ORD(ttag_aux16)..ORD(ttag_aux19)});
  IF env.no_struct IN env.config.tags THEN
    t.tags  := TTAG_SET{ttag_no_struct};
  ELSE
    t.tags  := TTAG_SET{};
  END;
  t.lref  := 0;
END ini_type;

PROCEDURE new_type* (mode: TY_MODE): STRUCT;
VAR t: STRUCT;
BEGIN
  NEW(t);
  ini_type(t, mode);
  RETURN t;
END new_type;

-----------------------------------------------------------------------------

PROCEDURE ini_obj* (o: OBJECT; mode: OB_MODE);
BEGIN
  o.lref:= 0;
  o.mode:= mode;
  o.flag:= flag_o2;
  o.mno := cur_mod;
--    o.tags:= SYSTEM.VAL(OTAG_SET, env.config.tags*{ORD(otag_C_pref)});
  IF env.gencpref IN env.config.tags THEN
    o.tags:= OTAG_SET{otag_C_pref};
  ELSE
    o.tags:= OTAG_SET{};
  END;
  o.marks := OMARK_SET{};
  o.pos := env.null_pos;
  o.end := env.null_pos;
  IF (sys_mods#NIL) & (sys_mods[PRED(-mno_pervasive)]#NIL) THEN
    o.name := sys_mods[PRED(-mno_pervasive)].name;
  END;
END ini_obj;


PROCEDURE new_obj* (mode: OB_MODE): OBJECT;
VAR
  o: OBJECT;
BEGIN
  NEW(o);
  ini_obj(o, mode);
  RETURN o;
END new_obj;

-----------------------------------------------------------------------------
-- little bit more complicated object creation.
-- used primarily in pcConst, but also in back-ends.
PROCEDURE new_object_ex*(  t,h: STRUCT;
                           md: OB_MODE;
                           prof: BOOLEAN
                        )     :         OBJECT;

VAR o,oo: OBJECT;
BEGIN
  o := new_obj(md);
  o.lev  := h.obj.lev;
  IF h.obj.mode IN PROCs THEN
    INC (o.lev)
  END;
  o.flag := flag_o2;
  o.type := t;
  o.mno  := h.mno;
  o.sno  := -1;
  o.tags := OTAG_SET{};
  o.marks:= OMARK_SET{};
  o.pos  := env.null_pos;
  o.end  := env.null_pos;
  NEW (o.name, 12);
  fmt.prn_txt (o.name^(*=>*), "&&%.8X", o);
  o.host := h;
  IF prof THEN
    o.next := h.prof; h.prof := o;
    IF h.mode = ty_module THEN
      INCL (o.tags, otag_public)
    END;
  ELSE
    (* mem list: preserve order (need for exit_no_var pair and for pcSafe.StackAlloc) *)
    o.next := NIL;
    IF (h.mem = NIL) THEN
      h.mem  := o;
    ELSE
      oo := h.mem;
      WHILE oo.next # NIL DO oo := oo.next; END;
      oo.next := o;
    END;
  END;
  RETURN o;
END new_object_ex;

-------------------------------------------------------------------------

PROCEDURE CreatePrimitiveTypes*();
BEGIN
  -- we must create ZZ_type before all other integer types,
  -- and RR_type before all other float types,
  -- because ZZ_type and RR_type are used to create 
  -- min/max values during types creation.
  ZZ_type       := new_type( ty_ZZ );
  RR_type       := new_type( ty_RR );
  AA_type       := new_type( ty_AA );
  CC_type       := new_type( ty_CC );
  char_type     := new_type( ty_char     );
  uchar_type    := new_type( ty_uchar    );
  longint_type  := new_type( ty_longint  );
  void_type     := new_type( ty_void     );
  shortint_type := new_type( ty_shortint );
  integer_type  := new_type( ty_integer  );
  longlongint_type := new_type( ty_longlongint );
  longlongcard_type := new_type( ty_longlongcard );
  shortcard_type:= new_type( ty_shortcard);
  cardinal_type := new_type( ty_cardinal );
  longcard_type := new_type( ty_longcard );
  real_type     := new_type( ty_real     );
  longreal_type := new_type( ty_longreal );
  ld_real_type  := new_type( ty_ld_real  );
  complex_type  := new_type( ty_complex  );
  lcomplex_type := new_type( ty_lcomplex );
END CreatePrimitiveTypes;

PROCEDURE RefreshPrimitiveTypes*();

  PROCEDURE RefreshOne(type:STRUCT);
  BEGIN
    IF pars.lang IN LangSet{flag_java, flag_jbc} THEN
      type.flag := flag_java;
    ELSE
      type.flag := flag_m2;
    END;
  END RefreshOne;

BEGIN
  RefreshOne( ZZ_type );
  RefreshOne( RR_type );
  RefreshOne( AA_type );
  RefreshOne( CC_type );
  RefreshOne( char_type     );
  RefreshOne( uchar_type    );
  RefreshOne( longint_type  );
  RefreshOne( void_type     );
  RefreshOne( shortint_type );
  RefreshOne( integer_type  );
  RefreshOne( longlongint_type);
  RefreshOne( longlongcard_type);
  RefreshOne( shortcard_type);
  RefreshOne( cardinal_type );
  RefreshOne( longcard_type );
  RefreshOne( real_type     );
  RefreshOne( longreal_type );
  RefreshOne( ld_real_type  );
  RefreshOne( complex_type  );
  RefreshOne( lcomplex_type );
END RefreshPrimitiveTypes;


BEGIN
  value   := NIL;
  code    := NIL;
  pars    := NIL;
  mod_cnt := ZEROMno;
END pcK.

