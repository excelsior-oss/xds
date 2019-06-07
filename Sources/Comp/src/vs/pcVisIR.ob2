
(** Copyright (c) 1991,98 XDS Ltd, Russia. All Rights Reserved. *)
MODULE pcVisIR; (** Vit 07-Apr-98 *)
(* Tracing facilities and IR tree visualisation. To be used in DB_TRACE+ only *)

IMPORT pc := pcK,
  <* IF TARGET_C THEN *>
       cc := ccK,
  <* END *>
      env := xiEnv,
             TextIO,
             xcStr,
             StdChans,
             SYSTEM;
IMPORT Strings;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
IMPORT hgr, hgl;
<* END *>
IMPORT Printf;

CONST
    otag_trace = pc.otag_aux21;
    ttag_trace = pc.ttag_aux31;
    ntag_trace = pc.ntag_aux31;
TYPE
    edgeLabelType = ARRAY 10 OF CHAR;
    AddressesType = POINTER TO ARRAY OF SYSTEM.ADDRESS;
VAR
  cur_mod : pc.Mno;
  Nnodes,Nedges  : LONGINT;
  addresses: AddressesType; --POINTER TO ARRAY OF SYSTEM.ADDRESS;

  feirvisproc:pc.STRING;
(*====================== ROUTINES TRACING SUPPORT ==========================*)

VAR TrcInd: INTEGER; -- indentation counter
    TrcEol: BOOLEAN; -- newline flag: set when CodeTrace output stream
             --   is interrupted by CallTrace/CallTraceInd output

-------------------------------------------------------------------------------

PROCEDURE Trace* (    f-: ARRAY OF CHAR   -- format text
                 ; SEQ x: SYSTEM.BYTE );  -- any data
(*
   Trace a formatted text with no output buffer flush
*)
VAR txt: env.String;

BEGIN
  xcStr.dprn_txt (txt(*=>*), f, x);
  TextIO.WriteString (StdChans.OutChan(), txt^);
END Trace;

------------------------------------------------------------------------------

PROCEDURE CallTraceInd* (    md: INTEGER         -- +1/0/-1 - indentation delta
                        ;    f-: ARRAY OF CHAR   -- format text to output
                        ; SEQ x: SYSTEM.BYTE );  -- any data to format
(*
   (Call) Trace a formatted text with indentation predecr/none/postincr
*)
VAR i:INTEGER;

BEGIN
  IF md < 0 THEN DEC(TrcInd) END;        -- -1: predecr.indentation
  TrcEol := TRUE;                        -- fresh newline flag on
  Trace ('\n');
  FOR i:=1 TO TrcInd DO Trace(' ') END;  -- indentation
  Trace (f,x);
  IF md > 0 THEN INC(TrcInd) END;        -- +1: postincr.indentation
END CallTraceInd;

-------------------------------------------------------------------------------

PROCEDURE CallTrace* (    f-: ARRAY OF CHAR   -- format text
                     ; SEQ x: SYSTEM.BYTE );  -- any data
(*
   (Call) Trace a formatted text with no output buffer flush
*)
BEGIN
  Trace(f,x);
  TrcEol := TRUE;
END CallTrace;

-------------------------------------------------------------------------------

PROCEDURE CodeTrace* (    f-: ARRAY OF CHAR   -- format text
                     ; SEQ x: SYSTEM.BYTE );  -- any data
(*
   (Code) Trace a formatted text with no output buffer flush
*)
BEGIN
  IF TrcEol THEN
    Trace('\n>>> ');
    TrcEol := FALSE;
  END;
  Trace(f,x);
END CodeTrace;

------------------------------------------------------------------------------

PROCEDURE TracePos* ( pos: env.TPOS );
(*
   Trace position
*)
VAR l,c: LONGINT;
    fnm: env.String;

BEGIN
  IF pos.IsNull() THEN
    Trace('(null) ');
  ELSE
    pos.unpack(fnm,l,c);
    Trace('(%s,%d.%d) ',fnm^,l,c);
  END;
END TracePos;

------------------------------------------------------------------------------

PROCEDURE Ini* ;
(*
   Init tracing mode: drop indentation;
*)
BEGIN
  TrcInd := 0;
  TrcEol := TRUE;
END Ini;


(*======================= NODES MODES MNEMONIZATION ========================*)
TYPE
   Array19OfChar = ARRAY pc.OB_MODE OF ARRAY 14 OF CHAR;

CONST
    VisObjMode* =  Array19OfChar
    {
      "ob_inv",
      "ob_var",
      "ob_varpar",
      "ob_seq",
      "ob_label",
      "ob_proc",
      "ob_xproc",
      "ob_eproc",
      "ob_cproc",
      "ob_lproc",
      "ob_cons",
      "ob_type",
      "ob_sproc",
      "ob_field",
      "ob_field_bts",
      "ob_unused",
      "ob_module",
      "ob_header",
      "ob_interface",
      "ob_aux1",
      "ob_aux2",
      "ob_aux3",
      "ob_aux4",
      "ob_aux5",
      "ob_aux6"
    };
-----------------------------------------------------------------------------

TYPE
  Array34OfChar = ARRAY pc.TY_MODE OF ARRAY 16 OF CHAR;

CONST
    VisStrMode* =  Array34OfChar
    {
      "ty_shortcard",
      "ty_cardinal",
      "ty_longcard",
      "ty_shortint",
      "ty_integer",
      "ty_longint",
      "ty_longlongint",
      "ty_ZZ",
      "ty_real",
      "ty_longreal",
      "ty_ld_real",
      "ty_RR",
      "ty_complex",
      "ty_lcomplex",
      "ty_CC",
      "ty_boolean",
      "ty_char",
      "ty_AA",
      "ty_range",
      "ty_enum",
      "ty_opaque",
      "ty_pointer",
      "ty_set",
      "ty_proctype",
      "ty_array",
      "ty_array_of",
      "ty_SS",
      "ty_record",
      "ty_protection",
      "ty_uchar",
      "ty_void",
      "ty_loc",
      "ty_module",
      "ty_process",
      "ty_free",
      "ty_aux1",
      "ty_aux2",
      "ty_aux3",
      "ty_aux4",
      "ty_aux5",
      "ty_aux6",
      "ty_aux7",
      "ty_longlongcard"
    };
-----------------------------------------------------------------------------

TYPE Array36OfChar = ARRAY pc.ND_MODE OF ARRAY 15 OF CHAR;

CONST
    VisNodMode* =  Array36OfChar
    {
     "nd_inv",
     "nd_module",
     "nd_var",
     "nd_proc",
     "nd_sproc",
     "nd_method",
     "nd_type",
     "nd_prot",
     "nd_field",
     "nd_index",
     "nd_deref",
     "nd_eguard",
     "nd_guard",
     "nd_binary",
     "nd_unary",
     "nd_lconv",
     "nd_value",
     "nd_aggregate",
     "nd_sequence",
     "nd_pair",
     "nd_node",
     "nd_replace",
     "nd_call",
     "nd_assign",
     "nd_while",
     "nd_repeat",
     "nd_loop",
     "nd_exit",
     "nd_return",
     "nd_for",
     "nd_with",
     "nd_wtrap",
     "nd_ftrap",
     "nd_if",
     "nd_case",
     "nd_caselse",
     "nd_casedo",
     "nd_null",
     "nd_eval",
     "nd_goto",
     "nd_label",
     "nd_block",
     "nd_finally",
     "nd_except",
     "nd_reraise",
     "nd_activate",
     "nd_retry",
     "nd_protect",
     "nd_synchron", (** J syncronized block *)
     "nd_throw",    (** J throw with a param *)
     "nd_catch",
     "nd_break",    (** J break statement *)
     "nd_continue", (** J continue statement *)
     "nd_java_for", (** J for statement *)
     "nd_last"
   };
-----------------------------------------------------------------------------

PROCEDURE VisNodSubMode* ( n : pc.NODE );
BEGIN
  Trace(" ");
  CASE n.sub OF
  | pc.su_none:
  | pc.su_bits:     Trace("su_bits");
  | pc.su_bytes:    Trace("su_bytes");
  | pc.su_size:     Trace("su_size");
  | pc.su_words:    Trace("su_words");
  | pc.su_min:      Trace("su_min");
  | pc.su_max:      Trace("su_max");
  | pc.su_is:       Trace("su_is");
  | pc.su_abs:      Trace("su_abs");
  | pc.su_neg:      Trace("su_neg");
  | pc.su_cc:       Trace("su_cc");
  | pc.su_adr:      Trace("su_adr");
  | pc.su_adr_o2:   Trace("su_adr_o2");
  | pc.su_cap:      Trace("su_cap");
  | pc.su_conv:     Trace("su_conv");
  | pc.su_cast:     Trace("su_cast");
  | pc.su_odd:      Trace("su_odd");
  | pc.su_not:      Trace("su_not");
  | pc.su_compl:    Trace("su_compl");
  | pc.su_entier:   Trace("su_entier");
  | pc.su_length:   Trace("su_length");
  | pc.su_im:       Trace("su_im");
  | pc.su_re:       Trace("su_re");
  | pc.su_bit_offs: Trace("su_bit_offs");
  | pc.su_byte_offs:Trace("su_byte_offs");
  | pc.su_word_offs:Trace("su_word_offs");
  | pc.su_width:    Trace("su_width");
  | pc.su_ptr2vptr: Trace("su_ptr2vptr");
  | pc.su_vptr2ptr: Trace("su_vptr2ptr");

  | pc.sb_high:     Trace("sb_high");
  | pc.sb_len:      Trace("sb_len");
  | pc.sb_equ:      Trace("sb_equ");
  | pc.sb_neq:      Trace("sb_neq");
  | pc.sb_lss:      Trace("sb_lss");
  | pc.sb_leq:      Trace("sb_leq");
  | pc.sb_gtr:      Trace("sb_gtr");
  | pc.sb_geq:      Trace("sb_geq");
  | pc.sb_in:       Trace("sb_in");
  | pc.sb_mul:      Trace("sb_mul");
  | pc.sb_div:      Trace("sb_div");
  | pc.sb_mod:      Trace("sb_mod");
  | pc.sb_rem:      Trace("sb_rem");
  | pc.sb_slash:    Trace("sb_slash");
  | pc.sb_plus:     Trace("sb_plus");
  | pc.sb_minus:    Trace("sb_minus");
  | pc.sb_and:      Trace("sb_and");
  | pc.sb_or:       Trace("sb_or");
  | pc.sb_xor:      Trace("sb_xor");
  | pc.sb_bic:      Trace("sb_bic");
  | pc.sb_cand:     Trace("sb_cand");
  | pc.sb_cor:      Trace("sb_cor");
  | pc.sb_ash:      Trace("sb_ash");
  | pc.sb_lsh:      Trace("sb_lsh");
  | pc.sb_rot:      Trace("sb_rot");
  | pc.sb_addadr:   Trace("sb_addadr");
  | pc.sb_subadr:   Trace("sb_subadr");
  | pc.sb_difadr:   Trace("sb_difadr");
  | pc.sb_pre_inc:  Trace("sb_pre_inc");
  | pc.sb_pre_dec:  Trace("sb_pre_dec");
  | pc.sb_post_inc: Trace("sb_post_inc");
  | pc.sb_post_dec: Trace("sb_post_dec");
  | pc.sb_concat:   Trace("sb_concat");
  | pc.sb_bit:      Trace("sb_bit");
  | pc.sb_cmplx:    Trace("sb_cmplx");
  | pc.sb_exp:      Trace("sb_exp");
  | pc.sb_shl:      Trace("sb_shl");
  | pc.sb_shr:      Trace("sb_shr");

  | pc.sp_assert:   Trace("sp_assert");
  | pc.sp_halt:     Trace("sp_halt");
  | pc.sp_abort:    Trace("sp_abort");
  | pc.sp_new:      Trace("sp_new");
  | pc.sp_dispose:  Trace("sp_dispose");
  | pc.sp_sysnew:   Trace("sp_sysnew");
  | pc.sp_code:     Trace("sp_code");
  | pc.sp_move:     Trace("sp_move");
  | pc.sp_incl:     Trace("sp_incl");
  | pc.sp_excl:     Trace("sp_excl");
  | pc.sp_get:      Trace("sp_get");
  | pc.sp_getreg:   Trace("sp_getreg");
  | pc.sp_put:      Trace("sp_put");
  | pc.sp_putreg:   Trace("sp_putreg");
  | pc.sp_fill:     Trace("sp_fill");
  | pc.sp_copy:     Trace("sp_copy");
  ELSE              Trace("<su_%d>",n.sub);
  END;
END VisNodSubMode;


(*====================== IR NODES VISUALIZATION =============================*)

<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
PROCEDURE FillAddresses( index: LONGINT; adr: SYSTEM.ADDRESS);
VAR
    q: AddressesType;--POINTER TO ARRAY OF SYSTEM.ADDRESS;
    i: LONGINT;
BEGIN
   IF index = LEN (addresses^) THEN
       NEW (q, LEN (addresses^) * 2);
       FOR i:=0 TO index-1 DO
           q^[i] := addresses^[i];
       END;
       addresses := q;
   END;
   addresses[index] := adr;
END FillAddresses;
<* END *>

PROCEDURE FindVertex(a: SYSTEM.ADDRESS):LONGINT;
VAR i: LONGINT;
BEGIN
  FOR i := 0 TO LEN(addresses^)-1 DO
    IF addresses[i] = a THEN
      RETURN i;
    END;
  END;
  RETURN -1;
END FindVertex;

-----------------------------------------------------

PROCEDURE Ln ( indent: INTEGER );
VAR i: INTEGER;
BEGIN
    Trace("\n");
    FOR i:=0 TO indent-1 DO
        Trace("    ");
    END;
END Ln;

-----------------------------------------------------
<* WOFF301+ *>
PROCEDURE ^ VisStruct ( t : pc.STRUCT; indent: INTEGER; edgeFrom: LONGINT; edgelabel:edgeLabelType);
PROCEDURE ^ VisNode   ( n : pc.NODE; indent: INTEGER; edgeFrom: LONGINT; edgelabel:edgeLabelType );

PROCEDURE VisValue* ( val: pc.VALUE;
                      type: pc.STRUCT(*nv: pc.NODE*);
                      indent: INTEGER;
                      edgeFrom: LONGINT;
                      edgelabel:edgeLabelType  );   -- node, representing a value
VAR
-- t: pc.STRUCT;
    s: ARRAY 30 OF CHAR;
--    tmp: ARRAY 100 OF CHAR;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    v: hgl.PVertex;
    currentVertex: LONGINT;
<* ELSE *>
CONST
  currentVertex = 0;

<* END *>
BEGIN

  IF val = NIL THEN RETURN END;
  Ln(indent);
  Trace("----");
  Ln(indent);
  Trace("VALUE [%X]",val);
--  IF nv.val = NIL THEN VisNode(nv, indent+1, edgeFrom, "val"); RETURN END;
--  t := type;
  IF (type # NIL) & (type.mode IN +pc.TY_SET{pc.ty_enum,pc.ty_range}) THEN
    type := type.base
  END;
  IF    (type = NIL) OR         -- nd_pair has no type: assumed int
        (type.mode IN pc.INTs)
  THEN Printf.sprintf(s,"%d",val.get_integer());
  ELSIF val.is_ordinal(type) THEN Printf.sprintf(s,"%d",val.get_cardinal());
--  ELSIF val.is_RR()       THEN Printf.sprintf(s,"%f",val.get_real());
  ELSE RETURN
  END;
  Trace(" -> (%s)",s);

<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  currentVertex := 0;
<* END *>
  IF FindVertex(val) < 0 THEN
    INC(Nnodes);
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    IF env.config.Option("gen_hgr") THEN
        FillAddresses(Nnodes-1, val);
        hgr.AddVertex (hgr.pcValueType, 0, 0, 0);
        hgr.SetLabVal (hgl.ObjVertex, Nnodes-1, hgr.pcValueLabelType, s);
        v := hgr.GetVertex(Nnodes-1);
        v.w := (Strings.Length(s)+3)*7;
        currentVertex := Nnodes-1;
    END;
<* END *>
  END;

  IF val.expr <> NIL THEN
     Ln(indent);
     Trace("  expr: [%X]", val.expr);
  END;
  VisNode (val.expr, indent+1, currentVertex, "expr");

<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  IF env.config.Option("gen_hgr") AND (edgeFrom # -1) THEN
      hgr.AddEdge(hgr.woodEdgeType, edgeFrom, FindVertex(val));
      hgr.SetLabVal (hgl.ObjEdge,
                     Nedges,
                     hgr.woodEdgeLabel,
                     edgelabel);
      INC(Nedges);
  END;
<* END *>

END VisValue;

-------------------------------------------------------------------------------
PROCEDURE VisObject* ( o : pc.OBJECT; indent: INTEGER; edgeFrom: LONGINT; edgelabel:edgeLabelType  );
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
VAR
    tmp: ARRAY 100 OF CHAR;
    v: hgl.PVertex;
    currentVertex: LONGINT;
<* ELSE *>
CONST
  currentVertex = 0;
<* END *>
VAR    foo: ARRAY 100 OF CHAR;
BEGIN
  IF o = NIL THEN RETURN END;
  IF o.mno < pc.ZEROMno  THEN RETURN END;
  IF (cur_mod # pc.INVMno) AND (o.mno # cur_mod) THEN RETURN END;
  IF ~(otag_trace IN o.tags) THEN --RETURN END;
      INCL(o.tags, otag_trace);

      INC(Nnodes);
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
      currentVertex := 0;
      IF env.config.Option("gen_hgr") THEN
          FillAddresses(Nnodes-1, o);
--          addresses[Nnodes-1] := o;
          hgr.AddVertex (hgr.pcObjectType, 0, 0, 0);
          COPY(VisObjMode[o.mode], tmp) ;
          Strings.Append(": ", tmp);
          Strings.Append(o.name^, tmp);
          hgr.SetLabVal (hgl.ObjVertex, Nnodes-1, hgr.pcObjectLabelType, tmp);
          v := hgr.GetVertex(Nnodes-1);
          v.w := Strings.Length(tmp)*7;
          currentVertex := Nnodes-1;
      END;
<* END *>
      Ln(indent);
      Trace("----");
      Ln(indent);
      Trace("OBJECT ");
      Trace(VisObjMode[o.mode]);
      IF o.name # NIL THEN
        COPY(o.name^,foo);
      ELSE
        foo := "!!NO NAME!!";
      END;
      Trace(" %s [%X]: mno=%d lev=%d lang=%d",foo,o,o.mno,o.lev,o.flag);
      (* FE tags *)
      IF pc.otag_public     IN o.tags THEN Trace(" public") END;
      IF pc.otag_valpar     IN o.tags THEN Trace(" valpar") END;
      IF pc.otag_public_f   IN o.tags THEN Trace(" public_f") END;
      IF pc.otag_RO         IN o.tags THEN Trace(" RO") END;
      IF pc.otag_RO_public  IN o.tags THEN Trace(" RO_public") END;
      IF pc.otag_no_threat  IN o.tags THEN Trace(" no_threat") END;
      IF pc.otag_no_aliases IN o.tags THEN Trace(" no_aliases") END;
      IF pc.otag_with       IN o.tags THEN Trace(" with") END;
      IF pc.otag_volatile   IN o.tags THEN Trace(" volatile") END;
      IF pc.xot_statini     IN o.xtags THEN Trace(" statini") END;
      IF pc.omark_code         IN o.marks THEN Trace(" code") END;
      IF pc.omark_used_by_code IN o.marks THEN Trace(" used_code") END;
      (* ME tags *)
      IF (*omark_used*)  pc.omark_aux20    IN o.marks THEN Trace(" used") END;
      IF (*omark_incl*)  pc.omark_aux21    IN o.marks THEN Trace(" incl") END;
      IF (*omark_keep*)  pc.omark_aux22    IN o.marks THEN Trace(" keep") END;
      <* IF TARGET_C THEN *>
      (* BE tags *)
      IF cc.otag_declared    IN o.tags THEN Trace(" declared") END;
      IF cc.otag_declaring   IN o.tags THEN Trace(" declaring") END;
      IF cc.otag_defining    IN o.tags THEN Trace(" defining") END;
      IF cc.otag_pub_defined IN o.tags THEN Trace(" pub_def") END;
      IF cc.otag_pri_defined IN o.tags THEN Trace(" pri_def") END;
      IF cc.otag_notype      IN o.tags THEN Trace(" notype/ver") END;
      IF cc.otag_headerfile  IN o.tags THEN Trace(" hdr/bitfld") END;
      IF cc.otag_bitfield_nm IN o.tags THEN Trace(" bitf_nm/reference") END;
      <* END *>
      IF o.type <> NIL THEN
        Ln(indent);
        Trace("  type: [%X] ", o.type); Trace(VisStrMode[o.type.mode]); END;
      IF o.val  <> NIL THEN
        Ln(indent);
        Trace("   val: [%X]", o.val);
      END;
      IF o.next <> NIL THEN
         Ln(indent);
         Trace("  next: [%X] %s", o.next,o.next.name^); END;
      IF o.host <> NIL THEN
         Ln(indent);
         Trace("  host: [%X]", o.host); END;
      IF o.attr <> NIL THEN
         Ln(indent);
         Trace("  attr: [%X]", o.attr); END;

      IF ~(o.mode IN pc.PROCs) OR (feirvisproc=NIL) OR (o.name^=feirvisproc^) THEN
--        VisStruct (o.host, indent+1, currentVertex, "host");
--        VisStruct (o.type, indent+1, currentVertex, "type");
        VisNode   (o.val, indent+1, currentVertex, "val");
        IF o.attr # NIL THEN
          VisNode   (o.attr(pc.NODE), indent+1, currentVertex, "attr");
        END;
      END;
      VisObject (o.next, indent, currentVertex, "next");
  END;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  IF env.config.Option("gen_hgr") AND (edgeFrom # -1) THEN
      hgr.AddEdge(hgr.woodEdgeType, edgeFrom, FindVertex(o));
      hgr.SetLabVal (hgl.ObjEdge,
                     Nedges,
                     hgr.woodEdgeLabel,
                     edgelabel);
      INC(Nedges);
  END;
<* END *>
END VisObject;

-----------------------------------------------------------------------------

PROCEDURE VisStruct* ( t : pc.STRUCT; indent: INTEGER; edgeFrom: LONGINT; edgelabel:edgeLabelType );
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
VAR
    tmp: ARRAY 100 OF CHAR;
    v: hgl.PVertex;
    currentVertex : LONGINT;
<* ELSE *>
CONST
  currentVertex = 0;
<* END *>
BEGIN
  IF t = NIL THEN RETURN END;
  IF t.mno < pc.ZEROMno  THEN RETURN END;
  IF (cur_mod # pc.INVMno) AND (t.mno # cur_mod) THEN RETURN END;
  IF ~(ttag_trace IN t.tags) THEN --RETURN END;
      INCL(t.tags, ttag_trace);

      INC(Nnodes);
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
      currentVertex := 0;
      IF env.config.Option("gen_hgr") THEN
          FillAddresses(Nnodes-1, t);
--          addresses[Nnodes-1] := t;
          hgr.AddVertex (hgr.pcStructType, 0, 0, 0);

          COPY(VisStrMode[t.mode], tmp) ;
          v := hgr.GetVertex(Nnodes-1);
          v.w := Strings.Length(tmp)*10;
          v.h := 20;

          hgr.SetLabVal (hgl.ObjVertex, Nnodes-1, hgr.pcStructLabelType, tmp);
          currentVertex := Nnodes-1;
      END;
<* END *>

      Ln(indent);
      Trace("----");
      Ln(indent);
      Trace("STRUCT ");
      Trace(VisStrMode[t.mode]);
      Trace(" [%X]: mno=%d",t,t.mno);
      (* FE tags *)
      IF pc.ttag_usage_ok   IN t.tags THEN Trace(" usage_ok") END;
      IF pc.ttag_has_o2_ptr IN t.tags THEN Trace(" has_o2_ptr") END;
      IF pc.ttag_packed     IN t.tags THEN Trace(" packed") END;
      IF pc.ttag_c_type     IN t.tags THEN Trace(" c_type") END;
      IF pc.ttag_volatile   IN t.tags THEN Trace(" volatile") END;
      (* ME tags *)
      IF (*tmark_ingen*)    pc.tmark_aux20 IN t.marks THEN Trace(" ingen") END;
      IF (*tmark_genok*)    pc.tmark_aux21 IN t.marks THEN Trace(" genok") END;
      IF (*tmark_inlined*)  pc.tmark_aux22 IN t.marks THEN Trace(" inlined") END;
      IF (*tmark_basetype*) pc.tmark_aux23 IN t.marks THEN Trace(" basetype") END;
      IF (*tmark_inl_dis*)  pc.tmark_aux24 IN t.marks THEN Trace(" inl_dis") END;
      <* IF TARGET_C THEN *>
      (* BE tags *)
      IF cc.ttag_ownheader  IN t.tags THEN Trace(" ownhdr") END;
      IF cc.ttag_nocode     IN t.tags THEN Trace(" nocode") END;
      IF cc.ttag_cstdlib    IN t.tags THEN Trace(" cstdlib") END;
      IF cc.ttag_union      IN t.tags THEN Trace(" union") END;
      <* END *>
      IF t.obj <> NIL THEN
         Ln(indent);
         Trace("   obj: [%X] %s", t.obj,t.obj.name^) END;
      IF t.base <> NIL THEN
         Ln(indent);
         Trace("  base: [%X] ",t.base); Trace(VisStrMode[t.base.mode]); END;
      IF t.inx <> NIL THEN
         Ln(indent);
         Trace("   inx: [%X] ",t.inx); Trace(VisStrMode[t.inx.mode]); END;
      IF t.min <> NIL THEN
         Ln(indent);
         Trace("   min: [%X] ",t.min); (*3 VisNodMode(t.min); *) END;
--         VisValue  (t.min, t, indent+1, currentVertex,  "min");
      IF t.max <> NIL THEN
         Ln(indent);
         Trace("   max: [%X] ",t.max); (*3 VisNodMode(t.max); *) END;
--         VisValue  (t.max, t, indent+1, currentVertex,  "max");
      IF t.prof <> NIL THEN
         Ln(indent);
         Trace("  prof: [%X]", t.prof); END;
      IF t.mem  <> NIL THEN
         Ln(indent);
         Trace("   mem: [%X]", t.mem);  END;
      IF t.use  <> NIL THEN
         Ln(indent);
         Trace("   use: -> [%X]...", t.use.obj);  END;
      Ln(indent);
      Trace("   len: %d", t.len);
      Ln(indent);
      Trace("  size: %d", t.num);

      VisObject (t.obj, indent+1, currentVertex, "obj");
      VisObject (t.prof, indent+1, currentVertex, "prof");
      VisObject (t.mem, indent+1, currentVertex, "mem");
      VisStruct (t.base, indent+1, currentVertex, "base");
      VisStruct (t.inx, indent+1, currentVertex,  "inx");
      VisValue  (t.min, t, indent+1, currentVertex,  "min");
      VisValue  (t.max, t, indent+1, currentVertex,  "max");
    (*3
      VisNode   (t.min);
      VisNode   (t.max);
    *)
  END;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  IF env.config.Option("gen_hgr") AND (edgeFrom # -1) THEN
      IF edgelabel = "host" THEN
          hgr.AddEdge(hgr.hostEdgeType, edgeFrom, FindVertex(t));
      ELSE
          hgr.AddEdge(hgr.woodEdgeType, edgeFrom, FindVertex(t));
          hgr.SetLabVal (hgl.ObjEdge,
                         Nedges,
                         hgr.woodEdgeLabel,
                         edgelabel);
      END;

      INC(Nedges);
  END;
<* END *>
END VisStruct;

-----------------------------------------------------------------------------

PROCEDURE VisNode* ( n : pc.NODE; indent: INTEGER; edgeFrom: LONGINT; edgelabel: edgeLabelType );
VAR
    fname: pc.STRING;
    line, col: LONGINT;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
VAR
    tmp: ARRAY 100 OF CHAR;
    v: hgl.PVertex;
    currentVertex: LONGINT;
<* ELSE *>
CONST
  currentVertex = 0;
<* END *>
BEGIN
  IF n = NIL THEN RETURN END;
  IF ~(ntag_trace IN n.tags) THEN --RETURN END;
      INCL(n.tags, ntag_trace);

      INC(Nnodes);
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
      currentVertex := 0;
      IF env.config.Option("gen_hgr") THEN
          FillAddresses(Nnodes-1, n);
--          addresses[Nnodes-1] := n;
          hgr.AddVertex (hgr.pcNodeType, 0, 0, 0);

          COPY(VisNodMode[n.mode], tmp) ;
          v := hgr.GetVertex(Nnodes-1);
          v.w := Strings.Length(tmp)*7;

          hgr.SetLabVal (hgl.ObjVertex, Nnodes-1, hgr.labelNode, VisNodMode[n.mode]);
          currentVertex := Nnodes-1;
      END;
<* END *>
      Ln(indent);
      Trace("----");
      Ln(indent);
      Trace("NODE ");
      Trace(VisNodMode[n.mode]);
      VisNodSubMode(n);
      Trace(" [%X]",n);
      IF pc.ntag_elsif_node   IN n.tags THEN Trace(" elsif") END;
      IF pc.ntag_substitute   IN n.tags THEN Trace(" substitute") END;
      IF pc.ntag_chk_overflow IN n.tags THEN Trace(" chk_ovfl") END;
      IF pc.ntag_chk_range    IN n.tags THEN Trace(" chk_range") END;
      IF pc.ntag_no_exit      IN n.tags THEN Trace(" no_exit") END;
      IF pc.ntag_hex          IN n.tags THEN Trace(" hex") END;
      IF NOT n.pos.IsNull() THEN
        n.pos.unpack(fname,line,col);
        Trace('['); Trace(fname^); Trace(':'); Trace("%d",line+1); Trace(':'); Trace("%d",col+1); Trace(']');
      END;
      IF n.type <> NIL THEN
         Ln(indent);
         Trace("  type: [%X] ", n.type); Trace(VisStrMode[n.type.mode]); END;
      IF n.obj  <> NIL THEN
         Ln(indent);
         Trace("   obj: [%X] %s",n.obj,n.obj.name^); END;
      IF n.val  <> NIL THEN
        Ln(indent);
        Trace("   val: [%X]", n.val);
--        VisValue(n.val, n.type, indent+1, currentVertex, "val");
      END;
      IF n.next <> NIL THEN
         Ln(indent);
         Trace("  next: [%X] ",n.next); Trace(VisNodMode[n.next.mode]); END;
      IF n.l    <> NIL THEN
         Ln(indent);
         Trace("     l: [%X] ",n.l); Trace(VisNodMode[n.l.mode]); END;
      IF n.r    <> NIL THEN
         Ln(indent);
         Trace("     r: [%X] ",n.r); Trace(VisNodMode[n.r.mode]); END;
      IF n.mode = pc.nd_module THEN
        VisStruct (n.type, indent+1, currentVertex, "type");
      END;
      VisObject (n.obj, indent+1, currentVertex, "obj");
      IF n.mode#pc.nd_goto THEN
        VisNode   (n.l, indent+1, currentVertex, "l");
      END;
      VisNode   (n.r, indent+1, currentVertex, "r");
      VisNode   (n.next, indent, currentVertex, "next");
      VisValue(n.val, n.type, indent+1, currentVertex, "val");
  END;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  IF env.config.Option("gen_hgr") AND (edgeFrom # -1) THEN
      hgr.AddEdge(hgr.woodEdgeType, edgeFrom, FindVertex(n));
      hgr.SetLabVal (hgl.ObjEdge,
                     Nedges,
                     hgr.woodEdgeLabel,
                     edgelabel);
      INC(Nedges);
  END;
<* END *>
END VisNode;

-----------------------------------------------------------------------------

PROCEDURE VisIR* (n: pc.NODE; str: ARRAY OF CHAR; _cur_mod: pc.Mno);
(*
   Visualise IR subtree generated for node 'n' (usually module)
*)
VAR
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    s: ARRAY 128 OF CHAR;
<* END *>
BEGIN
  cur_mod := _cur_mod;
  env.config.Equation("fe_irvis_proc",feirvisproc);
  NEW( addresses, 100);
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  IF env.config.Option("gen_hgr") THEN
      hgr.Init;
      hgr.CreateNewGraph(1);
      hgr.SetFrTitle(0, "wood");
  END;
<* END *>
  Nnodes := 0;
  Nedges := 0;

  Trace(str);
  VisNode (n, 0, -1, "");

<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  IF env.config.Option("gen_hgr") THEN
      hgr.RearrangeGraph(Nnodes, 60, 50);
      hgr.CountLPTs(Nedges);
      hgr.NormalizeGraph(1);
      Printf.sprintf(s,"%s_wood.hgr",n.obj.name^);
      hgr.SaveGraph(s);
  END;
<* END *>
  Trace("\n");
END VisIR;

BEGIN
END pcVisIR.
