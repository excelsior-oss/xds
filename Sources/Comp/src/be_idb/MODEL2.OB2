<* WOFF903+ *>
<* NEW SL4IV- *>
MODULE model2;

<* +o2addkwd *>
FROM SYSTEM IMPORT PRED,SUCC;
IMPORT
  iv := IVERAS,
  pc := pcK,
  str:= Strings,
  env:= xiEnv,
  ent:= Entities,
  xfs:= xiFiles,
  io := opIO,
  sf := SeqFile,
  db := DBAPI,
  mk := xcMake,
  ls := lists,
  as := Attributes,
  ir,
  pcO,
  ObjNames,
  xFilePos,
  ic := ivConst,
  rm := RelManager,
  Emodule,
  IVProject,
  SYSTEM,
  DStrings,
  WholeStr,
  ConvTypes;
TYPE
STRING_LIST =ls.STRING_LIST;

EMODULE = Emodule.EMODULE;
EMODULE_STRUCTURE = Emodule.EMODULE_STRUCTURE;

CONST
  Ecache_len = 50;
TYPE
  EMODULE_CACHE = RECORD
    mods   : ARRAY Ecache_len OF EMODULE_STRUCTURE;
    counts : ARRAY Ecache_len OF INTEGER; (* number of attempts to load the emodule*)
    names  : ARRAY Ecache_len OF DStrings.String;
  END;


VAR
  match : LONGINT;
  emc :  EMODULE_CACHE;

PROCEDURE NeedsSaving*( em: EMODULE; e : iv.ENTITY): BOOLEAN;
BEGIN
  RETURN ( (e.obj<>NIL)AND
           (e.obj.tags * pc.OTAG_SET{pc.otag_public,pc.otag_public_f} <> pc.OTAG_SET{})AND
	   (e.type IN ic.et_DECLS)
	 ) OR (e.type = ic.et_module)OR(pcO.def) ;
END NeedsSaving;

PROCEDURE   (VAR ec: EMODULE_CACHE)ini();
VAR
  i : INTEGER;
BEGIN
  FOR i := 0 TO Ecache_len -1  DO
    ec.mods[i] := NIL;
    ec.counts[i] := 0;
  END;
END ini;

PROCEDURE   (VAR ec: EMODULE_CACHE)eliminate_expired();
VAR
  i : INTEGER;
BEGIN
  FOR i := 0 TO Ecache_len -1  DO
    IF ec.counts[i]  <= 0 THEN
      ec.mods[i] := NIL;
    END;
    DEC(ec.counts[i]);
  END;
  --env.info.print('\nsize = %d\n',size);
END eliminate_expired;

PROCEDURE   (VAR ec: EMODULE_CACHE)if_preloaded(name : DStrings.String):EMODULE_STRUCTURE;
VAR
  i : INTEGER;
BEGIN
  FOR i := 0 TO Ecache_len -1  DO
    IF (ec.names[i] <> NIL) AND (name^ = ec.names[i]^) THEN
      IF ec.counts[i] < 10 THEN
        INC(ec.counts[i], 2 );
      END;
      IF ec.mods[i] <> NIL THEN INC(match); END;
      RETURN ec.mods[i];
    END;
  END;
  RETURN NIL;
END if_preloaded;

PROCEDURE   (VAR ec: EMODULE_CACHE)add(emod : EMODULE_STRUCTURE);
VAR
  i : INTEGER;
BEGIN
  FOR i := 0 TO Ecache_len -1  DO
    IF (ec.names[i] <> NIL) AND (ec.names[i]^ = emod.mname^) THEN
      IF ec.counts[i] >= 2 THEN
        ec.mods[i] := emod;
      ELSIF ec.counts[i] >= 0 THEN
        INC (ec.counts[i] , 2 );
      ELSE
        ec.counts[i] := 0;
      END;
      RETURN;
    END;
  END;
  FOR i := 0 TO Ecache_len -1  DO
    IF ec.counts[i] < 0  THEN
      ec.counts[i] := 0;
      ec.names[i]  := emod.mname;
    END;
  END;
END add;



PROCEDURE establish_entity(em : EMODULE; par : LONGINT; type : INTEGER; n:pc.NODE) : LONGINT;
VAR
  e:iv.ENTITY;
BEGIN
  e := iv.create_from_node(n);
  ASSERT( e # NIL );
  e.type := type ;
  em.add_entity( par, e );
  RETURN e.entity_id;
END establish_entity;




PROCEDURE establish_obj_entity(em : EMODULE; par : LONGINT; type : INTEGER; o : pc.OBJECT) : LONGINT;
VAR
  e:iv.ENTITY;
BEGIN
  e := iv.create_from_object(o);
  e.ext_obj_name := iv.get_ext_name(o);
  ASSERT( e # NIL );
  e.type := type ;
  em.add_entity( par, e );
  RETURN e.entity_id;
END establish_obj_entity;

PROCEDURE establish_imported_object(par : LONGINT; em : EMODULE;  I : pc.ImportedObject):LONGINT;
VAR
  e  : iv.ENTITY;
BEGIN
  NEW(e);
  e.children.ini_by_len(0);
  e.rels.ini_by_len(0);
  e.attrs.ini_by_len(0);
  e.ext_obj_name := iv.get_ext_name(I.obj);
  e.get_pos(I.pos);
  e.get_end_pos(I.end);
  e.node      := NIL;
  e.obj       := I.obj;
  e.struct    := NIL;
  e.origin    := ent.or_object;
  e.type      := ic.et_imported_object;
  e.caption   := I.obj.name;
  em.add_entity(par,e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;
  I.obj.eno := e.entity_id;
  RETURN e.entity_id;
END establish_imported_object;

PROCEDURE establish_block(em: EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
VAR e: iv.ENTITY;
BEGIN
  e := iv.create_from_node(n);
  ASSERT( e # NIL );
  e.type:=ic.et_block;
  NEW(e.caption,6);
  str.Assign('Block', e.caption^);
  em.add_entity(par,e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;
  RETURN e.entity_id;
END establish_block;

PROCEDURE establish_module(em : EMODULE; n:pc.NODE):LONGINT;
			         (*Creates entity and insert it in entities*)
 VAR
     e,
     imp_sec : iv.ENTITY;
     fnam : xfs.String;
     r,c  : LONGINT;
     I    : pc.ImportedObject;
 BEGIN
  e := iv.create_from_node(n);
  ASSERT( e # NIL );
  e.type:=ic.et_module;
  e.from_row:=0;
  e.from_col:=0;
  n.pos.unpack(fnam,r,c);
  e.ext_obj_name:=iv.get_ext_name(n.obj);
  em.add_entity(Emodule.no_parent,e); --add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;


  IF pc.import.objects # NIL THEN
    r := establish_entity(em, e.entity_id, ic.et_import_section, n);
    imp_sec := em.get_entity(r);
    imp_sec.get_pos(pc.import.pos);
    imp_sec.get_end_pos(pc.import.end);

    I:= pc.import.objects;
    WHILE I # NIL  DO
      c := establish_imported_object(imp_sec.entity_id , em, I);
      I := I.next;
    END;
  END;
  RETURN e.entity_id;
 END establish_module;

PROCEDURE establish_procdecl(em : EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
 VAR
     e     :   iv.ENTITY;
     at    :   as.ATTR_REC;
 BEGIN
  e := iv.create_from_node(n);
  e.get_pos(n.obj.pos);
  e.get_end_pos(n.obj.end);
  e.type:=ic.et_procdecl;
  e.ext_obj_name:=iv.get_ext_name(n.obj);
  em.add_entity(par,e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;
  IF n.type#NIL THEN
    e.struct:=n.type.base;
  ELSE
    e.struct:=NIL;
  END;


  at.type:=ic.at_public;
  at.set_bool_val(pc.otag_public IN n.obj.tags);
  e.attrs.add(at);

  at.type:=ic.at_self_recursive;
  at.set_bool_val( FALSE (*default*) );
  e.attrs.add(at);

  RETURN e.entity_id;
 END establish_procdecl;


PROCEDURE establish_return(em:EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
			         (*Creates entity and inserts it in entities*)
VAR
   e:iv.ENTITY;
BEGIN
  e := iv.create_from_node(n);
  e.type:=ic.et_return;
  em.add_entity(par,e);
  RETURN e.entity_id;
END establish_return;


PROCEDURE establish_usage(em:EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
VAR
  e:iv.ENTITY;
BEGIN
  CASE n.mode OF
  | pc.nd_value:
    e := iv.create_from_node(n.val.expr);
    e.get_pos(n.pos);
    e.get_end_pos(n.end);
    ASSERT(n.val.expr.obj.mode = pc.ob_cons);
    e.type:=ic.et_constusage;
  | pc.nd_var:
    e := iv.create_from_node(n);
    CASE n.obj.mode OF
    | pc.ob_var, pc.ob_varpar:
      e.type:=ic.et_varusage;
    | pc.ob_cons:
      e.type:=ic.et_constusage;
    END;
  | pc.nd_field:
    e := iv.create_from_node(n);
    e.type:=ic.et_fieldusage;
  END;
  em.add_entity(par,e);
  RETURN e.entity_id;
END establish_usage;


PROCEDURE establish_assign(em:EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
VAR
  e,var_e:iv.ENTITY;
BEGIN
  e := iv.create_from_node(n);
  e.type:=ic.et_assign;
  em.add_entity(par,e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;

  IF n.l=NIL THEN
    IF n.obj.mode IN pc.VARs + pc.OB_SET{pc.ob_field} THEN
  	  var_e := iv.create_from_node(n);
      IF n.obj.mode = pc.ob_field THEN
        var_e.type:=ic.et_fieldusage;
      ELSE
        var_e.type:=ic.et_varusage;
      END;
  	em.add_entity(e.entity_id,var_e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;
  	var_e.get_pos(n.pos);
  	var_e.to_row:=var_e.from_row;
        var_e.to_col:=var_e.from_col+SYSTEM.VAL(LONGINT,str.Length(n.obj.name^))-1;
    END;
  END;
  RETURN e.entity_id;
END establish_assign;


PROCEDURE establish_for(em:EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
VAR
  for_e,var_e:iv.ENTITY;
BEGIN
  for_e := iv.create_from_node(n);
  IF for_e = NIL THEN RETURN par; END;
  for_e.type    := ic.et_for;
  for_e.caption := NIL;
  em.add_entity(par,for_e); --add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;

  var_e := iv.create_from_node(n);
  IF n.obj.mode = pc.ob_field THEN
    var_e.type:=ic.et_fieldusage;
  ELSE
    var_e.type:=ic.et_varusage;
  END;
  em.add_entity(for_e.entity_id,var_e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;
  var_e.get_pos(n.pos);
  var_e.from_col:=var_e.from_col+4;(*Length('FOR ')=4*)
  var_e.get_end_pos(n.l.l.pos);
  RETURN for_e.entity_id;
END establish_for;


PROCEDURE establish_proccall(em:EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
VAR
  e:iv.ENTITY;
BEGIN
  e := iv.create_from_node(n);
  IF e = NIL THEN RETURN par; END;
  e.caption := NIL;
  IF (n.l#NIL)AND(n.l.mode=pc.nd_method)THEN
     e.type:=ic.et_methodcall;
     e.obj:=n.l.obj;
  ELSE
    CASE n.mode OF
      | pc.nd_call:
        e.type:=ic.et_proccall;
      | pc.nd_sproc:
        e.type:=ic.et_sproc;	
    END;
    e.obj:=n.obj;
  END;
  em.add_entity(par,e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;
  RETURN e.entity_id;
END establish_proccall;

PROCEDURE establish_typedecl_EX(em:EMODULE; s:pc.STRUCT;par:LONGINT) : LONGINT;
VAR
  e  : iv.ENTITY;
  at : as.ATTR_REC;
BEGIN
  e := iv.create_from_struct(s);
  IF e = NIL THEN RETURN par; END;

  e.type:=ic.et_typedecl;

  IF (s.obj # NIL) AND ObjNames.valid_name(s.obj.name) THEN
    e.ext_obj_name:=iv.get_ext_name(s.obj);
    at.type:=ic.at_public;
    at.set_bool_val(pc.otag_public IN s.obj.tags);
    e.attrs.add(at);
  END;
  em.add_entity(par,e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;

  RETURN e.entity_id;
END establish_typedecl_EX;

PROCEDURE establish_vardecl(em:EMODULE; o:pc.OBJECT;par:LONGINT):LONGINT;
VAR
  e  : iv.ENTITY;
  at : as.ATTR_REC;
BEGIN
  IF pc.otag_secretvar IN o.tags THEN RETURN par; END;
  e := iv.create_from_object(o);
  IF e = NIL THEN RETURN par; END;
  e.type:=ic.et_vardecl;
  e.ext_obj_name:=iv.get_ext_name(o);
  em.add_entity(par,e); --add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;

  at.type:=ic.at_public;
  at.set_bool_val(pc.otag_public IN o.tags);
  e.attrs.add(at);

  at.type:=ic.at_nest_used;
  at.set_bool_val( FALSE (*default*) );
  e.attrs.add(at);
  RETURN e.entity_id;
END establish_vardecl;

PROCEDURE establish_param(em:EMODULE; o:pc.OBJECT;par:LONGINT):LONGINT;
VAR
  e  : iv.ENTITY;
  at : as.ATTR_REC;
BEGIN
  IF pc.otag_secretvar IN o.tags THEN RETURN par; END;
  e := iv.create_from_object(o);
  IF e = NIL THEN RETURN par; END;
  e.type:=ic.et_paramdecl;
  e.ext_obj_name:=iv.get_ext_name(o);
  em.add_entity(par,e); --add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;

  at.type:=ic.at_nest_used;
  at.set_bool_val( FALSE (*default*) );
  e.attrs.add(at);
  RETURN e.entity_id;
END establish_param;


PROCEDURE establish_fielddecl(em:EMODULE; o:pc.OBJECT;par:LONGINT):LONGINT;
VAR
  e  : iv.ENTITY;
  at : as.ATTR_REC;
BEGIN
  e:=iv.create_from_object(o);
  IF e=NIL THEN RETURN par; END;
  e.type:=ic.et_fielddecl;
  e.ext_obj_name:=iv.get_ext_name(o);
  em.add_entity(par,e); --add entity modifies e.parent, e.entity_id and uses e.ext_obj_name;

  at.type:=ic.at_public;
  at.set_bool_val(pc.otag_public IN o.tags);
  e.attrs.add(at);
  RETURN e.entity_id;
END establish_fielddecl;

PROCEDURE establish_constdecl(em:EMODULE; o:pc.OBJECT;par:LONGINT):LONGINT;
VAR
  e  : iv.ENTITY;
  at : as.ATTR_REC;
BEGIN
  e:=iv.create_from_object(o);
  IF e=NIL THEN RETURN par; END;
  e.type:=ic.et_constdecl;
  e.ext_obj_name:=iv.get_ext_name(o);
  em.add_entity(par,e);--add entity modifies e.parent, e.entity_id and uses e.ext_obj_name

  at.type:=ic.at_public;
  at.set_bool_val(pc.otag_public IN o.tags);
  e.attrs.add(at);

  at.type:=ic.at_nest_used;
  at.set_bool_val( FALSE (*default*) );
  e.attrs.add(at);
  RETURN e.entity_id;
END establish_constdecl;


PROCEDURE establish_then(em:EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
VAR
  e : iv.ENTITY;
CONST
  cpt = "THEN";
BEGIN
  e := iv.create_from_node(n.l);
  IF e = NIL THEN RETURN par; END;
  e.get_end_pos(n.pos);
  e.type:=ic.et_then_branch;
  NEW(e.caption,SIZE(cpt));
  COPY(cpt, e.caption^);
  em.add_entity(par,e);
  RETURN e.entity_id;
END establish_then;

PROCEDURE establish_else(em:EMODULE; n:pc.NODE;par:LONGINT):LONGINT;
VAR
  e : iv.ENTITY;
CONST
  cpt = "ELSE";
BEGIN
  e := iv.create_from_node(n.r);
  IF (e = NIL) OR n.end.IsNull() THEN RETURN par; END;
  e.get_end_pos(n.end);
  e.type:=ic.et_else_branch;
  NEW(e.caption,SIZE(cpt));
  COPY(cpt, e.caption^);
  em.add_entity(par,e);
  RETURN e.entity_id;
END establish_else;




PROCEDURE generate*(em:EMODULE; n:pc.NODE);

  VAR
    make_params : BOOLEAN; (* if TRUE obj() creates et_paramdecl instead of et_vardecl*)
    InExpr      : BOOLEAN;
  (* All the procedures below (struct, obj, node, ParseExpr,..) allow their
     arguments to be NIL. So no nil check is needed.
  *)
  PROCEDURE ^ obj(o:pc.OBJECT; par : LONGINT);
  PROCEDURE ^ ParseExpr_Ex(n:pc.NODE; par : LONGINT; e_type : INTEGER) : LONGINT;
  PROCEDURE ^ ParseExpr(n:pc.NODE; par : LONGINT; e_type : INTEGER);
  PROCEDURE ^ UnnamedStruct (s : pc.STRUCT; par : LONGINT);
  PROCEDURE ^ parse_statement(n:pc.NODE;par:LONGINT);
  PROCEDURE ^ parse_statement_list(n:pc.NODE;par:LONGINT);
  PROCEDURE ^ parse_procdecl_list(n:pc.NODE;par:LONGINT);
  PROCEDURE ^ parse_procdecl (n:pc.NODE;par:LONGINT);

  PROCEDURE NamedStruct(s : pc.STRUCT; par : LONGINT);   (*Is called to parse type declaration*)
  VAR
      i : LONGINT;
  BEGIN
    IF (s = NIL) OR (s.mno # pc.ZEROMno) THEN RETURN; END;
    IF (s.obj # NIL) AND (~s.obj.pos.IsNull() ) THEN
      s.pos := s.obj.pos;
    END;
    IF ~ (iv.tmark_passed IN s.marks) THEN
       i := establish_typedecl_EX(em, s, par); (*Not for standard types*)
       CASE s.mode OF
        |pc.ty_array, pc.ty_array_of:
	    IF s.min # NIL THEN
	      ParseExpr(s.min.expr, i, ic.et_expr);
	    END;
	    IF s.max # NIL THEN
	      ParseExpr(s.max.expr, i, ic.et_expr);	
	    END;
            UnnamedStruct(s.base,i); (*base type*)
	|pc.ty_range,
         pc.ty_enum , pc.ty_pointer , pc.ty_set :
            UnnamedStruct(s.base,i); (*base type*)

        |pc.ty_record:
           obj(s.prof,i); (*field list*)
       ELSE
       END;
    END;
  END NamedStruct;

  PROCEDURE UnnamedStruct(s : pc.STRUCT; par : LONGINT);   (*Is called to parse type declaration*)
  VAR
      i : LONGINT;
  BEGIN
    IF (s = NIL) OR (s.mno # pc.ZEROMno) OR iv.IsNamedType(s) THEN RETURN; END; --that`s the difference between UnnamedStruct and NamedStruct

    IF ~ (iv.tmark_passed IN s.marks) THEN
       i := establish_typedecl_EX(em, s,par); (*Not for standard types*)
       CASE s.mode OF
        |pc.ty_array,
          pc.ty_array_of,
          pc.ty_range,
          pc.ty_enum ,
          pc.ty_pointer,
          pc.ty_set :
            UnnamedStruct(s.base,i); (*base type*)

        |pc.ty_record:
           obj(s.prof,i); (*field list*)

       ELSE
       END;
    END;
  END UnnamedStruct;

  VAR
     cur_var_sect : iv.ENTITY;

  PROCEDURE obj(o:pc.OBJECT; par : LONGINT);
  VAR
    i : LONGINT;
    vs_id : LONGINT;
    line,col : LONGINT;
    fnam : xfs.String;
  BEGIN
    IF o = NIL THEN RETURN; END;
    CASE o.mode OF
    | pc.ob_var :
      IF (o.type_use # NIL)AND( (cur_var_sect = NIL) OR (cur_var_sect.node # o.type_use) ) THEN
         vs_id := establish_entity(em, par, ic.et_varsec ,o.type_use );
	 cur_var_sect := em.get_entity(vs_id);
	 cur_var_sect.get_pos(o.pos);
      END;
      IF (o.type_use # NIL) AND ObjNames.valid_name(o.name) AND (o.type.mode <> pc.ty_process)  THEN
        IF make_params THEN
          i := establish_param(em, o,cur_var_sect.entity_id);
        ELSE  
          i := establish_vardecl(em, o,cur_var_sect.entity_id);
        END;
        UnnamedStruct(o.type, par);
      ELSE
       o.pos.unpack(fnam, line,col);
       --env.info.print('Type use = NIL, line = %d, col= %d\n ', line,col);
      END;

    | pc.ob_varpar:
      IF make_params THEN
        i :=  establish_param(em, o,par);
      ELSE  
        i :=  establish_vardecl(em, o,par);
      END;
      UnnamedStruct(o.type, par);


    | pc.ob_type:
      NamedStruct(o.type, par);

    | pc.ob_field:
      i := establish_fielddecl(em, o,par);
      UnnamedStruct(o.type, par);


    | pc.ob_cons:
      i := establish_constdecl(em, o, par);
      ParseExpr(o.val, i , ic.et_expr);
      UnnamedStruct(o.type, par);
    | pc.ob_xproc,
      pc.ob_eproc :
      IF pcO.def THEN
           i := establish_obj_entity( em, par, ic.et_proc_header, o);(* for *.def  files only*)
      END;
    ELSE

    END;

    obj(o.next,par);
  END obj;

  PROCEDURE parse_module ( n : pc.NODE );
  VAR
    ext     : xfs.String;
    mod_ext : xfs.String;
    i       : LONGINT;
  BEGIN
    ASSERT( n.mode = pc.nd_module);
    i:=establish_module(em, n); -- i is always 0

    xfs.sys.GetExt(em.source^, ext);
    env.config.Equation('MOD',mod_ext);
    IF ext^ # mod_ext^ THEN  (* if not *.mod file*)
      obj(n.type.prof,i); (*     public objects*)
    END;
    obj(n.type.mem,i);  (* non public objects*)
    parse_procdecl_list(n.l,i);
    IF n.r <> NIL THEN
      parse_statement_list(n.r, establish_block(em, n, i) );
    END;
  END parse_module;

  PROCEDURE parse_procdecl_list(n:pc.NODE;par:LONGINT);
  BEGIN
    WHILE n <> NIL DO
      parse_procdecl(n, par);
      n := n.next;
    END;
  END parse_procdecl_list;

  PROCEDURE parse_procdecl(n:pc.NODE;par:LONGINT);
  VAR
    i : LONGINT;
    body: pc.NODE;
  BEGIN
    ASSERT( n.mode = pc.nd_proc );
    IF n.obj.mno = env.info.mod_no THEN
      i:= establish_procdecl(em, n,par);
      make_params := TRUE;
      obj(n.type.prof,i);(*Param list*)
      make_params := FALSE;
      obj(n.type.mem,i);	(*Local list*)
      parse_procdecl_list(n.l,i);
      IF n.r <> NIL THEN
        CASE n.r.mode OF
        | pc.nd_block:
          body := n.r.r;
        | pc.nd_except:
          body := n.r.l;
          parse_statement_list(n.r.r, establish_block(em, n, i) );
        ELSE
          body := n.r;
        END;
      ELSE
        body := NIL;
      END;
      parse_statement_list(body, establish_block(em, n, i) );


    END;
  END parse_procdecl;

  PROCEDURE parse_statement_list(n:pc.NODE;par:LONGINT);
  BEGIN
    WHILE n <> NIL DO
      parse_statement(n, par);
      n := n.next;
    END;
  END parse_statement_list;

  PROCEDURE parse_statement(n:pc.NODE;par:LONGINT);
  VAR
    n1 : pc.NODE;
    i,j  : LONGINT;
    branch : pc.NODE;
  BEGIN
   IF n = NIL THEN RETURN END;
   CASE n.mode OF
    | pc.nd_sproc:
      i := establish_proccall(em, n, par);
      n1 := n.r;
      WHILE n1 <> NIL DO
        ParseExpr(n1,i,ic.et_actual_parameter);
        n1 := n1.next;
      END;

    | pc.nd_assign:
      i:=establish_assign(em, n,par);
      ParseExpr(n.l, i, ic.et_expr);
      ParseExpr(n.r, i, ic.et_expr);
    | pc.nd_call:
      i := par;
      IF n.l#NIL THEN
	    IF (n.l.mode=pc.nd_method) OR (n.obj#NIL) THEN
          i := establish_proccall(em, n,par);
        END;
      ELSIF (n.obj#NIL) THEN
        i := establish_proccall(em, n,par);
      END;
      ParseExpr(n.l,i,ic.et_actual_parameter);
      n1 := n.r;
      WHILE n1 <> NIL DO
        ParseExpr(n1,i,ic.et_expr);
        n1 := n1.next;
      END;
    | pc.nd_case:
      i:=establish_entity(em,par, ic.et_case, n);
      ParseExpr(n.l, i, ic.et_expr);
      IF n.r# NIL THEN
        branch := n.r.l;
        WHILE branch <> NIL DO
          IF branch.r <> NIL THEN
             j := establish_entity(em, i, ic.et_case_branch, branch);
             parse_statement_list(branch.r, j);

             n1 := branch.l;
             WHILE n1 <> NIL DO
               ParseExpr(n1, j, ic.et_expr);
               n1 := n1.next;
             END;
          END;
          branch := branch.next;
        END;
        IF n.r.r <> NIL THEN
           parse_statement_list(n.r.r,establish_else(em, n.r.r,i));
        END;
      END;

    | pc.nd_return:
      ParseExpr(n.l,establish_return(em, n,par),ic.et_expr);

    | pc.nd_for:
      i:=establish_for(em, n,par);
      ParseExpr(n.l.l,i,ic.et_expr);
      ParseExpr(n.l.r,i,ic.et_expr);
      parse_statement_list(n.r, i); (* op. sequence*)

    | pc.nd_with:
      i := establish_entity(em, par, ic.et_with, n);
      ParseExpr(n.l,i,ic.et_expr);
      parse_statement_list(n.r, i); (* op. sequence*)

    | pc.nd_while:
      i:=establish_entity(em, par, ic.et_while, n);
      ParseExpr(n.l,i,ic.et_bool_expr);
      parse_statement_list(n.r, i); (* op. sequence*)

    | pc.nd_exit:
      i := establish_entity(em, par, ic.et_exit, n);

    | pc.nd_loop:
      parse_statement_list(n.r,establish_entity(em, par, ic.et_loop, n) );

    | pc.nd_repeat:
      i:=establish_entity(em, par, ic.et_repeat, n);
      parse_statement_list(n.l, i); (* op. sequence*)
      ParseExpr(n.r,i,ic.et_bool_expr);

    | pc.nd_if:
      IF NOT (pc.ntag_substitute IN n.tags) THEN --if
        i:=establish_entity(em, par, ic.et_if, n);
        ParseExpr(n.l,i,ic.et_bool_expr);
        IF n.r# NIL THEN
          parse_statement_list(n.r.l, establish_then(em, n.r,i) ); (* op. sequence*)
          branch := n.r;
          LOOP
            IF branch.r = NIL THEN EXIT END;
            IF (branch.r.mode # pc.nd_if) OR NOT (pc.ntag_elsif_node IN branch.r.tags) THEN
             parse_statement_list(branch.r, establish_else(em, branch,i));
             EXIT;
            ELSE
             j := establish_entity(em, i, ic.et_elsif, branch.r);
             ParseExpr(branch.r.l, j, ic.et_bool_expr);
             parse_statement_list( branch.r.r.l, j);
             branch := branch.r.r	;
            END;
          END;
        END;
      ELSE --o2_with
        i:=establish_entity(em, par, ic.et_with, n);
        IF n.r# NIL THEN

          branch := n.r;
          LOOP
	    IF branch = NIL THEN
	      EXIT;
  	    END;
	    IF branch.end.IsNull() THEN branch.end := branch.pos;END;
            j := establish_entity(em, i, ic.et_with_option, branch ) ;
            parse_statement_list( branch.l, j);
	    IF (branch.r # NIL) AND (branch.r.mode = pc.nd_if) THEN
              branch := branch.r.r;
	    ELSIF (branch.r # NIL) AND (branch.r.mode # pc.nd_wtrap) THEN
              j := establish_entity(em, i, ic.et_with_option, branch.r ) ;
              parse_statement_list( branch.r, j);
	      EXIT
	    ELSE
	      EXIT
	    END;
          END;
	
        END;
      END;
    | pc.nd_eval:
      IF (n.l.mode =pc.nd_binary ) AND (   (n.l.sub = pc.sb_pre_inc)
                                      OR (n.l.sub = pc.sb_pre_dec)
                                      OR (n.l.sub = pc.sb_post_inc)
                                      OR (n.l.sub = pc.sb_post_dec)				      				
				                                  )THEN
						
        i := establish_entity(em, par, ic.et_sproc, n );
        ParseExpr(n.l.l, i, ic.et_actual_parameter);
        ParseExpr(n.l.r, i, ic.et_expr);
      ELSE
        parse_statement(n.l, par);
        parse_statement(n.r, par);
      END;
    | pc.nd_method,
      pc.nd_finally,
      pc.nd_reraise:
       parse_statement(n.l, par);
    | pc.nd_activate,
      pc.nd_except:
      (* Nothing*)
   ELSE
     parse_statement(n.l, par);
     parse_statement(n.r, par);
   END;
  END parse_statement;


<* IF SL4IV THEN *>
  VAR wasModule : BOOLEAN;
<* END *>



  PROCEDURE ParseExpr_Ex(n:pc.NODE; par : LONGINT; e_type : INTEGER) : LONGINT;
  VAR i, j : LONGINT;
      ei, ej, p : iv.ENTITY;
      n1 : pc.NODE;
      OldInExpr : BOOLEAN;
  BEGIN
    IF n = NIL THEN RETURN par; END;

    CASE n.mode OF
    | pc.nd_method:
      RETURN ParseExpr_Ex(n.l, par, e_type);
    | pc.nd_call,
      pc.nd_sproc:
      i := par;
      IF n.l # NIL THEN
        IF (n.l.mode=pc.nd_method) OR (n.obj#NIL) THEN
          i := establish_proccall(em, n,par);
        END;
      ELSIF (n.obj#NIL) THEN
        i := establish_proccall(em, n,par);
      END;
      ParseExpr(n.l,i,ic.et_expr);
      n1 := n.r;
      WHILE n1 <> NIL DO
        ParseExpr(n1,i,ic.et_actual_parameter);
        n1 := n1.next;
      END;                                                      
      RETURN i;

    | pc.nd_var:
      IF n.obj.mode = pc.ob_cons THEN
	RETURN establish_entity(em, par, ic.et_constusage, n);
      ELSE
	RETURN establish_entity(em, par, ic.et_varusage, n);
      END;

    | pc.nd_value:
      ASSERT(n.val # NIL);
      IF (n.val.expr # NIL) AND (n.val.expr.mode = pc.nd_var) AND (n.val.expr.obj.mode=pc.ob_cons) THEN
        RETURN establish_usage(em, n, par);
      ELSE
        RETURN ParseExpr_Ex(n.val.expr, par ,e_type);
      END;

    | pc.nd_deref:
      i := ParseExpr_Ex(n.l,par,e_type);
      j := establish_entity(em, i, ic.et_deref, n);
      RETURN j;

    | pc.nd_index:
      i := ParseExpr_Ex(n.l,par,e_type);
      j := establish_entity(em, i, ic.et_index, n);
      ParseExpr(n.r, j , e_type);
      RETURN j;

    | pc.nd_field:
      IF (n.obj.mno<LEN(pc.mods^)) & (n.obj.mode =pc.ob_field) THEN
        i := ParseExpr_Ex(n.l,par,e_type);
        j := establish_usage(em, n, i);
        ej := em.get_entity(j);
        p := em.get_entity(ej.parent);
        LOOP
          IF p.type # ic.et_deref THEN
            IF p.to_row < ej.to_row THEN p.to_row := ej.to_row; END;
            IF p.to_col < ej.to_col THEN p.to_col := ej.to_col; END;
            p.caption := NIL;
            IF  (p.type # ic.et_fieldusage)AND
	        (p.type # ic.et_index) AND
	        (p.type # ic.et_deref) THEN
              EXIT;
            END;
          END;
          p := em.get_entity(p.parent);
        END;
        RETURN j;
      END;
    | pc.nd_guard :
        RETURN ParseExpr_Ex(n.l, par, e_type);
    | pc.nd_unary :
      IF n.sub IN pc.SUB_SET{pc.su_cast, pc.su_conv, pc.su_neg} THEN
        RETURN ParseExpr_Ex(n.l, par, ic.et_expr);
      END;
    | pc.nd_pair :
        ParseExpr(n.r, par, ic.et_expr);
        RETURN ParseExpr_Ex(n.l, par, ic.et_expr);
    ELSE
    END;
    i := par;
    IF ~InExpr THEN
      i := establish_entity(em, par, e_type, n);
    END;
    OldInExpr := InExpr;
    InExpr := TRUE;
    ParseExpr(n.l, i, ic.et_expr);
    ParseExpr(n.r, i, ic.et_expr);
    InExpr := OldInExpr;

    CASE n.mode OF
    | pc.nd_index:
      ei := em.get_entity(i);
      ej := em.get_entity(ei.children.get_elem(0));
      IF ei.from_row > ej.from_row THEN ei.from_row := ej.from_row END;
      IF ei.from_col > ej.from_col THEN ei.from_col := ej.from_col END;
    ELSE
    END;

    RETURN i;
  END ParseExpr_Ex;

  PROCEDURE ParseExpr(n:pc.NODE; par : LONGINT; e_type : INTEGER);
  VAR
    i: LONGINT;
  BEGIN
    i := ParseExpr_Ex(n, par, e_type);
  END ParseExpr;

 BEGIN
  make_params := FALSE;
  InExpr := FALSE;
  parse_module( n );
 END generate;

PROCEDURE initialize*(all : BOOLEAN);
VAR
  starts_from : xfs.String;
  res : ConvTypes.ConvResults;
  v   : LONGINT;
BEGIN
  IF all THEN
    db.delete_common_tables();
  END;
  emc.ini();
  env.config.Equation('FidStart',starts_from);
  IF starts_from <> NIL THEN
    WholeStr.StrToInt(starts_from^, v , res);
    IF res <> WholeStr.strAllRight THEN
      env.info.print(' %s is an invalid value for FidStart\n' ,starts_from);
      HALT();
    END;
    iv.FidStart := SYSTEM.VAL(INTEGER, v);
  END;
END initialize;

PROCEDURE process_module*(n:pc.NODE);
VAR
  cur : EMODULE;
  i   : pc.Mno;
  r,c : LONGINT;
  fnam: xfs.String;
  dir,
  name,
  ext : xfs.String;
  eq_ext : xfs.String;
BEGIN
  NEW(iv.mods,pc.mod_cnt);
  NEW(cur);
  xfs.sys.Get(env.info.file^, dir, name, ext) ;
  DStrings.Append('.',name);
  DStrings.Append(ext^,name);
  NEW( fnam, 300 );
  xfs.sys.GetFullPathName(env.info.file^, fnam^);
  cur.ini( iv.get_file_id(n.pos), name, fnam, env.info.main);

  iv.mods[pc.ZEROMno]:=cur;
  match := 0;
  FOR i:=pc.Mno{1} TO PRED(pc.mod_cnt) DO
     iv.mods[i]:=emc.if_preloaded(pc.mods[i].name);
     IF iv.mods[i] = NIL THEN
       NEW(cur);
       ASSERT(~pc.mods[i].end.IsNull());
       pc.mods[i].pos.unpack(fnam,r,c); (*source file*)
       xfs.sys.Get(fnam^, dir, name, ext) ;
       DStrings.Append('.',name);
       DStrings.Append(ext^,name);

       IF cur.load_from_file(name, TRUE) THEN
            iv.mods[i]:=cur;
            emc.add(cur);
       END;
     END;
  END;
  emc.eliminate_expired();

  generate(iv.mods[pc.ZEROMno](EMODULE),n);

  iv.mods[pc.ZEROMno](EMODULE).sort_children;
  c := 0;
  iv.mods[pc.ZEROMno](EMODULE).seq_nest(c);
  iv.mods[pc.ZEROMno](EMODULE).calculate_sel;
  iv.mods[pc.ZEROMno](EMODULE).SetOwners({ic.et_procdecl});
  IF pcO.imp THEN
    xfs.sys.Get(env.info.file^, dir, name, ext)  ;
    env.config.Equation('DEF',eq_ext);
    DStrings.Append('.',name);
    DStrings.Append(eq_ext^,name);

    NEW(iv.def_emf);
    IF ~ iv.def_emf.load_from_file(name, TRUE) THEN
       iv.def_emf := NIL ;
    END;
  ELSE
    iv.def_emf := NIL;
  END;
  rm.find_rels( iv.mods[pc.ZEROMno](EMODULE) );
END process_module;

PROCEDURE end_module*();
VAR
  file_name,dir,name,ext : xfs.String;
BEGIN
 IF iv.mods#NIL THEN
  xfs.sys.Get(iv.mods[pc.ZEROMno].source^,dir,name,ext);
  NEW(file_name,LEN(name^)+LEN(ext^)+1);
  str.Concat(name^,'.',file_name^);
  str.Append(ext^,file_name^);

  iv.mods[pc.ZEROMno](EMODULE).calc_final_pos(ic.et_module);
  iv.mods[pc.ZEROMno](EMODULE).save_to_file(NeedsSaving);

  iv.mods[pc.ZEROMno](EMODULE).db_write();

  iv.mods:=NIL;
--  env.info.print('\nmatch = %d\n', match);

 END;
END end_module;

PROCEDURE nil_deref*(n : pc.NODE);
VAR 
  at : as.ATTR_REC;
  e  : iv.STRUCT_ENTITY;
  s  : env.String;
  sa : db.Str250;
BEGIN
  IF (n=NIL) OR (n.obj=NIL) THEN RETURN; END;
  e:=iv.mods[pc.ZEROMno].find_entity(iv.get_ext_name(n.obj));
  IF e=NIL THEN RETURN; END;
  env.errors.GetLastMsg(s);
  IF s = NIL THEN RETURN; END;
  at.type:=ic.at_compwarn;
  str.Assign(s^,sa);
  at.set_str_val(sa);
  e(iv.ENTITY).attrs.add(at);
END nil_deref;

PROCEDURE warn_at_pos*(p : pc.TPOS);
VAR
  at : as.ATTR_REC;
  e  : iv.STRUCT_ENTITY;
  s  : env.String;
  sa : db.Str250;
BEGIN
  e:=iv.mods[pc.ZEROMno](EMODULE).entity_by_pos(p,-1);
  IF e=NIL THEN RETURN; END;
  env.errors.GetLastMsg(s);
  IF s = NIL THEN RETURN; END;
  at.type:=ic.at_compwarn;
  str.Assign(s^,sa);
  at.set_str_val(sa);
  e(iv.ENTITY).attrs.add(at);
END warn_at_pos;


PROCEDURE not_used*(o:pc.OBJECT);
VAR 
  at : as.ATTR_REC;
  e  : iv.STRUCT_ENTITY;
  s  : env.String;
  sa : db.Str250;
BEGIN
  IF o=NIL THEN RETURN; END;
  e:=iv.mods[pc.ZEROMno].find_entity(iv.get_ext_name(o));
  IF e=NIL THEN RETURN; END;
  env.errors.GetLastMsg(s);
  IF s = NIL THEN RETURN; END;
  at.type:=ic.at_compwarn;
  str.Assign(s^,sa);
  at.set_str_val(sa);
  e(iv.ENTITY).attrs.add(at);
END not_used;




PROCEDURE post*( curpro : mk.Project );
VAR
  i     : INTEGER;
  c     : EMODULE;
  name  : xfs.String;
  emname: xfs.String;
  ext   : xfs.String;
  n     : mk.Node;
BEGIN


 IF (curpro=NIL) OR (curpro.fname=NIL) THEN
  env.info.print('No project name specified\n');
  env.info.print('Use xc.exe =i  projname.prj\n');
  HALT;
 END;

 xfs.sys.GetName(curpro.fname^ ,name);
 IF (name=NIL)OR(str.Length(name^)=0)THEN
  env.info.print('Invalid project name\n');
  HALT;
 END;

 IVProject.Create_IV_Project(name);
 NEW(c);

 n:=curpro.nodes;
 WHILE n <> NIL  DO
    IF n.mod.mode IN {mk.md_mod,mk.md_def,mk.md_main,mk.md_oberon} THEN
      xfs.sys.GetName( n.mod.fname^ , emname);
      xfs.sys.GetExt(  n.mod.fname^ , ext);
      DStrings.Append('.', emname );
      DStrings.Append(ext^, emname );
      IF c.load_from_file(emname, FALSE) THEN
        IVProject.add_line_to_project(c.fid, c.source, c.mname, c.main);
      END;
    ELSIF n.mod.mode IN {mk.md_sym} THEN---Bloody dirt code(--FIXME)
      xfs.sys.GetName( n.mod.fname^ , emname);
      env.config.Equation('DEF',ext);
      DStrings.Append('.', emname );
      DStrings.Append(ext^, emname );
      IF c.load_from_file(emname, FALSE) THEN
        IVProject.add_line_to_project(c.fid, c.source, c.mname, c.main);
      END;

      xfs.sys.GetName( n.mod.fname^ , emname);
      env.config.Equation('MOD',ext);
      DStrings.Append('.', emname );
      DStrings.Append(ext^, emname );
      IF c.load_from_file(emname, FALSE) THEN
        IVProject.add_line_to_project(c.fid, c.source, c.mname, c.main);
      END;

      xfs.sys.GetName( n.mod.fname^ , emname);
      env.config.Equation('OBERON',ext);
      DStrings.Append('.', emname );
      DStrings.Append(ext^, emname );
      IF c.load_from_file(emname, FALSE) THEN
        IVProject.add_line_to_project(c.fid, c.source, c.mname, c.main);
      END;

    END;
    n:=n.next;
 END;


 IF ~db.common_tables_exist() THEN
   IF NOT (db.open_common_tables(name)) THEN
     env.info.print('Can`t open common tables in %s\n');
     ASSERT(FALSE);
   END;

 <* IF NOT SL4IV THEN *>
   FOR i:=0 TO LEN(iv.entity_type_names^)-1 DO
     db.write_EntTypeIdx(
       iv.entity_type_names[i],
       iv.entity_type_ext_names[i],
       iv.entity_type_show[i],
       i);
   END;
   FOR i:=0 TO LEN(iv.rel_type_names^)-1 DO
     db.write_RelNameIdx(
     iv.rel_type_names[i],
     iv.rel_type_ext_left_names[i],
     iv.rel_type_ext_right_names[i],
     i);
   END;
  <* ELSE *>
   FOR i:=0 TO LEN(iv.entity_type_names^)-1 DO
     IF NOT i IN {ic.et_repeat,ic.et_methodcall} THEN
       db.write_EntTypeIdx(
       iv.entity_type_names[i],
       iv.entity_type_ext_names[i],
       iv.entity_type_show[i],
       i);
     END;
   END;
   FOR i:=0 TO LEN(iv.rel_type_names^)-1 DO
     IF NOT i IN {ic.rt_IMPORT,ic.rt_BASE,ic.rt_METHOD} THEN
       db.write_RelNameIdx(
       iv.rel_type_names[i],
       iv.rel_type_ext_left_names[i],
       iv.rel_type_ext_right_names[i],
       i);
     END;
   END;
  <* END *>
   FOR i:=0 TO LEN(as.attr_types^)-1 DO
     db.write_attr_type(i, as.attr_types[i].attr_type_name,as.attr_types[i].domain);
   END;
   db.close_common_tables;
 END; --common tables don't exist;

 --env.info.print('match = %d', match);
 iv.mods := NIL;
END post;

BEGIN

 NEW(iv.entity_type_names,ic.et_count);
 NEW(iv.entity_type_ext_names,ic.et_count);
 NEW(iv.entity_type_show,ic.et_count);

 str.Assign('MODULE',iv.entity_type_names[ic.et_module]);
 str.Assign('VarDecl',iv.entity_type_names[ic.et_vardecl]);
 str.Assign('ProcDecl',iv.entity_type_names[ic.et_procdecl]);
 str.Assign('VarUsage',iv.entity_type_names[ic.et_varusage]);
 str.Assign('ProcCall',iv.entity_type_names[ic.et_proccall]);
 str.Assign('While',iv.entity_type_names[ic.et_while]);
 str.Assign('For',iv.entity_type_names[ic.et_for]);
 str.Assign('Repeat',iv.entity_type_names[ic.et_repeat]);
 str.Assign('Assign',iv.entity_type_names[ic.et_assign]);
 str.Assign('CONDIT',iv.entity_type_names[ic.et_bool_expr]);
 str.Assign('thenbra',iv.entity_type_names[ic.et_then_branch]);
 str.Assign('elsebra',iv.entity_type_names[ic.et_else_branch]);
 str.Assign('IFELSE',iv.entity_type_names[ic.et_if]);
 str.Assign('Type',iv.entity_type_names[ic.et_typedecl]);
 str.Assign('MethCall',iv.entity_type_names[ic.et_methodcall]);
 str.Assign('fielddec',iv.entity_type_names[ic.et_fielddecl]);
 str.Assign('Constdec',iv.entity_type_names[ic.et_constdecl]);
 str.Assign('fieldusg',iv.entity_type_names[ic.et_fieldusage]);
 str.Assign('constusg',iv.entity_type_names[ic.et_constusage]);
 str.Assign('return',iv.entity_type_names[ic.et_return]);
 str.Assign('LoopStmt',iv.entity_type_names[ic.et_loop]);
 str.Assign('ExitStmt',iv.entity_type_names[ic.et_exit]);
 str.Assign('WithStmt',iv.entity_type_names[ic.et_with]);
 str.Assign('Expr',iv.entity_type_names[ic.et_expr]);
 str.Assign('ELSIF',iv.entity_type_names[ic.et_elsif]);
 str.Assign('CASE',iv.entity_type_names[ic.et_case]);
 str.Assign('casebra',iv.entity_type_names[ic.et_case_branch]);
 str.Assign('actparam',iv.entity_type_names[ic.et_actual_parameter]);
 str.Assign('deref',iv.entity_type_names[ic.et_deref]);
 str.Assign('fparam',iv.entity_type_names[ic.et_paramdecl]);
 str.Assign('block',iv.entity_type_names[ic.et_block]);
 str.Assign('import',iv.entity_type_names[ic.et_imported_object]);
 str.Assign('impsec',iv.entity_type_names[ic.et_import_section]);
 str.Assign('sproc',iv.entity_type_names[ic.et_sproc]);
 str.Assign('prochead',iv.entity_type_names[ic.et_proc_header]);
 str.Assign('withopt',iv.entity_type_names[ic.et_with_option]);
 str.Assign('index',iv.entity_type_names[ic.et_index]);
 str.Assign('varsec',iv.entity_type_names[ic.et_varsec]);

 str.Assign('Module',iv.entity_type_ext_names[ic.et_module]);
 str.Assign('Variable declaration',iv.entity_type_ext_names[ic.et_vardecl]);
 str.Assign('Procedure declaration',iv.entity_type_ext_names[ic.et_procdecl]);
 str.Assign('Variable',iv.entity_type_ext_names[ic.et_varusage]);
 str.Assign('Procedure call',iv.entity_type_ext_names[ic.et_proccall]);
 str.Assign('While statement',iv.entity_type_ext_names[ic.et_while]);
 str.Assign('For statement',iv.entity_type_ext_names[ic.et_for]);
 str.Assign('Repeat statement',iv.entity_type_ext_names[ic.et_repeat]);
 str.Assign('Assign statement',iv.entity_type_ext_names[ic.et_assign]);
 str.Assign('Condition',iv.entity_type_ext_names[ic.et_bool_expr]);
 str.Assign('Then branch',iv.entity_type_ext_names[ic.et_then_branch]);
 str.Assign('Else branch',iv.entity_type_ext_names[ic.et_else_branch]);
 str.Assign('If statement',iv.entity_type_ext_names[ic.et_if]);
 str.Assign('Type definition',iv.entity_type_ext_names[ic.et_typedecl]);
 str.Assign('MethodCall',iv.entity_type_ext_names[ic.et_methodcall]);
 str.Assign('Field declaration',iv.entity_type_ext_names[ic.et_fielddecl]);
 str.Assign('Constant declaration',iv.entity_type_ext_names[ic.et_constdecl]);
 str.Assign('Field',iv.entity_type_ext_names[ic.et_fieldusage]);
 str.Assign('Constant usage',iv.entity_type_ext_names[ic.et_constusage]);
 str.Assign('Return',iv.entity_type_ext_names[ic.et_return]);
 str.Assign('Loop statement',iv.entity_type_ext_names[ic.et_loop]);
 str.Assign('Exit statement',iv.entity_type_ext_names[ic.et_exit]);
 str.Assign('With statement',iv.entity_type_ext_names[ic.et_with]);
 str.Assign('Expression',iv.entity_type_ext_names[ic.et_expr]);
 str.Assign('Elsif branch',iv.entity_type_ext_names[ic.et_elsif]);
 str.Assign('Case statement',iv.entity_type_ext_names[ic.et_case]);
 str.Assign('Case branch',iv.entity_type_ext_names[ic.et_case_branch]);
 str.Assign('Actual parameter',iv.entity_type_ext_names[ic.et_actual_parameter]);
 str.Assign('Dereference',iv.entity_type_ext_names[ic.et_deref]);
 str.Assign('Formal parameter',iv.entity_type_ext_names[ic.et_paramdecl]);
 str.Assign('Block',iv.entity_type_ext_names[ic.et_block]);
 str.Assign('Imported object',iv.entity_type_ext_names[ic.et_imported_object]);
 str.Assign('Import section',iv.entity_type_ext_names[ic.et_import_section]);
 str.Assign('Standard procedure call',iv.entity_type_ext_names[ic.et_sproc]);
 str.Assign('Procedure header',iv.entity_type_ext_names[ic.et_proc_header]);
 str.Assign('With option',iv.entity_type_ext_names[ic.et_with_option]);
 str.Assign('Index',iv.entity_type_ext_names[ic.et_index]);
 str.Assign('Variable declaration section',iv.entity_type_ext_names[ic.et_varsec]);


 NEW(as.attr_types,ic.at_count);
 as.attr_types[ic.at_public].domain:=as.ad_boolean;
 str.Assign('public',as.attr_types[ic.at_public].attr_type_name);

 as.attr_types[ic.at_compwarn].domain:=as.ad_string;
 str.Assign('compiler warning',as.attr_types[ic.at_compwarn].attr_type_name);

 as.attr_types[ic.at_name].domain:=as.ad_string;
 str.Assign('object name',as.attr_types[ic.at_name].attr_type_name);


 as.attr_types[ic.at_nest_used].domain:=as.ad_boolean;
 str.Assign('used in nested procedures',as.attr_types[ic.at_nest_used].attr_type_name);

 as.attr_types[ic.at_nest_used].domain:=as.ad_boolean;
 str.Assign('never used',as.attr_types[ic.at_never_used].attr_type_name);

 as.attr_types[ic.at_self_recursive].domain:=as.ad_boolean;
 str.Assign('self recursive procedure',as.attr_types[ic.at_self_recursive].attr_type_name);

 as.attr_types[ic.at_modified].domain:=as.ad_boolean;
 str.Assign('modified',as.attr_types[ic.at_modified].attr_type_name);

 as.attr_types[ic.at_readonly].domain:=as.ad_boolean;
 str.Assign('read only',as.attr_types[ic.at_readonly].attr_type_name);

 as.attr_types[ic.at_struct_param].domain:=as.ad_boolean;
 str.Assign('structured parameter',as.attr_types[ic.at_struct_param].attr_type_name);

 as.attr_types[ic.at_modified_vpar].domain:=as.ad_boolean;
 str.Assign('val parameter is modified',as.attr_types[ic.at_modified_vpar].attr_type_name);


 ir.unreachable:=warn_at_pos;

END model2.
