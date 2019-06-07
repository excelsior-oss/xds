(*This module is model-independant*)
MODULE IVERAS ;(*InterView Entity, Relation and Attribute Search*)
IMPORT
  pc := pcK,
  ent:= Entities,
  env:= xiEnv,
  ObjNames,
  ls :=lists,
  as := Attributes,
  Emodule,
  db := DBAPI,
  DStrings,
  pcO;
TYPE
  ENTITY*  = ent.ENTITY;
  STRUCT_ENTITY*  = ent.STRUCT_ENTITY;
  EMODULE* = Emodule.EMODULE;
  EMODULE_STRUCTURE* = Emodule.EMODULE_STRUCTURE;
  EMODULEs*= POINTER TO ARRAY (* <*INDEXBY="pc.Mno"*> *)OF EMODULE_STRUCTURE;

CONST
  (* !!! tmark_passed value depends on tmark set of values
     if later is changed (in pcK) then
     tmark_passed value should be changed too
     !!!
  *)
  tmark_passed *= pc.tmark_aux16;(* this type mark means that ENTITY was generated for this STRUCT*)
                     (* it is set in establish_typedecl*)



VAR

 FidStart* : INTEGER; (** File Identifiers starts from FidStart *)

PROCEDURE get_file_id*(pos:env.TPOS):INTEGER;
BEGIN
  RETURN pos.get_fid() + FidStart;
END get_file_id;



PROCEDURE create_from_node*(n:pc.NODE):ENTITY;
VAR
  e:ENTITY;
BEGIN
  IF (n=NIL) OR n.pos.IsNull() THEN RETURN NIL END;
  NEW(e);
  e.children.ini_by_len(0);
  e.rels.ini_by_len(0);
  e.attrs.ini_by_len(0);
  e.ext_obj_name.ini_by_len(0);
  e.get_pos(n.pos);
  IF  n.end.IsNull() THEN
    e.get_end_pos(n.pos);
  ELSE
    e.get_end_pos(n.end);
  END;
  e.node:=n;
  e.obj:=n.obj;
  e.struct:=n.type;
  e.origin := ent.or_node;
  e.caption := NIL;
  IF ( (e.from_row = e.to_row) AND (e.from_col > e.to_col) )OR (e.from_row > e.to_row)  THEN
     e.to_row := e.from_row;
     e.to_col := e.from_col;
  END;
--  ASSERT( ( (e.from_row = e.to_row) AND (e.from_col <= e.to_col) )OR (e.from_row < e.to_row) );
  RETURN e;
END create_from_node;

PROCEDURE create_from_object*(o:pc.OBJECT):ENTITY;
VAR
  e : ENTITY;
BEGIN
  IF (o=NIL) THEN RETURN NIL END;
  ASSERT(~o.pos.IsNull());
  NEW(e);
  e.children.ini_by_len(0);
  e.rels.ini_by_len(0);
  e.attrs.ini_by_len(0);
  e.ext_obj_name.ini_by_len(0);
  e.get_pos(o.pos);
  e.get_end_pos(o.end);
  e.node   :=NIL;
  e.obj    := o;
  e.struct := o.type;
  e.origin := ent.or_object;
  e.caption:= NIL;
  RETURN e;
END create_from_object;

PROCEDURE create_from_struct*(s : pc.STRUCT):ENTITY;
VAR
  e : ENTITY;
  start_pos, end_pos : env.TPOS;
BEGIN
  IF (s.mno#pc.ZEROMno) OR (pc.ttag_std_type IN s.tags) OR
    ((s.obj # NIL) AND (pc.otag_secretvar IN s.obj.tags))
  THEN
    RETURN NIL;
  END;
  IF s.mode IN pc.TY_SET{pc.ty_SS, pc.ty_enum} THEN--FIXME (STRUCT.end contains wrong data - bug  of fe)
    RETURN NIL;
  END;

  start_pos := s.pos;
  end_pos   := s.end;
  IF ( start_pos.IsNull() OR end_pos.IsNull() ) AND (s.obj # NIL) THEN
    start_pos := s.obj.pos;
    end_pos   := s.obj.end;
  END;

  IF start_pos.IsNull()  THEN RETURN NIL END;

  INCL(s.marks,tmark_passed);
  NEW(e);
  e.children.ini_by_len(0);
  e.rels.ini_by_len(0);
  e.ext_obj_name.ini_by_len(0);
  e.get_pos(start_pos);
  e.get_end_pos(end_pos);
  e.obj:=s.obj;
  e.node:=NIL;
  e.struct:=s;
  e.origin := ent.or_struct;
  RETURN e;
END create_from_struct;





(*The field ext_obj_name in ENTITY_REC is implied to be unique.
So it is used not in all entities which refer to objects, but in
those ones which really respond to objects*)


PROCEDURE get_ext_name*(o:pc.OBJECT): ls.STRING_LIST;
VAR
  sl : ls.STRING_LIST;
BEGIN
  sl.ini_by_len(0);
  IF (o.mode=pc.ob_module) OR (o.host=NIL) THEN
    sl.add(o.name);
  ELSE
    sl:=get_ext_name(o.host.obj);
    sl.add(o.name)
  END;
  RETURN sl;
END get_ext_name;


PROCEDURE IsNamedType*(s : pc.STRUCT):BOOLEAN;
BEGIN
  IF s = NIL THEN RETURN FALSE ; END;
  IF s.obj = NIL THEN RETURN FALSE ; END;
  IF  ~ObjNames.valid_name(s.obj.name) THEN RETURN FALSE ; END;
  RETURN TRUE;
END IsNamedType;

VAR
 mods*                        : EMODULEs;
 def_emf*                     : EMODULE_STRUCTURE; (* used when *.mod file is processed to build relationship
                                          between *.def and *.mod (see rt_DUR, rt_HOSTVAR, rt_TYPE_OF OR rt_TYPEDECL)*)

 entity_type_names*           : POINTER TO ARRAY OF db.Str8;
 entity_type_ext_names*       : POINTER TO ARRAY OF db.Str32;
 entity_type_show*            : POINTER TO ARRAY OF BOOLEAN;

 rel_type_names*              : POINTER TO ARRAY OF db.Str8;
 rel_type_ext_left_names*     : POINTER TO ARRAY OF db.Str32;
 rel_type_ext_right_names*    : POINTER TO ARRAY OF db.Str32;

(*************************************************************************)



(*--------------------------------------------------------------------------*)

BEGIN
  entity_type_names := NIL;
  rel_type_names := NIL;
  mods := NIL;
  FidStart := 0;
END IVERAS.
 