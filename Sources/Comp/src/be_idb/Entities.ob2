MODULE Entities;
IMPORT
  ls := lists,
  rs := Relations,
  sf := SeqFile,
  as := Attributes,
  pc := pcK,
  RawIO,
  DStrings,
  StrSer;

CONST
(* origin constants for field "origin" in ENTITY"*)
 or_nothing* = 0;
 or_node*    = 1;
 or_object*  = 2;
 or_struct*  = 3;


TYPE

STRUCT_ENTITY*= POINTER TO STRUCT_ENTITY_REC;
STRUCT_ENTITY_REC*=RECORD
  entity_id*   : LONGINT; (*unique  id for entities from the ssame EMODULE, entity_id is index in EMODULE.entities*)
  type*        : INTEGER; (* see ivConst.et_* *)
  parent*      : LONGINT;(* index in entity_list*)
  file_id      : INTEGER;(* is set in EMODULE.add and in EMODULE.load_from_file*)

  seq_no*      : LONGINT; -- / Thns fields are set in EMODULE.seq_nest
  nest_lev*    : LONGINT; -- \

  ext_obj_name*:ls.STRING_LIST;
END;


ENTITY*	      = POINTER TO ENTITY_REC;
ENTITY_REC*=RECORD(STRUCT_ENTITY_REC)

  from_row*   : LONGINT;
  from_col*   : LONGINT;
  to_row*     : LONGINT;
  to_col*     : LONGINT;

  children*   : ls.LONGINT_LIST;    (**)
  rels*       : rs.REL_LIST;    (*Relations of  the rel_type(this_entity,any_entity) kind
                              except child relation are stored here*)	
  attrs*      : as.ATTR_LIST;

  obj*        : pc.OBJECT;--this four fields are not stored in emf file and are valid for
  node*       : pc.NODE;  --current module
  struct*     : pc.STRUCT;--only
  origin*     : SHORTINT; --shows whether entity was created from NODE or from OBJECT
                        --or from STRUCT or from something else (see or_ consts)

  from_sel*   : LONGINT;
  to_sel*     : LONGINT;
  caption*    : DStrings.String;
  NeedsSaving*: BOOLEAN; (**Shows that entity must be written to emf-file
                            (this field is set in EMODULE.NeedsSaving()) *)
  owner*      : LONGINT;(* index in entity_list*)			
END;

ENTITY_ARRAY=POINTER TO ARRAY OF STRUCT_ENTITY;

ENTITY_LIST*=RECORD
  a :ENTITY_ARRAY;
  len-:LONGINT; (*Number of stored elements, NOT the length of a*)
END;

(*-------------------------ENTITY  --- - --------- ---------------- ---*)

PROCEDURE (e:STRUCT_ENTITY)save_to_file*(cid:sf.ChanId);
BEGIN
  RawIO.Write(cid,e.entity_id);
  RawIO.Write(cid,e.type);
  RawIO.Write(cid,e.parent);
  RawIO.Write(cid,e.seq_no);
  RawIO.Write(cid,e.nest_lev);
  e.ext_obj_name.save_to_file(cid);
END save_to_file;

PROCEDURE (e:STRUCT_ENTITY)load_from_file*(cid:sf.ChanId);
BEGIN
  RawIO.Read(cid,e.entity_id);
  RawIO.Read(cid,e.type);
  RawIO.Read(cid,e.parent);
  RawIO.Read(cid,e.seq_no);
  RawIO.Read(cid,e.nest_lev);
  e.ext_obj_name.load_from_file(cid);
END load_from_file;

PROCEDURE (e:STRUCT_ENTITY)set_fid*(fid : INTEGER);
BEGIN
  e.file_id := fid;
END set_fid;

PROCEDURE (e:ENTITY)add_rel*(type : INTEGER; toe : STRUCT_ENTITY);
VAR
  r : rs.RELATION_REC;
BEGIN
  IF toe = NIL THEN RETURN ; END;
  r.rel_type := type;
  r.file_id  := toe.file_id;
  r.entity_id:= toe.entity_id;
  r.seq_no   := toe.seq_no;
  e.rels.add(r);
END add_rel;


PROCEDURE (e : ENTITY) get_end_pos*(pos : pc.TPOS );
VAR
  fnam : DStrings.String;
BEGIN
 ASSERT(~pos.IsNull());
 pos.unpack(fnam,e.to_row,e.to_col);
 IF ((e.from_row # e.to_row) OR (e.from_col # e.to_col)) AND (e.to_col > 0) THEN
   DEC(e.to_col);
 END;
END get_end_pos;

PROCEDURE (e : ENTITY) get_pos*(pos : pc.TPOS);
VAR
  fnam : DStrings.String;
BEGIN
 ASSERT(~pos.IsNull());
 pos.unpack(fnam,e.from_row,e.from_col);
END get_pos;

(*--------------------------ENTITY LIST-------------------------------------*)

PROCEDURE (VAR el: ENTITY_LIST)ini_by_len*(len : LONGINT);
BEGIN
  el.len:=0;
  IF len=0 THEN
    el.a := NIL;
  ELSE
    NEW(el.a,len);
  END;
END ini_by_len;

PROCEDURE (VAR el: ENTITY_LIST)add*(E:STRUCT_ENTITY);
VAR
  t:ENTITY_ARRAY;
  i:LONGINT;
BEGIN 		
  IF el.a=NIL THEN NEW(el.a,300);END;	
  IF LEN(el.a^)=el.len THEN
      NEW(t,300+LEN(el.a^));
      FOR i:=0 TO LEN(el.a^)-1 DO t[i]:=el.a[i]; END;
      el.a:=t;
  END;
  el.a[el.len]:=E;
  INC(el.len);
END add;
						
(* Element from seq_no and file_id *)
PROCEDURE (VAR el: ENTITY_LIST)get_from_seq*( seq:LONGINT): STRUCT_ENTITY;
VAR
  i : INTEGER;
BEGIN
  FOR i := 0 TO el.len-1 DO
    IF el.a[ i ].seq_no = seq THEN RETURN el.a[i]; END;
  END;
  RETURN NIL;
END get_from_seq;


(* Element from entity_id *)
PROCEDURE (VAR el: ENTITY_LIST)get_elem*(n:LONGINT): STRUCT_ENTITY;
BEGIN
  RETURN  el.a[n];
END get_elem;

PROCEDURE (VAR el: ENTITY_LIST)save_to_file*(cid:sf.ChanId);
VAR
  i : LONGINT;
BEGIN
  RawIO.Write(cid, el.len);
  FOR i:=0 TO el.len-1 DO
    el.a[i].save_to_file(cid);
  END;
END save_to_file;

PROCEDURE (VAR el: ENTITY_LIST)load_from_file*(cid:sf.ChanId);
VAR
  i,len  :LONGINT;(*ATTENTION len and el.len must have the same type*)
BEGIN
  RawIO.Read(cid,len);
  el.ini_by_len(len);
  FOR i:=0 TO len-1 DO
    NEW( el.a[i] );
    el.a[i].load_from_file(cid);
  END;
  el.len := len;
END load_from_file;


END Entities.