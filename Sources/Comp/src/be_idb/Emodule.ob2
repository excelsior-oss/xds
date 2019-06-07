MODULE Emodule;
IMPORT
 ent := Entities,
 env := xiEnv,
 xfs := xiFiles,
 str := Strings,
 ls  := lists,
 rs  := Relations,
 as  := Attributes,
 pc  := pcK,
 sf  := SeqFile,
 rf  := RndFile,
 db  := DBAPI,
 xFilePos,
 IOResult,
 TextIO,
 SYSTEM,
 RawIO,
 StrSer,
 StringListHash,
 Dirs,
 DStrings;
CONST
  no_parent*=db.no_parent;

CONST
  max_caption*=79;


TYPE
STRUCT_ENTITY*= ent.STRUCT_ENTITY;
ENTITY*       = ent.ENTITY;
ENTITY_LIST*  = ent.ENTITY_LIST;

EMODULE_STRUCTURE* = POINTER TO EMODULE_STRUCTURE_REC;
EMODULE_STRUCTURE_REC*=RECORD
  entities  : ENTITY_LIST;
  mname-    : xfs.String;
  source-   : xfs.String;
  fid-      : INTEGER;
  main-     : BOOLEAN;
END;

EMODULE*     = POINTER TO EMODULE_REC;
EMODULE_REC* = RECORD(EMODULE_STRUCTURE_REC);
END;

NeedsSavindPredicate* = PROCEDURE(em : EMODULE; e : ENTITY) : BOOLEAN ;

VAR
  obj_names : StringListHash.NameHash_REC;

(*--------------------------EMODULE-------------------------------------*)




PROCEDURE (em: EMODULE)ini*( fid : INTEGER; name, source : xfs.String; main : BOOLEAN);
BEGIN
  em.entities.ini_by_len(env.info.lines*3);
  DStrings.Assign(   name^, em.mname  );
  DStrings.Assign( source^, em.source);
  em.main := main;
  em.fid := fid;
END ini;

PROCEDURE ( em: EMODULE_STRUCTURE )entity_by_id*(e_id:LONGINT):STRUCT_ENTITY;
VAR
  l , r : LONGINT;
  e : STRUCT_ENTITY;
BEGIN
  l := 0;
  r := em.entities.len-1;
  e := em.entities.get_elem( (l+r)DIV 2);
  REPEAT
    IF  e.entity_id < e_id THEN
      l := (l+r+1(*if r-l = 1 then l is increased*))DIV 2
    ELSE
      r := (l+r)DIV 2
    END;
    e := em.entities.get_elem( (l+r)DIV 2);
  UNTIL (e.entity_id = e_id) OR (l=r);
  IF e.entity_id = e_id THEN
    RETURN e;
  ELSE
    RETURN NIL;
  END;
END entity_by_id;


PROCEDURE (em : EMODULE_STRUCTURE) find_entity* (obj_name-:ls.STRING_LIST):STRUCT_ENTITY;
VAR
  i : LONGINT;
  e : STRUCT_ENTITY;
BEGIN
  i := obj_names.get( obj_name );
  IF (i > 0) AND (i < em.entities.len) THEN
    e:=em.entity_by_id(i);
    IF (e <> NIL) AND (ls.compare_string_lists(obj_name,e.ext_obj_name) ) THEN
      RETURN e;
    END;
  END;

  FOR i:=0 TO em.entities.len-1 DO
    e:=em.entities.get_elem(i);
    IF ls.compare_string_lists(obj_name,e.ext_obj_name) THEN
      RETURN e;
    END;
  END;
  RETURN NIL;
END find_entity;

PROCEDURE (em:EMODULE)get_entity*(e_id:LONGINT):ENTITY;
VAR
  se : STRUCT_ENTITY;
BEGIN
  se := em.entities.get_elem(e_id);
  RETURN se(ENTITY);
END get_entity;

PROCEDURE (em:EMODULE)entity_by_node*( n-: pc.NODE) : ENTITY;
VAR
   e : ENTITY;
BEGIN
  IF (n=NIL) THEN RETURN NIL; END;
  e := em.get_entity(n.eno);
  IF e.node=n THEN
    RETURN e;
  END;
  RETURN NIL;
END entity_by_node;

PROCEDURE (em:EMODULE)entity_by_struct*(s-:pc.STRUCT;type:INTEGER):ENTITY;--type=-1 -any type
VAR
  i:LONGINT;
  e:ENTITY;
BEGIN
  FOR i:=0 TO em.entities.len-1 DO
    e:=em.get_entity(i);
    IF ( e.struct=s ) THEN
          IF (e.type=type)OR(type=-1) THEN
               RETURN e;
          END;
    END;
  END;
  RETURN NIL;
END entity_by_struct;

PROCEDURE (em:EMODULE)entity_by_pos*(p-:pc.TPOS;type:INTEGER):ENTITY;--type=-1 -any type
VAR
  i,row,col : LONGINT;
  e         : ENTITY;
  fnam      : xfs.String;
BEGIN
  p.unpack(fnam,row,col);
  FOR i:=0 TO em.entities.len-1 DO
    e:=em.get_entity(i);
    IF ( (e.node#NIL)AND(e.node.pos.equ(p)) ) OR
       ( (e.obj#NIL)AND(e.obj.pos.equ(p)) )   OR
       ( (e.struct#NIL)AND(e.struct.pos.equ(p)) ) THEN
          IF (e.type=type)OR(type=-1) THEN
               RETURN e;
          END;
    END;
  END;
  RETURN NIL;
 END entity_by_pos;

PROCEDURE ( em: EMODULE)entities_by_pos*(p-:pc.TPOS;type:INTEGER) : ENTITY_LIST;--type=-1 -any type
VAR
  i,row,col : LONGINT;
  el        : ENTITY_LIST;
  e         : ENTITY;
  fnam      : xfs.String;
BEGIN
  el.ini_by_len(0);
  p.unpack(fnam,row,col);
  FOR i:=0 TO em.entities.len-1 DO
    e:=em.get_entity(i);
    IF ( (e.node#NIL)AND(e.node.pos.equ(p)) ) OR
       ( (e.obj#NIL)AND(e.obj.pos.equ(p)) )   OR
       ( (e.struct#NIL)AND(e.struct.pos.equ(p)) ) THEN
          IF (e.type=type)OR(type=-1) THEN
               el.add(e);
          END;
    END;
  END;
  RETURN el;
END entities_by_pos;




PROCEDURE ( em: EMODULE_STRUCTURE )get_from_seq*(seq:LONGINT): STRUCT_ENTITY;
BEGIN
  RETURN em.entities.get_from_seq(seq);
END get_from_seq;

PROCEDURE (em:EMODULE)len*():LONGINT;
BEGIN
  RETURN em.entities.len;
END len;


PROCEDURE (em:EMODULE)add_entity*(par:LONGINT;e:ENTITY);
VAR p:ENTITY;
BEGIN
  em.entities.add(e);
  e.entity_id:=em.entities.len-1;
  obj_names.add(e.ext_obj_name, e.entity_id);
  CASE e.origin OF
   |ent.or_node :
     e.node.eno := e.entity_id;
   |ent.or_object :
     e.obj.eno := e.entity_id;
   |ent.or_struct :
     e.struct.eno := e.entity_id;
  END;
  e.parent:=par;
  e.set_fid (em.fid);
  IF (e.parent # db.no_parent) THEN
     p := em.get_entity(par);
     p.children.add( e.entity_id );
  END;
END add_entity;



PROCEDURE (em : EMODULE) calc_final_pos*(top_level_type : INTEGER);
VAR
    i,
    max_row,
    min_row : LONGINT;
    cm, e : ENTITY;

BEGIN
   cm := NIL;
   max_row := -1;
   min_row := MAX(LONGINT);
   FOR i := 0 TO em.entities.len-1 DO
     e:=em.get_entity(i);
     IF e.from_row < min_row THEN
       min_row := e.from_row;
     END;
     IF e.to_row > max_row THEN
       max_row := e.to_row;
     END;
     IF e.type = top_level_type THEN
       cm := e;
     END;
   END;
   IF (cm <> NIL) & (min_row <> MAX(LONGINT)) & (max_row <> -1) THEN
     cm.from_row := min_row;
     cm.to_row := max_row+1;
   END;
END calc_final_pos;

PROCEDURE (em:EMODULE)save_to_file*(NeedsSaving : NeedsSavindPredicate );
VAR
     emf_id : sf.ChanId;
     res    : sf.OpenResults;
     s,dir,a: xfs.String;
     emf_ext: xfs.String;
     ext,nam: xfs.String;
     i, num : LONGINT;(* !num in save_to file and load_from_file must have the same type*)
     e      : ENTITY;
  PROCEDURE set_needs_saving(e: ENTITY): LONGINT;
  VAR
    i, add, res : LONGINT;
  BEGIN
    res := 0;
    e.NeedsSaving := NeedsSaving( em, e );

    FOR i := 0 TO e.children.len -1 DO
      add := set_needs_saving( em.get_entity( e.children.get_elem(i) ) );
      e.NeedsSaving := e.NeedsSaving OR (add > 0);
      INC(res, add);
    END;
    IF e.NeedsSaving THEN INC( res ); END;
    RETURN res;
  END set_needs_saving;
BEGIN

  env.config.Equation('EMFEXT',emf_ext);
  DStrings.Assign(em.mname^, s);
  DStrings.Append( '.' , s);
  DStrings.Append( emf_ext^ , s);
  xfs.sys.UseFirst(s^,s);
  xfs.sys.GetDir( s^, dir);

  IF env.config.Option("MAKEDIRS") THEN
    xfs.sys.ConvertToHost(dir^,a);
    IF Dirs.mkdirs(a) THEN END;
  END;

  sf.OpenWrite(emf_id,s^,sf.write+sf.old+sf.raw,res);
  IF res#sf.opened THEN
    env.info.print('Can`t open file %s to write\n',s^);
    HALT;
  END;
  StrSer.Write_xfs_string(emf_id,em.mname);
  StrSer.Write_xfs_string(emf_id,em.source);
  RawIO.Write(emf_id,em.fid);
  RawIO.Write(emf_id,em.main);
  num := set_needs_saving( em.get_entity(0) );

  RawIO.Write(emf_id, num );

  FOR i := 0 TO em.entities.len-1 DO
    e := em.get_entity(i);
    IF e.NeedsSaving THEN
      e.save_to_file(emf_id);
      DEC(num);
    END;
  END;
  sf.Close(emf_id);
  ASSERT(num = 0);
END save_to_file;

PROCEDURE ( em : EMODULE_STRUCTURE ) load_from_file*(name:xfs.String; entire : BOOLEAN):BOOLEAN;
VAR
  i, num   : LONGINT; (* !num in save_to file and load_from_file must have the same type*)
  emf_id   : sf.ChanId;
  res      : sf.OpenResults;
  s, ext   : xfs.String;
  nam, dir : xfs.String;
  emf_ext  : xfs.String;
  e        : ENTITY;
BEGIN

  env.config.Equation('EMFEXT',emf_ext);
  DStrings.Assign(name^, s);
  DStrings.Append( '.' , s);
  DStrings.Append( emf_ext^ , s);
  xfs.sys.Lookup(s^,s);

  sf.OpenRead(emf_id,s^,sf.read+sf.raw+sf.old,res);
  IF res#sf.opened THEN
    IF res=sf.noSuchFile THEN
       RETURN FALSE;
    ELSE
      env.info.print('Can`t open file %s to read\n',s^);
      HALT;
    END;
  END;
  em.mname:=StrSer.Read_xfs_string(emf_id);
  em.source:=StrSer.Read_xfs_string(emf_id);
  RawIO.Read(emf_id,em.fid);
  RawIO.Read(emf_id,em.main);
  IF ~entire THEN
    sf.Close(emf_id);
    RETURN TRUE;
  END;
  RawIO.Read(emf_id,num);
  em.entities.ini_by_len(num);
  FOR i:=0 TO num-1 DO
    NEW(e);
    e.load_from_file(emf_id);
    em.entities.add( e );
    e.set_fid ( em.fid );
    obj_names.add( e.ext_obj_name, e.entity_id );
  END;
  sf.Close(emf_id);
  RETURN TRUE;
END load_from_file;


PROCEDURE (em:EMODULE)seq_nest*(VAR start_seq: LONGINT);
  PROCEDURE sn(e_id:LONGINT;VAR seq:LONGINT;nest:LONGINT);
  VAR
    e:ENTITY;
    i:LONGINT;
  BEGIN
    e:=em.get_entity(e_id);
    e.seq_no := seq;
    INC(seq);
    e.nest_lev:=nest;
    FOR i:=0 TO e.children.len-1 DO
      sn(e.children.get_elem(i),seq,nest+1);
    END;
  END sn;
BEGIN
  sn(0, start_seq ,0);
END seq_nest;



PROCEDURE (em:EMODULE)sort_children*;

  PROCEDURE is_successor(e1,e2:ENTITY):BOOLEAN;(*e2<e1,
		it is implied that entities are not  enclosed one in another *)
  BEGIN
     IF e1.from_row#e2.from_row THEN RETURN e1.from_row>e2.from_row;
     ELSE RETURN e1.from_col>e2.from_col;END;
  END is_successor;

 PROCEDURE sc(n:LONGINT);
   VAR e:ENTITY;
       i,j,tmp:LONGINT;
   BEGIN
     e:=em.get_entity(n);
     FOR i:=1 TO e.children.len-1 DO
      	    FOR j:=0 TO e.children.len-1-i DO
          	  IF is_successor(em.get_entity( e.children.get_elem(j)   ),
  				              em.get_entity( e.children.get_elem(j+1) ) ) THEN
                tmp:=e.children.get_elem(j+1);
     		    e.children.set_elem ( j+1, e.children.get_elem(j) );
		        e.children.set_elem ( j, tmp);
              END;
	       END;
     END;
     FOR i:=0 TO e.children.len-1 DO
       sc(e.children.get_elem(i));
     END;
   END sc;
 BEGIN
  sc(0);
END sort_children;



PROCEDURE (em:EMODULE)calculate_sel*;
VAR
  lines : ls.LONGINT_LIST;
  i     : LONGINT;
  e     : ENTITY;
  cid   : rf.ChanId;

  PROCEDURE SetEntityCaption(e : ENTITY);
  VAR c, i : LONGINT;
      pos : xFilePos.FilePos;
  BEGIN
    IF e.caption <> NIL THEN RETURN END;
    IF e.from_row <> e.to_row THEN
      c := max_caption;
    ELSE
      c := e.to_col-e.from_col + 1;
      IF c = 0 THEN c := 1; END;
      IF c > max_caption THEN
        c := max_caption;
      END;
    END;
    NEW(e.caption,c);
    xFilePos.CardToPos(pos,e.from_sel);
    rf.SetPos(cid,pos);
    RawIO.Read(cid,e.caption^);
    FOR i := 0 TO LEN(e.caption^) - 1 DO
      CASE e.caption[i] OF
      | 15C, 12C:
        e.caption[i] := 0C;
        RETURN;
      | 11C:
        e.caption[i] := ' ';
      ELSE
      END;

    END;
  END SetEntityCaption;

  PROCEDURE create_lines_table(fnam:xfs.String;VAR cid : rf.ChanId) : ls.LONGINT_LIST;
  VAR
    l       : SYSTEM.CARD32;
    lines   : ls.LONGINT_LIST;
    res     : rf.OpenResults;
  BEGIN
    lines.ini_by_len(env.info.lines + 10);
    rf.OpenOld(cid,fnam^,rf.text,res);
    ASSERT(res=rf.opened);
    l := 0;
    WHILE IOResult.ReadResult(cid)# IOResult.endOfInput DO
      ASSERT( xFilePos.PosToCard(l, rf.CurrentPos(cid) ) ) ;
      lines.add(l);
      TextIO.SkipLine(cid);
    END;
    rf.Close(cid);
    rf.OpenOld(cid,fnam^,rf.raw,res);
    RETURN lines;
  END create_lines_table;

  PROCEDURE set_sel(e:ENTITY; lines: ls.LONGINT_LIST);
  BEGIN
    e.from_sel:=lines.get_elem( e.from_row ) + e.from_col;
    e.to_sel:=lines.get_elem(e.to_row)+e.to_col;
  END set_sel;

BEGIN
   lines:=create_lines_table(em.source, cid);
   FOR i:=0 TO em.entities.len-1 DO
     e:=em.get_entity(i);
     set_sel(e, lines);
     SetEntityCaption(e);
   END;
   rf.Close(cid);
   lines.exi();
END calculate_sel;

PROCEDURE (em:EMODULE)SetOwners*( s : SET );
  PROCEDURE set(cur_owner : ENTITY; e : ENTITY );
  VAR i : INTEGER;
  BEGIN
    IF cur_owner <> NIL THEN
      e.owner := cur_owner.entity_id;
    ELSE
      e.owner := no_parent;
    END;
    IF e.type IN s THEN
      FOR i := 0 TO e.children.len-1 DO
        set(e, em.get_entity( e.children.get_elem(i) ) );
      END;
    ELSE
      FOR i := 0 TO e.children.len-1 DO
        set(cur_owner , em.get_entity( e.children.get_elem(i) ) );
      END;
    END;
  END set;
BEGIN
  IF em.entities.len > 0 THEN
    set(NIL, em.get_entity(0) );
  END;
END SetOwners;


PROCEDURE (em:EMODULE)db_write*;
VAR
  file_name,
  dir,name,
  ext         : xfs.String;

  starts   : ls.LONGINT_LIST;
  lines    : ls.LONGINT_LIST;
  cur_line : LONGINT;
  i        : LONGINT;
  pos_table: POINTER TO ARRAY OF LONGINT;
  index_fname :  xfs.String;
  cid      : sf.ChanId;
  res      : sf.OpenResults;

  PROCEDURE db_wr(e:ENTITY);
  VAR j:LONGINT;
      p : ENTITY;
      pSeqNo : LONGINT;
      oSeqNo : LONGINT;
      r : rs.RELATION_REC;
      at : as.ATTR_REC;
      pos: rf.FilePos;
      card_pos : SYSTEM.CARD32;
  BEGIN
     IF e.parent = no_parent THEN
       pSeqNo := no_parent;
     ELSE
       p := em.get_entity(e.parent);
       pSeqNo := p.seq_no;
     END;

     IF e.owner = no_parent THEN
       oSeqNo := no_parent;
     ELSE
       p := em.get_entity(e.owner);
       oSeqNo := p.seq_no;
     END;

      db.write_entity(em.fid,
			e.type,
			e.seq_no,
                        e.nest_lev,
			e.from_row,
			e.from_col,
			e.to_row,
 			e.to_col,
 			pSeqNo,
 			oSeqNo,			
			e.from_sel,
			e.to_sel,
                        e.children.len>0,
			e.caption,
			pos );


       IF e.from_row > cur_line THEN
         cur_line := e.from_row;
	 lines.add( e.from_row );
         ASSERT( xFilePos.PosToCard(card_pos, pos ) ) ;
	 starts.add( SYSTEM.VAL( LONGINT, card_pos ) );
       END;		

       FOR j:=0 TO e.attrs.len-1 DO
           at:=e.attrs.a[ j ]; --fixme (linker bug)
   	     IF at.is_boolean() THEN
	       db.write_iva_bool(at.type,em.fid,e.seq_no,at.get_bool_val());
	     ELSIF at.is_int() THEN
	       db.write_iva_int(at.type,em.fid,e.seq_no,at.get_int_val());	
	     ELSIF at.is_string() THEN
	       db.write_iva_str(at.type,em.fid,e.seq_no,at.get_str_val());		
         ELSE
           ASSERT(FALSE,601);
         END;
       END;--for

      FOR j:=0 TO e.rels.len-1 DO
        r:=e.rels.a[ j ];
        db.write_relation(r.rel_type,
			 em.fid,
			 e.seq_no,
			 r.file_id,
 	  	         r.seq_no);
      END;

      FOR j:=0 TO e.children.len-1 DO
        db_wr(em.get_entity( e.children.get_elem(j) ) );
      END;
 END db_wr;
BEGIN
   DStrings.Assign(em.mname^, file_name);

   IF NOT (db.open_unique_tables(file_name)) THEN
     env.info.print('Fatal error : can`t open tables for %s\n',file_name^);
     HALT ;
   END;
   cur_line := -1;
   starts.ini_by_len(3000);
   lines.ini_by_len(3000);

   db_wr(em.get_entity(0));

   ASSERT(lines.len = starts.len);

   IF lines.len > 0 THEN
     NEW(pos_table, lines.get_elem( lines.len-1 ) +1 );
   END;
   FOR i := 0 TO LEN(pos_table^)-1 DO
     pos_table[i] := -1;
   END;

   FOR i := 0 TO lines.len -1 DO
     pos_table[ lines.get_elem(i) ] := starts.get_elem(i);
   END;

   index_fname := db.get_full_txt_dir();
   DStrings.Append('\' , index_fname);
   DStrings.Append(em.mname^ , index_fname);
   DStrings.Append('.IVS.txt.idx' , index_fname);
   sf.OpenWrite(cid, index_fname^,  sf.write+sf.old+sf.raw,res);
   IF res#sf.opened THEN
     env.info.print('Can`t open file %s to write\n');
     HALT;
   END;
   RawIO.Write(cid, LEN(pos_table^) );
   IF lines.len > 0 THEN
     RawIO.Write(cid, pos_table^ );
   END;
   sf.Close(cid);
   db.close_unique_tables;

END db_write;

END Emodule.