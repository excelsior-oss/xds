<* PROCINLINE+ *>
MODULE DBAPI;

IMPORT
  Printf,
  cc:=ChanConsts,
  rf:=RndFile,
  Strings,
  SYSTEM,
  xfs:=xiFiles,
  Dirs,
  DStrings,
  FileSys,
  env := xiEnv;

TYPE
 Str8*=ARRAY 8 OF CHAR;
 Str32*=ARRAY 32 OF CHAR;
 Str250*=ARRAY 250 OF CHAR;
CONST
 db_INVALID*=0;
 db_CSF*=1;

 no_parent*=-1;
VAR
  delimiter: CHAR;
  ivs,ivr,eti,rni,ivaInt,ivaStr,ivaBool,AttrType:rf.ChanId;
  format-:INTEGER;

PROCEDURE ToCard(n:LONGINT) : SYSTEM.CARD32;
BEGIN
  RETURN SYSTEM.VAL(SYSTEM.CARD32,n);
END ToCard;

PROCEDURE ParentToStr(p_id : LONGINT; VAR s : ARRAY OF CHAR);
BEGIN
  IF p_id = no_parent THEN
    COPY('',s);
  ELSE
    Printf.sprintf(s,"%d",p_id);
  END;
END ParentToStr;

PROCEDURE BoolToYN(b : BOOLEAN) : CHAR;
BEGIN
  IF b THEN RETURN 'y'
  ELSE RETURN 'n'
  END;
END BoolToYN;


PROCEDURE get_full_txt_dir*():xfs.String;--full name of directory where *.txt files are stored;
VAR
  dir,fnam,s:xfs.String;
BEGIN
  xfs.sys.UseFirst('a.txt',s);
  NEW(fnam,300);

  xfs.sys.GetDir(s^,dir);
  IF (dir # NIL) AND (dir[0] # 0X) THEN;
    xfs.sys.GetFullPathName(dir^,fnam^);
    RETURN fnam;
  END;
  xfs.sys.GetFullPathName('.',fnam^);
  RETURN fnam;
END get_full_txt_dir;


PROCEDURE open_common_tables*(pname : xfs.String):BOOLEAN;
(*
 Opens tables, corresponding to ALL modules (AttrType,EntTypeIdx,RelNameIdx.txt)
*)
VAR
  name:xfs.String;
  res:rf.OpenResults;
BEGIN
  xfs.sys.UseFirst('AttrType.txt', name);
  rf.OpenClean(AttrType,name^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',name^);
    RETURN FALSE;
  END;

  xfs.sys.UseFirst('EntTypeIdx.txt',name);
  rf.OpenClean(eti,name^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',name^);
    RETURN FALSE;
  END;
  xfs.sys.UseFirst('RelNameIdx.txt',name);
  rf.OpenClean(rni,name^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',name^);
    RETURN FALSE;
  END;
  RETURN TRUE;
 END open_common_tables;

PROCEDURE delete_common_tables*();
VAR
  buf  :xfs.String;
  done :BOOLEAN;
BEGIN
  xfs.sys.UseFirst('AttrType.txt',buf);
  FileSys.Remove(buf^,done);

  xfs.sys.UseFirst('EntTypeIdx.txt',buf);
  FileSys.Remove(buf^,done);

  xfs.sys.UseFirst('RelNameIdx.txt',buf);
  FileSys.Remove(buf^,done);
END delete_common_tables;


PROCEDURE common_tables_exist*():BOOLEAN;
(*
   checks whether ALL common tables  exist.
*)
VAR
 buf : xfs.String;
BEGIN
  xfs.sys.UseFirst('AttrType.txt',buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;

  xfs.sys.UseFirst('EntTypeIdx.txt',buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;

  xfs.sys.UseFirst('RelNameIdx.txt',buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;

  RETURN TRUE;
END common_tables_exist;

PROCEDURE open_unique_tables*(name:xfs.String):BOOLEAN;
(*
   opens iva and ivs tables for module specified by name (name implies name+'.'+extension).
   Table's name contains module's name + '.ivs.txt' extension .
*)
VAR
  buf, dir, a : xfs.String;
  res         : rf.OpenResults;

BEGIN
  NEW(buf,255);
  Strings.Concat(name^,'.IVS.txt',buf^);
  xfs.sys.UseFirst(buf^,buf);
  xfs.sys.GetDir( buf^, dir);

  IF env.config.Option("MAKEDIRS") THEN
    xfs.sys.ConvertToHost(dir^,a);
    IF Dirs.mkdirs(a) THEN END;
  END;

  rf.OpenClean(ivs,buf^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',name^);
    RETURN FALSE;
  END;

  NEW(buf,255);
  Strings.Concat(name^,'.IVR.txt',buf^);
  xfs.sys.UseFirst(buf^,buf);
  rf.OpenClean(ivr,buf^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',name^);
    RETURN FALSE;
  END;

  NEW(buf,255);
  Strings.Concat(name^,'.IVAInt.txt',buf^);
  xfs.sys.UseFirst(buf^,buf);
  rf.OpenClean(ivaInt,buf^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',buf^);
    RETURN FALSE;
  END;
  NEW(buf,255);
  Strings.Concat(name^,'.IVAStr.txt',buf^);
  xfs.sys.UseFirst(buf^,buf);
  rf.OpenClean(ivaStr,buf^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',buf^);
    RETURN FALSE;
  END;
  NEW(buf,255);
  Strings.Concat(name^,'.IVABool.txt',buf^);
  xfs.sys.UseFirst(buf^,buf);
  rf.OpenClean(ivaBool,buf^,rf.write+rf.old+rf.text,res);
  IF res # cc.opened THEN
    Printf.printf('Can`t open %s\n',buf^);
    RETURN FALSE;
  END;
  RETURN TRUE;
END open_unique_tables;


PROCEDURE delete_unique_tables*(name:xfs.String);
(*
   deletes all iva, ivr and ivs tables for module specified by name.
*)
VAR
  buf,fnam :xfs.String;
  done :BOOLEAN;
BEGIN
  NEW(fnam,255);
  Strings.Concat(name^,'.IVS.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  FileSys.Remove(buf^,done);
  Strings.Concat(name^,'.IVR.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  FileSys.Remove(buf^,done);
  Strings.Concat(name^,'.IVAInt.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  FileSys.Remove(buf^,done);
  Strings.Concat(name^,'.IVAStr.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  FileSys.Remove(buf^,done);
  Strings.Concat(name^,'.IVABool.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  FileSys.Remove(buf^,done);
END delete_unique_tables;


PROCEDURE unique_tables_exist*(name:xfs.String):BOOLEAN;
(*
   checks whether ALL unique tables for file 'name' exist.
*)
VAR
  buf,fnam :xfs.String;

BEGIN
  NEW(fnam,255);
  Strings.Concat(name^,'.IVS.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;
  Strings.Concat(name^,'.IVAInt.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;
  Strings.Concat(name^,'.IVAStr.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;
  Strings.Concat(name^,'.IVABool.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;
  Strings.Concat(name^,'.IVR.txt',fnam^);
  xfs.sys.UseFirst(fnam^,buf);
  IF NOT ( FileSys.Exists(buf^)) THEN RETURN FALSE; END;
  RETURN TRUE;
END unique_tables_exist;


PROCEDURE close_unique_tables*;
BEGIN
    rf.Close(ivs);
    rf.Close(ivr);
    rf.Close(ivaInt);
    rf.Close(ivaStr);
    rf.Close(ivaBool);
END close_unique_tables;


PROCEDURE write_entity*(file_id:LONGINT;
			entity_type:LONGINT;
			seq_no   : LONGINT;
                        nest_lev : LONGINT;
			from_row : LONGINT;
			from_col : LONGINT;
			to_row   : LONGINT;
 			to_col   : LONGINT;
 			parent   : LONGINT;
 			owner    : LONGINT;			
			from_sel : LONGINT;
			to_sel   : LONGINT;
                        has_children:BOOLEAN;
			caption  : xfs.String;
			VAR start_pos : rf.FilePos);
VAR s,s1,s2: Str250;
BEGIN
  start_pos := rf.CurrentPos( ivs );
  IF caption=NIL THEN
    Strings.Assign("",s); --FIXME, illegal case
  ELSE
    Strings.Assign(caption^,s); --truncate
  END;
  ParentToStr(parent, s1);
  ParentToStr(owner , s2);
<* IF SL4IV THEN *>
  from_col := 0;
  to_col := 0;
<* END *>

  Printf.fprintf(
    ivs,
    "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%s%c%s%c%s%c%s%c%s%s%s\n",
    ToCard(file_id),
    delimiter,
    ToCard(seq_no),
    delimiter,
    ToCard(nest_lev),
    delimiter,
    ToCard(entity_type),
    delimiter,
    ToCard(from_row),
    delimiter,
    ToCard(from_col),
    delimiter,
    ToCard(to_row),
    delimiter,
    ToCard(to_col),
    delimiter,
    ToCard(from_sel),
    delimiter,
    ToCard(to_sel),
    delimiter,
    s1, (* Parent *)
    delimiter,
    s2, (* Owner *)
    delimiter,
    '"',BoolToYN(has_children), '"',
    delimiter,
    '"',s, '"');
END write_entity;

PROCEDURE write_EntTypeIdx*(ent_type:Str8;ent_name:Str32;show : BOOLEAN; id:INTEGER);
BEGIN
  Printf.fprintf(
    eti,
    "%s%s%s%c%d%c%s%s%s%c%s%c%s\n",
    '"',ent_type,'"',
    delimiter,
    ToCard(id),
    delimiter,
    '"',ent_name, '"',
    delimiter,
    '"',BoolToYN(show),'"');
END write_EntTypeIdx;

PROCEDURE write_RelNameIdx*(rel_name:Str8;rel_left_name,rel_right_name:Str32;id:INTEGER);
BEGIN
  Printf.fprintf(
    rni,
    "%s%s%s%c%d%c%s%s%s%c%s%s%s\n",
    '"',rel_name,'"',
    delimiter,
    ToCard(id),
    delimiter,
    '"',rel_left_name,'"',
    delimiter,
    '"',rel_right_name,'"');
END write_RelNameIdx;

PROCEDURE write_relation*(rel_type:LONGINT;
			  f_id1:INTEGER;e_id1:LONGINT;
                   	  f_id2:INTEGER;e_id2:LONGINT);
BEGIN
  Printf.fprintf(
    ivr,
    "%d%c%d%c%d%c%d%c%d\n",
    ToCard(rel_type),
    delimiter,
    ToCard(f_id1),
    delimiter,
    ToCard(e_id1),
    delimiter,
    ToCard(f_id2),
    delimiter,
    ToCard(e_id2));
END write_relation;

PROCEDURE write_iva_int*(type: INTEGER;f_id:INTEGER;e_id:LONGINT;val:LONGINT);
BEGIN
  Printf.fprintf(
    ivaInt,
    "%d%c%d%c%d%c%d\n",
    type,
    delimiter,
    ToCard(f_id),
    delimiter,
    ToCard(e_id),
    delimiter,
    ToCard(val));
END write_iva_int;

PROCEDURE write_iva_str*(type: INTEGER;f_id:INTEGER;e_id:LONGINT;val:xfs.String);
BEGIN
  Printf.fprintf(
    ivaStr,
    "%d%c%d%c%d%c%s%s%s\n",
    type,
    delimiter,
    ToCard(f_id),
    delimiter,
    ToCard(e_id),
    delimiter,
    '"',val^,'"');
END write_iva_str;

PROCEDURE write_iva_bool*(type: INTEGER;f_id:INTEGER;e_id:LONGINT;val:BOOLEAN);
BEGIN
  Printf.fprintf(
    ivaBool,
    "%d%c%d%c%d%c%s%c%s\n",
    type,
    delimiter,
    ToCard(f_id),
    delimiter,
    ToCard(e_id),
    delimiter,
    '"',BoolToYN(val), '"');
END write_iva_bool;

PROCEDURE write_attr_type*(inx: INTEGER; type-:ARRAY  OF CHAR; val:INTEGER);
VAR
  s32:ARRAY 32 OF CHAR;
BEGIN
  Strings.Assign(type,s32);--extra symbols will be truncated;
  Printf.fprintf(
    AttrType,
    "%d%c%d%c%s%s%s\n",
    inx,
    delimiter,
    val,
    delimiter,
    '"',s32,'"');
END write_attr_type;

PROCEDURE close_common_tables*;
BEGIN
  rf.Close(AttrType);
  rf.Close(eti);
  rf.Close(rni);
END close_common_tables;

BEGIN
 format:=db_INVALID;
 delimiter:=';';
END DBAPI.
