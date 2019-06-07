MODULE IVProject;
IMPORT
  Windows,
  DStrings,
  str := Strings,
  SYSTEM,
  ProgEnv,
  xfs := xiFiles,
  Printf,
  WholeStr,
  db := DBAPI;


VAR
  full_ivp_file_name : DStrings.String;

PROCEDURE relate_file_name*(base-, full_name-:ARRAY OF CHAR; VAR rel_name :ARRAY OF CHAR);
VAR
  common,i : INTEGER;
BEGIN
  i := 0;
  common := 0;
  str.Assign('', rel_name);

  WHILE (i < SYSTEM.VAL( INTEGER, str.Length(base) ))
     AND(i < SYSTEM.VAL( INTEGER, str.Length(full_name) ))
     AND(base[i] = full_name[ i ])  DO
    INC(i);

    IF (base[i] = '/') OR (base[i] = '\') THEN
      common := i;
    END;
  END;

  IF common # 0 THEN

    FOR i := common + 1 TO SYSTEM.VAL( INTEGER, str.Length(base) )-1 DO
      IF (base[i] = '\') OR (base[i] = '/') THEN
        str.Append('..\',rel_name);
      END;
    END;

    FOR i := common + 1 TO SYSTEM.VAL( INTEGER, str.Length(full_name) )-1 DO
      str.Append(full_name[i],rel_name);
    END;

  ELSE
    str.Assign(full_name, rel_name);
  END;

END relate_file_name;

PROCEDURE Create_IV_Project*(name:DStrings.String);
VAR
  s        : DStrings.String;
  rel_s    : DStrings.String;
  ProgName : DStrings.String;
  s1       : DStrings.String;
  i        : INTEGER;
BEGIN
 NEW(s,str.Length(name^)+4);
 str.Concat(name^,'.ivp',s^);
 xfs.sys.UseFirst(s^,s);
 NEW(full_ivp_file_name,1000);
 xfs.sys.GetFullPathName(s^,full_ivp_file_name^);
 NEW(s,512);
 ProgEnv.ProgramName(s^);
 NEW(ProgName, 300);
 xfs.sys.GetFullPathName(s^,ProgName^);
 Windows.WritePrivateProfileString("COMMON",
                                   "COMPILER",
                                   ProgName^,
                                   full_ivp_file_name^);
 xfs.sys.GetFullPathName('.',s^);
 Windows.WritePrivateProfileString("COMMON",
                                   "WORKDIR",
                                   s^,
                                   full_ivp_file_name^);
 DStrings.Assign("",s1);
 FOR i := 0 TO SYSTEM.VAL(INTEGER, ProgEnv.ArgNumber() ) - 1 DO
   ProgEnv.GetArg(i, s^);  (* Use ProgEnv instead of env.args
                              because arguments starting with '='
                              are eliminated from env.args in xm.Do*)
   DStrings.Append(s^,s1);
   DStrings.Append(" ",s1);
 END;
 Windows.WritePrivateProfileString("COMMON",
                                   "COMMANDLINE",
                                   s1^,
                                   full_ivp_file_name^);
 Windows.WritePrivateProfileString("DATABASE",
                                   "DRIVERNAME",
                                   "STANDARD",
                                   full_ivp_file_name^);
 Windows.WritePrivateProfileString("DATABASE",
                                   "DEFAULT DRIVER",
                                   "DBASE",
                                   full_ivp_file_name^);
 DStrings.Assign("%prjbase%\",s1);
 DStrings.Append(name^,s1);
 DStrings.Append(".DBASE",s1);
 Windows.WritePrivateProfileString("DATABASE",
                                   "PATH",
                                   s1^,
                                   full_ivp_file_name^);
 s:=db.get_full_txt_dir();
 NEW(rel_s,300);
 relate_file_name(full_ivp_file_name^, s^, rel_s^);
 Windows.WritePrivateProfileString("PATH",
                                   "CSVDIR",
                                   rel_s^,
                                   full_ivp_file_name^);
 DStrings.Assign(0C,s1);
 DStrings.Append(0C,s1);
 Windows.WritePrivateProfileSection("FILES",
                                   s1^,
                                   full_ivp_file_name^);
END Create_IV_Project;


PROCEDURE add_line_to_project*( f_id:INTEGER; fnam, csvnames : DStrings.String; main : BOOLEAN);
VAR
  s, rel_s : DStrings.String;
  i        : LONGINT;
  ivsfile, full_ivs  : DStrings.String;
  csvdir  : DStrings.String;
BEGIN
 NEW(s,255);
 NEW(rel_s,300);
 xfs.sys.GetFullPathName(fnam^,s^);
 relate_file_name(full_ivp_file_name^, s^, rel_s^);

 DStrings.Assign(csvnames^ , ivsfile);
 DStrings.Append('.IVS.txt', ivsfile);
 xfs.sys.Lookup(ivsfile^, ivsfile );
 NEW( full_ivs, 300);
 xfs.sys.GetFullPathName(ivsfile^, full_ivs^);
 csvdir := db.get_full_txt_dir();
 DStrings.Append('/x', csvdir );
 NEW( ivsfile, 300);
 relate_file_name(csvdir^, full_ivs^, ivsfile^ );
 str.Delete(ivsfile^, str.Length(ivsfile^) -8 , 8);


 FOR i:=0 TO SYSTEM.VAL(LONGINT,str.Length(rel_s^))-1 DO
   IF rel_s[i]='/' THEN rel_s[i]:='\';END;
 END;

 FOR i:=0 TO SYSTEM.VAL(LONGINT,str.Length(ivsfile^))-1 DO
   IF ivsfile[i]='/' THEN ivsfile[i]:='\';END;
 END;

 Printf.sprintf(s^, "%d",f_id);
 DStrings.Append(';',rel_s);
 DStrings.Append(ivsfile^,rel_s);
 Windows.WritePrivateProfileString("FILES",
                                   s^,
                                   rel_s^,
                                   full_ivp_file_name^);
 IF main THEN
   Windows.WritePrivateProfileString("COMMON",
                                  "MAIN",
                                  s^,
                                  full_ivp_file_name^);
 END;
END add_line_to_project;

PROCEDURE FileFromId*( id : INTEGER ;ivp_name : DStrings.String) : DStrings.String;
VAR
  len       : SYSTEM.CARD32;
  file_name : DStrings.String;
  strid     : DStrings.String;
  full_ivp_file   : DStrings.String;
  tmp       : DStrings.String;
BEGIN
  NEW( file_name , 1024 );
  NEW( full_ivp_file , 1024 );
  NEW( strid , 10 );
  DStrings.Assign(ivp_name^, tmp);
  DStrings.Append('.ivp',    tmp);
  xfs.sys.UseFirst(tmp^,tmp);
  xfs.sys.GetFullPathName(tmp^, full_ivp_file^);
  WholeStr.IntToStr( id, strid^ );
  len := Windows.GetPrivateProfileString("FILES", strid^ ,"",file_name^,1023, full_ivp_file^);
  RETURN file_name;
END FileFromId;

PROCEDURE IdFromFile*( full_file_name, ivp_name : DStrings.String ): INTEGER;
VAR
  len     : SYSTEM.CARD32;
  buf     : ARRAY 100000 OF CHAR;
  fid     : DStrings.String;
  name    : DStrings.String;
  src     : DStrings.String;
  i       : INTEGER;
  num     : LONGINT;
  res     : WholeStr.ConvResults;
  full_ivp_file   : DStrings.String;
  tmp             : DStrings.String;
  rel_file_name   : DStrings.String;

BEGIN
  NEW( full_ivp_file , 1024 );
  DStrings.Assign(ivp_name^, tmp);
  DStrings.Append('.ivp',    tmp);
  xfs.sys.UseFirst(tmp^,tmp);
  xfs.sys.GetFullPathName(tmp^, full_ivp_file^);
  NEW(rel_file_name, 1024);
  relate_file_name(full_ivp_file^, full_file_name^, rel_file_name^);
  len := Windows.GetPrivateProfileSection("FILES",buf, 100000 , full_ivp_file^);
  ASSERT(len < 100000);
  i := 0;
  REPEAT
    fid  := NIL;
    name := NIL;
    src  := NIL;
    WHILE (buf[i] <> '=') AND (buf[i] <> 0C ) DO(*scan file id*)
      DStrings.Append(buf[i], fid);
      INC(i);
    END;
    IF buf[i] = '=' THEN (* skip '=' *)
      INC(i);
    END;
    WHILE (buf[i] <> ';') AND (buf[i] <> 0C) DO (*scan source file*)
      DStrings.Append( buf[i] , src );
      INC(i);
    END;
    IF buf[i] = ';' THEN (* skip ';' *)
      INC(i);
    END;
    WHILE (buf[i] <> ';') AND (buf[i] <> 0C) DO(*scan logical name*)
      DStrings.Append(buf[i], name );
      INC(i);
    END;
    WHILE (buf[i] <> 0C) DO  (*skip*)
      INC(i);
    END;
    ASSERT(i < SYSTEM.VAL(INTEGER, len) +2 );
    INC(i);
    IF (src <> NIL)AND(src^ = rel_file_name^) THEN
      num := -1;
      WholeStr.StrToInt(fid^, num, res);
      RETURN SYSTEM.VAL( INTEGER, num );
    END;
  UNTIL buf[i] = 0C;
  RETURN -1;
END IdFromFile;


END IVProject.