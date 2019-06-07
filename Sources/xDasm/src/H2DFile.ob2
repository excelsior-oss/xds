(* File operations. Routines *** Lion  Fri  11-17-95 *)

MODULE H2DFile;
(*------------------------------------------------------------------------*)
IMPORT io:= Printf, adt, Strings,
       RndFile, ChanConsts, RawIO, TextIO, IOChan, IOConsts,
       fsys:= FileSys,
       plat:= platform,
       lstr:= LongStrs,
       RegComp,
       ProgEnv,
       sys:= SYSTEM;

(*  Synonyms  *)
TYPE
  INT  = sys.INT32;
  CARD = sys.CARD32;
(*------------------------------------------------------------------------
       Exported types, constants and global variables.
------------------------------------------------------------------------*)

CONST
  EOS * = 0X;
  EOF * = 1AX;

  rdmode * = 1; (* open file only for read, file must be exist *)
  crmode * = 2; (* open file for read & write, if file exist then make
		   it empty *)
  rdwrmode * = 3; (* open file for read & write, file must be exist *)

TYPE FILE * = RndFile.ChanId;

VAR
  SizeOutputLine * : INT;
  DivideChars	 * : lstr.String;
  CurrentDir	 * : lstr.String;
  ProgramName	 * : lstr.String;
  UnlimitedSizeOutputLine * : BOOLEAN;


(*--------------------------------------------------------------------*)
VAR curDir: adt.NamedElement;

(*------------------------------------------------------------------------
	Nonexported types, constants and global variables.
------------------------------------------------------------------------*)

CONST

  (* Mode for open file *)
  read  = RndFile.read;
  write = RndFile.write;
  read_write = RndFile.read+RndFile.write;
  create = RndFile.read+RndFile.write+RndFile.old;

  text_file * = RndFile.text;
  raw_file  * = RndFile.raw;


  (* Blanks *)
  comment_character = '%';
  default_cur_dir   = '';
  ext_prefix = '*';
  point      = '.';
  space      = ' ';
  point_com  = ';';
  equal      = '=';

  Box_size = 150;
  default_size_output_line = 86;   (* default size output line *)

  unknown = -2;
  none	  = -1;

TYPE
  Buffer = POINTER TO BufferDesc;
  BufferDesc = RECORD ( adt.ElementDesc )
    box : ARRAY Box_size OF CHAR;
  END;

  Group = POINTER TO GroupDesc;
  GroupDesc = RECORD (adt.NamedElementDesc)
    expr: RegComp.Expr;
    pathes: adt.List;
  END;

VAR
  PathStore: adt.List;
  OpenResults: RndFile.OpenResults;
  extSep, pathSep  : lstr.String;



(*-----------------------------------------------------------------------*)
(*			   Temporary variables				 *)
(*-----------------------------------------------------------------------*)
VAR
  BlankString: lstr.String; (* String filled with spaces by SpaceStr *)
  outStr     : lstr.String; (* String used by FWriteStr(Ln) *)


(*-----------------------------------------------------------------------*)
PROCEDURE CleanBuffer(VAR b: adt.List);
VAR e: adt.Element;
BEGIN
  b.FindFirst(e);
  WHILE e # NIL DO
    adt.Deallocate(e);
    b.FindNext(e);
  END;
  adt.Deallocate(b); b:= NIL;
END CleanBuffer;


(*-----------------------------------------------------------------------*)
PROCEDURE NewGroup ( VAR group: Group );
BEGIN
  NEW( group );
  group.expr:= NIL;
  adt.NewList( group.pathes );
END NewGroup;

(*-----------------------------------------------------------------------*)
PROCEDURE Length ( str-: ARRAY OF CHAR ): INT;
VAR pos,size: INT;
BEGIN
  pos:=0;
  size:= LEN( str );
  WHILE (pos < size) & (str[pos] # EOS) DO
    INC( pos );
  END;
  RETURN pos;
END Length;

(*-----------------------------------------------------------------------*)
PROCEDURE CutSpace * ( VAR str: ARRAY OF CHAR );
VAR size, pos: INT;
BEGIN
  size:= lstr.Length( str );
  IF size = 0 THEN RETURN;
  ELSE
    pos:= size-1;
    WHILE (pos >= 0) & (str[pos] = space) DO
      DEC(pos);
    END;
    Strings.Delete( str, pos+1, size-pos-1 );
    pos:= 0;
    WHILE (pos < size) & (str[pos] = space) DO
      INC(pos);
    END;
    Strings.Delete( str, 0, pos );
  END;
END CutSpace;

(*--------------------------------------------------------------------*)
PROCEDURE FlushBuffer ( buffer: adt.List; VAR str: lstr.String );
(*  Conncatinate all elements from buffer list into string   *)
VAR elm: adt.Element;
BEGIN
  lstr.Assign( '', str );
  buffer.Reset();
  buffer.FindNext( elm );
  WHILE elm # NIL DO
    lstr.Append( elm(Buffer).box, str );
    elm(Buffer).box[0]:= EOS;
    buffer.FindNext( elm );
  END;
END FlushBuffer;

(*--------------------------------------------------------------------*)
PROCEDURE MakePath ( dir-, name-: ARRAY OF CHAR; VAR path: lstr.String);
BEGIN
  lstr.Assign( dir, path );
  lstr.Append( name, path );
END MakePath;

(*--------------------------------------------------------------------*)
PROCEDURE SplitName * ( wholeName: ARRAY OF CHAR;
			     VAR path, name, ext: lstr.String );
(* Only Split extention, file name and path from each other *)
VAR size: INT;
    pos: CARD;
    found: BOOLEAN;
BEGIN
  lstr.Assign( '', path );
  lstr.Assign( '', name );
  lstr.Assign( '', ext );
  CutSpace( wholeName );
  size:= lstr.Length( wholeName );
  IF size <= 0 THEN RETURN END;
  Strings.FindPrev( pathSep^, wholeName, size-1, found, pos );
  IF found THEN
    lstr.Extract( wholeName, pos+1, sys.VAL(CARD,size)-pos, name );
    lstr.Extract( wholeName, 0, pos, path );
  ELSE
    lstr.Assign( wholeName, name );
  END;
  size:= lstr.Length( name^ );
  Strings.FindPrev( extSep^, name^, size-1, found, pos );
  IF found THEN
    lstr.Extract( name^, pos+1, sys.VAL(CARD,size)-pos, ext );
    Strings.Delete( name^, pos, size-1 );
  END;
END SplitName;

(*--------------------------------------------------------------------*)
PROCEDURE GetName * ( wholeName-: ARRAY OF CHAR;
			      VAR name: lstr.String );
(* Extract file name without extention and path *)
VAR path, ext: lstr.String;
BEGIN
  SplitName( wholeName, path, name, ext );
  CutSpace( name^ );
END GetName;

(*--------------------------------------------------------------------*)
PROCEDURE CreateName * ( path-, name-, ext-: ARRAY OF CHAR;
			 VAR fullname: lstr.String );
VAR found: BOOLEAN;
    pos: CARD;
BEGIN
  lstr.Assign('', fullname);
  IF lstr.Length( path ) > 0 THEN
    lstr.Assign( path, fullname );
    lstr.Append( pathSep^, fullname );
  END;
  lstr.Append( name, fullname );
  IF lstr.Length( ext ) > 0 THEN
    Strings.FindNext( extSep^, ext, 0, found, pos );
    IF ~found THEN
      lstr.Append( extSep^, fullname );
    END;
    lstr.Append( ext, fullname );
  END;
END CreateName;

(*--------------------------------------------------------------------*)
PROCEDURE IsAbsolutePath * ( name-: ARRAY OF CHAR ): BOOLEAN;
VAR value_pos, size, pos: CARD;
    found: BOOLEAN;
BEGIN
  size:= lstr.Length( name );
  IF size = 0 THEN RETURN FALSE;
  ELSE
    value_pos:= 0;
    WHILE (value_pos < size) & (name[value_pos] = space) DO
      value_pos:= value_pos + 1;
    END;
    Strings.FindNext('/', name, 0, found, pos);
    IF found & (pos = value_pos) THEN  RETURN TRUE END;
    Strings.FindNext(':', name, 0, found, pos);
    IF found THEN RETURN TRUE END;
  END;
  RETURN FALSE;
END IsAbsolutePath;

(*--------------------------------------------------------------------*)
PROCEDURE GetPathesList ( wholeName: ARRAY OF CHAR; VAR pathes: adt.List );
(* analyze file name for define group of this file *)
VAR elm: adt.Element;
    name, path, ext, pattern: lstr.String;

  (*-----------------------------*)
  PROCEDURE append_list(to, from: adt.List);
  VAR e, e1: adt.Element;
  BEGIN
    from.FindFirst(e);
    WHILE e # NIL DO
      to.Find(e, e1);
      IF e1 = NIL THEN
	to.Insert(e);
      END;
      from.FindNext(e);
    END;
  END append_list;

(*-----------------------------*)
BEGIN
  adt.NewList( pathes );
  CutSpace( wholeName );
  SplitName( wholeName , path, name, ext );
  CreateName( '', name^, ext^, pattern );
  lstr.Deallocate(path); lstr.Deallocate(name); lstr.Deallocate(ext);
  PathStore.FindFirst( elm );
  WHILE elm # NIL DO
    IF RegComp.Match( elm(Group).expr, pattern^, 0 ) THEN
      append_list( pathes, elm(Group).pathes );
    END;
    PathStore.FindNext( elm );
  END;
  lstr.Deallocate(pattern);
END GetPathesList;

(*--------------------------------------------------------------------*)
PROCEDURE Open * ( name-: ARRAY OF CHAR;
                   mode: INT; VAR hand: FILE; raw_text_mode:= text_file: RndFile.FlagSet): BOOLEAN;
(* Search file on list of path and open its *)
VAR path: lstr.String;
    pathes: adt.List;
    dir: adt.Element;
    id: INT;
BEGIN
  GetPathesList( name, pathes );
  IF ( IsAbsolutePath( name ) ) OR (  pathes.IsEmpty() ) THEN
    CASE mode OF
      rdmode: RndFile.OpenOld( hand, name, read + raw_text_mode, OpenResults );
     |crmode: RndFile.OpenClean( hand, name, create + raw_text_mode, OpenResults );
     |rdwrmode: RndFile.OpenOld( hand, name, read_write + raw_text_mode, OpenResults );
    END;
    IF OpenResults = ChanConsts.opened THEN
      adt.Deallocate(pathes);
      RETURN TRUE;
    END;
  ELSE
    pathes.FindFirst( dir );
    WHILE dir # NIL DO
      pathes.Backup( id );
      MakePath( dir(adt.NamedElement).name^, name, path);
      CASE mode OF
         rdmode: RndFile.OpenOld( hand, path^, read + raw_text_mode, OpenResults );
        |crmode: RndFile.OpenClean( hand, path^, create + raw_text_mode, OpenResults );
        |rdwrmode: RndFile.OpenOld( hand, path^, read_write + raw_text_mode, OpenResults );
      END; lstr.Deallocate(path);
      IF OpenResults = ChanConsts.opened THEN
        adt.Deallocate(pathes);
        RETURN TRUE;
      END;
      pathes.Restore( id );
      pathes.FindNext( dir );
    END;
  END;
  adt.Deallocate(pathes);
  RETURN FALSE;
END Open;

(*--------------------------------------------------------------------*)
PROCEDURE Close * ( file: FILE );
BEGIN
  RndFile.Close( file );
END Close;

(*--------------------------------------------------------------------*)
PROCEDURE Delete * ( name-: ARRAY OF CHAR; VAR done: BOOLEAN );
VAR path: lstr.String;
    pathes: adt.List;
    dir: adt.Element;
    id: INT;
BEGIN
  done:= FALSE;
  GetPathesList( name, pathes );
  IF pathes.IsEmpty() THEN
    fsys.Remove( name , done );
  ELSE
    pathes.FindFirst( dir );
    WHILE dir # NIL DO
      pathes.Backup( id );
      MakePath(dir(adt.NamedElement).name^, name, path);
      fsys.Remove( path^ , done );
      IF done THEN RETURN END;
      pathes.Restore( id );
      pathes.FindNext( dir );
    END;
  END;
END Delete;

(*--------------------------------------------------------------------*)
(*
PROCEDURE SwitchRdWrOperation( file: FILE );
VAR filePos: RndFile.FilePos;
BEGIN
  filePos:= RndFile.CurrentPos( file );
  RndFile.SetPos( file, filePos );
END SwitchRdWrOperation;
*)

(*--------------------------------------------------------------------*)
(*
PROCEDURE ResetFilePos * ( file: FILE );
VAR filePos: RndFile.FilePos;
BEGIN
  filePos:= RndFile.StartPos( file );
  RndFile.SetPos( file, filePos );
END ResetFilePos;
*)

(*--------------------------------------------------------------------*)
PROCEDURE RdBin * (file: FILE; VAR data: ARRAY OF sys.BYTE): BOOLEAN;
(* Returns TRUE if end of input appears *)
BEGIN
  RawIO.Read(file, data);
  RETURN IOChan.ReadResult( file ) # IOConsts.endOfInput;
END RdBin;

(*--------------------------------------------------------------------*)
VAR RdStr_BufferList: adt.List;
PROCEDURE RdStr * ( file: FILE; VAR str: lstr.String );
(* If caused end of file then return NIL *)
VAR elm: adt.Element;
    buffer: Buffer;
BEGIN
(*
  SwitchRdWrOperation( file );
*)
  RdStr_BufferList.Reset();
  REPEAT
    RdStr_BufferList.FindNext(elm);
    IF elm = NIL THEN
      NEW(buffer);
      RdStr_BufferList.Insert(buffer);
      RdStr_BufferList.FindLast(elm);
    END;
    buffer:= elm(Buffer);
    buffer.box[0]:= EOS;
    TextIO.ReadString( file, buffer.box );
  UNTIL (IOChan.ReadResult( file ) = IOConsts.endOfLine) OR
	(IOChan.ReadResult( file ) = IOConsts.endOfInput);
  FlushBuffer( RdStr_BufferList, str );
  TextIO.SkipLine( file );
  IF (IOChan.ReadResult( file ) = IOConsts.endOfInput) & (str[0] = EOS ) THEN
     lstr.Deallocate(str);
  END;
END RdStr;

(*--------------------------------------------------------------------*)
PROCEDURE WrStr * ( file: FILE; str-: ARRAY OF CHAR );
BEGIN
(*
  SwitchRdWrOperation( file );
*)
  TextIO.WriteString( file, str );
END WrStr;


(*--------------------------------------------------------------------*)
PROCEDURE WrStrLn * ( file: FILE; str-: ARRAY OF CHAR );
BEGIN
  TextIO.WriteLn( file );
  TextIO.WriteString( file, str );
END WrStrLn;

(*--------------------------------------------------------------------*)
PROCEDURE SpaceStr * ( VAR str: lstr.String; number: INT );
(* Create string consist space sumbols *)
BEGIN
  lstr.Assign('', str);
  WHILE number > 0 DO
    lstr.AppendChar( space, str );
    DEC(number);
  END;
END SpaceStr;

(*--------------------------------------------------------------------*)
PROCEDURE IsDivideChar ( char: CHAR ): BOOLEAN;
VAR i: INT;
BEGIN
  FOR i:= 0 TO lstr.Length1( DivideChars ) - 1 DO
    IF DivideChars[i] = char THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END IsDivideChar;

(*--------------------------------------------------------------------*)
PROCEDURE FindDivideChar ( str-: ARRAY OF CHAR; startPos: INT): INT;
VAR pos, size: INT;
BEGIN
  size:= lstr.Length( str );
  pos:= startPos;
  LOOP
    IF pos >= size THEN RETURN none END;
    IF IsDivideChar( str[pos]  ) THEN
      IF ( pos+1 < size ) & ( ~IsDivideChar(str[pos+1]) ) THEN
	RETURN pos;
      END;
    END;
    INC( pos );
  END;
END FindDivideChar;

(*--------------------------------------------------------------------*)
PROCEDURE SplitStr ( VAR str:ARRAY OF CHAR; VAR extStr: lstr.String;
							  size: INT );
(* Split string on part consist of 'size' sumbol *)
VAR pos, prevPos: INT;
BEGIN
  prevPos:= 0;
  lstr.Deallocate(extStr);
  IF Length(str) > size THEN
    pos:= FindDivideChar( str, 0 );
    IF pos = none THEN RETURN;
    ELSIF  pos >= size	THEN
      lstr.Extract( str, 0, pos+1, extStr );
      Strings.Delete( str, 0, pos+1 );
    ELSE
      WHILE (pos # none) & (pos < size) DO
	prevPos:= pos;
	pos:= FindDivideChar( str, pos+1 );
      END;
      lstr.Extract( str, 0, prevPos+1, extStr );
      Strings.Delete( str, 0, prevPos+1 );
    END;
  END;
END SplitStr;

(*--------------------------------------------------------------------*)
PROCEDURE CarveStr ( str: ARRAY OF CHAR; VAR buffer: adt.List; tab: INT );
(* Split string on part consist of 'SizeOutputLine' sumbol *)
VAR nelm: adt.NamedElement;
BEGIN
  adt.NewNamedElement(nelm, '');
  adt.NewList( buffer );
  SplitStr( str, nelm.name, SizeOutputLine );
  WHILE nelm.name # NIL DO
    buffer.Insert( nelm );
    adt.NewNamedElement(nelm, '');
    SplitStr( str, nelm.name, SizeOutputLine-tab );
  END;
  lstr.Assign( str, nelm.name );
  buffer.Insert( nelm );
END CarveStr;

(*--------------------------------------------------------------------*)
PROCEDURE FWriteStrLn * ( file: FILE; str-: ARRAY OF CHAR;
			  leadtab, tab: INT ): INT;
(* Formationly output string in file
   leadtab - space placed before string
   tab - space placed before every new lines, except first line
   used WriteStrLn
*)
VAR buffer: adt.List;
    numberLine: INT;

   (*------------------------*)
   PROCEDURE FlushWriteBuffer ( ): INT;
   VAR elm: adt.Element;
       numberLine: INT;
   BEGIN
     numberLine:= 0;
     SpaceStr( BlankString, tab );
     buffer.FindFirst( elm );
     IF elm # NIL THEN
       WrStrLn( file, elm(adt.NamedElement).name^ );
       numberLine:= 1;
       buffer.FindNext( elm );
       WHILE elm # NIL DO
	 lstr.Insert( BlankString^, 0, elm(adt.NamedElement).name );
	 WrStrLn( file, elm(adt.NamedElement).name^ );
	 INC( numberLine );
	 buffer.FindNext( elm );
       END;
     END; CleanBuffer(buffer);
     RETURN numberLine;
   END FlushWriteBuffer;

(*--------------------*)
BEGIN
  SpaceStr( outStr, leadtab );
  lstr.Append( str, outStr );
  IF ( lstr.Length( outStr^ ) <= SizeOutputLine ) OR
     ( UnlimitedSizeOutputLine )
  THEN
    WrStrLn( file, outStr^ );
    numberLine:= 1;
  ELSE
    CarveStr( outStr^, buffer, tab );
    numberLine:= FlushWriteBuffer();
  END;
  RETURN numberLine;
END FWriteStrLn;

(*--------------------------------------------------------------------*)
PROCEDURE FWriteStr * ( file: FILE; str-: ARRAY OF CHAR;
			leadtab, tab: INT ): INT;
(* Formationly output string in file
   leadtab - space placed before string
   tab - space placed before every new lines, except first line
   used WriteStrLn
*)
VAR buffer: adt.List;
    numberLine: INT;

   (*-------------------------*)
   PROCEDURE FlushWriteBuffer ( ): INT;
   VAR elm: adt.Element;
       numberLine: INT;
   BEGIN
     numberLine:= 0;
     SpaceStr( BlankString, tab );
     buffer.FindFirst( elm );
     IF elm # NIL THEN
       WrStr( file, elm(adt.NamedElement).name^ );
       buffer.FindNext( elm );
       WHILE elm # NIL DO
	 lstr.Insert( BlankString^, 0, elm(adt.NamedElement).name );
	 WrStrLn( file, elm(adt.NamedElement).name^ );
	 INC( numberLine );
	 buffer.FindNext( elm );
       END;
     END;
     RETURN numberLine;
   END FlushWriteBuffer;

(*-----------------------*)
BEGIN
  SpaceStr( outStr, leadtab );
  lstr.Append( str, outStr );
  IF ( lstr.Length( outStr^ ) <= SizeOutputLine ) OR
     ( UnlimitedSizeOutputLine )
  THEN
    WrStr( file, outStr^ );
    numberLine:= 0;
  ELSE
    CarveStr( outStr^, buffer, tab );
    numberLine:= FlushWriteBuffer();
  END;
  RETURN numberLine;
END FWriteStr;


(*--------------------------------------------------------------------*)
PROCEDURE Exist * ( filename-: ARRAY OF CHAR ): BOOLEAN;
(* Check exist or not this file *)
VAR file: FILE;
BEGIN
  IF Open( filename, rdmode, file ) THEN
    Close( file );
    RETURN TRUE;
  ELSIF  (OpenResults = ChanConsts.alreadyOpen) THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END Exist;


(*------------------------------------------------------------------------

			Pars Rederection File

------------------------------------------------------------------------*)
PROCEDURE GetGroup( VAR str: ARRAY OF CHAR; VAR group: Group );
VAR pos: CARD;
    found: BOOLEAN;
    expr: lstr.String;
    res: INT;
BEGIN
  Strings.FindNext( '=', str, 0, found, pos );
  IF found THEN
    NewGroup( group ); expr:= NIL;
    lstr.Extract( str, 0, pos, expr );
    Strings.Delete( str, 0, pos+1 );
    CutSpace(expr^);
    RegComp.Compile(expr^, group.expr, res);
    IF res > 0 THEN
      PathStore.Insert( group );
    ELSE
      group:= NIL;
    END;
  END;
END GetGroup;

(*--------------------------------------------------------------------*)
PROCEDURE AdaptationPath ( VAR path: lstr.String );
(* give the path correct form *)
VAR size: INT;
BEGIN
  IF path # NIL THEN
    CutSpace( path^ );
    size:= Length( path^ );
    IF (size # 0) & (path[size-1] # pathSep[0] ) THEN
      lstr.AppendChar( plat.pathSep, path );
    END;
  END;
END AdaptationPath;

(*--------------------------------------------------------------------*)
PROCEDURE ParsePathes( str: ARRAY OF CHAR; group: Group );
(* separate  path in _str_ and insert them in list of path *)
VAR found: BOOLEAN;
    pos: CARD;
    path: adt.NamedElement;
BEGIN
  IF group # NIL THEN
    CutSpace( str );
    Strings.FindNext( ';', str, 0, found, pos );
    WHILE found DO
      NEW( path );
      lstr.Extract( str, 0, pos, path.name );
      AdaptationPath( path.name );
      group.pathes.Insert( path );
      Strings.Delete( str, 0, pos+1 );
      Strings.FindNext( ';', str, 0, found, pos );
    END;
    CutSpace( str );
    IF Length( str ) # 0 THEN
      NEW( path );
      lstr.Assign( str, path.name );
      AdaptationPath( path.name );
      group.pathes.Insert( path );
    END;
  END;
END ParsePathes;

(*--------------------------------------------------------------------*)
PROCEDURE LoadRedFile  ( );
(* Must add load redirection file from H2D's directories *)
VAR str: lstr.String;
    redfile: FILE;
    group: Group;
    res: RndFile.OpenResults;
    name, path, ext: lstr.String;
    elm: adt.Element;
BEGIN
  name:= NIL; ProgramName:= NIL; path:= NIL; ext:= NIL; group:= NIL;

  NEW( curDir );
  adt.NewList( PathStore );
  lstr.Assign( default_cur_dir, CurrentDir );
  curDir.SetName( CurrentDir^ );

  lstr.Allocate(name, ProgEnv.ProgramNameLength() + 1);
  ProgEnv.ProgramName(name^);
  SplitName(name^, path, ProgramName, ext);

  CreateName('', ProgramName^, 'red', name);
  RndFile.OpenOld( redfile, name^, read+text_file, res );
  IF res # ChanConsts.opened THEN
    CreateName(path^, ProgramName^, 'red', name);
    RndFile.OpenOld( redfile, name^, read+text_file, res );
  END;
  IF res = ChanConsts.opened THEN
    RdStr( redfile, str );
    WHILE str # NIL DO
      IF str[0] # comment_character THEN
	GetGroup( str^, group );
	ParsePathes( str^, group );
      END;
      RdStr(redfile,str);
    END;
    RndFile.Close( redfile );
  END;

  PathStore.FindFirst( elm );
  WHILE elm # NIL DO
    elm(Group).pathes.Insert( curDir );
    PathStore.FindNext( elm );
  END;

  CreateName('', ProgramName^, 'cfg', str);
  lstr.Append('=', str); lstr.Append(path^, str);
  GetGroup( str^, group ); ParsePathes( str^, group );

  CreateName('', ProgramName^, 'msg', str);
  lstr.Append('=', str); lstr.Append(path^, str);
  GetGroup( str^, group ); ParsePathes( str^, group );

  lstr.Deallocate(ext); lstr.Deallocate(name);
  lstr.Deallocate(path); lstr.Deallocate(str);
END LoadRedFile;

(*------------------------------------------------*)
BEGIN
  UnlimitedSizeOutputLine:= FALSE;
  adt.NewList(RdStr_BufferList);
  DivideChars:= NIL;
  extSep:= NIL;
  lstr.AppendChar( plat.extSep, extSep );
  pathSep:= NIL;
  lstr.AppendChar( plat.pathSep, pathSep );
  SizeOutputLine:= default_size_output_line;
  BlankString:= NIL;
  outStr:= NIL;
  LoadRedFile();
END H2DFile.
