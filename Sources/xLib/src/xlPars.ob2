(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS Librarian. Import definition parser. *)
MODULE xlPars; (* Hady Oct 25, 1995 *)

IMPORT
  xlK
  ,xlFiles
  ,xlStrings
  ,xlMsg
  ,IOResult
  ,CharClass
  ,WholeConv
  ;


CONST
  IDSTEP = 16;

CONST (* symbols *)
  eof    = 0; (* end of input *)
  eol    = 1; (* end of line *)
  from   = 2; (* FROM *)
  import = 3; (* IMPORT *)
  comma  = 4; (* , *)
  colon  = 5; (* ; *)
  ident  = 6; (* identifier *)
  as     = 7; (* AS *)
  number = 8; (* ordinal number *)
  idents = {from,import,ident,as};

  EOL = 0AX;
  EOF = 0BX;
  TAB = 09X;

TYPE
  String = xlStrings.String;

VAR
  str: String;
  line,col: LONGINT;
  file: xlFiles.ChanId;
  filename: String;
  char: CHAR;
  symbol: INTEGER;
  sline,scol: LONGINT;
  idname: String;
  fault: BOOLEAN;
  ioerr: IOResult.ReadResults;

  result: xlK.Dll;
  rlast: xlK.Dll;
  elist: xlK.Entry;
  elast: xlK.Entry;

PROCEDURE error(msg-: ARRAY OF CHAR);
BEGIN
  IF fault THEN
    xlMsg.error('I/O error reading file "%s"',filename^);
  ELSE
    xlMsg.error('file "%s" line %3d column %2d:\n  %s',filename^,sline,scol,msg);
  END;
END error;

PROCEDURE nextline;
  VAR res: IOResult.ReadResults;
BEGIN
  xlFiles.ReadString(file,str,res);
  IF res=IOResult.endOfInput THEN
    str:=NIL
  ELSIF res=IOResult.allRight THEN
    col:=0; INC(line);
  ELSE
    str:=NIL;
    fault:=TRUE;
    ioerr:=res;
  END;
END nextline;

PROCEDURE getchar;
BEGIN
  IF str=NIL THEN char:=EOF
  ELSIF (col>=LEN(str^)) OR (str[col]=0X) THEN
    char:=" ";
    nextline;
  ELSE
    char:=str[col];
    IF (col=0) & (char="%") THEN char:=" "; nextline;
    ELSE
      INC(col);
    END;
  END;
END getchar;

PROCEDURE Ident;
  VAR i: LONGINT; t: String;
BEGIN
  i:=0;
  REPEAT
    IF i>=LEN(idname^) THEN
      NEW(t,LEN(idname^)+IDSTEP);
      COPY(idname^,t^);
      idname:=t;
    END;
    idname[i]:=char;
    INC(i);
    getchar;
  UNTIL (char=EOF) OR (char=" ") OR (char=TAB) OR (char=",") OR (char=";");
  IF i<LEN(idname^) THEN idname[i]:=0X END;
END Ident;

PROCEDURE getsy;
BEGIN
  WHILE (char=" ") OR (char=TAB) DO getchar END;
  sline:=line; scol:=col;
  CASE char OF
    |EOF: symbol:=eof;   getchar;
    |",": symbol:=comma; getchar;
    |";": symbol:=colon; getchar;
  ELSE
    Ident;
    IF fault THEN symbol:=eof
    ELSE
      IF    idname^="FROM"   THEN symbol:=from
      ELSIF idname^="IMPORT" THEN symbol:=import
      ELSIF idname^="AS"     THEN symbol:=as
      ELSIF CharClass.IsNumeric(idname^[0]) THEN symbol:=number;
      ELSE                        symbol:=ident
      END;
    END;
  END;
END getsy;

PROCEDURE skipto(sy: SET);
BEGIN
  WHILE ~ (symbol IN sy) DO getsy END;
END skipto;

PROCEDURE oneEntry(VAR err: BOOLEAN);
  VAR
    ename,iname :String;
    ord         :LONGINT; 
    e           :xlK.Entry;
BEGIN
  IF (symbol IN idents) THEN
    ename:=xlStrings.Make(idname^);
    ord  := -1;
    getsy;
    IF symbol=as THEN
      getsy;
      IF symbol IN idents THEN
        iname:=xlStrings.Make(idname^);
        getsy;
      ELSE
        err:=TRUE;
        error('internal entry name expected');
        skipto({comma,colon,eof});
      END;
    ELSE
      iname:=ename;
    END;
  ELSIF symbol=number THEN
     ename:= NIL;
     ord  := WholeConv.ValueCard(idname^);
     getsy;
     IF symbol=as THEN
       getsy;
       IF symbol IN idents THEN
         iname:=xlStrings.Make(idname^);
         getsy;
       ELSE
         err:=TRUE;
       END;
     ELSE
       err:=TRUE;
     END;
     IF err THEN
       error('internal entry name expected');
       skipto({comma,colon,eof});
     END;
  ELSE
    err:=TRUE;
    error(' entry name expected');
    skipto({comma,colon,eof});
  END;
  IF ~err THEN
    NEW(e);
    e.ext    := ename;
    e.int    := iname;
    e.ord    := ord;
    e.next   := NIL;
    IF (elast = NIL) THEN elist:=e ELSE elast.next:=e END;
    elast:=e;
  END;
END oneEntry;

PROCEDURE oneDll(VAR err: BOOLEAN);
  VAR dllname: String; d: xlK.Dll;
BEGIN
  IF symbol=from THEN
    getsy;
    IF symbol IN idents THEN
      dllname:=xlStrings.Make(idname^);
      getsy;
      IF symbol=import THEN
        elist:=NIL; elast:=NIL;
        getsy;
        oneEntry(err);
        WHILE symbol=comma DO
          getsy; oneEntry(err)
        END;
        IF symbol=colon THEN
          getsy;
          IF ~err THEN
            NEW(d);
            d.name:=dllname;
            d.list:=elist;
            d.next:=NIL;
            IF rlast=NIL THEN result:=d ELSE rlast.next:=d END;
            rlast:=d;
          END;
        ELSIF ~err THEN
          err:=TRUE;
          error('symbol ";" expected');
        END;
      ELSE
        err:=TRUE;
        error('symbol "IMPORT" expected');
      END;
    ELSE
      err:=TRUE;
      error('dll name expected');
    END;
  ELSE (* no FROM symbol *)
    err:=TRUE;
    error('symbol "FROM" expected');
  END;
END oneDll;

PROCEDURE ParseDef* (cid :xlFiles.ChanId; fname- :ARRAY OF CHAR;
                     VAR imp :xlK.Dll; VAR err :BOOLEAN);
BEGIN
  err:=FALSE;
  file:=cid; filename:=xlStrings.Make(fname);
  imp:=NIL;
  result:=NIL; rlast:=NIL;
  nextline;
  getchar;
  getsy;
  oneDll(err);
  WHILE ~err & (symbol#eof) DO oneDll(err) END;
  IF ~err THEN imp:=result END;
END ParseDef;

BEGIN
  NEW(idname,IDSTEP);
END xlPars.
