<* MAIN+ *>
<* NEW noregvars+*>
MODULE visimp;
IMPORT hgr, hgl;
IMPORT arg := ProgEnv;
IMPORT Printf;
IMPORT SYSTEM;
IMPORT SeqFile;
IMPORT TextIO, IOChan, IOConsts;
IMPORT FileName, Strings;
IMPORT WholeStr;

VAR
  Nnodes,Nedges : LONGINT;
VAR
  k, i: SYSTEM.CARD32;
  infile, fname: ARRAY 255 OF CHAR;
  n,m: LONGINT;
  v: hgl.PVertex;



PROCEDURE Help;
BEGIN
    Printf.printf("Need file name\n");
    HALT(0);
END Help;


VAR
    names: POINTER TO ARRAY OF ARRAY 64 OF CHAR;

PROCEDURE ReadData;
VAR
    file:   SeqFile.ChanId;

  PROCEDURE Open (s-: ARRAY OF CHAR);
    VAR res: SeqFile.OpenResults;
  BEGIN
    SeqFile.OpenRead (file, s, SeqFile.read + SeqFile.old, res);
  END Open;

  PROCEDURE Close;
  BEGIN
      SeqFile.Close (file);
  END Close;

VAR
    foo, name, ext : ARRAY 64 OF CHAR;
    ok: BOOLEAN;
    i,j,bar: INTEGER;
    index : INTEGER;
    tmp: POINTER TO ARRAY OF ARRAY 64 OF CHAR;
BEGIN
  Open(infile);
  Nnodes := 0;
  Nedges := 0;
  NEW (names, 1000);
--  WHILE IOChan.ReadResult(file) # IOConsts.endOfInput DO
  TextIO.ReadString(file, foo);
  TextIO.SkipLine(file);
  REPEAT
    hgr.AddVertex (hgr.irNodeType, 0, 0, 0);
    FileName.GetName(foo, name);
    FileName.GetExt(foo, ext);
    Strings.Append(".", name);
    Strings.Append(ext, name);
    hgr.SetLabVal (hgl.ObjVertex, Nnodes, hgr.labelNode, name);
    v := hgr.GetVertex(Nnodes);
    v.h := 20;
    v.w := Strings.Length(name)*8;
    COPY(foo, names[Nnodes]);
    INC(Nnodes);
    TextIO.ReadString(file, foo);
    TextIO.SkipLine(file);
  UNTIL foo = "---";

--  DEC(Nnodes);
  TextIO.ReadString(file, foo);
  TextIO.SkipLine(file);
  FOR i := 0 TO Nnodes-1 DO
    TextIO.ReadString(file, foo);
    TextIO.SkipLine(file);
   REPEAT
      bar := 0;
      LOOP
         IF foo = names[bar] THEN
           EXIT;
         END;
         INC(bar);
      END;
      IF bar # i THEN
         hgr.AddEdge(hgr.DefEdgeType, i, bar);
         INC(Nedges);
      END;
      TextIO.ReadString(file, foo);
      TextIO.SkipLine(file);
    UNTIL foo = "";
  END;


(*  IF foo = "" THEN
  ELSE
      FOR i := 0 TO Nnodes DO


      END;
  END;
--  END;
*)
  TextIO.SkipLine(file);
  Close;
END ReadData;

VAR
    e: hgl.PEdge;
    foo : SYSTEM.int;
    str: ARRAY 7 OF CHAR;
BEGIN
  k := arg.ArgNumber();
  IF k = 0 THEN Help; END;
  i := 0;
  arg.GetArg(0, infile);

  hgr.Init;
  hgr.CreateNewGraph(1);
  hgr.SetFrTitle(0, infile);

  ReadData;

  hgr.RearrangeGraph(Nnodes, 50, 50);
(*  FOR n := 0 TO Nedges-1 DO
    e := hgr.GetEdge(n);
    hgr.AddEdge(hgr.DefEdgeType, e.to, e.from);
  END;
  FOR n := 0 TO Nedges-1 DO
    hgr.DeleteEdge(n);
  END;

  FOR n := 0 TO Nnodes-1 DO
  END;
*)
  hgr.NormalizeGraph(1);
  Printf.sprintf(fname, "%s.hgr", infile);
  hgr.SaveGraph(fname);

END visimp.
