(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS Librarian. Files library. NON-PORTABLE. *)
MODULE xlFiles; (* Hady, Nov 9, 1995 *)

IMPORT
  SYSTEM
  ,xlStrings
  ,IOChan
  ,IOResult
  ,TextIO
  ,RndFile
  ,ChanConsts
  ,Strings
  ,FileName
  ,FileSys
  ;

CONST
  INPBUFLEN = 128;
  HANDLES = 2;

TYPE
  ChanId* = IOChan.ChanId;
  String = xlStrings.String;
  FilePos* = RndFile.FilePos;

  File* = POINTER TO FileDesc;
  FileDesc = RECORD
    cid- : ChanId;
    exist: BOOLEAN;
    new  : BOOLEAN;
    safe : BOOLEAN;
    open : BOOLEAN;          (* indicates that cid is valid *)
    pos  : RndFile.FilePos;  (* when ~open - the position where file was suspended *)
    oname: String;
    cname: String;
    flags: ChanConsts.FlagSet; (* open flags set - needed for resume *)
  END;

(* Non-portable functions *)

PROCEDURE Remove(name-: ARRAY OF CHAR): BOOLEAN;
  VAR done: BOOLEAN;
BEGIN
  FileSys.Remove(name,done); RETURN done;
END Remove;

PROCEDURE Rename(from-,to-: ARRAY OF CHAR): BOOLEAN;
  VAR done: BOOLEAN;
BEGIN
  FileSys.Rename(from,to,done); RETURN done;
END Rename;

PROCEDURE Split*(fname-: ARRAY OF CHAR; VAR dir,name,ext: String);
  VAR i,j: LONGINT;
BEGIN
  i:=LENGTH(fname);
  IF i=0 THEN
    NEW(ext,1); ext[0]:=0X;
    name:=ext;  dir:=ext;
    RETURN
  END;
  j:=i;
  REPEAT DEC(i)
  UNTIL (i=0) OR (fname[i]="\") OR (fname[i]=".") OR (fname[i]=":");
  IF (fname[i]=".") THEN (* ext exists *)
    NEW(ext,j-i);
    Strings.Extract(fname,i+1,j-i-1,ext^);
    j:=i;
    DEC(i); (* skip "." *)
  ELSE NEW(ext,1); ext[0]:=0X;
  END;
  WHILE (i>0) & (fname[i]#"\") & (fname[i]#":") DO DEC(i) END;
  IF (fname[i]="\") OR (fname[i]=":") THEN
    IF i=j THEN NEW(name,1); name[0]:=0X;
    ELSE
      NEW(name,j-i); Strings.Extract(fname,i+1,j-i-1,name^);
    END;
    IF (fname[i]#"\") OR (i=0) OR (fname[i-1]=":") THEN INC(i) END;
    NEW(dir,i+1);
    Strings.Extract(fname,0,i,dir^);
  ELSE (* no directory specified *)
    NEW(dir,1); dir[0]:=0X;
    NEW(name,j);
    Strings.Extract(fname,0,j,name^);
  END;
END Split;

PROCEDURE Merge*(VAR fname: String; dir-,name-,ext: ARRAY OF CHAR);
  VAR len,i,j: LONGINT; ps: BOOLEAN;
BEGIN
  IF ext[0]="." THEN Strings.Delete(ext,0,1) END;
  len:=LENGTH(dir);
  ps:=(len>0) & (dir[len-1]#"\") & (dir[len-1]#":"); IF ps THEN INC(len) END;
  len:=len+LENGTH(name);
  IF ext[0]#0X THEN INC(len); len:=len+LENGTH(ext) END;
  NEW(fname,len+1);
  i:=0;
  WHILE (i<LEN(dir)) & (dir[i]#0X) DO fname[i]:=dir[i]; INC(i) END;
  IF ps THEN fname[i]:="\"; INC(i) END;
  j:=0;
  WHILE (j<LEN(name)) & (name[j]#0X) DO fname[i]:=name[j]; INC(i); INC(j) END;
  IF ext[0]#0X THEN
    fname[i]:="."; INC(i);
    j:=0;
    WHILE (j<LEN(ext)) & (ext[j]#0X) DO fname[i]:=ext[j]; INC(i); INC(j) END;
  END;
  fname[i]:=0X;
END Merge;

PROCEDURE Create*(VAR f: File; name-: ARRAY OF CHAR; text,safe: BOOLEAN; VAR res: ChanConsts.OpenResults);
VAR
  cid   :ChanId;
  flags :RndFile.FlagSet;
  dir,fname,ext :String;
  tmp   :ARRAY 8 OF CHAR;
  carry :BOOLEAN;
  i     :LONGINT;
BEGIN
  flags := RndFile.read+RndFile.write;
  IF text THEN flags := flags+RndFile.text END;
  RndFile.OpenClean(cid,name,flags,res);
  IF res=ChanConsts.opened THEN
    NEW(f);
    f.exist:=FALSE;
    f.cid:=cid;
    f.new:=TRUE;
    f.safe:=safe;
    f.oname:=xlStrings.Make(name);
    f.cname:=f.oname;
    f.flags:=flags;
    f.open:=TRUE;
(*
    pos  : RndFile.FilePos; (* not needed while f.open *)
*)
    RETURN
  ELSIF res#ChanConsts.fileExists THEN
    f:=NIL; RETURN;
  END;
  Split(name,dir,fname,ext);
  NEW(f);
  f.exist:=TRUE;
  f.safe:=safe;
  f.flags:=flags;
  f.new:=TRUE;
  f.open:=TRUE;
  f.cname:=xlStrings.Make(name);
  NEW(f.oname,FileName.Length(LENGTH(dir^),8,3)+1);
  COPY("xlib0000",tmp);
  LOOP
    FileName.Create(dir^,tmp,"tmp",f.oname^);
    RndFile.OpenClean(cid,f.oname^,flags,res);
    IF (res=ChanConsts.opened) OR (res#ChanConsts.fileExists) THEN EXIT END;
    i:=7; carry:=TRUE;
    REPEAT
      IF carry THEN
        tmp[i]:=CHR(ORD(tmp[i])+1);
        carry:=tmp[i]>"9";
        IF carry THEN tmp[i]:=CHR(ORD(tmp[i])-10) END;
      END;
      DEC(i);
    UNTIL ~carry OR (i<4);
    IF carry THEN res:=ChanConsts.otherProblem; EXIT END;
  END;
  IF res#ChanConsts.opened THEN
    f:=NIL
  ELSE
    f.cid:=cid;
  END;
END Create;

PROCEDURE Open*(VAR f: File; name-: ARRAY OF CHAR; text: BOOLEAN; VAR res: ChanConsts.OpenResults);
  VAR cid: ChanId; flags: RndFile.FlagSet;
BEGIN
  flags:=RndFile.read;
  IF text THEN flags:=flags+RndFile.text END;
  RndFile.OpenOld(cid,name,flags,res);
  IF res=ChanConsts.opened THEN
    NEW(f);
    f.oname:=xlStrings.Make(name);
    f.cname:=f.oname;
    f.new:=FALSE;
    f.cid:=cid;
    f.open:=TRUE;
    f.flags:=flags;
(*
    exist: BOOLEAN; (* not needed -- NOT new *)
    safe : BOOLEAN; (* not needed -- NOT new *)
    pos  : RndFile.FilePos; (* not needed while f.open *)
*)
  ELSE 
    f:=NIL;
  END;
END Open;

(** File methods *)

PROCEDURE (f: File) Close*(register: BOOLEAN);
  VAR res: BOOLEAN;
      dir,name,ext,tmp: String;
BEGIN
  IF f.open THEN RndFile.Close(f.cid) END;
  IF ~f.new THEN RETURN END;
  IF register THEN
    IF f.exist THEN
      IF f.safe THEN
        Split(f.cname^,dir,name,ext);
        Merge(tmp,dir^,name^,"BAK");
        res:=Remove(tmp^);
        res:=Rename(f.cname^,tmp^);
      ELSE
        res:=Remove(f.cname^);
      END;
      res:=Rename(f.oname^,f.cname^);
    END;
  ELSE
    res:=Remove(f.oname^);
  END;
END Close;

PROCEDURE (f:File) Suspend*;
BEGIN
  ASSERT(~f.new);
  f.pos:=RndFile.CurrentPos(f.cid);
  RndFile.Close(f.cid);
  f.open:=FALSE;
END Suspend;

PROCEDURE (f: File) Resume*(VAR res: ChanConsts.OpenResults);
BEGIN
  IF f.open THEN res:=ChanConsts.opened; RETURN END;
  RndFile.OpenOld(f.cid,f.oname^,f.flags,res);
  IF res=ChanConsts.opened THEN
    RndFile.SetPos(f.cid,f.pos);
    f.open:=TRUE;
  END;
END Resume;

(** Collection of miscelanious functions *)

PROCEDURE ReadString*(cid: ChanId; VAR s: xlStrings.String; VAR res: IOResult.ReadResults);
  VAR buf: ARRAY INPBUFLEN OF CHAR;
BEGIN
  IF s#NIL THEN s[0]:=0X END;
  TextIO.ReadString(cid,buf);
  res:=IOResult.ReadResult(cid);
  IF res=IOResult.allRight THEN
    REPEAT
      xlStrings.Append(buf,s);
      TextIO.ReadString(cid,buf);
      res:=IOResult.ReadResult(cid);
    UNTIL res#IOResult.allRight;
  END;
  IF res=IOResult.endOfLine THEN
    res:=IOResult.allRight; TextIO.SkipLine(cid);
  ELSIF res=IOResult.endOfInput THEN
    IF (s#NIL) & (s[0]#0X) THEN res:=IOResult.allRight END;
  END;
END ReadString;

CONST
   WriteLn*     = TextIO.WriteLn;
   WriteString* = TextIO.WriteString;

END xlFiles.