(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS librarian. Root definitions. *)
MODULE xlK; (* Hady. Nov 2, 1995 *)

IMPORT
  xlFiles
  ,xlStrings
  ;

TYPE
  String = xlStrings.String;
  ChanId* = xlFiles.ChanId;
  File = xlFiles.File;

TYPE
  Module *= POINTER TO ModuleDesc;
  ModuleDesc *= RECORD
    name-: String;    (** Module (not file) name *)
    size-: LONGINT;   (** Size of nodule in file *)
    time-: LONGINT;   (** UNIX date/time longnit *)
    fofs*: LONGINT;   (** ofset in library file  *)
    mord*: LONGINT;   (** Module's ordinal number in library *)
    next-: Module;
  END;

  Iterator* = RECORD  (** Iterator object for the modules' publics *)
    info*: String;
  END;

  Library *= POINTER TO LibraryDesc;
  LibraryDesc *= RECORD
    time-: LONGINT;
    list-: Module;
    last : Module;
    mcnt-: LONGINT;   (** Count of modules in library *)
    mode : SET;
  END;

  Directory *= POINTER TO DirectoryDesc;
  DirectoryDesc *= RECORD
  END;

  Dll* = POINTER TO DllDesc; (** list of import information for import library *)
  Entry* = POINTER TO EntryDesc;

  DllDesc = RECORD
    name*: String;
    list*: Entry;
    next*: Dll;
  END;

  EntryDesc = RECORD
    int*    :String;     (** internal entry name *)
    ext*    :String;     (** entry name in DLL *)
    ord*    :LONGINT;    (** ordinal number; must be -1 if import by name *)
    next*   :Entry;
  END;

VAR
  dir-: Directory;

(** Name Iterator methods *)

PROCEDURE (VAR i: Iterator) Take*(name-: ARRAY OF CHAR);
(** Abstract *)
BEGIN
  ASSERT(FALSE);
END Take;

(** Module methods *)

PROCEDURE (m: Module) Init*(name-: ARRAY OF CHAR; size,time: LONGINT);
BEGIN
  NEW(m.name,LENGTH(name)+1);
  COPY(name,m.name^);
  m.size:=size;
  m.time:=time;
  m.next:=NIL;
  m.fofs:=0;
END Init;

PROCEDURE (m: Module) Write*(to: ChanId);
(** ABSTRACT *)
BEGIN
  ASSERT(FALSE)
END Write;

PROCEDURE (m: Module) IteratePublics*(VAR iter: Iterator);
(** ABSTRACT. Define as folows:
  FOR str IN m.PublicNames DO iter.Take(str) END;
*)
BEGIN
  ASSERT(FALSE)
END IteratePublics;

(** Library methods *)

PROCEDURE (l: Library) Init*(time: LONGINT);
BEGIN
  l.list:=NIL; l.last:=NIL; l.mcnt:=0; l.time:=time; l.mode := {};
END Init;

PROCEDURE (l: Library) Add*(m: Module);
BEGIN
  IF l.last=NIL THEN
    l.list:=m;
  ELSE
    l.last.next:=m;
  END;
  m.next:=NIL;
  l.last:=m;
  INC(l.mcnt);
END Add;

PROCEDURE (l: Library) Remove*(m: Module);
  VAR t,p: Module;
BEGIN
  t:=l.list; p:=NIL;
  WHILE (t#NIL) & (t#m) DO p:=t; t:=t.next END;
  IF t=NIL THEN RETURN END;
  DEC(l.mcnt);
  IF p=NIL THEN
    l.list:=t.next
  ELSE
    p.next:=t.next
  END;
  IF t=l.last THEN l.last:=p END;
END Remove;

PROCEDURE (l: Library) Search*(name-: ARRAY OF CHAR): Module;
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE);
  RETURN NIL;
END Search;

PROCEDURE (l: Library) Prepare*(VAR err: BOOLEAN);
(** ABSTRACT. *)
BEGIN
END Prepare;

PROCEDURE (l: Library) Write* (to: ChanId);
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE)
END Write;

CONST
  nodick_property* = 0;

PROCEDURE (l: Library) SetProperties* (pr :SET);
BEGIN
  l.mode := pr;
END SetProperties;


(** Directory methods *)

PROCEDURE (d: Directory) NewLibrary*(time: LONGINT): Library;
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE)
END NewLibrary;

PROCEDURE (d: Directory) NewImport*(import: Dll): Module;
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE);
END NewImport;

PROCEDURE (d: Directory) MakeImportLibrary*(VAR lib: Library; import: Dll);
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE);
END MakeImportLibrary;

PROCEDURE (d: Directory) IsModule*(file: ChanId): BOOLEAN;
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE);
  RETURN FALSE;
END IsModule;

PROCEDURE (d: Directory) IsLibrary*(file: ChanId): BOOLEAN;
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE);
  RETURN FALSE;
END IsLibrary;

PROCEDURE (d: Directory) OpenModule*(VAR m: Module; file: File);
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE);
END OpenModule;

PROCEDURE (d: Directory) OpenLibrary*(VAR lib: Library; file: File);
(** ABSTRACT. *)
BEGIN
  ASSERT(FALSE);
END OpenLibrary;

(** Set Up Directory *)

PROCEDURE Set*(d: Directory);
(** initialize "dir" global. Parameter "d" must be NIL if "dir"#NIL and vice versa *)
BEGIN
  ASSERT((d#NIL)#(dir#NIL));
  dir:=d;
END Set;

BEGIN
  dir:=NIL;
END xlK.
