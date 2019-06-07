(** Copyright (c) XDS 1998.  All Rights Reserved *)
MODULE Dirs;

IMPORT DStrings, xfs:=xiFiles;

TYPE
  String = DStrings.String;

  DirIter = RECORD (xfs.DirIterator)
    name: String;
  END;

VAR 

(**
   Gets the name of the parent directory.
   Returns: the parent directory, or null if one is not found.
*)
  PROCEDURE getParent*(f: String): String;
    VAR dir: String;
  BEGIN
    xfs.sys.GetDir(f^,dir);
    IF LENGTH(dir^) > 0 THEN RETURN dir ELSE RETURN NIL END;
  END getParent;

  PROCEDURE (VAR d: DirIter) Entry(name-: ARRAY OF CHAR; dir: BOOLEAN): BOOLEAN;
  BEGIN
    RETURN dir & (name=d.name^);
  END Entry;
  
(**
   Returns a boolean indicating whether or not a directory file 
   exists.
*)
  PROCEDURE isDirectory*(f: String): BOOLEAN;
    VAR p: String; d: DirIter;
  BEGIN
    IF ~xfs.sys.Exists(f^) THEN RETURN FALSE END;
    xfs.sys.GetDir(f^,p);
    xfs.sys.GetName(f^, d.name);
    RETURN xfs.sys.IterateDir(p^,d);
  END isDirectory;
  
(**
   Creates a directory and returns a boolean indicating the
   success of the creation.
*)
  PROCEDURE mkdir*(f: String): BOOLEAN;
  BEGIN
    RETURN xfs.sys.CreateDir(f^);
  END mkdir;
  
(**
   Creates all directories in this path.  This method 
   returns true if all directories in this path are created.
*)
  PROCEDURE mkdirs*(f: String): BOOLEAN;
    VAR p: String;
  BEGIN
    IF isDirectory(f) THEN RETURN TRUE END;
    p:=getParent(f);
    IF p#NIL THEN
      RETURN mkdirs(p) & mkdir(f)
    ELSE
      RETURN mkdir(f)
    END;
  END mkdirs;

END Dirs.