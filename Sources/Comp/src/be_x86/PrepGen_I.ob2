<* IF TARGET_68K OR TARGET_RISC OR TARGET_VAX THEN *>
This module can be used only in TARGET_386
<* END *>
MODULE PrepGen_I;

IMPORT
    PrepGen;

TYPE
     PrepGen_386_IDB     = POINTER TO RECORD(PrepGen.PrepGen_IDB_Rec) END;
VAR
     IDB : PrepGen_386_IDB;

BEGIN
  NEW(IDB);
  PrepGen.IDB := IDB;
END PrepGen_I.
