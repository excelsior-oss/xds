<* IF NOT TARGET_LLVM THEN *> This module can be used only in TARGET_LLVM <* END *>
<* +o2addkwd *>
MODULE PrepGen_I;

IMPORT ir;
IMPORT PrepGen;

--------------------------------------------------------------------------------
TYPE
  PrepGen_LLVM_IDB = POINTER TO RECORD (PrepGen.PrepGen_IDB_Rec) END;
  
--------------------------------------------------------------------------------
PROCEDURE (idb: PrepGen_LLVM_IDB) ReplaceCall(p: ir.TriadePtr);
BEGIN
END ReplaceCall;

--------------------------------------------------------------------------------
PROCEDURE (idb: PrepGen_LLVM_IDB) HasSpecificProcessing (op: ir.Operation): BOOLEAN;
BEGIN
  RETURN FALSE;
END HasSpecificProcessing;

--------------------------------------------------------------------------------
PROCEDURE (idb: PrepGen_LLVM_IDB) SpecificProcessing (p: ir.TriadePtr);
BEGIN
END SpecificProcessing;

--------------------------------------------------------------------------------
PROCEDURE (idb: PrepGen_LLVM_IDB)
          UseSimpleMoves (p: ir.TriadePtr; n: LONGINT): BOOLEAN;
BEGIN
  RETURN FALSE;
END UseSimpleMoves;

--------------------------------------------------------------------------------
VAR IDB: PrepGen_LLVM_IDB;
BEGIN
  NEW(IDB);
  PrepGen.IDB := IDB;
END PrepGen_I.
