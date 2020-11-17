<* IF NOT TARGET_LLVM THEN *> This module can be used only in TARGET_LLVM <* END *>
MODULE ir_D;

IMPORT ir_def;

--------------------------------------------------------------------------------
TYPE
  LCNum *= ir_def.LCNum;

  Param    *= RECORD END;
  ParamPtr *= POINTER TO Param;

--------------------------------------------------------------------------------
PROCEDURE (p: ParamPtr) Init *();
END Init;

END ir_D.
