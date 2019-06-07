<* IF TARGET_68K OR TARGET_RISC OR TARGET_VAX THEN *>
This module can be used only in TARGET_386
<* END *>
MODULE ir_D;
IMPORT SYSTEM;
TYPE
  Param    *= RECORD END;
  ParamPtr *= POINTER TO Param;

PROCEDURE(p : ParamPtr) Init*;
END Init;

END ir_D.
