(* Copyright (c) xTech 1993. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE XRaise; (* Ned 02-Mar-93 *)

(* Modifications:
   22-Mar-94 Ned: merging implementations
*)

IMPORT  xrtsOS;

PROCEDURE RAISE(no: CARDINAL; msg: ARRAY OF CHAR);
BEGIN
  xrtsOS.X2C_StdOutN;
  xrtsOS.X2C_StdOut("*** EXCEPTION: ",15);
  xrtsOS.X2C_StdOut(msg,LENGTH(msg));
  xrtsOS.X2C_StdOut(" ***",4);
  xrtsOS.X2C_StdOutN;
  xrtsOS.X2C_StdOutFlush;
  ASSERT(FALSE,no);
END RAISE;

END XRaise.
