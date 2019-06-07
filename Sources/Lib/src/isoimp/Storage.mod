(* Copyright (c) xTech 1993,95. All Rights Reserved. *)
IMPLEMENTATION MODULE Storage; (*  07-07-93 07:59pm *)

(* Modifications:
   22-Mar-94 Ned: merging implemantations
*)

IMPORT SYSTEM;

<* IF EXCEPTIONS THEN *> IMPORT  EXCEPTIONS; <* END *>

IMPORT  xmRTS;

<* IF EXCEPTIONS THEN *>
  VAR source: EXCEPTIONS.ExceptionSource;
<* END *>

PROCEDURE ALLOCATE(VAR a: SYSTEM.ADDRESS; size: CARDINAL);
BEGIN
  xmRTS.X2C_ALLOCATE(a,VAL(SYSTEM.size_t,size));
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR a: SYSTEM.ADDRESS; size: CARDINAL);
BEGIN
<* IF EXCEPTIONS THEN *>
  IF a=NIL THEN
    EXCEPTIONS.RAISE(source,
    		     ORD(nilDeallocation),
                     "first argument of DEALLOCATE is NIL");
  END;
<* END *>
  xmRTS.X2C_DEALLOCATE(a);
END DEALLOCATE;

PROCEDURE IsStorageException (): BOOLEAN;
BEGIN
<* IF EXCEPTIONS THEN *>
  RETURN EXCEPTIONS.IsCurrentSource(source)
<* ELSE *>
  RETURN FALSE
<* END *>
END IsStorageException;

PROCEDURE StorageException (): StorageExceptions;
BEGIN
<* IF EXCEPTIONS THEN *>
  RETURN VAL(StorageExceptions,EXCEPTIONS.CurrentNumber(source))
<* ELSE *>
  HALT;
  RETURN nilDeallocation
<* END *>
END StorageException;

BEGIN
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END Storage.

