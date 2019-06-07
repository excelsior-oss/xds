IMPLEMENTATION MODULE Memory;

<*-IOVERFLOW   *>
<*-COVERFLOW   *>
<*-CHECKDINDEX *>
<*-CHECKINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKTYPE   *>

IMPORT SYSTEM, LocalHeap, xiEnv;

VAR
  h :LocalHeap.heapID;

--------------------------------------------------------------------------------

PROCEDURE ERROR;
BEGIN
    xiEnv.errors^.Fault (xiEnv.null_pos, 950);
    HALT;
END ERROR;

--------------------------------------------------------------------------------

PROCEDURE ALLOCATE (VAR p: SYSTEM.ADDRESS; n: INTEGER);
BEGIN
  LocalHeap.ALLOCATE (h, p, n);
  IF (p = NIL) THEN ERROR END;
END ALLOCATE;

--------------------------------------------------------------------------------

PROCEDURE DEALLOCATE (VAR p: SYSTEM.ADDRESS; n: INTEGER);
BEGIN
  LocalHeap.DEALLOCATE (h, p, n);
END DEALLOCATE;

--------------------------------------------------------------------------------

PROCEDURE DEALLOCATE_ALL;
BEGIN
  LocalHeap.DEALLOCATE_ALL(h);
END DEALLOCATE_ALL;



BEGIN
  LocalHeap.Create (h);
END Memory.
