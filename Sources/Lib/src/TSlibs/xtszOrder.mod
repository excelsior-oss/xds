(* Copyrigth (C) 1996,99 XDS Ltd *)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xtszOrder;

<* IF multithread THEN *>
  IMPORT Threads;
<* END *>

-------------------------------------------------- Semaphore

<* IF multithread THEN *>
VAR
  semList: Threads.Mutex;
<* END *>

PROCEDURE GainAccess;
BEGIN
<* IF multithread THEN *>
  Threads.LockMutex ( semList );
<* END *>
END GainAccess;

PROCEDURE Relinquish;
BEGIN
<* IF multithread THEN *>
  Threads.UnlockMutex ( semList );
<* END *>
END Relinquish;

----------

PROCEDURE find ( hwnd :HWND; VAR i :CARDINAL ) :BOOLEAN;
BEGIN
  i := 1;
  WHILE ( i < top ) DO
    IF (list[i]=hwnd) THEN RETURN TRUE END;
    INC (i);
  END;
  RETURN FALSE;
END find;


PROCEDURE Kill ( hwnd :HWND );
VAR
  i, ki :CARDINAL;
BEGIN
  <* IF multithread THEN *> GainAccess; <* END *>
  IF find ( hwnd, ki )
    THEN FOR i:=ki TO top-2 DO
           list[i] := list [i+1];
         END;
         ASSERT (top>1);
         DEC (top);
  END;
  <* IF multithread THEN *> Relinquish; <* END *>
END Kill;



PROCEDURE PutOnTop ( hwnd :HWND );
BEGIN
  <* IF multithread THEN *> GainAccess; <* END *>
  Kill (hwnd);
  list[top] := hwnd;
  ASSERT (top<wndNMax);
  INC (top);
  <* IF multithread THEN *> Relinquish; <* END *>
END PutOnTop;



PROCEDURE PutBeneath ( newhwnd, hwnd :HWND);
VAR
  i, ki :CARDINAL;
BEGIN
  <* IF multithread THEN *> GainAccess; <* END *>
  Kill ( newhwnd );
  IF ( find ( hwnd, ki ) )
    THEN FOR i:=top TO ki+1 BY -1 DO
           list[i] := list [i-1];
         END;
         list [ki] := newhwnd;
         ASSERT (top<wndNMax);
         INC (top);
    ELSE ASSERT (FALSE );
  END;
  <* IF multithread THEN *> Relinquish; <* END *>
END PutBeneath;

---------------------------------------------------------------------------------------------

BEGIN
 <* IF multithread THEN *> Threads.CreateMutex ( semList ); <* END *>
 list[0] := HWND_DESKTOP;
 top     := 1;
FINALLY
  <* IF multithread THEN *> Threads.DeleteMutex ( semList ); <* END *>
END xtszOrder.

