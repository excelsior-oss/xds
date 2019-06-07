<* Storage+ *>

IMPLEMENTATION MODULE Dll;

IMPORT sys := SYSTEM;
IMPORT dll := dllRTS;

CONST
  HN_HMOD = 16;

TYPE
  PAHMOD = POINTER TO ARRAY OF dll.HMOD;
  HMODS = RECORD
            HMods: PAHMOD;
            Count: CARDINAL;
          END;

VAR
  HMods: HMODS;


PROCEDURE CheckHandle (handle: dll.HMOD): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH HMods DO
    IF Count # 0 THEN
      FOR i := 0 TO Count-1 DO
        IF HMods^[i] = handle THEN
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END CheckHandle;


PROCEDURE AddHandle (handle: dll.HMOD);
VAR
  tmp: PAHMOD;
BEGIN
  IF CheckHandle(handle) OR (handle = NIL) THEN RETURN; END;
  WITH HMods DO
    IF HMods = NIL THEN
      NEW(HMods, HN_HMOD);
      ASSERT (HMods#NIL);
    ELSIF Count > HIGH(HMods^) THEN
      NEW(tmp, HIGH(HMods^)+1 + HN_HMOD);
      ASSERT (tmp#NIL);
      sys.MOVE (sys.ADR(HMods^[0]), sys.ADR(tmp^[0]), SIZE(HMods^));
      DISPOSE(HMods);
      HMods := tmp;
    END;
    HMods^[Count] := handle;
    INC(Count);
  END;
END AddHandle;


PROCEDURE FreeHandles;
VAR
  i : CARDINAL;
  rc: BOOLEAN;
BEGIN
  WITH HMods DO
    IF Count # 0 THEN
      FOR i := 0 TO Count-1 DO
        dll.FreeModule(HMods^[i], rc);
      END;
    END;
  END;
END FreeHandles;


BEGIN
  HMods := HMODS{ NIL, 0 };
FINALLY
  FreeHandles;
END Dll.
