(* Copyright (c) xTech 1995,97.  All Rights Reserved *)

<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosCodeSeg;

IMPORT
  SYSTEM,
  xOS2;

FROM SYSTEM IMPORT ADDRESS, CAST;

PROCEDURE calcCodeExtent ( mh :ADDRESS; rva :CARDINAL; VAR from, to, sec :CARDINAL );

  CONST page_size = 4096;

  VAR rc    :xOS2.APIRET;
      size  :CARDINAL;
      flags :BITSET;
BEGIN
(* -- How to obtain code limits in the OS/2 ? - Not so simple -- VitVit 8-) *)

  from := CAST( CARDINAL, CAST(BITSET, rva )*{12..31});
  size := page_size;           (* 2nd parameter of DosQueryMem must be var one *)
  rc := xOS2.DosQueryMem (ADDRESS(from), size, flags);
  IF (rc = xOS2.ok) THEN
    to := from;
    LOOP
      IF ((flags*xOS2.PAG_BASE) # {}) THEN EXIT END;
      DEC (from, page_size);
      rc := xOS2.DosQueryMem (ADDRESS(from), size, flags);
      IF (rc # xOS2.ok) THEN to := 0; RETURN END;
    END;
    LOOP
      rc := xOS2.DosQueryMem (ADDRESS(to), size, flags);
      IF (rc # xOS2.ok) OR ((flags*xOS2.PAG_EXECUTE) = {}) THEN
        EXIT
      ELSIF ((flags*xOS2.PAG_FREE)#{}) OR ((flags*xOS2.PAG_COMMIT)={}) THEN
        EXIT
      END;
      INC (to, page_size);
    END;
    DEC (to);
  ELSE to := 0;  (* shit happens *)
  END;

  sec := 1;        (* section #1 (32-bit code) is always supposed - temp *)
END calcCodeExtent;


END xosCodeSeg.