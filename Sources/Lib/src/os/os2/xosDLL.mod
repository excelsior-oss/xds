(* Copyright (c) 2000 Excelsior, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosDLL;

IMPORT SYSTEM, xOS2;


PROCEDURE ["C"] X2C_GetModuleName (      hmod :SYSTEM.ADDRESS;
                                    VAR mname :ARRAY OF CHAR;
                                        nmlen :SYSTEM.CARD32 );
BEGIN
  xOS2.DosQueryModuleName ( xOS2.HMODULE(hmod), nmlen, mname );
END X2C_GetModuleName;


END xosDLL.
