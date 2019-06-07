(* Copyright (c) 1997,2000 Excelsior, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosDLL;

IMPORT SYSTEM, xWin32;


PROCEDURE ["C"] X2C_GetModuleName ( hmod      :SYSTEM.ADDRESS;
                                    VAR mname :ARRAY OF CHAR;
                                    nmlen     :SYSTEM.CARD32 );
BEGIN
  xWin32.GetModuleFileName ( xWin32.DWORD(hmod), mname, nmlen )
END X2C_GetModuleName;



END xosDLL.


