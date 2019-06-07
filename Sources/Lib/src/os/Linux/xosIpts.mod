(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosIpts; (* Hady. 31.05.96 11:05 *)

PROCEDURE ["C"] X2C_EnableIpts();
BEGIN
END X2C_EnableIpts;

PROCEDURE ["C"] X2C_DisableIpts();
BEGIN
END X2C_DisableIpts;

PROCEDURE ["C"] X2C_SaveIptHandler(no: CARDINAL);
BEGIN
END X2C_SaveIptHandler;

PROCEDURE ["C"] X2C_RestoreIptHandler(no: CARDINAL);
BEGIN
END X2C_RestoreIptHandler;

PROCEDURE ["C"] X2C_SetIptHandler(no: CARDINAL): BOOLEAN;
BEGIN
  RETURN FALSE
END X2C_SetIptHandler;

END xosIpts.
