<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
IMPLEMENTATION MODULE xrnGetTD; (* Hady. 29.05.96 10:31 *)

IMPORT  SYSTEM, xmRTS;

PROCEDURE [2] X2C_GET_TD(a: SYSTEM.ADDRESS): xmRTS.X2C_TD;
  VAR l: xmRTS.X2C_LINK;
BEGIN
  l:=SYSTEM.SUBADR(a,SIZE(xmRTS.X2C_LINK_STR));
  RETURN l^.td;
END X2C_GET_TD;

END xrnGetTD.
