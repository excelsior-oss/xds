<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*+ M2EXTENSIONS*>
IMPLEMENTATION MODULE xrHistory; (* Hady. 24.05.96 14:48 *)

IMPORT  sys := SYSTEM
     ,  os  := xrtsOS
     ,  xrnRTT
     ;

PROCEDURE ["C"] X2C_scanStackHistory(from,to: sys.ADDRESS; exact: BOOLEAN);
BEGIN
  IF X2C_stkScan#NIL THEN X2C_stkScan(from,to,exact) END;
END X2C_scanStackHistory;

PROCEDURE ["C"] X2C_show_history;
BEGIN
  IF X2C_hisShow#NIL THEN X2C_hisShow() END;
END X2C_show_history;

PROCEDURE ["C"] X2C_show_profile;
BEGIN
  (* Dummy *)
END X2C_show_profile;


PROCEDURE ["C"] X2C_PrintExceptionInfo ();
CONST
 StdOutS  = os.X2C_StdOutS;
 StdOutH  = os.X2C_StdOutH;
 StdOutLn = os.X2C_StdOutN;
VAR
  i   :INTEGER;
  esp :CARDINAL;
BEGIN
--  Printf.printf ("\n EAX = %X  EBX = %X\n ECX = %X  EDX = %X\n",
--                 ExcInfo.EAX, ExcInfo.EBX, ExcInfo.ECX, ExcInfo.EDX);
  StdOutLn ();
  StdOutS  ("  EAX = ", 1);  StdOutH (ExcInfo.EAX, 1);
  StdOutS  ("  EBX = ", 1);  StdOutH (ExcInfo.EBX, 1);
  StdOutLn ();
  StdOutS  ("  ECX = ", 1);  StdOutH (ExcInfo.ECX, 1);
  StdOutS  ("  EDX = ", 1);  StdOutH (ExcInfo.EDX, 1);
  StdOutLn ();

--  Printf.printf (" ESI = %X  EDI = %X\n EBP = %X  ESP = %X\n",
--                 ExcInfo.ESI, ExcInfo.EDI, ExcInfo.EBP, ExcInfo.ESP);
  StdOutS  ("  ESI = ", 1);  StdOutH (ExcInfo.ESI, 1);
  StdOutS  ("  EDI = ", 1);  StdOutH (ExcInfo.EDI, 1);
  StdOutLn ();
  StdOutS  ("  EBP = ", 1);  StdOutH (ExcInfo.EBP, 1);
  StdOutS  ("  ESP = ", 1);  StdOutH (ExcInfo.ESP, 1);
  StdOutLn ();

--  Printf.printf (" EIP = %X\n", ExcInfo.EIP);
  StdOutS  ("  EIP = ", 1);  StdOutH (ExcInfo.EIP, 1);
  StdOutLn ();

  StdOutS (" STACK:", 1);
  esp := ExcInfo.ESP;
  FOR i := 0 TO xrnRTT.STK_ENTRIES-1 DO
    IF ((i MOD 4) = 0) THEN
      StdOutLn ();
      StdOutS ("  ", 1);
      StdOutH (esp,  1);
      StdOutS (": ", 1);
    END;
    StdOutS (" ",  1);
    StdOutH (ExcInfo.stk[i],  1);
    esp := esp + SIZE(sys.ADDRESS);
  END;
  StdOutLn ();
END X2C_PrintExceptionInfo;

END xrHistory.
