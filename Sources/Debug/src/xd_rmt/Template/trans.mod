IMPLEMENTATION MODULE Trans;

IMPORT sys := SYSTEM;
IMPORT ws  := WinSock;
IMPORT rmt := RemTypes;


PROCEDURE ServerConnect (): CARDINAL;
BEGIN
  RETURN rmt.Wrong_connection;
END ServerConnect;


PROCEDURE ClientConnect (remote_host-: ARRAY OF CHAR; port: rmt.TRN_PORT): CARDINAL;
BEGIN
  RETURN rmt.Connection_not_established;
END ClientConnect;


PROCEDURE Disconnect;
BEGIN
END Disconnect;


PROCEDURE Send (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN;
BEGIN
  RETURN FALSE;
END Send;  
  

PROCEDURE Receive (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN; 
BEGIN
  RETURN FALSE;
END Receive;


PROCEDURE InitTransport (VAR transport: rmt.TRANSPORT): BOOLEAN;
BEGIN
  WITH transport DO
    IF id # 'TMP' THEN RETURN FALSE; END;
    srv_connect := ServerConnect;
    cli_connect := ClientConnect;
    disconnect  := Disconnect;
    send        := Send;
    receive     := Receive;
  END;
  RETURN TRUE;
END InitTransport;  


BEGIN
END Trans.