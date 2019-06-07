IMPLEMENTATION MODULE Trans;

IMPORT sys := SYSTEM;
IMPORT ws  := WinSock;
IMPORT rmt := RemTypes;

CONST
  MAX_PENDING_CONNECTS = 4;
  DEFAULT_PORT_NO      = 1055H;

VAR
  ListenSocket: ws.SOCKET;
  Socket : ws.SOCKET;
  Connected: BOOLEAN;


PROCEDURE MasterListen (port: rmt.TRN_PORT; VAR channel: rmt.TRN_CHANNEL): CARDINAL;
VAR
  psock_addr   : ws.PSOCKADDR;
  WSA_Data     : ws.WSADATA;
  acc_sin      : ws.SOCKADDR_IN;
  my_sin       : ws.SOCKADDR_IN;
  buffer       : ARRAY [0..1023] OF CHAR;

BEGIN
  IF port = 0 THEN
    port := DEFAULT_PORT_NO;
  END;

  IF NOT Connected THEN
    ASSERT (channel = rmt.INVALID_CHANNEL);
    ws.WSAStartup (0101H,  WSA_Data);
    WITH my_sin DO
      sin_family := ws.AF_INET;
      sin_addr.s_addr := ws.INADDR_ANY;
      sin_port := ws.htons (port);
    END;
    IF ws.gethostname (sys.ADR(buffer), SIZE(buffer)) # 0 THEN
      RETURN rmt.Host_unknown;
    END;
    ListenSocket := ws.socket (ws.PF_INET, ws.SOCK_STREAM, 0);
    IF ListenSocket = ws.INVALID_SOCKET THEN
      RETURN rmt.Wrong_connection;
    END;
    psock_addr := sys.ADR(my_sin);
    IF ws.bind (ListenSocket, psock_addr^, SIZE(my_sin)) # 0 THEN
      RETURN rmt.Connection_not_established;
    END;
    IF ws.listen (ListenSocket, MAX_PENDING_CONNECTS) < 0 THEN
      RETURN rmt.Over_max_pending_connects;
    END;
  END;
  psock_addr := sys.ADR(acc_sin);
  Socket := ws.accept (ListenSocket, psock_addr^, NIL);
  IF Socket = ws.INVALID_SOCKET THEN
    RETURN rmt.Wrong_socket;
  END;
  Connected := TRUE;
  channel := Socket;
  RETURN rmt.Connection_established;
END MasterListen;


PROCEDURE ServerConnect (channel: rmt.TRN_CHANNEL): CARDINAL;
VAR
  WSA_Data: ws.WSADATA;
BEGIN
  IF channel = ws.INVALID_SOCKET THEN
    RETURN rmt.Wrong_socket;
  END;
  IF NOT Connected THEN
    ws.WSAStartup (0101H,  WSA_Data);
    Connected := TRUE;
  END;
  Socket := ws.SOCKET (channel);
  RETURN rmt.Connection_established;
END ServerConnect;



PROCEDURE ClientConnect (remote_host-: ARRAY OF CHAR; port: rmt.TRN_PORT): CARDINAL;
VAR
  WSA_Data  : ws.WSADATA;
  Dest_sin  : ws.SOCKADDR_IN;
  psock_addr: ws.PSOCKADDR;
  Phe       : ws.PHOSTENT;

  buf: ARRAY [0..255] OF CHAR;

BEGIN
  IF Connected THEN
    RETURN rmt.Connection_already_established;
  END;
  IF port = 0 THEN
    port := DEFAULT_PORT_NO;
  END;

  ws.WSAStartup (0101H, WSA_Data);
  COPY (remote_host, buf);
  WITH Dest_sin DO
    sin_family := ws.AF_INET;
    Phe := ws.gethostbyname (buf);
    IF Phe = NIL THEN
      RETURN rmt.Host_unknown;
    END;
    sys.MOVE (sys.ADR(Phe^.h_addr_list^^), sys.ADR(sin_addr), Phe^.h_length);
    sin_port := ws.htons (port);
  END;
  Socket := ws.socket (ws.PF_INET, ws.SOCK_STREAM, 0);
  IF Socket = ws.INVALID_SOCKET THEN
    RETURN rmt.Wrong_socket;
  END;
  psock_addr := sys.ADR(Dest_sin);
  IF ws.connect (Socket, psock_addr^, SIZE(Dest_sin)) < 0 THEN
    RETURN rmt.Connection_not_established;
  END;
  Connected := TRUE;
  RETURN rmt.Connection_established;
END ClientConnect;


PROCEDURE Disconnect;
BEGIN
  IF Connected THEN
    IF ListenSocket # ws.INVALID_SOCKET THEN
      ws.closesocket (ListenSocket);
    END;
    ws.closesocket (Socket);
    Socket := ws.INVALID_SOCKET;
    ws.WSACleanup ();
    Connected := FALSE;
  END;  
END Disconnect;


PROCEDURE Send (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN;
VAR
  res: BOOLEAN;
BEGIN
  res := Connected AND (ws.send(Socket, addr, VAL(INTEGER, size), 0) = VAL(INTEGER, size));
  --ASSERT (res, ws.WSAGetLastError());
  RETURN res;
END Send;  
  

PROCEDURE Receive (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN; 
VAR
  s1: CARDINAL;
  s2: INTEGER;
BEGIN
  IF NOT Connected THEN
    RETURN FALSE;
  END;
  s1 := 0;
  LOOP
    s2 := ws.recv(Socket, sys.ADDADR(addr, VAL(CARDINAL, s1)), VAL(INTEGER, size-s1), 0);
    --ASSERT (s2 > 0, ws.WSAGetLastError());
    IF s2 <= 0 THEN RETURN FALSE; END;
    INC(s1, VAL(CARDINAL, s2));
    IF s1 = size THEN RETURN TRUE; END;
    IF s1 > size THEN RETURN FALSE; END;
  END;
END Receive;



PROCEDURE InitTransport (VAR transport: rmt.TRANSPORT): BOOLEAN;
BEGIN
  WITH transport DO
    IF id # 'TCP' THEN
      RETURN FALSE;
    END;
    mst_listen  := MasterListen;
    srv_connect := ServerConnect;
    cli_connect := ClientConnect;
    disconnect  := Disconnect;
    send        := Send;
    receive     := Receive;
  END;
  RETURN TRUE;
END InitTransport;  


BEGIN
  ListenSocket := ws.INVALID_SOCKET;
  Socket := ws.INVALID_SOCKET;
  Connected := FALSE;
END Trans.