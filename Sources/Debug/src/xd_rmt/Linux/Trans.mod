IMPLEMENTATION MODULE Trans;

IMPORT sys := SYSTEM;
IMPORT sk  := SocketW;
IMPORT rmt := RemTypes;

FROM Printf IMPORT printf;

CONST
  MAX_PENDING_CONNECTS = 4;
  DEFAULT_PORT_NO      = 1055H;

VAR
  ListenSocket: sk.SOCKET;
  Socket : sk.SOCKET;
  Connected: BOOLEAN;


PROCEDURE MasterListen (port: rmt.TRN_PORT; VAR channel: rmt.TRN_CHANNEL): CARDINAL;
VAR
  psock_addr   : sk.PSOCKADDR;
  acc_sin      : sk.SOCKADDR_IN;
  my_sin       : sk.SOCKADDR_IN;
  acc_len      : INTEGER;
  buffer       : ARRAY [0..1023] OF CHAR;
  reuse_addr   : INTEGER;

BEGIN
  IF port = 0 THEN
    port := DEFAULT_PORT_NO;
  END;

  printf (">>> Trans.MasterListen\n");
  IF NOT Connected THEN
    printf ("  It's not connected yet.\n");
    ASSERT (channel = rmt.INVALID_CHANNEL);
    WITH my_sin DO
      sin_family := sk.AF_INET;
      sin_addr.s_addr := sk.INADDR_ANY;
      sin_port := sk.htons (port);
    END;
    printf ("  gethostname...\n");
    IF sk.gethostname (sys.ADR(buffer), SIZE(buffer)) # 0 THEN
      printf ("  Error: Host unknown.\n");
      RETURN rmt.Host_unknown;
    END;
    printf ("  ListenSocket := socket...\n");
    ListenSocket := sk.socket (sk.PF_INET, sk.SOCK_STREAM, 0);
    IF ListenSocket = sk.INVALID_SOCKET THEN
      printf ("  Error: Wrong connection\n");
      RETURN rmt.Wrong_connection;
    END;
    reuse_addr := 1;
    sk.setsockopt(ListenSocket, sk.SOL_SOCKET, sk.SO_REUSEADDR, sys.ADR(reuse_addr), SIZE(reuse_addr));
    psock_addr := sys.ADR(my_sin);
    printf ("  bind (ListenSocket, ...\n");
    IF sk.bind (ListenSocket, psock_addr^, SIZE(my_sin)) # 0 THEN
      printf ("  Error: Connection not established\n");
      RETURN rmt.Connection_not_established;
    END;
    printf ("  listen (ListenSocket, ...\n");
    IF sk.listen (ListenSocket, MAX_PENDING_CONNECTS) < 0 THEN
      printf ("  Error: Over max pending connects\n");
      RETURN rmt.Over_max_pending_connects;
    END;
  END;
  printf ("  Connection established.\n");
  psock_addr := sys.ADR(acc_sin);
  acc_len := SIZE(acc_sin);
  Socket := sk.accept (ListenSocket, psock_addr^, acc_len);
  IF Socket = sk.INVALID_SOCKET THEN
    RETURN rmt.Wrong_socket;
  END;
  Connected := TRUE;
  channel := Socket;
  RETURN rmt.Connection_established;
END MasterListen;


PROCEDURE ServerConnect (channel: rmt.TRN_CHANNEL): CARDINAL;
BEGIN
  printf (">>> Trans.ServerConnect\n");
  IF channel = sk.INVALID_SOCKET THEN
    RETURN rmt.Wrong_socket;
  END;
  IF NOT Connected THEN
    Connected := TRUE;
  END;
  Socket := sk.SOCKET (channel);
  RETURN rmt.Connection_established;
END ServerConnect;



PROCEDURE ClientConnect (remote_host-: ARRAY OF CHAR; port: rmt.TRN_PORT): CARDINAL;
VAR
  Dest_sin  : sk.SOCKADDR_IN;
  psock_addr: sk.PSOCKADDR;
  Phe       : sk.PHOSTENT;

  buf: ARRAY [0..255] OF CHAR;

BEGIN
  printf (">>> Trans.ClientConnect\n");
  IF Connected THEN
    RETURN rmt.Connection_already_established;
  END;
  IF port = 0 THEN
    port := DEFAULT_PORT_NO;
  END;

  COPY (remote_host, buf);
  WITH Dest_sin DO
    sin_family := sk.AF_INET;
    Phe := sk.gethostbyname (buf);
    IF Phe = NIL THEN
      RETURN rmt.Host_unknown;
    END;
    sys.MOVE (sys.ADR(Phe^.h_addr_list^^), sys.ADR(sin_addr), Phe^.h_length);
    sin_port := sk.htons (port);
  END;
  Socket := sk.socket (sk.PF_INET, sk.SOCK_STREAM, 0);
  IF Socket = sk.INVALID_SOCKET THEN
    RETURN rmt.Wrong_socket;
  END;
  psock_addr := sys.ADR(Dest_sin);
  IF sk.connect (Socket, psock_addr^, SIZE(Dest_sin)) < 0 THEN
    RETURN rmt.Connection_not_established;
  END;
  Connected := TRUE;
  RETURN rmt.Connection_established;
END ClientConnect;


PROCEDURE Disconnect;
CONST
  DISCARD_BUFFER_SIZE = 1024;
VAR
  discard_buffer: ARRAY [0..DISCARD_BUFFER_SIZE] OF sys.BYTE;
  ret : INTEGER;
BEGIN
  printf (">>> Trans.Disconnect\n");
  IF Connected THEN
    IF ListenSocket # sk.INVALID_SOCKET THEN
      sk.closesocket (ListenSocket);
    END;

    sk.shutdown (Socket, sk.SHUT_WR);

    LOOP
      ret := sk.recv (Socket, sys.ADR (discard_buffer[0]), DISCARD_BUFFER_SIZE, 0);
      IF ret <= 0 THEN EXIT; END;
    END;

    sk.closesocket (Socket);
    Socket := sk.INVALID_SOCKET;
    Connected := FALSE;
  END;  
END Disconnect;


PROCEDURE Send (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN;
VAR
  res: BOOLEAN;
BEGIN
--  printf (">>> Trans.Send\n");
  res := Connected AND (sk.send(Socket, addr, VAL(INTEGER, size), 0) = VAL(INTEGER, size));
  --ASSERT (res, sk.WSAGetLastError());
  RETURN res;
END Send;  
  

PROCEDURE Receive (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN; 
VAR
  s1: CARDINAL;
  s2: INTEGER;
BEGIN
--  printf (">>> Trans.Receive\n");
  IF NOT Connected THEN
    RETURN FALSE;
  END;
  s1 := 0;
  LOOP
    s2 := sk.recv(Socket, sys.ADDADR(addr, VAL(CARDINAL, s1)), VAL(INTEGER, size-s1), 0);
    --ASSERT (s2 > 0, sk.WSAGetLastError());
    IF s2 < 0 THEN RETURN FALSE; END;
    INC(s1, VAL(CARDINAL, s2));
    IF s1 = size THEN RETURN TRUE; END;
    IF s1 > size THEN RETURN FALSE; END;
  END;
END Receive;



PROCEDURE InitTransport (VAR transport: rmt.TRANSPORT): BOOLEAN;
BEGIN
  printf (">>> Trans.InitTransport\n");
  WITH transport DO
    IF id # 'TCP' THEN
      printf ("  ERROR: invalid ID='', expected 'TCP'\n", id);
      RETURN FALSE;
    END;
    mst_listen  := MasterListen;
    srv_connect := ServerConnect;
    cli_connect := ClientConnect;
    disconnect  := Disconnect;
    send        := Send;
    receive     := Receive;
  END;
  printf ("  Transport initialized successfully.\n");
  RETURN TRUE;
END InitTransport;  


BEGIN
  ListenSocket := sk.INVALID_SOCKET;
  Socket := sk.INVALID_SOCKET;
  Connected := FALSE;
END Trans.