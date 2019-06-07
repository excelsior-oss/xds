IMPLEMENTATION MODULE Trans;

IMPORT sys := SYSTEM;
IMPORT sct := Socket;
IMPORT msg := MsgNo;
IMPORT rmt := RemTypes;

CONST
  MAX_PENDING_CONNECTS = 4;
  DEFAULT_PORT_NO      = 1055H;

VAR
  ListenSocket: sct.SOCKET;
  Socket      : sct.SOCKET;
  Connected   : BOOLEAN;

PROCEDURE MasterListen (port: rmt.TRN_PORT; VAR channel: rmt.TRN_CHANNEL): CARDINAL;
VAR
  psock_addr: sct.PSOCKADDR;
  acc_sin   : sct.SOCKADDR_IN;
  my_sin    : sct.SOCKADDR_IN;

  buffer: ARRAY [0..1023] OF CHAR;
  size  : INTEGER;

BEGIN
  IF port = 0 THEN
    port := DEFAULT_PORT_NO;
  END;

  IF NOT Connected THEN
    ASSERT (channel = rmt.INVALID_CHANNEL);
    WITH my_sin DO
      sin_family := sct.AF_INET;
      sin_addr.s_addr := sct.INADDR_ANY;
      sin_port := sct.bswap(port);
    END;
    IF sct.gethostname(sys.ADR(buffer), SIZE(buffer)) # 0 THEN RETURN msg.Host_unknown; END;
    ListenSocket := sct.socket(sct.PF_INET,sct.SOCK_STREAM,0);
    IF ListenSocket = sct.INVALID_SOCKET THEN
      RETURN msg.Wrong_connection;
    END;
    psock_addr := sys.ADR(my_sin);
    IF sct.bind(ListenSocket, psock_addr^, SIZE(my_sin)) # 0 THEN
      RETURN msg.Connection_not_established;
    END;
    IF sct.listen(ListenSocket, MAX_PENDING_CONNECTS) < 0 THEN
      RETURN msg.Over_max_pending_connects;
    END;
  END;
  psock_addr := sys.ADR(acc_sin);
  size := SIZE(acc_sin);
  Socket := sct.accept(ListenSocket, psock_addr^, size);
  IF Socket = sct.INVALID_SOCKET THEN RETURN msg.Wrong_socket; END;
  Connected := TRUE;
  channel := Socket;
  RETURN rmt.Connection_established;
END MasterListen;


PROCEDURE ServerConnect (channel: rmt.TRN_CHANNEL): CARDINAL;
BEGIN
  IF channel = sct.INVALID_SOCKET THEN
    RETURN rmt.Wrong_socket;
  END;
  IF NOT Connected THEN
    Connected := TRUE;
  END;
  Socket := sct.SOCKET (channel);
  RETURN rmt.Connection_established;
END ServerConnect;



PROCEDURE ClientConnect (remote_host-: ARRAY OF CHAR; port: rmt.TRN_PORT): CARDINAL;
VAR
  Dest_sin  : sct.SOCKADDR_IN;
  psock_addr: sct.PSOCKADDR;
  Phe       : sct.PHOSTENT;

  buf: ARRAY [0..255] OF CHAR;

BEGIN
  IF Connected THEN
    RETURN msg.Connection_already_established;
  END;
  IF port = 0 THEN
    port := DEFAULT_PORT_NO;
  END;

  COPY(remote_host, buf);
  WITH Dest_sin DO
    sin_family := sct.AF_INET;
    Phe := sct.gethostbyname(sys.ADR(buf));
    IF Phe = NIL THEN RETURN msg.Host_unknown; END;
    sys.MOVE(sys.ADR(Phe^.h_addr_list^^), sys.ADR(sin_addr), Phe^.h_length);
    sin_port := sct.bswap(port);
  END;
  Socket := sct.socket(sct.PF_INET, sct.SOCK_STREAM, 0);
  IF Socket = sct.INVALID_SOCKET THEN RETURN msg.Wrong_socket; END;
  psock_addr := sys.ADR(Dest_sin);
  IF sct.connect(Socket, psock_addr^, SIZE(Dest_sin)) < 0 THEN RETURN msg.Connection_not_established; END;
  Connected := TRUE;
  RETURN 0;
END ClientConnect;


PROCEDURE Disconnect;
BEGIN
  IF Connected THEN
    IF ListenSocket # sct.INVALID_SOCKET THEN
      sct.soclose(ListenSocket);
      ListenSocket := sct.INVALID_SOCKET;
    END;
    sct.soclose(Socket);
    Socket := sct.INVALID_SOCKET;
    Connected := FALSE;
  END;
END Disconnect;


PROCEDURE Send (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN;
BEGIN
  RETURN Connected AND (sct.send(Socket, addr, VAL(INTEGER, size), 0) = VAL(INTEGER, size));
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
    s2 := sct.recv(Socket, sys.ADDADR(addr, VAL(CARDINAL, s1)), VAL(INTEGER, size-s1), 0);
    IF s2 < 0 THEN RETURN FALSE; END;
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
  ListenSocket := sct.INVALID_SOCKET;
  Socket := sct.INVALID_SOCKET;
  Connected := FALSE;
END Trans.
