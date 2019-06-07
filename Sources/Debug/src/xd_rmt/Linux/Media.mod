<* storage+ *>

IMPLEMENTATION MODULE Media;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT rmt := RemTypes;
IMPORT msg := MsgNo;
IMPORT opt := Options;
IMPORT xs  := xStr;

IMPORT Trans; -- Direct use Trans

FROM Printf IMPORT printf;

VAR
  Transport: rmt.TRANSPORT;


PROCEDURE Init (): CARDINAL;
BEGIN
  printf (">>> Init\n");
  IF Transport.id = opt.RemoteTransport THEN
    RETURN 0;
  ELSIF Transport.id # "" THEN
    Disconnect;
  END;
  COPY (opt.RemoteTransport, Transport.id);
  xs.Uppercase (Transport.id);
  IF NOT Trans.InitTransport (Transport) THEN
    Transport.id := '';
    RETURN msg.Wrong_initialization_transport;
  END;
  RETURN 0;
END Init;



PROCEDURE rmt2msg (rmt_no: CARDINAL): CARDINAL;
BEGIN
  CASE rmt_no OF
  | rmt.Connection_established         : RETURN 0;
  | rmt.Host_unknown                   : RETURN msg.Host_unknown;
  | rmt.Wrong_socket                   : RETURN msg.Wrong_socket;
  | rmt.Connection_not_established     : RETURN msg.Connection_not_established;
  | rmt.Wrong_identification           : RETURN msg.Wrong_identification;
  | rmt.Connection_already_established : RETURN msg.Connection_already_established;
  | rmt.Over_max_pending_connects      : RETURN msg.Over_max_pending_connects;
  | rmt.Wrong_connection               : RETURN msg.Wrong_connection;
  | rmt.Unknown_transport              : RETURN msg.Unknown_transport;
  | rmt.Wrong_initialization_transport : RETURN msg.Wrong_initialization_transport;
  | rmt.DLL_transport_not_found        : RETURN msg.DLL_transport_not_found;
  | rmt.Wrong_format_DLL_transport     : RETURN msg.Wrong_format_DLL_transport;
  | rmt.Something_error                : RETURN msg.Something_error;
  ELSE
    RETURN MAX(CARDINAL);
  END;
END rmt2msg;  


PROCEDURE MasterListen (port: rmt.TRN_PORT; VAR channel: rmt.TRN_CHANNEL): CARDINAL;
VAR
  rc: CARDINAL;
  cli_key : xs.String;
  ver_key : sys.CARD8;

BEGIN
  printf (">>> MasterListen\n");
  rc := Init();
  IF rc # 0 THEN
    RETURN rc;
  END;

  printf ("  Calling Transport.mst_listen...\n");
  rc := Transport.mst_listen (port, channel);
  IF rc # 0 THEN
    RETURN rmt2msg(rc);
  END;
  ASSERT (channel # rmt.INVALID_CHANNEL);

  IF NOT SendStr (rmt.ServerKey)
  OR NOT ReceiveStr (cli_key) OR NOT (cli_key = rmt.ClientKey)
  OR NOT SendB (rmt.VersionKey)
  OR NOT ReceiveB(ver_key) AND (ver_key = rmt.VersionKey)
  THEN
    RETURN msg.Wrong_identification;
  END;

  RETURN 0;
END MasterListen;



PROCEDURE ServerConnect (channel: rmt.TRN_CHANNEL): CARDINAL;
VAR
  rc: CARDINAL;
BEGIN
  IF channel = rmt.INVALID_CHANNEL THEN
    RETURN msg.Wrong_socket;
  END;

  rc := Init();
  IF rc # 0 THEN
    RETURN rc;
  END;

  rc := Transport.srv_connect (channel);
  IF rc # 0 THEN
    RETURN rmt2msg (rc);
  END;

  IF NOT SendReady() THEN
    RETURN msg.Wrong_identification;
  END;

  RETURN 0;
END ServerConnect;


PROCEDURE ClientConnect (remote_host: ARRAY OF CHAR; port: rmt.TRN_PORT): CARDINAL;
VAR
  rc : CARDINAL;

  srv_key: xs.String;
  ver_key: sys.CARD8;

BEGIN
  rc := Init();
  IF rc # 0 THEN
    RETURN rc;
  END;

  rc := Transport.cli_connect (remote_host, port);
  IF rc # 0 THEN
    RETURN rmt2msg(rc);
  END;

  IF NOT ReceiveStr (srv_key) OR NOT (srv_key = rmt.ServerKey)
  OR NOT SendStr (rmt.ClientKey)
  OR NOT ReceiveB (ver_key) AND (ver_key = rmt.VersionKey)
  OR NOT SendB (rmt.VersionKey)
  OR NOT ReceiveReady()
  THEN
    RETURN msg.Wrong_identification;
  END;

  RETURN 0;
END ClientConnect;


PROCEDURE Disconnect;
BEGIN
  IF IsConnected() THEN
    Transport.disconnect();
    Transport.id := '';
  END;
END Disconnect;




CONST
  INITIAL_CACHE_SIZE = 02000H;

TYPE
  CACHE = POINTER TO ARRAY OF sys.CARD8;

VAR
  CacheBuf : CACHE;
  CacheMax : CARDINAL;
  CachePos : CARDINAL;
  CacheSend: CARDINAL;

PROCEDURE Reset;
BEGIN
  CacheMax  := 0;
  CachePos  := 0;
  CacheSend := 0;
END Reset;


PROCEDURE Receive (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN;
VAR
  max: CARDINAL;
BEGIN
  max := HIGH(CacheBuf^)+1;
  WHILE size > max DO
    IF NOT Transport.receive (addr, max) THEN
      RETURN FALSE;
    END;
    DEC (size, max);
    addr := sys.ADDADR(addr, max);
  END;
  RETURN Transport.receive (addr, size);
END Receive;





PROCEDURE send_desc (desc: rmt.DATA_DESC): BOOLEAN;
BEGIN
  ASSERT(CacheMax = 0);
  CacheMax  := SIZE(desc) + VAL (CARDINAL, desc.len);
  CachePos  := 0;
  CacheSend := 0;
  RETURN Send (sys.ADR(desc), SIZE(desc));
END send_desc;


PROCEDURE receive_desc (VAR desc: rmt.DATA_DESC): BOOLEAN;
BEGIN
  RETURN Receive(sys.ADR(desc), SIZE(rmt.DATA_DESC));
END receive_desc;


PROCEDURE Send (addr: sys.ADDRESS; size: CARDINAL): BOOLEAN;
VAR
  error: BOOLEAN;

  MODULE Exit;
  IMPORT error, Reset;
  BEGIN
  FINALLY
    IF error THEN Reset; END;
  END Exit;

VAR
  sz, max: CARDINAL;

BEGIN
  error := TRUE;
  ASSERT(CacheMax # 0);
  max := HIGH(CacheBuf^)+1;
  IF CachePos+size > max THEN
    IF NOT Transport.send (sys.ADR(CacheBuf^[0]), CachePos) THEN RETURN FALSE; END;
    CachePos := 0;
    IF size > max THEN
      sz := size;
      LOOP
        IF NOT Transport.send (addr, max) THEN
          RETURN FALSE;
        END;
        addr := sys.ADDADR(addr, max);
        DEC(sz, max);
        IF sz < max THEN
          EXIT;
        END;
      END;
      IF NOT Transport.send (addr, sz) THEN
        RETURN FALSE;
      END;
      INC (CacheSend, size);
    ELSE
      CachePos := size;
      sys.MOVE(addr, sys.ADR(CacheBuf^[0]), CachePos);
    END;
  ELSE
    sys.MOVE(addr, sys.ADR(CacheBuf^[CachePos]), size);
    INC(CachePos, size);
  END;
  IF CacheSend+size < CacheMax THEN
    error := FALSE;
    INC(CacheSend, size);
    RETURN TRUE;
  ELSIF CacheSend+size = CacheMax THEN
    RETURN Transport.send (sys.ADR(CacheBuf^[0]), CachePos);
  ELSIF CacheSend = CacheMax THEN
    RETURN TRUE;
  ELSIF CacheSend+size > CacheMax THEN
    ASSERT(FALSE);
  END;
END Send;



PROCEDURE SendReady (): BOOLEAN;
BEGIN
  RETURN send_desc(rmt.DATA_DESC{ rmt.ready, 0 } );
END SendReady;


PROCEDURE ReceiveReady (): BOOLEAN;
VAR
  desc: rmt.DATA_DESC;
BEGIN
  RETURN receive_desc(desc) AND (desc.sort = rmt.ready) AND (desc.len = 0);
END ReceiveReady;


PROCEDURE SendB (c: sys.CARD8): BOOLEAN;
BEGIN
  RETURN send_desc(rmt.DATA_DESC{ rmt.data, 1 } ) AND Send (sys.ADR(c), 1);
END SendB;


PROCEDURE SendW (c: sys.CARD16): BOOLEAN;
BEGIN
  RETURN send_desc(rmt.DATA_DESC{ rmt.data, 2}) AND Send (sys.ADR(c), 2);
END SendW;


PROCEDURE SendDW (c: sys.CARD32): BOOLEAN;
BEGIN
  RETURN send_desc(rmt.DATA_DESC{ rmt.data, 4}) AND Send (sys.ADR(c), 4);
END SendDW;


PROCEDURE ReceiveB (VAR c: sys.CARD8): BOOLEAN;
VAR
  desc: rmt.DATA_DESC;
BEGIN
  RETURN receive_desc(desc) AND
         (desc.sort = rmt.data) AND (desc.len = 1) AND
         Receive (sys.ADR(c), 1);
END ReceiveB;


PROCEDURE ReceiveW (VAR c: sys.CARD16): BOOLEAN;
VAR
  desc: rmt.DATA_DESC;
BEGIN
  RETURN receive_desc(desc) AND
         (desc.sort = rmt.data) AND (desc.len = 2) AND
         Receive (sys.ADR(c), 2);
END ReceiveW;


PROCEDURE ReceiveDW (VAR c: sys.CARD32): BOOLEAN;
VAR
  desc: rmt.DATA_DESC;
BEGIN
  RETURN receive_desc(desc) AND
         (desc.sort = rmt.data) AND (desc.len = 4) AND
         Receive (sys.ADR(c), 4);
END ReceiveDW;


PROCEDURE SendStr (str-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN send_desc (rmt.DATA_DESC{ rmt.string, LENGTH(str)+1}) AND
         Send (sys.ADR(str), LENGTH(str)+1);
END SendStr;


PROCEDURE ReceiveStr (VAR str: ARRAY OF CHAR): BOOLEAN;
VAR
  desc: rmt.DATA_DESC;
BEGIN
  RETURN receive_desc(desc) AND
         (desc.sort = rmt.string) AND (desc.len <= VAL(INTEGER, HIGH(str)+1)) AND
         Receive (sys.ADR(str), desc.len);
END ReceiveStr;


PROCEDURE SendCommand (CmdCode: rmt.COMMAND; CmdLen: CARDINAL): BOOLEAN;
VAR
  code: sys.CARD16;
BEGIN
  code := ORD(CmdCode);
  RETURN send_desc(rmt.DATA_DESC{ rmt.command, 2+CmdLen}) AND
         Send (sys.ADR(code), 2);
END SendCommand;


PROCEDURE SendResult (rc: CARDINAL; size: CARDINAL): BOOLEAN;
BEGIN
  RETURN send_desc (rmt.DATA_DESC{ rmt.result, 4+size}) AND
         Send (sys.ADR(rc), 4);
END SendResult;


PROCEDURE ReceiveResult (VAR rc: CARDINAL; VAR size: CARDINAL): BOOLEAN;
VAR
  desc: rmt.DATA_DESC;
BEGIN
  IF NOT receive_desc(desc) OR (desc.sort # rmt.result) THEN RETURN FALSE; END;
  IF desc.len = 4 THEN
    size := 0;
    RETURN Receive (sys.ADR(rc), 4);
  ELSIF desc.len > 4 THEN
    IF NOT Receive (sys.ADR(rc), 4) THEN RETURN FALSE; END;
    size := desc.len-4;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ReceiveResult;


PROCEDURE ReceiveCommand (VAR arg_size: CARDINAL): rmt.COMMAND;
VAR
  desc: rmt.DATA_DESC;
  c   : sys.CARD16;
BEGIN
  c := ORD(MIN(rmt.COMMAND));
  arg_size := 0;
  IF receive_desc(desc) AND (desc.sort = rmt.command) AND (desc.len >= 2) THEN
    IF Receive (sys.ADR(c), 2) AND (c <= ORD(MAX(rmt.COMMAND))) THEN
      arg_size := desc.len-2;
    END;
  END;
  RETURN VAL(rmt.COMMAND, c);
END ReceiveCommand;


PROCEDURE IsConnected (): BOOLEAN;
BEGIN
  RETURN Transport.id # '';
END IsConnected;


BEGIN
  Transport := rmt.EmptyTransport;
  NEW (CacheBuf, INITIAL_CACHE_SIZE);
  ASSERT (CacheBuf # NIL);
  CacheMax  := 0;
  CachePos  := 0;
  CacheSend := 0;
END Media.
