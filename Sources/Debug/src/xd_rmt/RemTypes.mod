IMPLEMENTATION MODULE RemTypes;

<* IF env_target = 'x86os2' THEN *>
<* ELSIF env_target = 'x86nt' THEN *>

IMPORT ws := WinSock;

<* END *>


BEGIN
<* IF env_target = 'x86os2' THEN *>
  INVALID_CHANNEL := MAX(CARDINAL);
<* ELSIF env_target = 'x86nt' THEN *>
  INVALID_CHANNEL := TRN_CHANNEL (ws.INVALID_SOCKET);
<* END *>
END RemTypes.