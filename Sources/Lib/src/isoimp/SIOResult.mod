(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE SIOResult;

IMPORT  IOChan, StdChans;

PROCEDURE ReadResult(): ReadResults;
BEGIN
  RETURN IOChan.ReadResult(StdChans.InChan())
END ReadResult;

END SIOResult.

