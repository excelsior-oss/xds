MODULE StrSer ;(* Strings serialization*)
IMPORT
  DStrings,
  IOChan,
  RawIO;


PROCEDURE Write_xfs_string*(cid : IOChan.ChanId; s : DStrings.String);
VAR
  i : INTEGER;
BEGIN
  IF s <> NIL THEN
    FOR i := 0 TO LENGTH(s^)-1 DO
      RawIO.Write(cid,s^[i]);
    END;
  END;
  RawIO.Write(cid,0C);
END Write_xfs_string;

PROCEDURE Read_xfs_string*(cid : IOChan.ChanId) : DStrings.String;
VAR
  buf : ARRAY 1000 OF CHAR;
  s   : DStrings.String;
  i   : INTEGER;
BEGIN
  i := 0;
  RawIO.Read(cid,buf[0]);
  IF buf[0] = 0C THEN RETURN NIL; END;
  WHILE buf[i] <> 0C DO
    INC(i);
    RawIO.Read(cid,buf[i]);
  END;
  DStrings.Assign(buf,s);
  RETURN s;
END Read_xfs_string;

END StrSer .