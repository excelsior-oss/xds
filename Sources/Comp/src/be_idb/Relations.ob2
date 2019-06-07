MODULE Relations;
IMPORT
  sf:=SeqFile,
  RawIO;

TYPE

RELATION_REC* = RECORD
 rel_type* :INTEGER;
 file_id*  :INTEGER;
 seq_no*   :LONGINT;
 entity_id*:LONGINT;
END;

REL_ARRAY=POINTER TO ARRAY OF RELATION_REC;
REL_LIST*=RECORD
 a* :REL_ARRAY;
 len-:LONGINT; (*Number of stored elements, NOT the length of a*)
END;

(*--------------------------REL LIST-------------------------------------*)

PROCEDURE (VAR rl: REL_LIST)ini_by_len*(len : LONGINT);
BEGIN
  rl.len:=0;
  IF len=0 THEN
    rl.a := NIL;
  ELSE
    NEW(rl.a,len);
  END;
END ini_by_len;

PROCEDURE (VAR rl: REL_LIST)add*(R:RELATION_REC);
VAR
  t:REL_ARRAY;
  i:LONGINT;
BEGIN 		
  IF rl.a=NIL THEN NEW(rl.a,8);END;	
  IF LEN(rl.a^)=rl.len THEN
      NEW(t,2*LEN(rl.a^));
      FOR i:=0 TO LEN(rl.a^)-1 DO t[i]:=rl.a[i]; END;
      rl.a:=t;
  END;
  rl.a[rl.len]:=R;
  INC(rl.len);
END add;
						
PROCEDURE (VAR rl: REL_LIST)get_elem*(n:LONGINT): RELATION_REC;
BEGIN
  RETURN  rl.a[n];
END get_elem;

PROCEDURE (VAR rl: REL_LIST)set_elem*(n:LONGINT;r:RELATION_REC);
BEGIN
  rl.a[n]:=r;
END set_elem;


PROCEDURE (VAR rl: REL_LIST)index_of*(type : INTEGER) : INTEGER;
(*index of element with specified type. -1 if not found*)
VAR
  i, inx : INTEGER;
BEGIN
  inx := -1;
  FOR i := 0 TO rl.len -1 DO
    IF rl.a[i].rel_type = type THEN
      inx := i;
    END;
  END;
  RETURN inx;
END index_of;



PROCEDURE (VAR rl: REL_LIST)save_to_file*(cid:sf.ChanId);
VAR
  i:LONGINT;
BEGIN
  RawIO.Write(cid, rl.len);
  FOR i:=0 TO rl.len-1 DO
    RawIO.Write(cid, rl.a[i])
  END;
END save_to_file;

PROCEDURE (VAR rl: REL_LIST)load_from_file*(cid:sf.ChanId);
VAR
  len:LONGINT;(*ATTENTION len and rl.len must have the same type*)
BEGIN
  RawIO.Read(cid,len);
  rl.ini_by_len(len);
  IF len <> 0 THEN RawIO.Read(cid,rl.a^); END;
  rl.len := len;
END load_from_file;

PROCEDURE (VAR rl: REL_LIST)exi*;
BEGIN
  rl.len:=0;
  rl.a:=NIL;
END exi;

END Relations.