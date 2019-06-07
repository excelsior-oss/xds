MODULE lists; (* serializable lists of LONGINT and String are declared and implemented here*)
IMPORT
  DStrings,
  sf:=SeqFile,
  RawIO,
  TextIO,
  WholeIO,
  IOResult,
  StrSer;

TYPE
LONGINT_ARRAY = POINTER TO ARRAY OF LONGINT;

LONGINT_LIST*=RECORD
  a : LONGINT_ARRAY;
  len-:LONGINT; (*Number of stored elements, NOT the length of a*)
END;

STRING_ARRAY  = POINTER TO ARRAY OF DStrings.String;

STRING_LIST*=RECORD
  a :STRING_ARRAY;
  len-:INTEGER; (*Number of stored elements, NOT the length of a*)
END;



(*--------------------------LONGINT LIST-------------------------------------*)

PROCEDURE (VAR ll: LONGINT_LIST)ini_by_len*(len : LONGINT);
BEGIN
  ll.len:=0;
  IF len=0 THEN
    ll.a := NIL;
  ELSE
    NEW(ll.a,len);
  END;
END ini_by_len;


PROCEDURE (VAR ll: LONGINT_LIST)get_elem*(n:LONGINT): LONGINT;
BEGIN
  RETURN  ll.a[n];
END get_elem;

PROCEDURE (VAR ll: LONGINT_LIST)set_elem*(n:LONGINT;v:LONGINT);
BEGIN
  ll.a[n]:=v;
END set_elem;

PROCEDURE (VAR ll: LONGINT_LIST)exi*;
BEGIN
  ll.len:=0;
  ll.a:=NIL;
END exi;

PROCEDURE (VAR ll: LONGINT_LIST)index_of*(v : INTEGER) : INTEGER;
(*index of element with specified value. -1 if not found*)
VAR
  i, inx : INTEGER;
BEGIN
  inx := -1;
  FOR i := 0 TO ll.len -1 DO
    IF ll.a[i] = v THEN
      inx := i;
    END;
  END;
  RETURN inx;
END index_of;

PROCEDURE (VAR ll: LONGINT_LIST)add*(l:LONGINT);
VAR
  t: LONGINT_ARRAY;
  i: LONGINT;
BEGIN 		
  IF ll.a=NIL THEN NEW(ll.a,3);END;	
  IF LEN(ll.a^)=ll.len THEN
      NEW(t,2*LEN(ll.a^));
      FOR i:=0 TO LEN(ll.a^)-1 DO t[i]:=ll.a[i]; END;
      ll.a:=t;
  END;
  ll.a[ll.len]:=l;
  INC(ll.len);
END add;


PROCEDURE (VAR ll: LONGINT_LIST)save_to_file*(cid:sf.ChanId);
VAR
  i:LONGINT;
BEGIN
  RawIO.Write(cid, ll.len);
  FOR i:=0 TO ll.len-1 DO
   RawIO.Write(cid,ll.a[i]);
  END;
END save_to_file;

PROCEDURE (VAR ll: LONGINT_LIST)load_from_file*(cid:sf.ChanId);
VAR
  len : LONGINT;(*ATTENTION len and ll.len must have the same type*)
BEGIN
  RawIO.Read(cid,len);
  ll.ini_by_len(len);
  IF len <> 0 THEN RawIO.Read(cid,ll.a^); END;
  ll.len := len;
END load_from_file;


(*-------------------------- STRING LIST-------------------------------------*)


PROCEDURE (VAR sl: STRING_LIST)ini_by_len*(len : LONGINT);
BEGIN
  sl.len:=0;
  IF len=0 THEN
    sl.a:=NIL;
  ELSE
    NEW(sl.a,len);
  END;
END ini_by_len;


PROCEDURE (VAR sl: STRING_LIST)add*(E:DStrings.String);
VAR
  t:STRING_ARRAY;
  i:INTEGER;
BEGIN 		
  IF sl.a=NIL THEN NEW(sl.a,4);END;	
  IF LEN(sl.a^)=sl.len THEN
    NEW(t,2*LEN(sl.a^));
    FOR i:=0 TO LEN(sl.a^)-1 DO t[i]:=sl.a[i]; END;
    sl.a:=t;
  END;
  sl.a[sl.len]:=E;
  INC(sl.len);
END add;
						
PROCEDURE (VAR sl: STRING_LIST)get_elem*(n:INTEGER): DStrings.String;
BEGIN
  RETURN  sl.a[n];
END get_elem;



PROCEDURE (VAR sl: STRING_LIST)find_string*(s-:DStrings.String):INTEGER;
VAR
  i:INTEGER;
BEGIN
  FOR i:=0 TO sl.len-1  DO
    IF sl.a[i]^ = s^ THEN RETURN i;END;
  END;
  RETURN -1;
END find_string;

PROCEDURE (VAR sl: STRING_LIST)save_to_file*(cid:sf.ChanId);
VAR i:INTEGER;
BEGIN
  RawIO.Write(cid, sl.len);
  FOR i:=0 TO sl.len-1 DO
    StrSer.Write_xfs_string(cid,sl.a[i]);
  END;
END save_to_file;

PROCEDURE (VAR sl: STRING_LIST)load_from_file*(cid:sf.ChanId);
VAR
  i, len:INTEGER;(*ATTENTION len and sl.len must have the same type*)
BEGIN
  RawIO.Read(cid,len);
  sl.ini_by_len(len);
  FOR i:=0 TO len-1 DO
    sl.add(StrSer.Read_xfs_string(cid));
  END;
END load_from_file;


PROCEDURE (VAR sl: STRING_LIST)exi*;
BEGIN
  sl.len:=0;
  sl.a:=NIL;
END exi;

PROCEDURE compare_string_lists*(sl1-: STRING_LIST; sl2-: STRING_LIST):BOOLEAN;
VAR
  i:INTEGER;
  s1,s2:DStrings.String;
BEGIN
  IF sl1.len#sl2.len THEN RETURN FALSE;END;
  FOR i := 0 TO sl1.len-1 DO
    s1:=sl1.a[i];
    s2:=sl2.a[i];
    IF (((s1=NIL) OR (s2=NIL)) AND (s1#s2)) OR (s1^ # s2^) THEN RETURN FALSE; END;
  END;
  RETURN TRUE;
END compare_string_lists;

END lists.