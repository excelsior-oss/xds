MODULE Attributes;
IMPORT
  DStrings,
  sf:=SeqFile,
  RawIO,
  TextIO,
  WholeIO,
  IOResult,
  StrSer;


CONST
  (* ATTR_DESC domain constants*)
  ad_string*=2;
  ad_int*=1;
  ad_boolean*=0;
  ad_finite={ad_int,ad_boolean};

TYPE
ATTR_REC* =RECORD
  type*:INTEGER;    (*index in attr_types[]*)
  value:LONGINT;
  str_value:DStrings.String;
END;

ATTR_ARRAY=POINTER TO ARRAY OF ATTR_REC;
ATTR_LIST*=RECORD
  a* : ATTR_ARRAY;
  len-:LONGINT; (*Number of stored elements, NOT the length of a*)
END;

ATTR_DESC*=RECORD
  attr_type_name*:ARRAY 32 OF CHAR;
  domain*:SHORTINT;(*see ad_* constants *)
END;


VAR
  attr_types*     : POINTER TO ARRAY OF ATTR_DESC;

(*--------------------------ATTR_LIST-------------------------------------*)

PROCEDURE (VAR al: ATTR_LIST)ini_by_len*(len : LONGINT);
BEGIN
  al.len:=0;
  IF len = 0 THEN
    al.a := NIL;
  ELSE
    NEW(al.a,len);
  END;
END ini_by_len;

PROCEDURE (VAR al: ATTR_LIST)get_elem*(n:LONGINT): ATTR_REC;
BEGIN
  RETURN  al.a[n];
END get_elem;

PROCEDURE (VAR al: ATTR_LIST)set_elem*(n:LONGINT;v:ATTR_REC);
BEGIN
  al.a[n]:=v;
END set_elem;

PROCEDURE (VAR al: ATTR_LIST)index_of*(type : INTEGER) : INTEGER;
(*index of element with specified type. -1 if not found*)
VAR
  i, inx : INTEGER;
BEGIN
  inx := -1;
  FOR i := 0 TO al.len -1 DO
    IF al.a[i].type = type THEN
      inx := i;
    END;
  END;
  RETURN inx;
END index_of;



PROCEDURE (VAR al: ATTR_LIST)exi*;
BEGIN
  al.len := 0;
  al.a   := NIL;
END exi;

PROCEDURE (VAR al: ATTR_LIST)add*(l:ATTR_REC);
VAR
  t : ATTR_ARRAY;
  i : LONGINT;
BEGIN 		
  IF al.a = NIL THEN NEW(al.a,1);END;	
  IF LEN(al.a^) = al.len THEN
    NEW( t, 2*LEN(al.a^) );
    FOR i := 0 TO LEN(al.a^)-1 DO t[i] := al.a[i]; END;
    al.a := t;
  END;
  al.a[al.len] := l;
  INC(al.len);
END add;


PROCEDURE (VAR al: ATTR_LIST)save_to_file*(cid:sf.ChanId);
VAR
  i  : LONGINT;
  at : ATTR_REC;
BEGIN
  RawIO.Write(cid, al.len);
  FOR i:=0 TO al.len-1 DO
    at:=al.a[i];
    RawIO.Write(cid,at.type);
    IF attr_types[at.type].domain IN ad_finite THEN
      RawIO.Write(cid,at.value);
    ELSE
      StrSer.Write_xfs_string(cid,at.str_value);
    END;
  END;
END save_to_file;

PROCEDURE (VAR al: ATTR_LIST)load_from_file*(cid:sf.ChanId);
VAR
  i,len: LONGINT;
  buf  : ATTR_REC;(*ATTENTION len and al.len must have the same type*)
BEGIN
  RawIO.Read(cid,len);
  al.ini_by_len(len);
  FOR i:=0 TO len-1 DO
    RawIO.Read(cid,buf.type);
    IF attr_types[buf.type].domain IN ad_finite THEN
      RawIO.Read(cid,buf.value);
    ELSE
      buf.str_value:=StrSer.Read_xfs_string(cid);
    END;
    al.add(buf);
  END;
END load_from_file;

(*--------------------------ATTR_REC-------------------------------------*)
PROCEDURE (VAR at:ATTR_REC)is_boolean*():BOOLEAN;
BEGIN
  RETURN attr_types[at.type].domain=ad_boolean;
END is_boolean;

PROCEDURE (VAR at:ATTR_REC)is_int*():BOOLEAN;
BEGIN
  RETURN attr_types[at.type].domain=ad_int;
END is_int;

PROCEDURE (VAR at:ATTR_REC)is_string*():BOOLEAN;
BEGIN
  RETURN attr_types[at.type].domain=ad_string;
END is_string;



PROCEDURE (VAR at:ATTR_REC)get_bool_val*():BOOLEAN;
BEGIN
  ASSERT( at.is_boolean() );
  RETURN at.value=1;
END get_bool_val;

PROCEDURE (VAR at:ATTR_REC)set_bool_val*(v:BOOLEAN);
BEGIN
  ASSERT( at.is_boolean() );
  IF v THEN
    at.value:=1;
  ELSE
    at.value:=0;
  END;
END set_bool_val;


PROCEDURE (VAR at:ATTR_REC)set_int_val*(v:LONGINT);
BEGIN
  ASSERT( attr_types[at.type].domain=ad_int );
  at.value:=v;
END set_int_val;

PROCEDURE (VAR at:ATTR_REC)get_int_val*():LONGINT;
BEGIN
  ASSERT( attr_types[at.type].domain=ad_int );
  RETURN at.value;
END get_int_val;

PROCEDURE (VAR at:ATTR_REC)set_str_val*(s: ARRAY OF CHAR);
VAR
  i : INTEGER;
BEGIN
  FOR i := 0 TO LEN(s) -1 DO
    IF s[i] = '"' THEN s[i] := "'"; END;
  END;
  ASSERT(attr_types[at.type].domain=ad_string);
  DStrings.Assign(s,at.str_value);
END set_str_val;

PROCEDURE (VAR at:ATTR_REC)get_str_val*():DStrings.String;
BEGIN
  ASSERT(attr_types[at.type].domain=ad_string);
  RETURN at.str_value;
END get_str_val;

END Attributes.