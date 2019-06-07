MODULE StringListHash;
IMPORT
  ls := lists,
  str:= Strings,
  SYSTEM,
  DStrings;

CONST
 NameHashSz* = 65536 DIV 1;

TYPE
NameHash_REC*=RECORD
  data : ARRAY NameHashSz OF  LONGINT;
END;

(*--------------------------HashTable-------------------------------------*)

PROCEDURE get_key* (sl: ls.STRING_LIST) :LONGINT;
VAR
  key : LONGINT;
  len : INTEGER;
  i,j : INTEGER;
  s   : DStrings.String;
BEGIN
  key := 0;
  FOR i := 0 TO sl.len -1 DO
    s := sl.get_elem(i);
    len :=  SYSTEM.VAL(INTEGER , str.Length( s^ ) ) ;
    FOR j:= 0 TO len -1  DO
      key := 31* key;
      key := key + ( ORD( s[j] ) MOD 31 );
      key := key MOD NameHashSz;
    END;
  END;

  FOR i := sl.len -1 TO 0 BY -1 DO
    s := sl.get_elem(i);
    len :=  SYSTEM.VAL(INTEGER , str.Length( s^ ) ) ;
    FOR j:= len -1 TO 0 BY -1 DO
      key := 7* key;
      key := key + ( ORD( s[j] ) MOD 3 );
      key := key MOD NameHashSz;
    END;
  END;

  FOR i := sl.len -1 TO 0 BY -1 DO
    s := sl.get_elem(i);
    len :=  SYSTEM.VAL(INTEGER , str.Length( s^ ) ) ;
    FOR j:= len -2 TO 0 BY -1 DO
      key := 3* key;
      key := key + (  ORD( s[ j ] ) MOD 3 );
      key := key MOD NameHashSz + 1 ;
    END;
  END;


  FOR i := sl.len -1 TO 0 BY -1 DO
    s := sl.get_elem(i);
    len :=  SYSTEM.VAL(INTEGER , str.Length( s^ ) ) ;
    FOR j:=  len - 2  TO 0 BY -1 DO
      key := key +  ORD( s[j] ) MOD 2 ;
      key := key MOD NameHashSz ;
    END;
  END;

  RETURN key ;
END get_key;

PROCEDURE (VAR nh: NameHash_REC) add*(sl: ls.STRING_LIST; id :LONGINT);
BEGIN
  nh.data[ get_key(sl) ] := id;
END add;

PROCEDURE (VAR nh: NameHash_REC) get*(sl: ls.STRING_LIST) :LONGINT;
BEGIN
  RETURN nh.data[ get_key(sl) ] ;
END get;


END StringListHash.