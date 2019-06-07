(* Long constants enumeration
*)
<* +WOFF301 *>
MODULE LCNumer;

TYPE LCNumer_IDB     *= POINTER TO LCNumer_IDB_Rec;
     LCNumer_IDB_Rec *= RECORD END;

PROCEDURE(i : LCNumer_IDB) ProcessNodes*(recalc_profits : BOOLEAN);
BEGIN
  ASSERT(FALSE);
END ProcessNodes;

(* обход всех деревяшек, нахождение и нумерация длинных констант,
   инициализация LongConsts
*)
PROCEDURE(i : LCNumer_IDB) MakeLCInfo*();
BEGIN
  ASSERT(FALSE);
END MakeLCInfo;

VAR IDB *: LCNumer_IDB;
BEGIN
  NEW(IDB);
END LCNumer.
