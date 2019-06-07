
<* Storage+ *>

IMPLEMENTATION MODULE DI_Tools;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT dt  := DI_Types;
IMPORT bld := DI_Build;

IMPORT kt  := KrnTypes;

IMPORT xs  := xStr;
IMPORT sor := Sort;
IMPORT txt := Texts;
IMPORT fil := File;
IMPORT Ud  := Unidata;


<* IF DEST_XDS THEN *>

IMPORT opt := Options;
IMPORT trn := Translit;

<* END *>

IMPORT cdi := DI_Read;



PROCEDURE CheckDebugInfoForModule (com: dt.ComNo; mod: dt.ModNo): BOOLEAN;
BEGIN
  IF IsDataValid (com, mod) AND (mod # dt.Fake_Module) THEN
    WITH dt.Components DO
      WITH Components^[com] DO
        WITH DI.Modules^[mod-1] DO
          IF (DebugInfoReference # NIL) AND NOT DebugInfoProcessed THEN
            IF cdi.CheckDebugInfoVersion (EI) THEN
              cdi.BuildForMod (mod-1, com, Components^[com]);
            END;
          END;
          RETURN HasInfo;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END CheckDebugInfoForModule;



PROCEDURE AddAdr (addr: kt.ADDRESS; offs: CARDINAL): kt.ADDRESS;
BEGIN
  RETURN kt.ADDRESS(CARDINAL(addr)+offs);
END AddAdr;


PROCEDURE SubAdr (addr: kt.ADDRESS; offs: CARDINAL): kt.ADDRESS;
BEGIN
  RETURN kt.ADDRESS(CARDINAL(addr)-offs);
END SubAdr;


PROCEDURE DifAdr (addr1, addr2: kt.ADDRESS): sys.INT32;
BEGIN
  IF addr1 > addr2 THEN
    RETURN sys.INT32(CARDINAL(addr1)-CARDINAL(addr2));
  ELSE
    RETURN -sys.INT32(CARDINAL(addr2)-CARDINAL(addr1));
  END;
END DifAdr;






PROCEDURE FindComponentByHandle (Handle: CARDINAL; VAR com: CARDINAL): BOOLEAN;
BEGIN
  WITH dt.Components DO
    IF (Components # NIL) AND (Count # 0) THEN
      com := 0;
      LOOP
        IF Handle = Components^[com].EI.Handle THEN
          RETURN TRUE;
        END;
        INC(com);
        IF com = Count THEN EXIT; END;
      END;
    END;
  END;
  com := dt.Invalid_Component;
  RETURN FALSE;
END FindComponentByHandle;


PROCEDURE RemoveComponent (Handle: CARDINAL): BOOLEAN;
VAR
  com, i: CARDINAL;
BEGIN
  IF FindComponentByHandle (Handle, com) THEN
    WITH dt.Components DO
      WITH Components^[com] DO

        IF EI.Objects # NIL THEN DISPOSE(EI.Objects); END;
        ClearDebugInfo(com);
      END;
      FOR i := 1 TO Count-1-com DO
        Components^[com+i-1] := Components^[com+i];
      END;
      DEC(Count);
    END;
  END;
  RETURN TRUE;
END RemoveComponent;


PROCEDURE ClearComponents (all: BOOLEAN);
VAR
  i, k : CARDINAL;
BEGIN
  WITH dt.Components DO
    IF Count = 0 THEN RETURN; END;
    IF all THEN
       k := 0;
       Ud.ClearData(bld.Names);
       Ud.ClearData(bld.TypesTbl);
    ELSE
      k:= 1;
      LOOP
        IF k >= Count THEN
          RETURN;
        ELSIF NOT Components^[k].IsLoadTime THEN
          EXIT;
        END;
        INC(k);
      END;
    END;
    FOR i := k TO Count-1 DO
      ASSERT(RemoveComponent(Components^[k].EI.Handle));
    END;
  END;
END ClearComponents;


PROCEDURE FindComObjByAddrInCom (com: dt.ComNo; addr: kt.ADDRESS; VAR obj: CARDINAL): BOOLEAN;
VAR
  j: CARDINAL;
BEGIN
  obj := MAX(CARDINAL);
  WITH dt.Components DO
    IF Count <= com THEN
      RETURN FALSE;
    END;
    WITH Components^[com].EI DO
      IF Objects # NIL THEN
        FOR j := 0 TO HIGH(Objects^) DO
          WITH Objects^[j] DO
            IF (Begin <= addr) AND (addr <= End) THEN
              obj := j;
              RETURN TRUE;
            END;
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindComObjByAddrInCom;


PROCEDURE FindComObjByAddr (addr: kt.ADDRESS; VAR com: dt.ComNo; VAR obj: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  com := dt.Invalid_Component;
  obj := MAX(CARDINAL);
  WITH dt.Components DO
    IF Count = 0 THEN
      RETURN FALSE;
    END;
    FOR i := 0 TO Count-1 DO
      IF FindComObjByAddrInCom (i, addr, obj) THEN
        com := i;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindComObjByAddr;


PROCEDURE FindComponentByAddr (addr: kt.ADDRESS; VAR com: dt.ComNo): BOOLEAN;
VAR
  obj: CARDINAL;
BEGIN
  com := dt.Invalid_Component;
  RETURN FindComObjByAddr(addr, com, obj);
END FindComponentByAddr;


PROCEDURE FindComponentByName (name-: ARRAY OF CHAR; VAR com: dt.ComNo): BOOLEAN;
VAR
  i   : CARDINAL;
  Name: xs.txt_ptr;
  upname1: xs.String;
  upname2: xs.String;
BEGIN
  com := dt.Invalid_Component;
  WITH dt.Components DO
    IF Count = 0 THEN RETURN FALSE; END;
    COPY(name, upname1);
    xs.Uppercase(upname1);
    FOR i := 0 TO Count-1 DO
      Name := GetName (Components^[i].Name);
      COPY(Name^, upname2);
      xs.Uppercase(upname2);
      IF upname1 = upname2 THEN
        com := i;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindComponentByName;


PROCEDURE GetIndex (com: dt.ComNo; VAR index: dt.INDEX): BOOLEAN;
BEGIN
  WITH dt.Components DO
    IF com < Count THEN
      index := Components^[com].Index;
      RETURN TRUE;
    ELSE
      index := dt.Invalid_Index;
      RETURN FALSE;
    END;
  END;
END GetIndex;


PROCEDURE GetComNo (VAR com: dt.ComNo; index: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH dt.Components DO
    IF Count > 0 THEN
      FOR i := 0 TO Count-1 DO
        IF index = Components^[com].Index THEN
          com := i;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  com := dt.Invalid_Component;
  RETURN FALSE;
END GetComNo;


(* Выдает идентификатор языка компонеты *)
PROCEDURE ComLanguage (com: dt.ComNo): dt.LANGUAGE;
BEGIN
  RETURN dt.Components.Components^[com].DI.Language;
END ComLanguage;


PROCEDURE IsComValid (com: dt.ComNo): BOOLEAN;
BEGIN
  RETURN (com < dt.Components.Count);
END IsComValid;


PROCEDURE IsDataValid (com: dt.ComNo; mod: dt.ModNo): BOOLEAN;
BEGIN
  RETURN IsComValid(com) AND (mod#dt.Invalid_Module) AND
         ((mod=dt.Fake_Module) OR
          ((mod#0) AND (mod<=dt.Components.Components^[com].DI.LastModule)));
END IsDataValid;


(* Проверяет, можно ли использовать заданную позицию *)
PROCEDURE IsPosValid (pos: dt.POS): BOOLEAN;
BEGIN
  RETURN IsDataValid (pos.ComN, pos.ModN);
END IsPosValid;


(* Сделать ссылку на обьекты *)
PROCEDURE MakePObjects (com: dt.ComNo; mod: dt.ModNo): dt.POBJECTS;
BEGIN
  IF IsDataValid (com, mod) THEN
    IF mod = dt.Fake_Module THEN
      RETURN sys.ADR(dt.Components.Components^[com].DI.GlobalObjects);
    ELSE
      RETURN sys.ADR(dt.Components.Components^[com].DI.Modules^[mod-1].ModuleObjects);
    END;
  ELSE
    RETURN NIL;
  END;
END MakePObjects;


(* Сделать ссылку на обьекты по обьекту *)
PROCEDURE MakeObjPObjects (obj: dt.OBJECT): dt.POBJECTS;
BEGIN
  RETURN MakePObjects (obj.com, obj.mod);
END MakeObjPObjects;


(* Выдает по обьекту его представление *)
PROCEDURE MakeObjPRawObject (obj: dt.OBJECT; VAR praw_obj: dt.PRAW_OBJECT): BOOLEAN;
VAR
  PObjects: dt.POBJECTS;
BEGIN
  PObjects := MakeObjPObjects (obj);
  IF PObjects # NIL THEN
    WITH PObjects^ DO
      IF KObjects = NIL THEN
        bld.ReadObjects (obj.com, obj.mod);
      END;
      IF KObjects # NIL THEN
        praw_obj := sys.ADR(RawObjects.RawObjects^[KObjects^[obj.rec]]);
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END MakeObjPRawObject;


(* Выдает по обьекту его представление *)
PROCEDURE MakeObjRawObject (obj: dt.OBJECT; VAR raw_obj: dt.RAW_OBJECT): BOOLEAN;
VAR
  praw_obj: dt.PRAW_OBJECT;
BEGIN
  IF MakeObjPRawObject (obj, praw_obj) THEN
    raw_obj := praw_obj^;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END MakeObjRawObject;


PROCEDURE MakeObject (Com: dt.ComNo; Mod: dt.ModNo; Rec: CARDINAL): dt.OBJECT;
VAR
  obj: dt.OBJECT;
BEGIN
  WITH obj DO
    com := Com;
    mod := Mod;
    rec := Rec;
  END;
  RETURN obj;
END MakeObject;



VAR
  CurrCom       : dt.ComNo;
  CurrMod       : dt.ModNo;    (* Текущий номер модуля для поиска    *)
  CurrName      : xs.String; (* Имя для поиска                     *)
  CurrAddr      : kt.ADDRESS;  (* Адрес для поиска                   *)
  CurrKey       : dt.KEY;      (* Ключ по обьектам                   *)
  CurrLeftIndex : CARDINAL;    (* текущее смещение в ключе по именам *)
  FindExactly   : BOOLEAN;     (* Точное соответствие адресов        *)
  EmptyString   : sys.ADDRESS; (* Пустая строчка                     *)

  String        : ARRAY [0..0] OF CHAR;



(* Получить имя из таблицы имен модуля *)
PROCEDURE GetName ( rname: CARDINAL): xs.txt_ptr;
VAR
     image: Ud.IMAGE;
     pstr:   xs.txt_ptr;

BEGIN
     image:= Ud.Get(bld.Names, Ud.IMAGE_REF(rname));
     ASSERT(image # NIL);
     pstr := xs.txt_ptr(image);
     RETURN pstr;
END GetName;


PROCEDURE CompareObjectAddr (i: CARDINAL): INTEGER;
VAR
  tmp : kt.ADDRESS;
  obj : dt.OBJECT;
  size: CARDINAL;
BEGIN
  obj := MakeObject (CurrCom, CurrMod, CurrKey^[i]);
  ASSERT(ObjectAddr(obj, tmp));
  IF FindExactly THEN
    size := 0;
  ELSE
    ASSERT(ObjectSize(obj, size));
    DEC(size);
  END;
  IF    tmp+size < CurrAddr THEN RETURN -1;
  ELSIF tmp      > CurrAddr THEN RETURN +1;
  ELSE                           RETURN  0;
  END;
END CompareObjectAddr;


PROCEDURE ComName (com: dt.ComNo; VAR name: xs.txt_ptr): BOOLEAN;
BEGIN
  IF IsComValid(com) THEN
    name := GetName (dt.Components.Components^[com].Name);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ComName;


PROCEDURE ComFullName (com: dt.ComNo; VAR fname: xs.txt_ptr): BOOLEAN;
BEGIN
  IF IsComValid(com) THEN
    fname := sys.ADR(dt.Components.Components^[com].EI.full_name);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ComFullName;


PROCEDURE ModName (com: dt.ComNo; mod: dt.ModNo; VAR name: xs.txt_ptr): BOOLEAN;
BEGIN
  IF IsDataValid (com, mod) AND (mod # dt.Fake_Module) THEN
    name := GetName (dt.Components.Components^[com].DI.Modules^[mod-1].ModuleName);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ModName;


PROCEDURE SourceName (com: dt.ComNo; mod: dt.ModNo; VAR name: ARRAY OF CHAR): BOOLEAN;
VAR
  nm: xs.txt_ptr;
BEGIN
  IF CheckDebugInfoForModule (com, mod) THEN
    nm := GetName (dt.Components.Components^[com].DI.Modules^[mod-1].SourceName);
    COPY(nm^, name);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END SourceName;


PROCEDURE CompareModuleNames (i: CARDINAL): INTEGER;
VAR
  ModuleName: xs.txt_ptr;
  UpModName : xs.String;
BEGIN
  ASSERT(ModName(CurrCom, CurrKey^[i]+1, ModuleName));
  COPY(ModuleName^, UpModName);
  xs.Uppercase(UpModName);
  IF    UpModName < CurrName THEN RETURN -1;
  ELSIF UpModName > CurrName THEN RETURN +1;
  ELSE                            RETURN  0;
  END;
END CompareModuleNames;


PROCEDURE FindModInComp (com: dt.ComNo; name-: ARRAY OF CHAR; VAR mod: dt.ModNo): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  mod := dt.Invalid_Module;
  IF IsComValid (com) THEN
    COPY (name, CurrName);
    xs.Uppercase (CurrName);
    WITH dt.Components DO
      CurrCom := com;
      WITH Components^[CurrCom] DO
        IF DI.LastModule = 0 THEN
          RETURN FALSE;
        END;
        IF DI.KModules = NIL THEN
          -- Создание ключей по модулям
          bld.CreateKeysByModules (CurrCom);
        END;
        CurrKey := DI.KModules;
        IF sor.BinaryFind(HIGH(CurrKey^)+1, CompareModuleNames, i) THEN
          mod := CurrKey^[i]+1;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindModInComp;


PROCEDURE FindMod (name-: ARRAY OF CHAR; VAR com: dt.ComNo; VAR mod: dt.ModNo): BOOLEAN;
VAR
  c: CARDINAL;
BEGIN
  com := dt.Invalid_Component;
  mod := dt.Invalid_Module;
  IF IsComValid (0) THEN
    WITH dt.Components DO
      FOR c := 0 TO Count-1 DO
        IF FindModInComp (c, name, mod) THEN
          com := c;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindMod;


PROCEDURE CompareSegmentsAddress (i: CARDINAL): INTEGER;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.KGSegments^[i] DO
    IF    Begin > CurrAddr THEN RETURN +1;
    ELSIF End   < CurrAddr THEN RETURN -1;
    ELSE                        RETURN  0;
    END;
  END;
END CompareSegmentsAddress;


PROCEDURE FindModInCompByAddr (com: dt.ComNo; addr: kt.ADDRESS; VAR mod: dt.ModNo): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  IF IsComValid (com) THEN
    mod := dt.Invalid_Module;
    WITH dt.Components.Components^[com].DI DO
      IF (LastModule # 0) AND (KGSegments # NIL) THEN
        CurrCom := com;
        CurrAddr := addr;
        IF sor.BinaryFind(HIGH(KGSegments^)+1, CompareSegmentsAddress, i) THEN
          mod := KGSegments^[i].ModNo+1;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindModInCompByAddr;


PROCEDURE FindModByAddr (addr: kt.ADDRESS; VAR com: dt.ComNo; VAR mod: dt.ModNo): BOOLEAN;
VAR
  c: CARDINAL;
BEGIN
  com := dt.Invalid_Component;
  IF IsComValid (0) THEN
    FOR c := 0 TO dt.Components.Count-1 DO
      IF FindModInCompByAddr (c, addr, mod) THEN
        com := c;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindModByAddr;


(* Если тип модуля неизвестен из внешнего представления отладочной *)
(* информации, можно попытаться выяснить тип по расширению имени   *)
(* файла, содержащего текст исходого модуля.                       *)
PROCEDURE TryDetectModLang (src_fname-: ARRAY OF CHAR): dt.LANGUAGE;
VAR
  ext: ARRAY [0..7] OF CHAR;
  lng: dt.LANGUAGE;
BEGIN
  fil.GetExtension (src_fname, ext);
  xs.Uppercase (ext);
  FOR lng := MIN(dt.LANGUAGE) TO MAX(dt.LANGUAGE) DO
    IF ext = dt.LangExtTable[lng] THEN
      RETURN lng;
    END;
  END;
  RETURN dt.Lng_Unknown;
END TryDetectModLang;


(* Выдает идентификатор модуля *)
PROCEDURE ModLanguage (com: dt.ComNo; mod: dt.ModNo): dt.LANGUAGE;
BEGIN
  IF CheckDebugInfoForModule (com, mod) AND (mod # dt.Fake_Module) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      IF HasInfo THEN
        RETURN Language;
      END;
    END;
  END;
  RETURN dt.Lng_Unknown;
END ModLanguage;


PROCEDURE IsModActive (com: dt.ComNo; mod: dt.ModNo): BOOLEAN;
BEGIN
  RETURN IsDataValid (com, mod) AND (mod # dt.Fake_Module)
         AND dt.Components.Components^[com].DI.Modules^[mod-1].Active;
END IsModActive;


PROCEDURE DeactivateMod (com: dt.ComNo; mod: dt.ModNo);
BEGIN
  ASSERT (IsDataValid (com, mod));
  dt.Components.Components^[com].DI.Modules^[mod-1].Active := FALSE;
END DeactivateMod;


PROCEDURE ActivateMod (com: dt.ComNo; mod: dt.ModNo);
BEGIN
  ASSERT (IsDataValid (com, mod));
  dt.Components.Components^[com].DI.Modules^[mod-1].Active := TRUE;
END ActivateMod;


PROCEDURE ModHaveDebugInfo (com: dt.ComNo; mod: dt.ModNo): BOOLEAN;
BEGIN
  RETURN IsDataValid (com, mod) AND dt.Components.Components^[com].DI.Modules^[mod-1].HasInfo;
END ModHaveDebugInfo;


PROCEDURE ResetModTryRead (com: dt.ComNo; mod: dt.ModNo);
BEGIN
  IF IsDataValid (com, mod) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      IF Text # txt.nil THEN
        txt.Close(Text);
      END;
      TryRead := FALSE;
    END;
  END;
END ResetModTryRead;


PROCEDURE SegmentsNo (com: dt.ComNo; mod: dt.ModNo): CARDINAL;
BEGIN
  IF IsDataValid (com, mod) AND (mod # dt.Fake_Module) THEN
    RETURN HIGH(dt.Components.Components^[com].DI.Modules^[mod-1].Segments^)+1;
  ELSE
    RETURN 0;
  END;
END SegmentsNo;


PROCEDURE SegmentName (com: dt.ComNo; mod: dt.ModNo; seg:CARDINAL; VAR name: ARRAY OF CHAR): BOOLEAN;
VAR
  nm: xs.txt_ptr;
BEGIN
  IF IsDataValid (com, mod) AND (mod # dt.Fake_Module) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      IF Segments # NIL THEN
        IF seg <= HIGH(Segments^) THEN
          nm := GetName (Segments^[seg].Name);
          COPY(nm^, name);
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END SegmentName;


PROCEDURE CompareSegmentsAddressInMod (i: CARDINAL): INTEGER;
VAR
  size: CARDINAL;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Modules^[CurrMod-1] DO
    WITH Segments^[i] DO
      IF Sz # 0 THEN size := Sz-1; ELSE size := 0; END;
      IF    Begin+size < CurrAddr THEN RETURN -1;
      ELSIF Begin      > CurrAddr THEN RETURN +1;
      ELSE                             RETURN  0;
      END;
    END;
  END;
END CompareSegmentsAddressInMod;


PROCEDURE FindSegmentInMod (com    : dt.ComNo;
                            mod    : dt.ModNo;
                            addr   : kt.ADDRESS;
                            VAR seg: CARDINAL): BOOLEAN;
BEGIN
  IF IsDataValid (com, mod) AND (mod # dt.Fake_Module) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      IF Segments # NIL THEN
        CurrCom := com;
        CurrMod := mod;
        CurrAddr := addr;
        IF sor.BinaryFind(HIGH(Segments^)+1, CompareSegmentsAddressInMod, seg) THEN RETURN TRUE; END;
      END;
    END;
  END;
  RETURN FALSE;
END FindSegmentInMod;


PROCEDURE FindSegmentInCom (com    : dt.ComNo;
                            addr   : kt.ADDRESS;
                            VAR mod: dt.ModNo;
                            VAR seg: CARDINAL): BOOLEAN;
VAR
  m: CARDINAL;
BEGIN
  mod := dt.Invalid_Module;
  seg := MAX(CARDINAL);
  IF IsComValid(0) AND (dt.Components.Components^[com].DI.LastModule > 0) THEN
    FOR m := 1 TO dt.Components.Components^[com].DI.LastModule DO
      IF FindSegmentInMod (com, m, addr, seg) THEN
        mod := m;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindSegmentInCom;


PROCEDURE FindSegment (addr   : kt.ADDRESS;
                       VAR com: dt.ComNo;
                       VAR mod: dt.ModNo;
                       VAR seg: CARDINAL): BOOLEAN;
VAR
  c: CARDINAL;
BEGIN
  com := dt.Invalid_Component;
  mod := dt.Invalid_Module;
  seg := MAX(CARDINAL);
  IF IsComValid(0) THEN
    FOR c := 0 TO dt.Components.Count-1 DO
      IF FindSegmentInCom (com, addr, mod, seg) THEN
        com := c;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindSegment;


PROCEDURE GetSegmentInfo(com      : dt.ComNo;
                         mod      : dt.ModNo;
                         seg      : CARDINAL;
                         VAR start: kt.ADDRESS;
                         VAR size : CARDINAL): BOOLEAN;
BEGIN
  IF IsDataValid (com, mod) AND (mod # dt.Fake_Module) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      IF Segments # NIL THEN
        IF seg <= HIGH(Segments^) THEN
          WITH Segments^[seg] DO
            start := Begin;
            size  := Sz;
            RETURN TRUE;
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END GetSegmentInfo;


-- Метки --
  -------

(* По адресу возвращает обьект *)
PROCEDURE FindObjectByAddr (com: dt.ComNo; mod: dt.ModNo; addr: kt.ADDRESS; exactly: BOOLEAN): dt.OBJECT;
VAR
  k: CARDINAL;
  PObjects: dt.POBJECTS;
BEGIN
  IF NOT CheckDebugInfoForModule (com, mod) THEN
    RETURN dt.Invalid_Object;
  END;
  PObjects := MakePObjects (com, mod);
  IF PObjects # NIL THEN
    WITH PObjects^ DO
      IF KObjects = NIL THEN
        bld.ReadObjects (com, mod);
      END;
      IF KObjAddr # NIL THEN
        CurrCom := com;
        CurrMod := mod;
        CurrAddr := addr;
        CurrKey := KObjAddr;
        FindExactly := exactly;
        IF sor.BinaryFind (HIGH(KObjAddr^)+1, CompareObjectAddr, k) THEN
          RETURN MakeObject (CurrCom, CurrMod, KObjAddr^[k]);
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;
END FindObjectByAddr;


(* По адресу возвращает, если есть, метку *)
PROCEDURE FindLabelByAddr (com: dt.ComNo; mod: dt.ModNo; addr: kt.ADDRESS): dt.OBJECT;
VAR
  object : dt.OBJECT;
  obj_tag: dt.SYM_TAG;
BEGIN
  object := FindObjectByAddr (com, mod, addr, TRUE);
  IF IsObjectValid (object) THEN
    ASSERT(ObjectTag (object, obj_tag));
    IF obj_tag = dt.Sy_Proc THEN
      RETURN object;
    END;
  END;
  RETURN dt.Invalid_Object;
END FindLabelByAddr;


(* По адресу возвращает, если есть, метку *)
PROCEDURE FindProcByAddr (com: dt.ComNo; mod: dt.ModNo; addr: kt.ADDRESS): dt.OBJECT;
VAR
  object : dt.OBJECT;
  obj_tag: dt.SYM_TAG;
BEGIN
  object := FindObjectByAddr (com, mod, addr, FALSE);
  IF IsObjectValid (object) THEN
    ASSERT(ObjectTag (object, obj_tag));
    IF obj_tag = dt.Sy_Proc THEN
      RETURN object;
    END;
  END;
  RETURN dt.Invalid_Object;
END FindProcByAddr;


PROCEDURE IsLabelOnAddr (com: dt.ComNo; mod: dt.ModNo; addr: kt.ADDRESS): BOOLEAN;
VAR
  label: dt.OBJECT;
BEGIN
  label := FindLabelByAddr (com, mod, addr);
  RETURN IsObjectValid (label);
END IsLabelOnAddr;


(* Количество меток в модуле. Для некорректного номера модуля возвращает 0 *)
PROCEDURE LabelsNo (com: dt.ComNo; mod: dt.ModNo): CARDINAL;
VAR
  PObjects: dt.POBJECTS;
BEGIN
  IF NOT CheckDebugInfoForModule (com, mod) THEN
    RETURN 0;
  END;
  PObjects := MakePObjects (com, mod);
  IF PObjects # NIL THEN
    WITH PObjects^ DO
      IF KObjects = NIL THEN
        bld.ReadObjects (com, mod);
      END;
      IF KObjects # NIL THEN
        RETURN NestedObjects.ProcQuan;
      END;
    END;
  END;
  RETURN 0;
END LabelsNo;


(* Возвращает i метку в лексикографическом порядке их имен *)
PROCEDURE GetLabel (com: dt.ComNo; mod: dt.ModNo; i: CARDINAL): dt.OBJECT;
VAR
  PObjects: dt.POBJECTS;
BEGIN
  IF NOT CheckDebugInfoForModule (com, mod) THEN
    RETURN dt.Invalid_Object;
  END;
  PObjects := MakePObjects (com, mod);
  IF PObjects # NIL THEN
    WITH PObjects^ DO
      IF KObjects = NIL THEN
        bld.ReadObjects (com, mod);
        ASSERT(KObjName # NIL);
      END;
      WITH NestedObjects DO
        IF i < ProcQuan THEN
          RETURN MakeObject (com, mod, KObjName^[ProcPos+i]);
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;
END GetLabel;



PROCEDURE PrevBasePointCompare (i: CARDINAL): INTEGER;
VAR
  addr: kt.ADDRESS;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Modules^[CurrMod-1].CLTable DO
    addr := CLTable^[KCLTableAddr^[i]].Addr;
    IF addr < CurrAddr THEN
      IF (i = HIGH(CLTable^)) OR (CurrAddr <= CLTable^[KCLTableAddr^[i+1]].Addr) THEN
        RETURN 0;
      ELSE
        RETURN -1;
      END;
    ELSE
      RETURN +1;
    END;
  END;
END PrevBasePointCompare;


(* По адресу выдает адрес, меньший и ближайший к заданому *)
PROCEDURE PrevBasePoint (com: dt.ComNo; mod: dt.ModNo; addr: kt.ADDRESS; VAR prev: kt.ADDRESS): BOOLEAN;
VAR
  k: CARDINAL;
  proc: dt.OBJECT;
  tmp: kt.ADDRESS;
BEGIN
  prev := kt.NIL_ADDRESS;
  IF CheckDebugInfoForModule (com, mod) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1].CLTable DO
      IF CLTable # NIL THEN
        CurrCom := com;
        CurrMod := mod;
        CurrAddr := addr;
        IF KCLTableAddr = NIL THEN
          bld.CreateKCLTable (com, mod-1);
          ASSERT(KCLTableAddr # NIL);
        END;
        IF sor.BinaryFind(HIGH(CLTable^)+1, PrevBasePointCompare, k) THEN
          prev := CLTable^[KCLTableAddr^[k]].Addr;
        END;
      END;
    END;
    proc := FindProcByAddr (com, mod, addr-1);
    IF IsObjectValid (proc) THEN
      ASSERT (ObjectAddr (proc, tmp));
      IF prev < tmp THEN
        prev := tmp;
      END;
    END;
  END;
  RETURN prev # kt.NIL_ADDRESS;
END PrevBasePoint;



PROCEDURE ComparePublicAddr (i: CARDINAL): INTEGER;
VAR
  tmp: kt.ADDRESS;
  sz : CARDINAL;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Publics DO
    WITH Publics^[KPAddr^[i]] DO
      tmp := addr;
      sz := len-1;
    END;
  END;
  IF    tmp+sz < CurrAddr THEN RETURN -1;
  ELSIF tmp    > CurrAddr THEN RETURN +1;
  ELSE                         RETURN  0;
  END;
END ComparePublicAddr;


PROCEDURE ComparePublicAddrExactly (i: CARDINAL): INTEGER;
VAR
  tmp: kt.ADDRESS;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Publics DO
    tmp := Publics^[KPAddr^[i]].addr;
  END;
  IF    tmp < CurrAddr THEN RETURN -1;
  ELSIF tmp > CurrAddr THEN RETURN +1;
  ELSE                      RETURN 0;
  END;
END ComparePublicAddrExactly;


PROCEDURE FindPublicByAddrInCom (com : dt.ComNo;
                                 addr: kt.ADDRESS; exactly: BOOLEAN;
                                 VAR name: xs.txt_ptr): BOOLEAN;
VAR
  i: CARDINAL;
  found: BOOLEAN;
BEGIN
  IF NOT IsComValid (com) THEN
    RETURN FALSE;
  END;
  WITH dt.Components DO
    WITH Components^[com] DO
      WITH DI.Publics DO
        IF Count # 0 THEN
          IF KPAddr = NIL THEN
            bld.SortPublicsByAddr (com);
            ASSERT(KPAddr # NIL);
          END;
          CurrCom := com;
          CurrAddr := addr;
          IF exactly THEN
            found := sor.BinaryFind(Count, ComparePublicAddrExactly, i);
          ELSE
            found := sor.BinaryFind(Count, ComparePublicAddr, i);
          END;
          IF found THEN
            name := GetName (Publics^[KPAddr^[i]].name);
            RETURN TRUE;
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindPublicByAddrInCom;


PROCEDURE FindPublicByAddr (addr: kt.ADDRESS;
                            exactly: BOOLEAN;
                            VAR com : dt.ComNo;
                            VAR name: xs.txt_ptr): BOOLEAN;
BEGIN
  RETURN FindComponentByAddr (addr, com) AND FindPublicByAddrInCom (com, addr, exactly, name);
END FindPublicByAddr;


PROCEDURE GetPrevPublicByAddrInCom (com     : dt.ComNo;
                                    addr    : kt.ADDRESS;
                                    VAR prev: kt.ADDRESS): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  IF NOT IsComValid(com) THEN
    RETURN FALSE;
  END;
  WITH dt.Components DO
    WITH Components^[com] DO
      WITH DI.Publics DO
        IF Count # 0 THEN
          IF KPAddr = NIL THEN
            bld.SortPublicsByAddr (com);
            ASSERT(KPAddr # NIL);
          END;
          CurrCom := com;
          CurrAddr := addr;
          IF sor.BinaryFind(Count, ComparePublicAddr, i) THEN
            LOOP
              prev := Publics^[KPAddr^[i]].addr;
              IF (addr # prev) AND Publics^[KPAddr^[i]].code THEN
                RETURN TRUE;
              END;
              IF i = 0 THEN EXIT; END;
              DEC(i);
            END;
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END GetPrevPublicByAddrInCom;



PROCEDURE ComparePublicName (i: CARDINAL): INTEGER;
VAR
  tmp: xs.txt_ptr;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Publics DO
    tmp := GetName (Publics^[KPName^[i]].name);
  END;
  IF    tmp^ < CurrName THEN RETURN -1;
  ELSIF tmp^ > CurrName THEN RETURN +1;
  ELSE                       RETURN  0;
  END;
END ComparePublicName;


PROCEDURE FindPublicByNameInCom (com     : dt.ComNo;
                                 name-   : ARRAY OF CHAR;
                                 VAR addr: kt.ADDRESS): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  IF NOT IsComValid(com) THEN
    RETURN FALSE;
  END;
  WITH dt.Components.Components^[com].DI.Publics DO
    IF Count # 0 THEN
      IF KPName = NIL THEN
        bld.SortPublicsByName (com);
        ASSERT(KPName # NIL);
      END;
      CurrCom := com;
      COPY(name, CurrName);
      IF sor.BinaryFind(Count, ComparePublicName, i) THEN
        addr := Publics^[KPName^[i]].addr;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindPublicByNameInCom;



PROCEDURE FindPublicByName (name-: ARRAY OF CHAR;
                            VAR com : dt.ComNo;
                            VAR addr: kt.ADDRESS): BOOLEAN;
VAR
  c: CARDINAL;
BEGIN
  IF NOT IsComValid(0) THEN
    RETURN FALSE;
  END;
  WITH dt.Components DO
    FOR c := 0 TO Count-1 DO
      IF FindPublicByNameInCom (c, name, addr) THEN
        com := c;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindPublicByName;


PROCEDURE PublicsNo (com: dt.ComNo): CARDINAL;
BEGIN
  IF IsComValid (com) THEN
    RETURN dt.Components.Components^[com].DI.Publics.Count;
  ELSE
    RETURN 0;
  END;
END PublicsNo;


PROCEDURE GetPublic (com: dt.ComNo; i: CARDINAL): dt.PUBLIC;
BEGIN
  ASSERT (IsComValid(com));
  WITH dt.Components.Components^[com].DI.Publics DO
    ASSERT(i < Count);
    IF KPName = NIL THEN
      bld.SortPublicsByName (com);
      ASSERT(KPName # NIL);
    END;
    RETURN Publics^[KPName^[i]];
  END;
END GetPublic;

PROCEDURE GetPublicLen (com: dt.ComNo; i: CARDINAL): CARDINAL;
VAR
  public: dt.PUBLIC;
BEGIN
  public := GetPublic (com, i);
  RETURN public.len;
END GetPublicLen;

(* Получить паблик по номеру (отсортированы по именам) *)
PROCEDURE GetPublicName (com: dt.ComNo; i: CARDINAL): xs.txt_ptr;
VAR
  public: dt.PUBLIC;
BEGIN
  public := GetPublic (com, i);
  RETURN GetName (public.name);
END GetPublicName;


PROCEDURE GetPublicAddr (com: dt.ComNo; i: CARDINAL): kt.ADDRESS;
VAR
  public: dt.PUBLIC;
BEGIN
  public := GetPublic (com, i);
  RETURN public.addr;
END GetPublicAddr;



-- Переменные --
  ------------

PROCEDURE FindVarByAddrInMod (com: dt.ComNo; mod: dt.ModNo; addr: kt.ADDRESS; exactly: BOOLEAN): dt.OBJECT;
VAR
  object : dt.OBJECT;
  obj_tag: dt.SYM_TAG;
BEGIN
  object := FindObjectByAddr (com, mod, addr, exactly);
  IF IsObjectValid (object) THEN
    ASSERT(ObjectTag (object, obj_tag));
    IF obj_tag = dt.Sy_Var THEN
      RETURN object;
    END;
  END;
  RETURN dt.Invalid_Object;
END FindVarByAddrInMod;


PROCEDURE FindVarByAddr (com: dt.ComNo; addr: kt.ADDRESS; exactly: BOOLEAN): dt.OBJECT;
VAR
  object: dt.OBJECT;
  mod: dt.ModNo;
BEGIN
  IF IsComValid (com) THEN
    IF FindModInCompByAddr (com, addr, mod) THEN
      object := FindVarByAddrInMod (com, mod, addr, exactly);
      IF NOT EqualObjects (object, dt.Invalid_Object) THEN
        RETURN object;
      END;
    END;
    object := FindVarByAddrInMod (com, dt.Fake_Module, addr, exactly);
    IF NOT EqualObjects (object, dt.Invalid_Object) THEN
      RETURN object;
    END;
  END;
  RETURN dt.Invalid_Object;
END FindVarByAddr;


(* Количество переменных в модуле. Для некорректного номера модуля возвращает 0 *)
PROCEDURE VarsNo (com: dt.ComNo; mod: dt.ModNo): CARDINAL;
VAR
  PObjects: dt.POBJECTS;
BEGIN
  IF NOT CheckDebugInfoForModule (com, mod) THEN
    RETURN 0;
  END;
  PObjects := MakePObjects (com, mod);
  IF PObjects # NIL THEN
    WITH PObjects^ DO
      IF KObjects = NIL THEN
        bld.ReadObjects (com, mod);
      END;
      IF KObjects # NIL THEN
        RETURN NestedObjects.VarQuan;
      END;
    END;
  END;
  RETURN 0;
END VarsNo;


PROCEDURE LocalVarsNo (proc_obj: dt.OBJECT): CARDINAL;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (proc_obj, raw_object) THEN
    WITH raw_object DO
      ASSERT(Tag=dt.Sy_Proc);
      RETURN DataProc.NestedObjects.VarQuan;
    END;
  ELSE
    RETURN 0;
  END;
END LocalVarsNo;


PROCEDURE ParamVarsNo (proc_obj: dt.OBJECT): CARDINAL;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (proc_obj, raw_object) THEN
    WITH raw_object DO
      ASSERT(Tag=dt.Sy_Proc);
      RETURN DataProc.NestedObjects.ParamQuan;
    END;
  ELSE
    RETURN 0;
  END;
END ParamVarsNo;


(* Количество процедур для обьекта типа процедура *)
PROCEDURE LocalProcsNo (proc_obj: dt.OBJECT) : CARDINAL;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (proc_obj, raw_object) THEN
    WITH raw_object DO
      ASSERT(Tag=dt.Sy_Proc);
      RETURN DataProc.NestedObjects.ProcQuan;
    END;
  ELSE
    RETURN 0;
  END;
END LocalProcsNo;


PROCEDURE GetVar (com: dt.ComNo; mod: dt.ModNo; i: CARDINAL): dt.OBJECT;
VAR
  PObjects: dt.POBJECTS;
BEGIN
  IF NOT CheckDebugInfoForModule (com, mod) THEN
    RETURN dt.Invalid_Object;
  END;
  PObjects := MakePObjects (com, mod);
  IF PObjects # NIL THEN
    WITH PObjects^ DO
      IF KObjects = NIL THEN
        bld.ReadObjects (com, mod);
        ASSERT(KObjName # NIL);
      END;
      WITH NestedObjects DO
        IF i < VarQuan THEN
          RETURN MakeObject (com, mod, KObjName^[VarPos+i]);
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;;
END GetVar;


PROCEDURE GetLocalVar (proc_obj: dt.OBJECT; i: CARDINAL): dt.OBJECT;
VAR
  PObjects: dt.POBJECTS;
  raw_object: dt.RAW_OBJECT;
BEGIN
  PObjects := MakeObjPObjects (proc_obj);
  IF (PObjects # NIL) AND MakeObjRawObject (proc_obj, raw_object) THEN
    WITH PObjects^ DO
      WITH raw_object DO
        ASSERT(Tag=dt.Sy_Proc);
        WITH DataProc.NestedObjects DO
          IF i < VarQuan THEN
            RETURN MakeObject (proc_obj.com, proc_obj.mod, KObjName^[VarPos+i]);
          END;
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;;
END GetLocalVar;


(* Возвращает i [0..N-1] параметр для процедуры *)
PROCEDURE GetParamVar (proc_obj: dt.OBJECT; i: CARDINAL): dt.OBJECT;
VAR
  PObjects: dt.POBJECTS;
  raw_object: dt.RAW_OBJECT;
BEGIN
  PObjects := MakeObjPObjects (proc_obj);
  IF (PObjects # NIL) AND MakeObjRawObject (proc_obj, raw_object) THEN
    WITH PObjects^ DO
      WITH raw_object DO
        ASSERT(Tag=dt.Sy_Proc);
        WITH DataProc.NestedObjects DO
          IF i < ParamQuan THEN
            RETURN MakeObject (proc_obj.com, proc_obj.mod, KObjName^[ParamPos+i]);
          END;
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;;
END GetParamVar;



(* Выдает по заданному контексту локальный обьект и его контекст *)
(* для i из диапазона [0..P+L-1], где P - количетво параметров,  *)
(* L - количество локалов, иначе выдает ошибку                   *)
PROCEDURE GetLocalObj (parent: dt.OBJECT; i: CARDINAL; VAR child, obj: dt.OBJECT);
VAR
  params: CARDINAL;
  locals: CARDINAL;
  N: CARDINAL;
BEGIN
  -- обход всех объемлющих блоков
  child := parent;
  LOOP
    params := ParamVarsNo (child);
    locals := LocalVarsNo (child);
    N := params + locals;
    IF i < params THEN
      obj := GetParamVar (child, i);
      EXIT;
    ELSIF i < N THEN
      obj := GetLocalVar (child, i-params);
      EXIT;
    ELSE
      DEC(i, N);
      child := ObjectParentScope(child);
      ASSERT (NOT EqualObjects (child, dt.Invalid_Object));
    END;
  END;
END GetLocalObj;


(* Уровень вложенности обьекта относительно другого             *)
(* parent=dt.Invalid_Object, child=dt.Invalid_Object -> level=0 *)
(* parent=dt.Invalid_Object, child#dt.Invalid_Object -> level=N *)
(* parent#dt.Invalid_Object, child=dt.Invalid_Object -> FALSE   *)
(* parent#dt.Invalid_Object, child#dt.Invalid_Object -> level=N *)
PROCEDURE ObjectLevel (parent, child: dt.OBJECT; VAR level: CARDINAL): BOOLEAN;
BEGIN
  level := 0;
  IF EqualObjects (parent, dt.Invalid_Object) THEN
    LOOP
      IF EqualObjects (parent, child) THEN EXIT; END;
      child := ObjectParentScope(child);
      INC(level);
    END;
    RETURN TRUE;
  ELSIF EqualObjects (child, dt.Invalid_Object) THEN
    RETURN FALSE;
  ELSE
    LOOP
      IF EqualObjects (parent, child) THEN EXIT; END;
      child := ObjectParentScope(child);
      IF EqualObjects (child, dt.Invalid_Object) THEN
        RETURN FALSE;
      END;
      INC(level);
    END;
    RETURN TRUE;
  END;
END ObjectLevel;


(* Возвращает i [0..N-1] процедуру для процедуры *)
PROCEDURE GetLocalProc (proc_obj: dt.OBJECT; i: CARDINAL): dt.OBJECT;
VAR
  PObjects: dt.POBJECTS;
  raw_object: dt.RAW_OBJECT;
BEGIN
  PObjects := MakeObjPObjects (proc_obj);
  IF (PObjects # NIL) AND MakeObjRawObject (proc_obj, raw_object) THEN
    WITH PObjects^ DO
      WITH raw_object DO
        ASSERT(Tag=dt.Sy_Proc);
        WITH DataProc.NestedObjects DO
          IF i < ProcQuan THEN
            RETURN MakeObject (proc_obj.com, proc_obj.mod, KObjName^[ProcPos+i]);
          END;
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;;
END GetLocalProc;


PROCEDURE CompareObjectNames (i: CARDINAL): INTEGER;
VAR
  tmp: xs.String;
  obj: dt.OBJECT;
BEGIN
  obj := MakeObject (CurrCom, CurrMod, CurrKey^[CurrLeftIndex+i]);
  ObjectName (obj, tmp);
  IF    tmp < CurrName THEN RETURN -1;
  ELSIF tmp > CurrName THEN RETURN +1;
  ELSE                      RETURN  0;
  END;
END CompareObjectNames;


PROCEDURE FindObjectByName (com: dt.ComNo; mod: dt.ModNo; name-: ARRAY OF CHAR): dt.OBJECT;
VAR
  PObjects: dt.POBJECTS;
  k: CARDINAL;
BEGIN
  IF NOT CheckDebugInfoForModule (com, mod) THEN
    RETURN dt.Invalid_Object;
  END;
  PObjects := MakePObjects (com, mod);
  IF PObjects # NIL THEN
    WITH PObjects^ DO
      IF KObjects = NIL THEN
        bld.ReadObjects (com, mod);
      END;
      IF KObjName # NIL THEN
        COPY(name, CurrName);
        CurrCom := com;
        CurrMod := mod;
        CurrKey := KObjName;
        WITH NestedObjects DO
          CurrLeftIndex := VarPos;
          IF sor.BinaryFind (VarQuan, CompareObjectNames, k) THEN
            RETURN MakeObject (com, mod, KObjName^[VarPos+k]);
          END;
          CurrLeftIndex := ParamPos;
          IF sor.BinaryFind (ParamQuan, CompareObjectNames, k) THEN
            RETURN MakeObject (com, mod, KObjName^[ParamPos+k]);
          END;
          CurrLeftIndex := ProcPos;
          IF sor.BinaryFind (ProcQuan, CompareObjectNames, k) THEN
            RETURN MakeObject (com, mod, KObjName^[ProcPos+k]);
          END;
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;;
END FindObjectByName;


PROCEDURE FindLocalVar (proc: dt.OBJECT; name-: ARRAY OF CHAR): dt.OBJECT;
VAR
  PObjects: dt.POBJECTS;
  raw_object: dt.RAW_OBJECT;
  k: CARDINAL;
BEGIN
  PObjects := MakeObjPObjects (proc);
  IF (PObjects # NIL) AND MakeObjRawObject (proc, raw_object) THEN
    WITH PObjects^ DO
      IF KObjName # NIL THEN
        COPY(name, CurrName);
        CurrCom := proc.com;
        CurrMod := proc.mod;
        CurrKey := KObjName;
        WITH raw_object.DataProc.NestedObjects DO
          CurrLeftIndex := VarPos;
          IF sor.BinaryFind (VarQuan, CompareObjectNames, k) THEN
            RETURN MakeObject (proc.com, proc.mod, KObjName^[VarPos+k]);
          END;
          CurrLeftIndex := ParamPos;
          IF sor.BinaryFind (ParamQuan, CompareObjectNames, k) THEN
            RETURN MakeObject (proc.com, proc.mod, KObjName^[ParamPos+k]);
          END;
          CurrLeftIndex := ProcPos;
          IF sor.BinaryFind (ProcQuan, CompareObjectNames, k) THEN
            RETURN MakeObject (proc.com, proc.mod, KObjName^[ProcPos+k]);
          END;
        END;
      END;
    END;
  END;
  RETURN dt.Invalid_Object;;
END FindLocalVar;



-------------------------------------------------------------------------
------------------ Работа с исходным текстом программы ------------------
                  -------------------------------------

PROCEDURE AddrBySource (com: dt.ComNo; mod: dt.ModNo; line: CARDINAL; VAR addr: kt.ADDRESS): BOOLEAN;
(* По данному модулю и номеру строки выдает адрес в коде отлаживаемой программы. *)
(* Нумерация строк - с 1..N                                                      *)
VAR
  ln: CARDINAL;
BEGIN
  IF (line#0) AND (line <= LastLineHasCode(com, mod)) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1].CLTable DO
      ln := KCLTableLine^[line-1];
      IF ln # MAX(CARDINAL) THEN
        addr := CLTable^[ln].Addr;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END AddrBySource;


PROCEDURE CompareAddress (i: CARDINAL): INTEGER;
VAR
  tmp: kt.ADDRESS;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Modules^[CurrMod-1].CLTable DO
    tmp := CLTable^[KCLTableAddr^[i]].Addr;
    IF    tmp < CurrAddr THEN RETURN -1;
    ELSIF tmp > CurrAddr THEN RETURN +1;
    ELSE                      RETURN  0;
    END;
  END;
END CompareAddress;


PROCEDURE SourceByAddrInMod (com: dt.ComNo; mod: dt.ModNo; addr: kt.ADDRESS; VAR line: CARDINAL): BOOLEAN;
VAR
  i, m: CARDINAL;
BEGIN
  IF CheckDebugInfoForModule (com, mod) THEN
    CurrCom := com;
    CurrMod := mod;
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      WITH CLTable DO
        IF NOT HasInfo OR (CLTable = NIL) THEN
          RETURN FALSE;
        END;
        IF KCLTableAddr = NIL THEN
          bld.CreateKCLTable (com, mod-1);
          ASSERT(KCLTableAddr # NIL);
        END;
        IF (CLTable^[KCLTableAddr^[0]].Addr <= addr) THEN
          CurrAddr := addr;
          IF NOT sor.BinaryFind(HIGH(KCLTableAddr^)+1, CompareAddress, i) THEN
            IF (i # 0) AND (CLTable^[KCLTableAddr^[i-1]].Addr < addr) AND
              (CLTable^[KCLTableAddr^[i]].Addr > addr)
            THEN
              DEC(i);
            END;
          END;
          IF i = HIGH(KCLTableAddr^) THEN
            IF NOT FindSegmentInMod(com, mod, addr, m) THEN
              RETURN FALSE;
            END;
          END;
          line := CLTable^[KCLTableAddr^[i]].Line;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END SourceByAddrInMod;


PROCEDURE SourceByAddr (addr: kt.ADDRESS; VAR com: dt.ComNo; VAR mod: dt.ModNo; VAR line: CARDINAL): BOOLEAN;
BEGIN
  com := dt.Invalid_Component;
  mod := dt.Invalid_Module;
  line := MAX(CARDINAL);
  IF FindModByAddr (addr, com, mod) THEN
    RETURN SourceByAddrInMod(com, mod, addr, line);
  ELSE
    RETURN FALSE;
  END;
END SourceByAddr;


PROCEDURE CodeNum (com: dt.ComNo; mod: dt.ModNo; line: CARDINAL): CARDINAL;
VAR
  first, N: CARDINAL;
BEGIN
  IF (line # 0) AND (line <= LastLineHasCode(com, mod)) THEN
    DEC(mod);
    WITH dt.Components.Components^[com].DI.Modules^[mod].CLTable DO
      first := KCLTableLine^[line-1];
      IF first # MAX(CARDINAL) THEN         (* Есть ли для этой строки код? *)
        N := 1;
        LOOP
          IF first + N > HIGH(CLTable^) THEN EXIT; END; (* Больше нет строк *)
          IF CLTable^[first].Line # CLTable^[first + N].Line THEN
            EXIT;                           (* Номер строки изменился       *)
          END;
          INC(N);
        END;
        RETURN N;
      END;
    END;
  END;
  RETURN 0;
END CodeNum;


PROCEDURE GetNCode (com: dt.ComNo; mod: dt.ModNo; line, i: CARDINAL): kt.ADDRESS;
VAR
  N: CARDINAL;
BEGIN
  N := CodeNum (com, mod, line);
  ASSERT ((i#0) AND  (i<=N));
  WITH dt.Components.Components^[com].DI.Modules^[mod-1].CLTable DO
    RETURN CLTable^[KCLTableLine^[line-1]+(i-1)].Addr;
  END;
END GetNCode;


PROCEDURE LastLineHasCode (com: dt.ComNo; mod: dt.ModNo): CARDINAL;
VAR
  N: CARDINAL;
BEGIN
  N := 0;
  IF CheckDebugInfoForModule (com, mod) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      WITH CLTable DO
        IF HasInfo THEN
          IF (CLTable # NIL) THEN
            IF (KCLTableLine = NIL) THEN
              bld.CreateKCLTable (com, mod-1);
              ASSERT(KCLTableAddr # NIL);
            END;
            N := HIGH(KCLTableLine^)+1;
          END;
        END;
      END;
    END;
  END;
  RETURN N;
END LastLineHasCode;


PROCEDURE OpenSource (com: dt.ComNo; mod: dt.ModNo): BOOLEAN;
VAR
  name: xs.txt_ptr;

<* IF DEST_XDS THEN *>
  drive, path, file: xs.String;
  line_no: CARDINAL;
  line: xs.txt_ptr;
<* END *>

BEGIN
  IF CheckDebugInfoForModule (com, mod) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1] DO
      IF Text = txt.nil THEN
        IF NOT TryRead THEN
          name := GetName (SourceName);

         <* IF DEST_XDS THEN *>
          fil.SplitPath (name^, drive, path, file);
          IF drive # '' THEN
            IF opt.StripPathFromFullName THEN
              name := sys.ADR(file);
            END;
          ELSE
            IF opt.StripPathFromPartialName THEN
              name := sys.ADR(file);
            END;
          END;
         <* END *>

          txt.Open(Text, name^);

         <* IF DEST_XDS THEN *>
         line_no := txt.LastLine (Text);
         WHILE line_no > 0 DO
           DEC (line_no);
           txt.GetLine (Text, line_no, line);
           trn.TransliterateStr (opt.TranslirateTextFromTo, line^);
         END;
         <* END *>

          TryRead := TRUE;
        END;
      END;
      RETURN Text # txt.nil;
    END;
  END;
  RETURN FALSE;
END OpenSource;


PROCEDURE LastSourceLine (com: dt.ComNo; mod: dt.ModNo): CARDINAL;
BEGIN
  IF OpenSource (com, mod) THEN
    RETURN txt.LastLine(dt.Components.Components^[com].DI.Modules^[mod-1].Text);
  ELSE
    RETURN 0;
  END;
END LastSourceLine;


CONST
  NoSource = 'Source file not found';

PROCEDURE GetSourceLine (com: dt.ComNo; mod: dt.ModNo; line: CARDINAL): xs.txt_ptr;
VAR
  s: xs.txt_ptr;
BEGIN
  IF OpenSource (com, mod) THEN
    txt.GetLine(dt.Components.Components^[com].DI.Modules^[mod-1].Text, line, s);
  ELSE
    s := sys.ADR(NoSource);
  END;
  RETURN s;
END GetSourceLine;



-- найден исходный текст
PROCEDURE ModHaveSource (com: dt.ComNo; mod: dt.ModNo): BOOLEAN;
BEGIN
   RETURN OpenSource (com, mod);
END ModHaveSource;



PROCEDURE IsObjectValid (p: dt.OBJECT) : BOOLEAN;
BEGIN
  WITH dt.Components DO
    IF p.com < Count THEN
      WITH Components^[p.com].DI DO
        IF p.mod = dt.Fake_Module THEN
          WITH GlobalObjects DO
            RETURN (KObjects # NIL) AND (p.rec <= HIGH(KObjects^));
          END;
        ELSIF p.mod <= LastModule THEN
          WITH Modules^[p.mod-1].ModuleObjects DO
            RETURN (KObjects # NIL) AND (p.rec <= HIGH(KObjects^));
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END IsObjectValid;


PROCEDURE EqualObjects (obj1, obj2: dt.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj1.com=obj2.com) AND (obj1.mod=obj2.mod) AND (obj1.rec=obj2.rec);
END EqualObjects;


(* Выдать имя объекта *)
PROCEDURE Object_pName (object: dt.OBJECT; VAR name: xs.txt_ptr);
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  ASSERT (MakeObjRawObject (object, raw_object));
  name := GetName ( raw_object.Name);
END Object_pName;


PROCEDURE ObjectName (object: dt.OBJECT; VAR name: ARRAY OF CHAR);
VAR
  tmp: xs.txt_ptr;
BEGIN
  Object_pName (object, tmp);
  IF tmp = NIL THEN
    COPY ("", name);
  ELSE
    COPY (tmp^, name);
  END;
END ObjectName;


(* Выдать идентификатор модуля, в котором определен объект *)
PROCEDURE ObjectLanguage (object: dt.OBJECT): dt.LANGUAGE;
BEGIN
  RETURN ModLanguage (ObjectCom (object), ObjectMod (object));
END ObjectLanguage;


(* Выдает атрибуты обьекта *)
PROCEDURE ObjectAttr (object: dt.OBJECT; VAR attr: dt.SYM_ATTRIB): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    WITH raw_object DO
      CASE Tag OF
      | dt.Sy_Register:   (* register variable *)
        attr := DataReg.Attrib;
      | dt.Sy_Relative:   (* relative variable *)
        attr := DataRel.Attrib;
      ELSE
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ObjectAttr;


(* Выдает адрес оъекта, на корректность проверяется  *)
PROCEDURE ObjectAddr (object: dt.OBJECT; VAR addr: kt.ADDRESS): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    WITH raw_object DO
      CASE Tag OF
      | dt.Sy_Proc:       (* procedure *)
        addr := DataProc.Address;
      | dt.Sy_Var:        (* variable  *)
        addr := DataVar.Address;
      ELSE
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ObjectAddr;


(* Выдает размер объекта в байтах                       *)
PROCEDURE ObjectSize (object: dt.OBJECT; VAR size: CARDINAL): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
  type: dt.PTYPE;
BEGIN
  size := 0;
  IF MakeObjRawObject (object, raw_object) THEN
    WITH raw_object DO
      CASE Tag OF
      | dt.Sy_Proc:       (* procedure         *)
        size := raw_object.DataProc.Length;
      | dt.Sy_Var         (* variable          *)
      , dt.Sy_Register    (* register variable *)
      , dt.Sy_Relative:   (* relative variable *)
        ASSERT(ObjectType(object, type));
        size := TypeSize(type);
      ELSE
      END;
    END;
  END;
  RETURN size # 0;
END ObjectSize;


(* Выдает тип объекта: только для переменных и процедур *)
PROCEDURE ObjectType (object: dt.OBJECT; VAR type: dt.PTYPE): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    type := raw_object.Type;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ObjectType;


(* Выдает тип вывода объекта *)
PROCEDURE ObjectSymTypeID (object: dt.OBJECT; VAR st_id: dt.SYM_TYPE_ID): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    st_id := raw_object.ST_ID;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ObjectSymTypeID;


PROCEDURE SetObjectSymTypeID (object: dt.OBJECT; st_id: dt.SYM_TYPE_ID): BOOLEAN;
VAR
  praw_object: dt.PRAW_OBJECT;
BEGIN
  IF MakeObjPRawObject (object, praw_object) THEN
    praw_object^.ST_ID := st_id;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END SetObjectSymTypeID;


(* Выдает тип обькта *)
PROCEDURE ObjectTag (object: dt.OBJECT; VAR tag:dt.SYM_TAG): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    tag := raw_object.Tag;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ObjectTag;


(* Выдает компоненту объекта *)
PROCEDURE ObjectCom (object: dt.OBJECT): dt.ComNo;
BEGIN
  RETURN object.com;
END ObjectCom;


(* Выдает номер записи объекта *)
PROCEDURE ObjectRec (object: dt.OBJECT): CARDINAL;
BEGIN
  RETURN object.rec;
END ObjectRec;


(* Выдает модуль объекта *)
PROCEDURE ObjectMod (object: dt.OBJECT): dt.ModNo;
BEGIN
  RETURN object.mod;
END ObjectMod;


PROCEDURE IsParameter (object: dt.OBJECT): BOOLEAN;
VAR
  attr: dt.SYM_ATTRIB;
BEGIN
  RETURN ObjectAttr (object, attr) AND (dt.SA_Param IN attr);
END IsParameter;


(* По обьекту (локальному) выдает смещение/регистр *)
PROCEDURE GetLocalObject_Addr (object: dt.OBJECT; rel_reg_value: kt.ADDRESS; VAR addr: kt.ADDRESS): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    WITH raw_object DO
      ASSERT(Tag=dt.Sy_Relative);
      WITH DataRel DO
       <* PUSH *>
       <* COVERFLOW- *>
        IF Relative < 0 THEN
          addr := CARDINAL(rel_reg_value) - CARDINAL(-Relative);
        ELSE
          addr := CARDINAL(rel_reg_value) + CARDINAL(Relative);
        END;
       <* POP *>
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END GetLocalObject_Addr;


PROCEDURE GetLocalObject_Reg (object: dt.OBJECT; VAR reg_no: CARDINAL; VAR ref: BOOLEAN): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    WITH raw_object DO
      CASE Tag OF
      | dt.Sy_Register:
        WITH DataReg DO
          reg_no := RegNo;
          ref := (dt.SA_Param IN Attrib) AND (dt.SA_Reference IN Attrib);
        END;
        RETURN TRUE;
      | dt.Sy_Relative:
        WITH DataRel DO
          reg_no := RegNo;
          ref := (dt.SA_Param IN Attrib) AND (dt.SA_Reference IN Attrib);
        END;
        RETURN TRUE;
      ELSE
      END;
    END;
  END;
  RETURN FALSE;
END GetLocalObject_Reg;


(* По обьекту выдает обьемлющий блок *)
PROCEDURE ObjectParentScope (object: dt.OBJECT): dt.OBJECT;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  IF MakeObjRawObject (object, raw_object) THEN
    WITH raw_object DO
      IF ParentScope # MAX(sys.CARD16) THEN
        RETURN MakeObject (object.com, object.mod, ParentScope);
      END;
    END;
  END;
  RETURN dt.Invalid_Object;
END ObjectParentScope;


(* По обьекту типа процедура выдает адрес начала пролога и эпилога *)
PROCEDURE ProcAttr (proc: dt.OBJECT; VAR start, end: kt.ADDRESS);
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  ASSERT(MakeObjRawObject (proc, raw_object));
  WITH raw_object DO
    ASSERT(Tag = dt.Sy_Proc);
    WITH DataProc DO
      start := Address+Begin;
      end   := Address+End;
    END;
  END;
END ProcAttr;


PROCEDURE AddrInProcBody (proc: dt.OBJECT; addr: kt.ADDRESS): BOOLEAN;
VAR
  start, end: kt.ADDRESS;
BEGIN
  ProcAttr (proc, start, end);
  RETURN (start <= addr) AND (addr <= end);
END AddrInProcBody;


(* По обьекту типа процедура проверяет, сохраняется ли кадр при вызове  *)
PROCEDURE ProcHasFrame (proc: dt.OBJECT): BOOLEAN;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  ASSERT (MakeObjRawObject (proc, raw_object));
  WITH raw_object DO
    ASSERT (Tag = dt.Sy_Proc);
    RETURN DataProc.HasFrame;
  END;
END ProcHasFrame;


(* По обьекту типа процедура выдает размер начального кадр *)
PROCEDURE ProcFrameSize (proc: dt.OBJECT): CARDINAL;
VAR
  raw_object: dt.RAW_OBJECT;
BEGIN
  ASSERT (MakeObjRawObject (proc, raw_object));
  WITH raw_object DO
    ASSERT (Tag = dt.Sy_Proc);
    RETURN DataProc.FrameSize;
  END;
END ProcFrameSize;




(* Проверка типа на корректность                        *)
PROCEDURE IsTypeValid (type: dt.PTYPE) : BOOLEAN;
BEGIN
  IF type = dt.Invalid_Type THEN
    RETURN FALSE;
  ELSIF IsTypePrimitive (type) THEN
    RETURN TRUE;
  ELSIF Ud.IsRefValid (bld.TypesTbl, type-dt.FIRST_NONPRIMITIVE) THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END IsTypeValid;


(* Адрес образа не примитивного типа *)
PROCEDURE TypeImage (type: dt.PTYPE): sys.ADDRESS;
BEGIN
  IF IsTypePrimitive (type) THEN
    RETURN NIL;
  ELSIF IsTypeValid (type) THEN
    RETURN sys.ADDRESS (Ud.Get (bld.TypesTbl, type-dt.FIRST_NONPRIMITIVE));
  ELSE
    RETURN NIL;
  END;
END TypeImage;


(* Количество типов *)
PROCEDURE TypesNo (com: dt.ComNo; mod: dt.ModNo): CARDINAL;
BEGIN
  IF CheckDebugInfoForModule (com, mod) AND (mod # dt.Fake_Module) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1].ModuleTypes DO
      IF NOT Sorted THEN
        bld.SortKTName (com, mod);
      END;
      RETURN KTQuantity;
    END;
  ELSE
    RETURN 0;
  END;
END TypesNo;


(* Возвращает i-ый тип для именнованных типов *)
PROCEDURE GetType (com: dt.ComNo; mod: dt.ModNo; i: CARDINAL): dt.PTYPE;
BEGIN
  IF CheckDebugInfoForModule (com, mod) THEN
    RETURN dt.Components.Components^[com].DI.Modules^[mod-1].ModuleTypes.KTName^[i];
  ELSE
    RETURN dt.Invalid_Type;
  END;
END GetType;


(* Идентификатор типа типа *)
PROCEDURE TypeTagName (tag: dt.TYPE_TAG; VAR name: ARRAY OF CHAR);
BEGIN
  COPY (dt.TypeTagNames[tag], name);
END TypeTagName;


(* Базовый примитивный тип *)
PROCEDURE IsTypeBasePrimitive (type: dt.PTYPE): BOOLEAN;
BEGIN
<* PUSH *>
<* WOFF902+ *>
  RETURN (dt.FIRST_PRIMITIVE_TYPE <= type) AND (type < dt.PRIMITIVE_TYPES);
<* POP *>
END IsTypeBasePrimitive;

(* Указатель на базовый примитивный тип *)
PROCEDURE IsTypePointerPrimitive (type: dt.PTYPE): BOOLEAN;
BEGIN
  RETURN (dt.FIRST_POINTERS_TYPES <= type) AND (type < dt.FIRST_NONPRIMITIVE);
END IsTypePointerPrimitive;

(* Примитивный тип *)
PROCEDURE IsTypePrimitive (type: dt.PTYPE): BOOLEAN;
BEGIN
  RETURN IsTypeBasePrimitive (type) OR IsTypePointerPrimitive (type);
END IsTypePrimitive;



(* Получить имя типа *)
PROCEDURE TypeName (type: dt.PTYPE; VAR name: xs.txt_ptr): BOOLEAN;
VAR
  data: dt.PTYPE_DATA;
BEGIN
  IF NOT IsTypeValid(type) THEN
    RETURN FALSE;
  ELSIF IsTypeBasePrimitive (type) THEN
    name := sys.ADR (dt.STD_TYPES[type].Name);
  ELSIF IsTypePointerPrimitive (type) THEN
    name := EmptyString;
  ELSE
    data := TypeImage (type);
    name := GetName (data^.Name);
  END;
  RETURN TRUE;
END TypeName;


(* Переименовать тип *)
PROCEDURE RenameType (type: dt.PTYPE; name-: ARRAY OF CHAR): BOOLEAN;
VAR
  data: dt.PTYPE_DATA;
BEGIN
  IF NOT IsTypeValid(type) OR IsTypePrimitive (type) THEN
    RETURN FALSE;
  ELSE
    data := TypeImage (type);
    data^.Name := bld.AddName (name);
    RETURN TRUE;
  END;
END RenameType;


(* Выдать тип типа *)
PROCEDURE TypeTag (type: dt.PTYPE; VAR tag:dt.TYPE_TAG): BOOLEAN;
VAR
  data: dt.PTYPE_DATA;
BEGIN
  IF NOT IsTypeValid(type) THEN RETURN FALSE; END;
  IF IsTypeBasePrimitive (type) THEN
    tag := dt.STD_TYPES[type].Tag;
    RETURN TRUE;
  ELSIF IsTypePointerPrimitive (type) THEN
    tag := dt.Pointer;
    RETURN TRUE;
  END;
    data := TypeImage (type);
    tag := data^.Tag;
  RETURN TRUE;
END TypeTag;


PROCEDURE TypeMod (type: dt.PTYPE): dt.ModNo;
VAR
  pdata: dt.PTYPE_DATA;
BEGIN
  pdata := TypeImage (type);
  RETURN pdata^.Mod;
END TypeMod;


PROCEDURE TypeCom (type: dt.PTYPE): dt.ComNo;
VAR
  pdata: dt.PTYPE_DATA;
BEGIN
  pdata := TypeImage (type);
  RETURN pdata^.Com;
END TypeCom;



(* Поиск типа по имени, в том числе среди стандартных *)
VAR
  Name  : xs.txt_ptr; (* Искомое имя *)

PROCEDURE CompareStdTypeName (i:CARDINAL): INTEGER;
VAR
  name: xs.txt_ptr;
BEGIN
  name := sys.ADR(dt.STD_TYPES[dt.KNamesStdTypes[i]].Name);
  IF    name^ < Name^ THEN RETURN -1;
  ELSIF name^ > Name^ THEN RETURN +1;
  ELSE                     RETURN 0;
  END;
END CompareStdTypeName;

PROCEDURE FindStdType (name-: ARRAY OF CHAR; VAR type: dt.PTYPE): BOOLEAN;
BEGIN
  Name := sys.ADR(name);
  IF sor.BinaryFind( HIGH(dt.KNamesStdTypes)+1, CompareStdTypeName, type) THEN
    type :=  dt.KNamesStdTypes[type];
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END FindStdType;


PROCEDURE CompareTypeName (i:CARDINAL): INTEGER;
VAR
  type: dt.PTYPE;
  name: xs.txt_ptr;
BEGIN
  type :=  dt.Components.Components^[CurrCom].DI.Modules^[CurrMod-1].ModuleTypes.KTName^[i];
  ASSERT(TypeName(type, name));
  IF    name^ < Name^ THEN RETURN -1;
  ELSIF name^ > Name^ THEN RETURN +1;
  ELSE                     RETURN  0;
  END;
END CompareTypeName;

PROCEDURE FindType (com: dt.ComNo; mod: dt.ModNo; name-: ARRAY OF CHAR; VAR type: dt.PTYPE): BOOLEAN;
BEGIN
  IF CheckDebugInfoForModule (com, mod) THEN
    WITH dt.Components.Components^[com].DI DO
      IF NOT Ud.IsEmptyStorage(bld.TypesTbl) THEN
        WITH Modules^[mod-1].ModuleTypes DO
          IF NOT Sorted THEN
            bld.SortKTName (com, mod);
          END;
          IF KTQuantity > 0 THEN
            CurrCom := com;
            CurrMod := mod;
            Name := sys.ADR(name);
            IF sor.BinaryFind (KTQuantity, CompareTypeName, type) THEN
              type := KTName^[type];
              RETURN TRUE;
            END;
          END;
        END;
      END;
    END;
  END;
  RETURN FindStdType(name, type);
END FindType;



PROCEDURE ArrayDim (type: dt.PTYPE): CARDINAL;
VAR
  tag: dt.TYPE_TAG;
  tmp: dt.TYPE_TAG;
  dim: CARDINAL;
BEGIN
  ASSERT(TypeTag(type, tag));
  ASSERT((tag = dt.Array_of) OR (tag = dt.OpenArray));
  tmp := tag;
  dim := 1;
  LOOP
    SubType(type, type);
    ASSERT(TypeTag(type, tag));
    IF tag # tmp THEN EXIT; END;
    INC(dim);
  END;
  RETURN dim;
END ArrayDim;


PROCEDURE OpenArrayTypeDescription (type: dt.PTYPE; VAR desc: dt.TYPE_OPEN_ARRAY): BOOLEAN;
VAR
  pdesc: POINTER TO dt.TYPE_OPEN_ARRAY;
BEGIN
  ASSERT (IsTypeValid(type));
  pdesc := TypeImage (type);
  desc := pdesc^;
  RETURN TRUE;
END OpenArrayTypeDescription;


(* размер типа в байтах                                   *)
PROCEDURE TypeSize (type: dt.PTYPE): CARDINAL;
VAR
  ptag: POINTER TO dt.TYPE_TAG;
  size: CARDINAL;

  PPointerType   : POINTER TO dt.TYPE_POINTER;
  PRecordType    : POINTER TO dt.TYPE_RECORD;
  PClassType     : POINTER TO dt.TYPE_CLASS;
  PArrayType     : POINTER TO dt.TYPE_ARRAY;
  PEnumType      : POINTER TO dt.TYPE_ENUM;
  PRangeType     : POINTER TO dt.TYPE_RANGE;

BEGIN
  ASSERT(IsTypeValid(type));
  IF IsTypeBasePrimitive (type) THEN
    RETURN dt.STD_TYPES[type].Length;
  ELSIF IsTypePointerPrimitive (type) THEN
    RETURN 4;
  END;
  ptag := TypeImage (type);
  CASE ptag^ OF
  | dt.Procedure:
    size := 4;

  | dt.Pointer:
    PPointerType := sys.CAST(sys.ADDRESS, ptag);
    size := PPointerType^.Length;

  | dt.Record:
    PRecordType := sys.CAST(sys.ADDRESS, ptag);
    size := PRecordType^.Length;

  | dt.Class:
    PClassType := sys.CAST(sys.ADDRESS, ptag);
    size := PClassType^.Length;

  | dt.Range:
    PRangeType := sys.CAST(sys.ADDRESS, ptag);
    type := PRangeType^.Base;
    size := TypeSize(type);

  | dt.Enum:
    PEnumType := sys.CAST(sys.ADDRESS, ptag);
    type := PEnumType^.Base;
    size := TypeSize(type);

  | dt.Set:
    size := (TypeLen(type)+7) DIV 8;

  | dt.Array:
    size := TypeLen(type);
    PArrayType := sys.CAST(sys.ADDRESS, ptag);
    type := PArrayType^.Base;
    size := size * TypeSize(type);

  | dt.Array_of:
    size := 4+4*ArrayDim(type);

  | dt.OpenArray:
    size := 0;

  END;
  RETURN size;
END TypeSize;



(* длина типа - для атомарных типов: размер в байтах      *)
(*              для структурных:     количество элементов *)
PROCEDURE TypeLen (type: dt.PTYPE): CARDINAL;
VAR
  ptag: POINTER TO dt.TYPE_TAG;
  tag : dt.TYPE_TAG;
  len : CARDINAL;
  min ,
  max : CARDINAL;

  PPointerType: POINTER TO dt.TYPE_POINTER;
  PRecordType : POINTER TO dt.TYPE_RECORD;
  PClassType  : POINTER TO dt.TYPE_CLASS;
  PProcType   : POINTER TO dt.TYPE_PROCEDURE;
  PEnumType   : POINTER TO dt.TYPE_ENUM;
  PSetType    : POINTER TO dt.TYPE_SET;
  PArrayType  : POINTER TO dt.TYPE_ARRAY;
  PRangeType  : POINTER TO dt.TYPE_RANGE;

BEGIN
  ASSERT(IsTypeValid(type));
  IF IsTypeBasePrimitive (type) THEN
    RETURN dt.STD_TYPES[type].Length;
  ELSIF IsTypePointerPrimitive (type) THEN
    RETURN 4;
  END;
    ptag := TypeImage (type);
    CASE ptag^ OF
    | dt.Procedure  : PProcType := sys.CAST(sys.ADDRESS, ptag);
                      len := PProcType^.ParamCount;

    | dt.Pointer    : PPointerType := sys.CAST(sys.ADDRESS, ptag);
                      len := PPointerType^.Length;

    | dt.Record     : PRecordType := sys.CAST(sys.ADDRESS, ptag);
                      len := PRecordType^.Fields;

    | dt.Class      : PClassType := sys.CAST(sys.ADDRESS, ptag);
                      len := PClassType^.AllMembers;

    | dt.Enum       : PEnumType := sys.CAST(sys.ADDRESS, ptag);
                      len := PEnumType^.Quantity;

    | dt.Set        : PSetType := sys.CAST(sys.ADDRESS, ptag);
                      type := PSetType^.Base;
                      len := TypeLen(type);

    | dt.Array      : PArrayType := sys.CAST(sys.ADDRESS, ptag);
                      type := PArrayType^.Index;
                      ASSERT (TypeTag(type, tag));
                      Index (type, min, max);
                      IF tag = dt.Range THEN
                        SubType(type, type);
                      END;
                      ASSERT (TypeTag(type, tag));
                      IF tag = dt.Int THEN
                        CASE TypeSize (type) OF
                        | 1 : len := sys.INT8(max) - sys.INT8(min) + 1;
                        | 2 : len := sys.INT16(max) - sys.INT16(min) + 1;
                        | 4 : len := sys.INT32(max) - sys.INT32(min) + 1;
                        END;
                      ELSIF max = MAX(CARDINAL) THEN
                        len := max - min;
                      ELSE
                        len := max - min + 1;
                      END;

   | dt.Range      : PRangeType := sys.CAST(sys.ADDRESS, ptag);
                     WITH PRangeType^ DO
                       type := Base;
                       ASSERT (TypeTag(type, tag));
                       IF tag # dt.Int THEN
                         len := CARDINAL(Max) - CARDINAL(Min) + 1;
                       ELSE
                         len := LONGINT(Max) - LONGINT(Min) + 1;
                       END;
                     END;
    END;
  RETURN len;
END TypeLen;



(* Выдает базовый тип. Базовый тип имеется у следующих: *)

(* ARRAY OF BaseType       - открытый массив            *)
(* ARRAY Index OF BaseType - массив                     *)
(* POINTER TO BaseType     - указатель                  *)
(* REFERENCE TO BaseType   - ссылка                     *)
(* SET OF BaseType         - множество                  *)
(* [ BaseType ]            - диапазон                   *)
(* Enumeration on BaseType - перечислимый на базовом    *)
(* PROCEDURE: BaseType     - тип результата процедуры   *)
(* CLASS ( BaseType )      - базовый класс              *)

PROCEDURE SubType (type: dt.PTYPE; VAR subtype: dt.PTYPE);
VAR
  tag : dt.TYPE_TAG;
  ptag: POINTER TO dt.TYPE_TAG;

  PPointerType   : POINTER TO dt.TYPE_POINTER;
  PReferenceType : POINTER TO dt.TYPE_REFERENCE;
  PClassType     : POINTER TO dt.TYPE_CLASS;
  PProcType      : POINTER TO dt.TYPE_PROCEDURE;
  PEnumType      : POINTER TO dt.TYPE_ENUM;
  PSetType       : POINTER TO dt.TYPE_SET;
  PArrayType     : POINTER TO dt.TYPE_ARRAY;
  PArrayOfType   : POINTER TO dt.TYPE_ARRAY_OF;
  POpenArrayType : POINTER TO dt.TYPE_OPEN_ARRAY;
  PRangeType     : POINTER TO dt.TYPE_RANGE;
BEGIN
  ASSERT (IsTypeValid(type));
  subtype := type;
  IF IsTypeBasePrimitive (type) THEN        (* PRIMITIVE_TYPE     *)
    ASSERT (TypeTag(type, tag));
    ASSERT (tag=dt.Set );
    subtype := dt.STD_TYPE_CARD8;
  ELSIF IsTypePointerPrimitive (type) THEN  (* STD_POINTERS_TYPE  *)
    subtype := type-dt.STD_POINTERS_BEGIN;
  ELSE
    ptag := TypeImage (type);
    CASE ptag^ OF
    | dt.Enum:
      PEnumType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PEnumType^.Base;

    | dt.Set:
      PSetType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PSetType^.Base;

    | dt.Array:
      PArrayType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PArrayType^.Base;

    | dt.Array_of:
      PArrayOfType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PArrayOfType^.Base;

    | dt.OpenArray:
      POpenArrayType := sys.CAST(sys.ADDRESS, ptag);
      subtype := POpenArrayType^.Base;

    | dt.Pointer:
      PPointerType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PPointerType^.Base;

    | dt.Reference:
      PReferenceType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PReferenceType^.Base;

    | dt.Range:
      PRangeType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PRangeType^.Base;

    | dt.Class:
      PClassType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PClassType^.Base;

    | dt.Procedure:
      PProcType := sys.CAST(sys.ADDRESS, ptag);
      subtype := PProcType^.ResultType;
    END;
  END;
END SubType;


(* Выдает тип индекса массива  *)
(* ARRAY Index OF ...          *)
PROCEDURE ArrayIndexType (type: dt.PTYPE; VAR index: dt.PTYPE);
VAR
  PArrayType: POINTER TO dt.TYPE_ARRAY;
BEGIN
  ASSERT (IsTypeValid(type));
  PArrayType:= TypeImage (type);
  WITH PArrayType^ DO
    CASE TypeData.Tag OF
    | dt.Array:
      index := Index;
    | dt.Array_of
    , dt.OpenArray:
      index := dt.STD_TYPE_CARD32;
    END;
  END;
END ArrayIndexType;


(* Для типа RECORD или CLASS по номеру поля в записи       *)
(* из диапазона 1..TypeLen(type) выдает информацию о поле  *)
PROCEDURE Field (type: dt.PTYPE; no: CARDINAL; VAR field: dt.TYPE_RECORD_FIELD);

VAR
  PTypeData       : POINTER TO dt.TYPE_DATA;
  PRecordType     : POINTER TO dt.TYPE_RECORD;
  PRecFieldType   : POINTER TO dt.TYPE_RECORD_FIELD;
  PClassType      : POINTER TO dt.TYPE_CLASS;

BEGIN
  ASSERT (IsTypeValid(type));
   PTypeData := TypeImage (type);
    CASE PTypeData^.Tag OF
    | dt.Record:
      PRecordType := sys.ADDRESS(PTypeData);
      WITH PRecordType^ DO
        ASSERT ((no#0) AND (no<=Fields));
        PRecFieldType:= sys.ADDADR(PRecordType, SIZE(dt.TYPE_RECORD) + SIZE(dt.TYPE_RECORD_FIELD) * (no-1));
        field := PRecFieldType^;
      END;
    | dt.Class:
      PClassType := sys.ADDRESS(PTypeData);
      WITH PClassType^ DO
        ASSERT ((no#0) AND (no<=AllMembers));
        type := Base;
        IF no <= AllMembers-MyMembers THEN
          Field(type, no, field);
        ELSE
          PRecFieldType:=  sys.ADDADR(PClassType,SIZE(dt.TYPE_CLASS) + SIZE(dt.TYPE_RECORD_FIELD) * (no-1-(AllMembers-MyMembers)));
          field := PRecFieldType^;
        END;
      END;
    END;
END Field;


(* Устанавливает для поля типа нужный формат отображения *)
PROCEDURE SetFieldSTID (type: dt.PTYPE; no: CARDINAL; st_id: dt.SYM_TYPE_ID);
VAR
  PTypeData       : POINTER TO dt.TYPE_DATA;
  PRecordType     : POINTER TO dt.TYPE_RECORD;
  PRecFieldType   : POINTER TO dt.TYPE_RECORD_FIELD;
  PClassType      : POINTER TO dt.TYPE_CLASS;
BEGIN
  ASSERT (IsTypeValid(type));
    PTypeData := TypeImage (type);
    CASE PTypeData^.Tag OF
    | dt.Record:
      PRecordType := sys.ADDRESS(PTypeData);
      WITH PRecordType^ DO
        ASSERT((no#0) AND (no<=Fields));
      END;
      PRecFieldType := sys.ADDADR(PRecordType,SIZE(dt.TYPE_RECORD) + SIZE(dt.TYPE_RECORD_FIELD) * (no-1) );
    | dt.Class:
      PClassType := sys.ADDRESS(PTypeData);
      WITH PClassType^ DO
        ASSERT((no#0) AND (no<=AllMembers));
        type := Base;
        IF no <= AllMembers-MyMembers THEN
          SetFieldSTID (type, no, st_id);
          RETURN;
        END;
        PRecFieldType:= sys.ADDADR(PClassType,SIZE(dt.TYPE_CLASS) + SIZE(dt.TYPE_RECORD_FIELD) * (no-1-(AllMembers-MyMembers)));
      END;
    END;
    PRecFieldType^.FieldSTID := st_id;
END SetFieldSTID;


<* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
(* Для типа RECORD или CLASS по имени поля в записи выдает    *)
(* его номер из диапазона 1..TypeLen(type) и информацию о нем *)
PROCEDURE Field_no (    type  : dt.PTYPE; name: xs.txt_ptr;
                    VAR no    : CARDINAL;
                    VAR field : dt.TYPE_RECORD_FIELD);

VAR
  PTypeData       : POINTER TO dt.TYPE_DATA;
  PRecordType     : POINTER TO dt.TYPE_RECORD;
  PRecFieldType   : POINTER TO dt.TYPE_RECORD_FIELD;
  PClassType      : POINTER TO dt.TYPE_CLASS;
  PClassFieldType : POINTER TO dt.TYPE_CLASS_MEMBER;
  pos,i: CARDINAL;
  nam_f : xs.txt_ptr;
BEGIN
  ASSERT( IsTypeValid(type) );
  base := type;
  WITH dt.Components.Components^[type.com].DI.Types DO
    pos := key^[type.rec-dt.FIRST_NONPRIMITIVE];
    PTypeData := sys.ADR(ptr^[pos]);
    CASE PTypeData^.Tag OF
    | dt.Record:
      PRecordType := sys.ADDRESS(PTypeData);
        INC(pos, SIZE(dt.TYPE_RECORD));
    WITH PRecordType^ DO

     FOR i:=1 TO Fields DO
        PRecFieldType := sys.ADR(ptr^[pos]);
        WITH PRecFieldType^ DO
          nam_f    := GetName (type.com, FieldName);
          base.rec := FieldType;
          offs     := FieldOffs;
          st_id    := FieldSTID;
        END;
       IF nam_f^ = name^ THEN no:=i; RETURN;END;
        INC(pos, SIZE(dt.TYPE_RECORD_FIELD));
     END;
    END;
    | dt.Class:
(*      PClassType := sys.ADDRESS(PTypeData);
      WITH PClassType^ DO
        ASSERT( (no#0) AND (no<=AllMembers) );
        type.rec := Base;
        IF no <= AllMembers-MyMembers THEN
          Field(type, no, name, base, offs);
          RETURN;
        END;
        INC(pos, SIZE(dt.TYPE_CLASS) + SIZE(dt.TYPE_CLASS_MEMBER) * (no-1-(AllMembers-MyMembers)));
        PClassFieldType := sys.ADR(ptr^[pos]);
        WITH PClassFieldType^ DO
          name     := GetName (type.com, MemberName);
          base.rec := MemberType;
          offs     := MemberOffs;
          st_id    := MemberSTID;
        END;
      END;
*)
    ELSE
      ASSERT(FALSE);
    END;
  END;
END Field_no;
<* END *>


(* Для типа выдает минимальное и максимальное значение,       *)
(* допустимые для переменых этого типа, приведеные к LONGCARD *)
PROCEDURE Index (type: dt.PTYPE; VAR min, max: LONGCARD);

  PROCEDURE Max (l: CARDINAL): LONGCARD;
  BEGIN
    CASE l OF
    | 1 : RETURN 0FFH;
    | 2 : RETURN 0FFFFH;
    | 3 : RETURN 0FFFFFFH;
    | 4 : RETURN 0FFFFFFFFH;
    END;
  END Max;

VAR
  tag : dt.TYPE_TAG;
  ptag: POINTER TO dt.TYPE_TAG;
  i  : CARDINAL;
  PEnumType    : POINTER TO dt.TYPE_ENUM;
  PEnumItemType: POINTER TO dt.TYPE_ENUM_ITEM;
  PSetType     : POINTER TO dt.TYPE_SET;
  PArrayType   : POINTER TO dt.TYPE_ARRAY;
  PRangeType   : POINTER TO dt.TYPE_RANGE;

BEGIN
  ASSERT( IsTypeValid(type) );
  IF IsTypeBasePrimitive (type) THEN
    min := 0;
    ASSERT(TypeTag(type, tag));
    CASE tag OF
    | dt.Boolean:
      max := 1;
    | dt.Int:
      min := Max(dt.STD_TYPES[type].Length) DIV 2 + 1;
      max := Max(dt.STD_TYPES[type].Length) DIV 2;
    | dt.Card, dt.Char, dt.Address, dt.Byte:
      max := Max(dt.STD_TYPES[type].Length);
    | dt.Set:
      max := 8*dt.STD_TYPES[type].Length-1;
    END;
    RETURN;
  ELSIF IsTypePointerPrimitive (type) THEN
    min := 0;
    ASSERT(TypeTag(type, tag));
    ASSERT(tag=dt.Pointer);
    max := Max(4);
  END;
  ptag:= TypeImage (type);
  CASE ptag^ OF
  | dt.Range:
      PRangeType := sys.CAST(sys.ADDRESS, ptag);
      WITH PRangeType^ DO
        min := CARDINAL(Min);
        max := CARDINAL(Max);
      END;

  | dt.Enum:
      PEnumType := sys.CAST(sys.ADDRESS, ptag);
      min := MAX(LONGCARD);
      max := 0;
      FOR i := 1 TO PEnumType^.Quantity DO
        PEnumItemType:= sys.ADDADR (ptag, SIZE(dt.TYPE_ENUM)+(i-1)*SIZE(dt.TYPE_ENUM_ITEM));
        WITH PEnumItemType^ DO
          IF EnumValue < min THEN min := EnumValue; END;
          IF EnumValue > max THEN max := EnumValue; END;
        END;
      END;

  | dt.Set:
      PSetType := sys.CAST(sys.ADDRESS, ptag);
      type := PSetType^.Base;
      Index(type, min, max);
      ASSERT(TypeTag(type, tag));
      IF tag = dt.Int THEN
        INC (max, min);
        min := 0;
      END;

  | dt.Array:
      PArrayType := sys.CAST(sys.ADDRESS, ptag);
      type := PArrayType^.Index;
      Index(type, min, max);

  | dt.Array_of, dt.OpenArray:
     min := 0;
     max := MAX(LONGCARD);
  END;
END Index;


(* Для перечислимого типа по значению элемента выдает имя элемента *)
PROCEDURE EnumName (    type : dt.PTYPE;
                        value: CARDINAL;
                    VAR name : ARRAY OF CHAR);

VAR
  i  : CARDINAL;
  nm : xs.txt_ptr;
  PEnumType    : POINTER TO dt.TYPE_ENUM;
  PEnumItemType: POINTER TO dt.TYPE_ENUM_ITEM;

BEGIN
  ASSERT (IsTypeValid(type));
  PEnumType := TypeImage (type);
  WITH PEnumType^ DO
    ASSERT (TypeData.Tag = dt.Enum);
    FOR i := 1 TO PEnumType^.Quantity DO
      PEnumItemType := sys.ADDADR(PEnumType,SIZE(dt.TYPE_ENUM)+(i-1)*SIZE(dt.TYPE_ENUM_ITEM));
      WITH PEnumItemType^ DO
        IF value = EnumValue THEN
          nm := GetName ( EnumName);
          COPY(nm^, name);
          RETURN;
        END;
      END;
    END;
  END;
  fmt.print(name, "%u", value);
END EnumName;


(* Для перечислимого типа по имени элемента выдает его значение *)
PROCEDURE EnumValue (    type : dt.PTYPE;
                         name-: ARRAY OF CHAR;
                     VAR value: CARDINAL): BOOLEAN;

VAR
  i :  CARDINAL;
  nm: xs.txt_ptr;
  PEnumType    : POINTER TO dt.TYPE_ENUM;
  PEnumItemType: POINTER TO dt.TYPE_ENUM_ITEM;

BEGIN
  ASSERT( IsTypeValid(type) );
  PEnumType := TypeImage (type);
  WITH PEnumType^ DO
    ASSERT (TypeData.Tag = dt.Enum);
    FOR i := 1 TO PEnumType^.Quantity DO
      PEnumItemType := sys.ADDADR(PEnumType,SIZE(dt.TYPE_ENUM)+(i-1)*SIZE(dt.TYPE_ENUM_ITEM));
      WITH PEnumItemType^ DO
        nm := GetName (EnumName);
        IF name = nm^ THEN
          value := EnumValue;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END EnumValue;



PROCEDURE CompareEnumNames (i: CARDINAL): INTEGER;
VAR
  in: xs.txt_ptr;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Modules^[CurrMod-1].Enumerations DO
    in := GetName (Enums^[KEName^[i]].Name);
  END;
  IF    in^ < CurrName THEN RETURN -1;
  ELSIF in^ > CurrName THEN RETURN +1;
  ELSE                      RETURN  0;
  END;
END CompareEnumNames;


PROCEDURE FindValueByEnumeration (com      : dt.ComNo;
                                  mod      : dt.ModNo;
                                  name-    : ARRAY OF CHAR;
                                  VAR value: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  IF CheckDebugInfoForModule (com, mod) THEN
    WITH dt.Components.Components^[com].DI.Modules^[mod-1].Enumerations DO
      IF Count > 0 THEN
        IF KEName = NIL THEN
          bld.SortEnumerationsByName (com, mod);
          ASSERT(KEName#NIL);
        END;
        COPY(name, CurrName);
        CurrCom := com;
        CurrMod := mod;
        IF sor.BinaryFind(Count, CompareEnumNames, i) THEN
          value := Enums^[KEName^[i]].Value;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindValueByEnumeration;



(* Сравнение типов *)
PROCEDURE TypesCompatible (type1, type2: dt.PTYPE): BOOLEAN;
VAR
  tag1    : dt.TYPE_TAG;
  tag2    : dt.TYPE_TAG;
  subtype1: dt.PTYPE;
  subtype2: dt.PTYPE;
  min1    : CARDINAL;
  max1    : CARDINAL;
  min2    : CARDINAL;
  max2    : CARDINAL;
  i       : CARDINAL;
  value1  : CARDINAL;
  value2  : CARDINAL;
  count   : CARDINAL;
  res     : BOOLEAN;
  nm1, nm2: xs.String;
  field1  : dt.TYPE_RECORD_FIELD;
  field2  : dt.TYPE_RECORD_FIELD;

BEGIN
  IF NOT IsTypeValid(type1) OR NOT IsTypeValid(type2) THEN RETURN FALSE; END;
  ASSERT( TypeTag(type1, tag1) );
  ASSERT( TypeTag(type2, tag2) );
  IF tag1 # tag2 THEN RETURN FALSE; END;
  CASE tag1 OF
  | dt.T_Void:

  | dt.Byte,   dt.Char,    dt.Int,     dt.Card
  , dt.Real,   dt.Complex, dt.Address, dt.Pointer
  , dt.Boolean:
    IF TypeSize(type1) # TypeSize(type2) THEN RETURN FALSE; END;

  | dt.Range:
    SubType(type1, subtype1);
    SubType(type2, subtype2);
    Index(type1, min1, max1);
    Index(type2, min2, max2);
    IF NOT TypesCompatible(subtype1, subtype2) OR (min1#min2) OR (max1#max2) THEN RETURN FALSE; END;

  | dt.Array_of, dt.OpenArray, dt.Set, dt.Procedure:
    SubType(type1, subtype1);
    SubType(type2, subtype2);
    IF NOT TypesCompatible(subtype1, subtype2) THEN RETURN FALSE; END;

  | dt.Array:
    SubType(type1, subtype1);
    SubType(type2, subtype2);
    IF NOT TypesCompatible(subtype1, subtype2) THEN RETURN FALSE; END;
    ArrayIndexType(type1, subtype1);
    ArrayIndexType(type2, subtype2);
    IF NOT TypesCompatible(subtype1, subtype2) THEN RETURN FALSE; END;

  | dt.Enum:
    SubType(type1, subtype1);
    SubType(type2, subtype2);
    IF NOT TypesCompatible(subtype1, subtype2) THEN RETURN FALSE; END;
    count := TypeLen(type1);
    IF count # TypeLen(type2) THEN RETURN FALSE; END;
    FOR i := 0 TO count-1 DO
      EnumName(type1, i, nm1);
      EnumName(type2, i, nm2);
      IF nm1 # nm2 THEN RETURN FALSE; END;
      res := EnumValue(type1, nm1, value1);
      IF res # EnumValue(type2, nm2, value2) THEN RETURN FALSE; END;
      IF res AND (value1 # value2) THEN RETURN FALSE; END;
    END;

  | dt.Record:
    count := TypeLen(type1);
    IF count # TypeLen(type2) THEN RETURN FALSE; END;
    FOR i := 1 TO count DO
      Field(type1, i, field1);
      Field(type2, i, field2);
      subtype1 :=  field1.FieldType;
      subtype2 :=  field2.FieldType;
      IF NOT TypesCompatible(subtype1, subtype2) THEN RETURN FALSE; END;
      IF field1.FieldOffs # field2.FieldOffs THEN RETURN FALSE; END;
    END;

  | dt.Class:
    SubType(type1, subtype1);
    SubType(type2, subtype2);
    IF NOT TypesCompatible(subtype1, subtype2) THEN RETURN FALSE; END;
    count := TypeLen(type1);
    IF count # TypeLen(type2) THEN RETURN FALSE; END;
    FOR i := 1 TO count DO
      Field(type1, i, field1);
      Field(type2, i, field2);
      subtype1 :=  field1.FieldType;
      subtype2 :=  field2.FieldType;
      IF NOT TypesCompatible(subtype1, subtype2) THEN RETURN FALSE; END;
      IF field1.FieldOffs # field2.FieldOffs THEN RETURN FALSE; END;
    END;
  END;
  RETURN TRUE;
END TypesCompatible;



PROCEDURE Get_Next_Type(VAR type : dt.PTYPE);
VAR
  new_ref, ref: Ud.IMAGE_REF;
BEGIN
     ref := Ud.IMAGE_REF(type - dt.FIRST_NONPRIMITIVE);
     new_ref:= Ud.Next(bld.TypesTbl, ref,  bld.TypeImageLength  );
     IF Ud.IsRefValid (bld.TypesTbl, new_ref) THEN
       type:= dt.R_TYPE(new_ref) + dt.FIRST_NONPRIMITIVE;
     ELSE
       type := dt.Invalid_Type;
     END;
END  Get_Next_Type;



PROCEDURE FirstType(): Ud.IMAGE_REF;
BEGIN
  RETURN Ud.First(bld.TypesTbl) + dt.FIRST_NONPRIMITIVE;
END  FirstType;


PROCEDURE FindTypeLikeThis (com: dt.ComNo; type_image: ARRAY OF sys.LOC; casesensitive: BOOLEAN; VAR type: dt.PTYPE): BOOLEAN;
VAR
  typedata: dt.PTYPE_DATA;
  subtype1: dt.PTYPE;
  subtype2: dt.PTYPE;
  type_tag: dt.TYPE_TAG;
  name1   : xs.txt_ptr;
  name2   : xs.txt_ptr;
  Name1   : xs.String;
  Name2   : xs.String;

  PPointerType   : POINTER TO dt.TYPE_POINTER;
  PProcType      : POINTER TO dt.TYPE_PROCEDURE;
  PSetType       : POINTER TO dt.TYPE_SET;
  PArrayType     : POINTER TO dt.TYPE_ARRAY;
  PArrayOfType   : POINTER TO dt.TYPE_ARRAY;
  POpenArrayType : POINTER TO dt.TYPE_ARRAY;
  PRangeType     : POINTER TO dt.TYPE_RANGE;
  PReferenceType : POINTER TO dt.TYPE_REFERENCE;

BEGIN
  IF NOT IsComValid (com) THEN
    RETURN FALSE;
  END;
  IF NOT IsTypeValid (type) THEN
    RETURN FALSE;
  END;
      IF NOT Ud.IsEmptyStorage(bld.TypesTbl) THEN
      typedata := sys.ADR(type_image);
      CASE typedata^.Tag OF
      | dt.Array:
        PArrayType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := PArrayType^.Base;
      | dt.Array_of:
        PArrayOfType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := PArrayOfType^.Base;
      | dt.OpenArray:
        POpenArrayType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := POpenArrayType^.Base;
      | dt.Pointer:
        PPointerType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := PPointerType^.Base;
      | dt.Reference:
        PReferenceType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := PReferenceType^.Base;
      | dt.Range:
        PRangeType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := PRangeType^.Base;
      | dt.Procedure:
        PProcType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := PProcType^.ResultType;
      | dt.Set:
        PSetType := sys.CAST (sys.ADDRESS, typedata);
        subtype1 := PSetType^.Base;
      ELSE
        RETURN FALSE;
      END;
      ASSERT (TypeName (subtype1, name1));
      IF NOT casesensitive THEN
        COPY (name1^, Name1);
        xs.Uppercase (Name1);
      END;
      type := FirstType();
      LOOP
        IF TypeTag (type, type_tag) AND (type_tag = typedata^.Tag) THEN
          SubType (type, subtype2);
          ASSERT (TypeName (subtype2, name2));
          IF casesensitive THEN
            IF (name1^ = name2^) AND TypesCompatible (subtype1, subtype2) THEN
              RETURN TRUE;
            END;
          ELSE
            COPY (name2^, Name2);
            xs.Uppercase (Name2);
            IF (Name1 = Name2) AND TypesCompatible (subtype1, subtype2) THEN
              RETURN TRUE;
            END;
          END;
        END;
        Get_Next_Type(type);
        IF NOT IsTypeValid(type) THEN
          EXIT;
        END;
      END;
    END;
  RETURN FALSE;
END FindTypeLikeThis;


------ Clear debug processed flag ---------

PROCEDURE ClearDebugProcessed (com: dt.ComNo);
VAR
  i: CARDINAL;
BEGIN
  WITH dt.Components.Components^[com] DO
    WITH DI DO
      IF LastModule = 0 THEN
        RETURN;
      END;
      FOR i := 0 TO LastModule-1 DO
        WITH Modules^[i] DO
          IF NOT HasInfo THEN
            DebugInfoProcessed := FALSE;
            HasInfo := TRUE;
          END;
        END;
      END;
    END;
  END;
END ClearDebugProcessed;




------ Clear debug information ---------

PROCEDURE ClearDebugInfo (com: dt.ComNo);
VAR
  i: CARDINAL;
BEGIN
  ASSERT( IsComValid(com) );
  WITH dt.Components.Components^[com] DO
    WITH DI DO
      IF Modules # NIL THEN
        FOR i := 0 TO HIGH(Modules^) DO
          WITH Modules^[i] DO
            IF Text # txt.nil THEN txt.Close(Text); END;
            WITH CLTable DO
              IF CLTable # NIL THEN DISPOSE(CLTable); END;
              IF KCLTableAddr # NIL THEN DISPOSE(KCLTableAddr); END;
              IF KCLTableLine # NIL THEN DISPOSE(KCLTableLine); END;
            END;
            IF Segments # NIL THEN DISPOSE(Segments); END;
            WITH ModuleTypes DO
              IF KTName # NIL THEN
                DISPOSE(KTName);
              END;
              Sorted := FALSE;
              KTQuantity := 0;
            END;
            WITH ModuleObjects DO
              WITH RawObjects DO
                IF Count # 0 THEN
                  DISPOSE(RawObjects);
                  Count := 0;
                END;
              END;
              IF KObjects # NIL THEN DISPOSE(KObjects); END;
              IF KObjName # NIL THEN DISPOSE(KObjName); END;
              IF KObjAddr # NIL THEN DISPOSE(KObjAddr); END;
            END;
            WITH Enumerations DO
              IF Enums # NIL THEN DISPOSE(Enums); END;
              IF KEName # NIL THEN DISPOSE(KEName); END;
            END;
            IF DebugInfoReference # NIL THEN
              xs.dealloc_str (DebugInfoReference);
            END;
            DebugInfoProcessed := FALSE;
          END;
        END;
        DISPOSE(Modules);
      END;
      LastModule := 0;
      IF KModules # NIL THEN
        DISPOSE(KModules);
      END;
      WITH Publics DO
        IF Publics # NIL THEN DISPOSE(Publics); END;
        IF KPAddr # NIL THEN DISPOSE(KPAddr); END;
        IF KPName # NIL THEN DISPOSE(KPName); END;
      END;
      Publics := dt.EmptyPublics;
      IF KGSegments # NIL THEN DISPOSE(KGSegments); END;
      IF ProcBeginAddr # NIL THEN DISPOSE(ProcBeginAddr); END;
      WITH GlobalObjects DO
        WITH RawObjects DO
          IF Count # 0 THEN
            DISPOSE(RawObjects);
            Count := 0;
          END;
        END;
        IF KObjects # NIL THEN DISPOSE(KObjects); END;
        IF KObjName # NIL THEN DISPOSE(KObjName); END;
        IF KObjAddr # NIL THEN DISPOSE(KObjAddr); END;
      END;
    END;
  END;
END ClearDebugInfo;


BEGIN
  String := '';
  EmptyString := sys.ADR(String);
END DI_Tools.











