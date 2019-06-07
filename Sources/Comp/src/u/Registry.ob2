<*+O2EXTENSIONS *>
MODULE Registry; (* paul 04-Aug-98. *)

IMPORT Strings;
IMPORT DStrings;
IMPORT env := xiEnv;
IMPORT pc  := pcK;

TYPE
  NAME *= ARRAY 32 OF CHAR;

TYPE
  ITEM  *= POINTER TO item_rec;
  item_rec *= RECORD
                name*: NAME;
                next_item: ITEM;
              END;

TYPE
  LIST *= POINTER TO list_rec;
  list_rec *= RECORD(item_rec)
                head    : ITEM;
                active *: ITEM;
              END;

PROCEDURE (list: LIST) AddItem* (name-: ARRAY OF CHAR; i: ITEM);
BEGIN
   COPY(name, i.name);
   Strings.Capitalize(i.name);
   i.next_item := list.head;
   list.head := i;
END AddItem;

PROCEDURE (list: LIST) FindItem* (name-: ARRAY OF CHAR): ITEM;
VAR
  p: ITEM;
  n: NAME;
BEGIN
  COPY(name, n);
  Strings.Capitalize(n);
  p := list.head;
  WHILE (p # NIL) DO
    IF p.name = n THEN
      RETURN p;
    END;
    p := p.next_item;
  END;
  RETURN NIL;
END FindItem;

VAR
  registry-: LIST;

PROCEDURE Register* (listname-: ARRAY OF CHAR; iname-: ARRAY OF CHAR; item: ITEM);
VAR
  list: ITEM;
BEGIN                                
  list := registry.FindItem(listname);
  WITH list: LIST DO
    list.AddItem(iname, item);
  ELSE
    ASSERT(FALSE);
  END;
END Register;

PROCEDURE isExists* (listname-: ARRAY OF CHAR; iname-: ARRAY OF CHAR): BOOLEAN;
VAR
  list: ITEM;
  item: ITEM;
BEGIN
  list := registry.FindItem(listname);
  WITH list: LIST DO
    item := list.FindItem(iname);
    RETURN item # NIL;
  ELSE
    ASSERT(FALSE);
  END;
END isExists;

PROCEDURE SetActive* (listname-: ARRAY OF CHAR; iname-: ARRAY OF CHAR);
VAR
  list: ITEM;
  item: ITEM;
BEGIN
  list := registry.FindItem(listname);
  WITH list: LIST DO
    item := list.FindItem(iname);
--    ASSERT(item # NIL);
    list.active := item;
  ELSE
    ASSERT(FALSE);
  END;
END SetActive;

PROCEDURE GetActive* (name-: ARRAY OF CHAR): ITEM;
VAR
  list: ITEM;
BEGIN
  list := registry.FindItem(name);
  WITH list: LIST DO
    RETURN list.active;
  ELSE
    ASSERT(FALSE);
  END;
END GetActive;

PROCEDURE NewList* (name-: ARRAY OF CHAR);
VAR
  list: LIST;
BEGIN
  NEW(list);
  list.head := NIL;
  list.active := NIL;
  registry.AddItem(name, list);
END NewList;

PROCEDURE SetFromEquation* ( equationName-: ARRAY OF CHAR;
                             defaultName-: ARRAY OF CHAR;
                             reglistName-: ARRAY OF CHAR;
                             list: LIST ) : ITEM;
  VAR s   : env.String;
      eq  : NAME;
      txtpos: env.TPOS;
BEGIN
  COPY(defaultName, eq);
  env.config.Equation(equationName, s);
  IF (s # NIL) & (s[0] # 0X) THEN
    Strings.Capitalize(s^);
    IF (s^ # defaultName) THEN
    -- if nil passed as 'list' then 'list' should be found in registry
      IF ((list=NIL) OR (list.FindItem(s^) # NIL))
         & isExists(reglistName, s^) THEN
        COPY(s^, eq);
      ELSE
        IF pc.cur_mod < pc.mod_cnt THEN
          txtpos := pc.mods[pc.cur_mod].pos;
        ELSE
          txtpos := env.null_pos;
        END;
        env.errors.Warning (txtpos, 434, equationName, defaultName);
      END;
    END;
  END;

  SetActive(reglistName, eq); -- if not exists, reglist.active:=NIL
  RETURN GetActive(reglistName);
END SetFromEquation;

BEGIN
  NEW(registry);
  registry.head := NIL;
  registry.active := NIL;
  registry.next_item := NIL;
  COPY("registry", registry.name);
END Registry.
