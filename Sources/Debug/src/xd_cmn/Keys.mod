IMPLEMENTATION MODULE Keys;

IMPORT sys := SYSTEM;
IMPORT io  := InOut;

IMPORT con := Console;

PROCEDURE KeyPressed(): BOOLEAN;
BEGIN
  RETURN con.QueryEvent(con.KeyEvent);
END KeyPressed;

(* Return key code *)
PROCEDURE GetKey(): KEY;
VAR
  event: con.EVENT;
BEGIN
  LOOP
    con.GetEvent(event);
    IF event.kind = con.KeyEvent THEN
      RETURN event.key_code;
    END;
  END;
END GetKey;

(* Clear key buffer *)
PROCEDURE FlashKey;
BEGIN
  WHILE KeyPressed() DO
    sys.EVAL(GetKey());
  END;
END FlashKey;

(* Delay until key pressed *)
PROCEDURE DelayKeyPressed;
BEGIN
  REPEAT
  UNTIL KeyPressed();
  FlashKey;
END DelayKeyPressed;

(* Delay d fraction second (1 fraction = 1/100 second) *)
PROCEDURE Delay (d: CARDINAL);
BEGIN
  con.Delay (d);
END Delay;

(* Delay d fraction or until key pressed *)
PROCEDURE DelayUntilKeyPressed (d:CARDINAL);
CONST
 pause = 25;

VAR
  count : CARDINAL;

BEGIN
  count := 0;
  REPEAT
    Delay (pause-1);
    INC (count, pause);
  UNTIL KeyPressed() OR (count >= d);
  FlashKey;
END DelayUntilKeyPressed;


TYPE
  KEY_NAME  = ARRAY [0..9] OF CHAR;
  KEY_NAMES = ARRAY [0..255] OF KEY_NAME;

CONST
  KeyNamesMain = KEY_NAMES {

(*      --0--        --1--        --2--        --3--        --4--        --5--        --6--        --7--        --8--        --9--        --A--        --B--        --C--         --D--        --E--        --F--     *)

(* 0 *) ""          ,"Ctrl-A"    ,"Ctrl-B"    ,"Ctrl-C"    ,"Ctrl-D"    ,"Ctrl-E"    ,"Ctrl-F"    ,"Ctrl-G"    ,"Ctrl-H"    ,"Ctrl-I"    ,"Ctrl-J"    ,"Ctrl-K"    ,"Ctrl-L"    , "Ctrl-M"    ,"Ctrl-N"    ,"Ctrl-O"    ,
(* 1 *) "Ctrl-P"    ,"Ctrl-Q"    ,"Ctrl-R"    ,"Ctrl-S"    ,"Ctrl-T"    ,"Ctrl-U"    ,"Ctrl-V"    ,"Ctrl-W"    ,"Ctrl-X"    ,"Ctrl-Y"    ,"Ctrl-Z"    ,"Esc"       ,"Ctrl-\"    , ""          ,"Ctrl-6"    ,""          ,
(* 2 *) "Space"     ,""          ,""          ,"#"         ,""          ,""          ,""          ,""          ,""          ,""          ,"*"         ,"Plus"      ,""          , "Minus"     ,"."         ,"/"         ,
(* 3 *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,"?"         ,
(* 4 *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* 5 *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,"\"         , ""          ,""          ,""          ,
(* 6 *) ""          ,"a"         ,"b"         ,"c"         ,"d"         ,"e"         ,"f"         ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* 7 *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* 8 *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* 9 *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* A *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* B *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* C *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* D *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* E *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* F *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          };



  KeyNamesExtended = KEY_NAMES {

(*      --0--        --1--        --2--        --3--        --4--        --5--        --6--        --7--        --8--        --9--        --A--        --B--        --C--         --D--        --E--        --F--     *)

(* 0 *) ""          ,""          ,""          ,"Ctrl-2"    ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* 1 *) "Alt-Q"     ,"Alt-W"     ,"AlrE"      ,"Alt-R"     ,"Alt-T"     ,"Alt-Y"     ,"Alt-U"     ,"Alt-I"     ,"Alt-O"     ,"Alt-P"     ,"Alt-["     ,"Alt-]"     ,"Alt-Enter" , ""          ,"Alt-A"     ,"Alt-S"     ,
(* 2 *) "Alt-D"     ,"Alt-F"     ,"Alt-G"     ,"Alt-H"     ,"Alt-J"     ,"Alt-K"     ,"Alt-L"     ,"Alt-;"     ,'Alt-"'     ,""          ,""          ,"Alt-\"     ,"Alt-Z"     , "Alt-X"     ,"Alt-C"     ,"Alt-V"     ,
(* 3 *) "Alt-B"     ,"Alt-N"     ,"Alt-M"     ,"Alt-,"     ,"Alt-."     ,"Alt-/"     ,""          ,""          ,""          ,""          ,""          ,"F1"        ,"F2"        , "F3"        ,"F4"        ,"F5"        ,
(* 4 *) "F6"        ,"F7"        ,"F8"        ,"F9"        ,"F10"       ,""          ,""          ,"Home"      ,"Up"        ,"PgUp"      ,""          ,"Left"      ,""          , "Right"     ,""          ,"End"       ,
(* 5 *) "Down"      ,"PgDn"      ,"Ins"       ,"Del"       ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,"Ctrl-F1"   ,"Ctrl-F2"   ,
(* 6 *) "Ctrl-F3"   ,"Ctrl-F4"   ,"Ctrl-F5"   ,"Ctrl-F6"   ,"Ctrl-F7"   ,"Ctrl-F8"   ,"Ctrl-F9"   ,"Ctrl-F10"  ,"Alt-F1"    ,"Alt-F2"    ,"Alt-F3"    ,"Alt-F4"    ,"Alt-F5"    , "Alt-F6"    ,"Alt-F7"    ,"Alt-F8"    ,
(* 7 *) "Alt-F9"    ,"Alt-F10"   ,""          ,"Ctrl-Left" ,"Ctrl-Right","Ctrl-End"  ,"Ctrl-PgDn" ,"Ctrl-Home" ,"Alt-1"     ,"Alt-2"     ,"Alt-3"     ,"Alt-4"     ,"Alt-5"     , "Alt-6"     ,"Alt-7"     ,"Alt-8"     ,
(* 8 *) "Alt-9"     ,"Alt-0"     ,""          ,""          ,"Ctrl-PgUp" ,"F11"       ,"F12"       ,""          ,""          ,"Ctrl-F11"  ,"Ctrl-F12"  ,"Alt-F11"   ,"Alt-F12"   , "Ctrl-Up"   ,"Ctrl-Minus",""          ,
(* 9 *) "Ctrl-Plus" ,"Ctrl-Down" ,"Ctrl-Ins"  ,"Ctrl-Del"  ,"Ctrl-Tab"  ,""          ,"Ctrl-*"    ,"Alt-Home"  ,"Alt-Up"    ,"Alt-PgUp"  ,""          ,"Alt-Left"  ,""          , "Alt-Right" ,""          ,"Alt-End"   ,
(* A *) "Alt-Down"  ,"Alt-PgDn"  ,"Alt-Ins"   ,"Alt-Del"   ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* B *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* C *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* D *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* E *) ""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , ""          ,""          ,""          ,
(* F *) "Ctrl-Space",""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          ,""          , "Ctrl-Enter","Enter"     ,""          };





(* По клавише выдает ее имя *)
PROCEDURE GetKeyName (key: KEY; VAR key_name: ARRAY OF CHAR): BOOLEAN;
VAR
  KeyNames: POINTER TO KEY_NAMES;
BEGIN
  IF key <= 0FFH THEN
    KeyNames := sys.ADR(KeyNamesMain);
  ELSIF (00100H <= key) AND (key <= 0FF00H) THEN
    KeyNames := sys.ADR(KeyNamesExtended);
    key := key DIV 0100H;
  ELSE
    RETURN FALSE;
  END;
  IF KeyNames^[key] # "" THEN
    COPY(KeyNames^[key], key_name);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetKeyName;


(* По имени клавиши выдает ее код *)
PROCEDURE GetKeyByName (key_name-: ARRAY OF CHAR; VAR key: KEY): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  IF key_name = '' THEN RETURN FALSE; END;
  FOR i := 0 TO 0FFH DO
    IF KeyNamesMain[i] = key_name THEN
      key := i;
      RETURN TRUE;
    END;
    IF KeyNamesExtended[i] = key_name THEN
      key := i * 0100H;
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END GetKeyByName;

BEGIN
END Keys.
