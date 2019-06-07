--------------------------------------------------------------------------------
--                      Excelsior XDS Test Coverage System
--                          (c) 2015, Excelsior Ltd.
-- Module:   tcLib
-- Mission:  Library for Test Coverage System and related tools.
-- Authors:  Lvov Konstantin
-- Created:  07-Mar-2006
--
-- It's a part of Excelsior XDS O2/M2 compiler. 
-- Set option '-target_testcoverage:+' to build in this tool into compiler.
--------------------------------------------------------------------------------
MODULE tcLib;

IMPORT  sys := SYSTEM,         Strings,               RegComp
     ,  DStrings,              FileSys
     ;
TYPE 
  String = DStrings.String;


--------------------------------------------------------------------------------
PROCEDURE ReplaceAllSlashes * (VAR str: ARRAY OF CHAR);
VAR i: LONGINT;
BEGIN
  FOR i:=0 TO LEN(str)-1 DO
    IF str[i] ='\' THEN
       str[i]:='/'
    END;
  END;
END ReplaceAllSlashes;


--------------------------------------------------------------------------------
PROCEDURE InsertEscapeSymbols (str-: ARRAY OF CHAR): String;

    -- 1 -- InsertEscapeSymbols ------------------------------------------------
    PROCEDURE escapeRegSymbol (src-: ARRAY OF CHAR; ch: CHAR): String;
    VAR str: ARRAY 1024 OF CHAR;
    VAR tmp: String;
        found: BOOLEAN;
        start, pos: sys.CARD32;
    BEGIN
      Strings.Assign(src, str);

      Strings.FindNext(ch, str, 0, found, pos);
      WHILE found DO
        Strings.Insert("\", pos, str);
        start := pos + 2;
        Strings.FindNext(ch, str, start, found, pos);
      END;
      DStrings.Assign(str, tmp);
      RETURN tmp;
    END escapeRegSymbol;

-- 0 -- InsertEscapeSymbols ----------------------------------------------------
TYPE  RegSymbolsT = ARRAY 11 OF CHAR;
CONST RegSumbols  = RegSymbolsT{"?", "[", "]", "{", "}", "*", "&", "|", "^", "(", ")"};
VAR result: String;
    i: LONGINT;
BEGIN
  DStrings.Assign(str, result);
  FOR i:=0 TO LEN(RegSumbols)-1 DO
    result := escapeRegSymbol(result^, RegSumbols[i]);
  END;
  RETURN result;
END InsertEscapeSymbols;


--------------------------------------------------------------------------------
PROCEDURE GetAbsoluteName (fname-: ARRAY OF CHAR): String;
VAR full_name: ARRAY 1024 OF CHAR;
    result: String;
BEGIN
  FileSys.FullName(full_name, fname);
  Strings.Capitalize(full_name);
  ReplaceAllSlashes(full_name);
  result := InsertEscapeSymbols(full_name);
  RETURN result;
END GetAbsoluteName;


--------------------------------------------------------------------------------
PROCEDURE ReplaceSpecialSumbols (VAR str: ARRAY OF CHAR);
VAR i: LONGINT;
BEGIN
  FOR i:=0 TO LEN(str)-1 DO
    IF str[i] ='~' THEN
       str[i]:='^'
    END;
  END;
END ReplaceSpecialSumbols;

--------------------------------------------------------------------------------
PROCEDURE GetModuleMask * ( coverageMask-: ARRAY OF CHAR
                          ; VAR res: LONGINT ): RegComp.Expr;

    -- 1 -- InitModuleMask -----------------------------------------------------
    PROCEDURE getMaskItem (str-: ARRAY OF CHAR; VAR pos: sys.CARD32): String;
    VAR temp: ARRAY 1024 OF CHAR;

        -- 1 -- InitModuleMask.getMaskItem -------------------------------------
        PROCEDURE parseItem ( str-: ARRAY OF CHAR
                            ; VAR isRelative: BOOLEAN
                            ; VAR includeSubDir: BOOLEAN ): String;
        VAR res: String;
            temp: ARRAY 1024 OF CHAR;
            len: sys.CARD32;
        BEGIN
          len := LENGTH(str);
          IF (len > 2) AND (str[0] = ".") AND (str[1] = "/") THEN
            isRelative := TRUE;
            Strings.Extract(str, 2, len-2, temp);
            DStrings.Assign(temp, res);
        <* IF TARGET_OS="UNIX" THEN *>
          ELSIF (len > 1) AND (str[0] = "/") THEN
        <* ELSE *>
          ELSIF (len > 2) AND (str[1] = ":") THEN
        <* END *>
            isRelative := FALSE;
            DStrings.Assign(str, res);
          ELSE
            IF (len > 1) AND (str[len-1] = "*") THEN
              isRelative := FALSE;
              Strings.Extract(str, 0, len-1, temp);
              res := GetAbsoluteName(temp);
              DStrings.Append("*", res);
            ELSE
              isRelative := FALSE;
              res := GetAbsoluteName(str);
            END;
          END;

          len := LENGTH(res^);
          includeSubDir := (len > 1)
                         & (res[len-1] = "*")
                         & (res[len-2] = "/");
          IF includeSubDir THEN
            res[len-1] := 0C;
          END;
          RETURN res;
        END parseItem;

    -- 1 -- InitModuleMask.getMaskItem -----------------------------------------
    VAR found: BOOLEAN;
        end_pos: sys.CARD32;
        res, temp_res: String;
        isRelative: BOOLEAN;
        includeSubDir: BOOLEAN;
    BEGIN
      Strings.FindNext(";", str, pos, found, end_pos);
      IF found THEN
        Strings.Extract(str, pos, end_pos-pos, temp);
        DStrings.Assign(temp, res);
        pos := end_pos+1;
      ELSIF pos < VAL(sys.CARD32, LENGTH(str)) THEN
        Strings.Extract(str, pos, VAL(sys.CARD32, LENGTH(str))-pos, temp);
        DStrings.Assign(temp, res);
        pos := LENGTH(str);
      ELSE
        res := NIL;
        pos := LENGTH(str);
      END;
      -- replace
      IF res # NIL THEN
        IF res^ = "*" THEN
        ELSIF res^ = "." THEN
          -- .   ->  (./^(*[/:]*)&^(*..*))|(^(*[/:]*)&^(*..*))
          DStrings.Assign("(./^(*[/:]*)&^(*..*))|(^(*[/:]*)&^(*..*))", res);
        ELSIF res^ = "./" THEN
          -- ./  ->  (./^(*[/:]*)&^(*..*))|(^(*[/:]*)&^(*..*))
          DStrings.Assign("(./^(*[/:]*)&^(*..*))|(^(*[/:]*)&^(*..*))", res);
        ELSIF res^ = "./*" THEN
          -- ./* ->  ^(*:*)&^(*..*)&^(/*)
          DStrings.Assign("^(*:*)&^(*..*)&^(/*)", res);
        ELSE
          -- src     -> {./}src/^(*/*)
          -- ./src   -> {./}src/^(*/*)
          -- src/*   -> {./}src/^(*..*)
          -- ./src/* -> {./}src/^(*..*)
          res := parseItem(res^, isRelative, includeSubDir);
          Strings.Capitalize(res^);
          IF isRelative THEN
            DStrings.Assign("{./}", temp_res);
            DStrings.Append(res^, temp_res);
            res := temp_res;
          END;
          IF res[LENGTH(res^)-1] # "/" THEN
            DStrings.Append("/", res);
          END;
          IF includeSubDir THEN
            DStrings.Append("^(*..*)", res);
          ELSE
            DStrings.Append("^(*/*)", res);
          END;
        END;
      END;
      RETURN res;
    END getMaskItem;

VAR str: String;
VAR ModuleMaskExpr: RegComp.Expr;
    mask, item, tmp: String;
    pos: sys.CARD32;

BEGIN
  DStrings.Assign(coverageMask, str);
  ReplaceAllSlashes(str^);
  ReplaceSpecialSumbols(str^);

  pos := 0;
  item := getMaskItem(str^, pos);
  DStrings.Assign(item^, mask);
  item := getMaskItem(str^, pos);
  WHILE item # NIL DO
    DStrings.Assign("(", tmp);
    DStrings.Append(mask^, tmp);
    DStrings.Append(")|(", tmp);
    DStrings.Append(item^, tmp);
    DStrings.Append(")", tmp);
    mask := tmp;

    item := getMaskItem(str^, pos);
  END;

  RegComp.Compile(mask^, ModuleMaskExpr, res);
  RETURN ModuleMaskExpr;
END GetModuleMask;

--------------------------------------------------------------------------------
-- Returns TRUE, if expression matches with string "s" starting
-- from position "pos".
PROCEDURE MatchModuleMask * ( expr: RegComp.Expr
                            ; str: ARRAY OF CHAR
                            ; pos:=0: LONGINT ): BOOLEAN;
VAR full_name: String;
BEGIN
--  Strings.Capitalize(str);
--  ReplaceAllSlashes(str);
  full_name := GetAbsoluteName(str);
  RETURN RegComp.Match (expr, full_name^, pos);
END MatchModuleMask;

END tcLib.
