<*+M2ADDTYPES*>
<*+M2EXTENSIONS*>

IMPLEMENTATION MODULE Decode;

FROM Strings IMPORT
  Append, Assign, FindNext;

IMPORT
  Strings, SYSTEM;

TYPE
  Liter = SET OF CHAR ;(*,'a'..'z';*)

CONST
  TokensBase = 0;
  EOL                 = 00 + TokensBase;
  ident               = 01 + TokensBase;
  userunderline       = 02 + TokensBase;
  underline           = 03 + TokensBase;
  point_comma         = 04 + TokensBase;
  left_square_bracket = 05 + TokensBase;
  StartOfParams       = 06 + TokensBase;
  liter = Liter{'A'..'Z','a' .. 'z','0'..'9','$','<','>'};


VAR
  source: StringType;
  destination: StringType;
  index: CARDINAL;
  token: INTEGER;
  current_string: StringType;
  ErrorFlag: INTEGER;
  DEBUG_INFO: BOOLEAN;


PROCEDURE AppendChar ( ch: CHAR; VAR d: ARRAY OF CHAR);
VAR str: ARRAY [0..1] OF CHAR;
BEGIN
  str[0]:= ch; str[1]:= 0X;
  Strings.Append(str, d);
END AppendChar;


PROCEDURE GetToken();
VAR
  patternFound: BOOLEAN;
  posOfPattern: CARDINAL;
BEGIN
  Strings.Assign("", current_string);
  IF Strings.Length(source) = index THEN 
    token := EOL; 
    RETURN;
  END;
  IF (~DEBUG_INFO & (source[index]='/')) THEN
    INC(index);
    token := underline;RETURN;
  END;
  IF ( DEBUG_INFO & (source[index]=':')) THEN
    INC(index);
    IF (source[index]=':') THEN
      INC(index);
      token := underline;RETURN;
    END;
  END;
  IF ( DEBUG_INFO & (source[index]='`')) OR
     (~DEBUG_INFO & (source[index]='@')) THEN
    INC(index);
    IF ~DEBUG_INFO THEN
      Strings.FindNext("@", source, index, patternFound, posOfPattern);
      IF patternFound THEN
        token := underline;RETURN;
      ELSE
        token := StartOfParams;RETURN;
      END;
    ELSE
      Strings.FindNext("`", source, index, patternFound, posOfPattern);
      token := StartOfParams;RETURN;
    END;
  END;
  CASE source[index] OF
    'A'..'Z', 'a'..'z', '0'..'9', '>', '<':
             WHILE (source[index] IN liter) DO
               AppendChar(source[index], current_string);
               INC(index);
             END;
             token := ident;
             RETURN;
       |'_': INC(index);
             CASE source[index] OF
               '1': token := userunderline;INC(index);
              |'2': token := point_comma;INC(index);
              |'3': token := left_square_bracket;INC(index);
              ELSE  token := underline;
             END;
             RETURN;
       |ELSE ASSERT(FALSE, 1001);
  END;
END GetToken;

PROCEDURE EraseAllJavaLang(VAR str:ARRAY OF CHAR);
VAR
  searchIndex: CARDINAL;
  patternFound: BOOLEAN;
  posOfPattern: CARDINAL;
BEGIN
  IF DEBUG_INFO THEN
    Strings.FindNext("::", str, 0, patternFound, posOfPattern);
    IF patternFound THEN
      Strings.Delete (str, 0, posOfPattern+2);
    END;
  END;
  Strings.FindNext("Java_", str, 0, patternFound, posOfPattern);
  IF patternFound AND (posOfPattern < 2) THEN
    Strings.Delete (str, 0, posOfPattern+Strings.Length("Java_"));
  END;
  Strings.FindNext("java_lang_", str, 0, patternFound, posOfPattern);
  WHILE patternFound DO
    IF ((posOfPattern=0) OR (str[posOfPattern-1]='L')) THEN
      Strings.Delete (str, posOfPattern, Strings.Length("java_lang_"));
      searchIndex := posOfPattern;
    ELSE
      searchIndex := posOfPattern+1;
    END;
    Strings.FindNext("java_lang_", str, searchIndex, patternFound, posOfPattern);
  END;
  Strings.FindNext("main@0", str, 0, patternFound, posOfPattern);
  WHILE patternFound DO
    Strings.Delete (str, posOfPattern, Strings.Length("main@0"));
    Strings.Insert("<clinit>", posOfPattern, str);
    searchIndex := posOfPattern;
    Strings.FindNext("main@0", str, searchIndex, patternFound, posOfPattern);
  END;

  Strings.FindNext("main@153", str, 0, patternFound, posOfPattern);
  WHILE patternFound DO
    Strings.Delete (str, posOfPattern, Strings.Length("main@153"));
    Strings.Insert("<clinit_10>", posOfPattern, str);
    searchIndex := posOfPattern;
    Strings.FindNext("main@153", str, searchIndex, patternFound, posOfPattern);
  END;

  IF ~DEBUG_INFO THEN
    Strings.FindNext("java/lang/", str, 0, patternFound, posOfPattern);
    WHILE patternFound DO
      IF ((posOfPattern=0) OR (str[posOfPattern-1]='L')) THEN
        Strings.Delete (str, posOfPattern, Strings.Length("java/lang/"));
        searchIndex := posOfPattern;
      ELSE 
        searchIndex := posOfPattern+1;
      END;
      Strings.FindNext("java/lang/", str, searchIndex, patternFound, posOfPattern);
    END;
  END;
END EraseAllJavaLang;

PROCEDURE ProcessIdent();
VAR
  strToAppend: StringType;
BEGIN
  WHILE (token#EOL) & (token#point_comma) & (token#StartOfParams)
         & (token#left_square_bracket) DO
    CASE token OF
       ident        : strToAppend := current_string;
      |underline    : strToAppend := ".";
      |userunderline: strToAppend := "_";
      ELSE            ASSERT(FALSE, 1002);
    END;
    Append(strToAppend, destination);
    GetToken(); IF ErrorFlag#0 THEN RETURN;END;
  END;
END ProcessIdent;

PROCEDURE ProcessParams();
VAR
  strToAppend   :StringType;
  ArrayBrackets :CARDINAL;
BEGIN
    ArrayBrackets:=0;
    Append("(", destination);
    WHILE source[index]#0X DO
      strToAppend := "";
      WHILE source[index]='_' DO
        INC(index);
        IF source[index]# '3' THEN
           ASSERT(FALSE, 1007);
        END;
        INC(ArrayBrackets);INC(index);
      END;
      CASE source[index] OF
         'Z'    : strToAppend := "boolean";INC(index);
        |'B'    : strToAppend := "byte";INC(index);
        |'C'    : strToAppend := "char";INC(index);
        |'S'    : strToAppend := "short";INC(index);
        |'I'    : strToAppend := "int";INC(index);
        |'J'    : strToAppend := "long";INC(index);
        |'F'    : strToAppend := "float";INC(index);
        |'D'    : strToAppend := "double";INC(index);
        |'L'    : INC(index);
                  GetToken();
                  ProcessIdent();
                  IF token#point_comma THEN
                    ASSERT(FALSE, 1003);
                  END;
        ELSE      ASSERT(FALSE, 1004);
      END;
      Append(strToAppend, destination);
      WHILE ArrayBrackets > 0  DO
        Append("[]", destination); DEC(ArrayBrackets);
      END;
      IF source[index]#0C THEN
        Append(", ", destination);
      END;
  END;
  Append(")", destination);
END ProcessParams;


PROCEDURE DoDecode(VAR source_str: ARRAY OF CHAR;
              VAR destination_str: ARRAY OF CHAR;
              DebugMangling:       BOOLEAN):BOOLEAN;
BEGIN
  index:= 0;
  DEBUG_INFO := DebugMangling;
  Assign(source_str, source);
  Assign("", destination);
  EraseAllJavaLang(source);
  GetToken();
  ProcessIdent();
  IF token=StartOfParams THEN
    ProcessParams();
  ELSIF token#EOL THEN
     ASSERT(FALSE, 1005);
  END;
  Strings.Assign(destination, destination_str);
  RETURN TRUE;
END DoDecode;

PROCEDURE Demangle(VAR source_str: ARRAY OF CHAR;
              VAR destination_str: ARRAY OF CHAR;
              DebugMangling:       BOOLEAN):BOOLEAN;
BEGIN
  IF HIGH(source_str) > HIGH(destination_str) THEN
    ASSERT(FALSE, 1006);
  END;
  RETURN DoDecode(source_str, destination_str, DebugMangling);
END Demangle;


PROCEDURE ["C"] DemangleC(VAR source_str: StringType ;
              VAR destination_str: StringType ;
              DebugMangling:       BOOLEAN):BOOLEAN;
BEGIN
  RETURN DoDecode(source_str, destination_str, DebugMangling);
END DemangleC;

PROCEDURE ["C"] Demangle_PublicC(VAR source_str: StringType ;
              VAR destination_str: StringType):BOOLEAN;
BEGIN
  RETURN DemangleC(source_str, destination_str, FALSE);
END Demangle_PublicC;

PROCEDURE ["C"] Demangle_DebugC (VAR source_str: StringType ;
              VAR destination_str: StringType):BOOLEAN;
BEGIN
  RETURN DemangleC(source_str, destination_str, TRUE);
END Demangle_DebugC;


PROCEDURE Demangle_Public(VAR source_str: ARRAY OF CHAR;
              VAR destination_str: ARRAY OF CHAR):BOOLEAN;
BEGIN
  RETURN Demangle(source_str, destination_str, FALSE);
END Demangle_Public;

PROCEDURE Demangle_Debug (VAR source_str: ARRAY OF CHAR;
              VAR destination_str: ARRAY OF CHAR):BOOLEAN;
BEGIN
  RETURN Demangle(source_str, destination_str, TRUE);
END Demangle_Debug;

BEGIN
  ErrorFlag:=0;
END Decode.

