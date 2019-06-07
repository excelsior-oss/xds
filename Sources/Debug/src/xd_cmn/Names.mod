IMPLEMENTATION MODULE Names;


IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT opt := Options;
IMPORT xs  := xStr;

<* IF DEST_XDS THEN *>
IMPORT java := Decode;
<* END *>



<* IF NOT DEST_XDS THEN *>
 <* PUSH *>
 <* WOFF301+ *>
<* END *>

PROCEDURE ObjectNameCorrect (lang: dt.LANGUAGE; VAR name: ARRAY OF CHAR);
<* IF DEST_XDS THEN *>
VAR
  tmp: xs.String;
BEGIN
  IF opt.CorrectObjectName THEN
    IF lang = dt.Lng_Java THEN
      IF java.Demangle_Debug (name, tmp) THEN
        COPY (tmp, name);
      END;
    END;
  END;
EXCEPT
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  xs.Insert ("Demangler error: ", 0, name);
 <* END *>
  RETURN;
 <* END *>
END ObjectNameCorrect;

<* IF NOT DEST_XDS THEN *>
 <* POP *>
<* END *>


PROCEDURE ObjectNameGetAndCorrect (object: dt.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  tls.ObjectName (object, name);
  ObjectNameCorrect (tls.ObjectLanguage (object), name);
END ObjectNameGetAndCorrect;


BEGIN
END Names.