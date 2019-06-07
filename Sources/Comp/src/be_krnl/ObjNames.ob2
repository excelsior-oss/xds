MODULE ObjNames;

IMPORT pc  := pcK;
IMPORT env := xiEnv;

IMPORT tune:= opTune;
IMPORT at  := opAttrs;
IMPORT opProcs, CodeDef;

IMPORT fmt := FormStr;
IMPORT str := Strings;

TYPE INT = LONGINT;

(* --------------------------- Names -------------------------------- *)
                            (* ----- *)

PROCEDURE valid_name * (name: pc.STRING) : BOOLEAN;
  VAR i,ln: INT; ch: CHAR;
BEGIN
  IF name = NIL THEN RETURN FALSE END;
  ln := LEN(name^);
  i := 0;
  LOOP
    ch := name^[i];
    IF ch = 0X THEN RETURN (i#0) END;
    IF ((ch<'0') OR (ch>'9')) &  ((ch<'A') OR (ch>'Z')) &
       ((ch<'a') OR (ch>'z')) &  (ch#'_')
    THEN RETURN FALSE
    END;
    INC(i);
    IF i >= ln THEN RETURN TRUE END;
  END;
END valid_name;

CONST
  TDA_PRF = '@'+0C;
  PRE_PRF = '&'+0C;
  C_PRF   = '_'+0C;

<* IF TARGET_RISC THEN *>

VAR aix_name* : BOOLEAN;

PROCEDURE CheckPrefix(o: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  IF at.ABI = at.PowerOpen THEN 
    IF o.mode IN (pc.PROCs+pc.OB_SET{pc.ob_module}) THEN
      IF aix_name THEN
        str.Insert(".", 0, name);
      ELSE
        str.Insert("..", 0, name);
      END;
    END;
  END
END CheckPrefix;

<* END*>

PROCEDURE chk_C_pref (o: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  IF pc.otag_C_pref IN o.tags THEN str.Insert(C_PRF, 0, name) END;
END chk_C_pref;

PROCEDURE makeCname*(o: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  COPY (o.name^, name);
  chk_C_pref (o, name);
<* IF TARGET_RISC THEN *>
  CheckPrefix(o, name);
<* END*>
END makeCname;

PROCEDURE makePname*(o: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  COPY(o.name^, name);
  str.Capitalize(name);
<* IF TARGET_RISC THEN *>
  CheckPrefix(o, name);
<* END*>
END makePname;

PROCEDURE CalcFrameSize(o: pc.OBJECT): INT;
BEGIN
  RETURN opProcs.LenParams(opProcs.ProcProtoNum(opProcs.ProcNumByObj(o)));
END CalcFrameSize;

PROCEDURE makeStdCallMangle(o: pc.OBJECT; VAR name: ARRAY OF CHAR);
VAR
  fr_sz: INT; (* size of parameter frame on stack *)
BEGIN
  IF (o.mode IN (pc.PROCs + pc.OB_SET{pc.ob_module})) AND
     at.is_stdcall_mangled(o)
  THEN
    str.Insert(C_PRF, 0, name);
    fr_sz := CalcFrameSize(o);
    fmt.append(name, "%s%d", TDA_PRF, fr_sz);
  END;
END makeStdCallMangle;

PROCEDURE makeStdCallname*(o: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  COPY(o.name^, name);
  chk_C_pref (o, name);
  makeStdCallMangle(o, name);
<* IF TARGET_RISC THEN *>
  CheckPrefix(o, name);
<* END*>
END makeStdCallname;

PROCEDURE makeSysCallname*(o: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  COPY(o.name^, name);
  chk_C_pref (o, name);
<* IF TARGET_RISC THEN *>
  CheckPrefix(o, name);
<* END*>
END makeSysCallname;


CONST
  makeSL1name     = makeCname;

PROCEDURE mkmodname*(mno: pc.Mno; VAR name: ARRAY OF CHAR);
BEGIN
  IF mno=tune.x2c_mno THEN
    COPY("X2C", name);
  ELSE
    COPY(pc.mods[mno].name^,name);
  END;
END mkmodname;

CONST
  MOD_ENTR* = "BEGIN";
  MAIN_PROG_ENTR* = "main";
  DLL_ENTR* = "_dllmain";

PROCEDURE mk(VAR name: ARRAY OF CHAR; mno: pc.Mno; objname-: ARRAY OF CHAR);
BEGIN
  mkmodname(mno, name);
  str.Append (at.NM_SEP, name);
  str.Append (objname, name);
END mk;

PROCEDURE module_body_name(m: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  ASSERT(m.mode = pc.ob_module);
  mk(name, m.mno, MOD_ENTR);
  chk_C_pref (m, name);
 <* IF TARGET_RISC THEN *>
  CheckPrefix(m, name);
 <* END*>
  IF (at.otag_versionkey IN m.tags) THEN
    fmt.append(name, "_A%.8X", m.type.len);
  END;
END module_body_name;

<* IF TARGET_68k THEN *>   -- replace non-alphanum symbols

PROCEDURE replace_non_alpha(VAR name: ARRAY OF CHAR);
  VAR i: INT; ch: CHAR;
BEGIN
  i := 0;
  LOOP
    ch := name [i];
    IF ch = 0C THEN EXIT END;
    IF ((ch<'A') OR (ch>'Z')) & ((ch<'a') OR (ch>'z'))
      & ((ch<'0') OR (ch>'9')) & (ch # '_')
    THEN
      name [i] := '$';
    END;
    INC (i);
  END;
END replace_non_alpha;

<* ELSIF TARGET_RISC THEN *>   -- replace non-alphanum symbols

PROCEDURE replace_non_alpha(VAR name: ARRAY OF CHAR);
  VAR i: INT; ch: CHAR;
BEGIN
  i := 0;
  LOOP
    ch := name [i];
    IF ch = 0C THEN EXIT END;
    IF ((ch<'A') OR (ch>'Z')) & ((ch<'a') OR (ch>'z'))
      & ((ch<'0') OR (ch>'9')) & (ch # '_') & (ch # '.')
    THEN
      name [i] := '.';
    END;
    INC (i);
  END;
END replace_non_alpha;

<* ELSE *> -- TARGET_VAX OR TARGET_386

<* PUSH *>
<* WOFF301+ *>
PROCEDURE replace_non_alphaB(VAR name: ARRAY OF CHAR);
END replace_non_alphaB;
<* POP *>

PROCEDURE replace_non_alphaA(VAR name: ARRAY OF CHAR);
  VAR i: INT; ch: CHAR;
BEGIN
  i := 0;
  LOOP
    ch := name [i];
    IF ch = 0C  THEN EXIT END;
    IF ((ch<'A') OR (ch>'Z')) & ((ch<'a') OR (ch>'z'))
      & ((ch<'0') OR (ch>'9')) & (ch # '_')
    THEN
      name [i] := '$';
    END;
    INC (i);
  END;
END replace_non_alphaA;

PROCEDURE replace_non_alpha(VAR name: ARRAY OF CHAR);
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN
      replace_non_alphaA(name);
  ELSE
      replace_non_alphaB(name);
  END;
  IF at.CC = at.DJGPP THEN str.Insert("_", 0, name) END;
END replace_non_alpha;
<* END *>

PROCEDURE makename*(o: pc.OBJECT; VAR name: ARRAY OF CHAR);

  PROCEDURE method_name(proc: pc.OBJECT; VAR name: ARRAY OF CHAR);
    VAR T: pc.STRUCT;
  BEGIN
    ASSERT(proc^.mode = pc.ob_xproc);
    T := proc.host; (* proc.host - type of record containting this method *)
    IF T^.mode = pc.ty_pointer THEN T := T^.base END;
    ASSERT((T^.mode = pc.ty_record) & (T^.flag IN pc.OOP_langs));
    makename(T^.obj, name);     (* T.obj should be valid - there are no unnamed classes *)
    str.Append(TDA_PRF, name);
    str.Append(proc.name^, name);
  END method_name;

  VAR
(* for C-converter compatibility *)
  no: INTEGER; nm : pc.STRING;
(* -- *)

<* IF TARGET_RISC THEN *>
  p: pc.OBJECT;
<* END *>
BEGIN

(* for C-converter compatibility *)
  IF    o.mode = pc.ob_type   THEN no := at.n_type_desc_name;
  ELSIF o.mode = pc.ob_module THEN no := at.n_BEGIN_name;
  ELSE                             no := at.n_object_name;
  END;
  nm := at.get_name(o, no);
  IF nm # NIL THEN
    str.Assign(nm^, name); RETURN
  END;
(* -- *)
  IF NOT ((o.mode IN pc.PROCs) AND (o.host.mode = pc.ty_record))
     AND NOT (o.mode = pc.ob_module) THEN
    (* для методов не следует делать Си-шные имена, поскольку при пере-    *)
    (* определении их имена могут повториться несколько раз в одном модуле *)
    CASE o.flag OF
    | pc.flag_c:
        makeCname(o, name);
        replace_non_alpha (name);
        RETURN
    | pc.flag_p:
        makePname(o, name);
        replace_non_alpha (name);
        RETURN
    | pc.flag_stdcall, pc.flag_vmcall, pc.flag_lightcall, pc.flag_javacall:
        makeStdCallname(o, name);
        replace_non_alpha (name);
        RETURN
    | pc.flag_syscall:
        makeSysCallname(o, name);
        replace_non_alpha (name);
        RETURN
    | pc.flag_sl1:
        IF (o.lev = 0) & (pc.otag_public IN o.tags) THEN
          makeSL1name(o, name);
          replace_non_alpha (name);
          RETURN
        END;
    ELSE
    END;
  END;

  CASE o^.mode OF
  |pc.ob_proc, pc.ob_xproc, pc.ob_cproc, pc.ob_lproc, pc.ob_eproc:
     IF (o.host.mode = pc.ty_record) THEN
       method_name(o, name);
      <* IF TARGET_RISC THEN *>
       CheckPrefix(o, name);
      <* END*>
     ELSIF o.lev > 0 THEN makename(o^.host^.obj, name);
       str.Append (PRE_PRF, name);
       str.Append (o^.name^, name);
     ELSIF (o^.mode = pc.ob_eproc) & (o^.flag = pc.flag_sl1) THEN
       makeSL1name(o, name);
     ELSE
       mk(name, o^.mno, o^.name^);
       chk_C_pref (o, name);
      <* IF TARGET_RISC THEN *>
       CheckPrefix(o, name);
      <* END*>
     END;

  |pc.ob_type:
     mk(name, o.mno, o.name^);
     chk_C_pref (o, name);

  |pc.ob_var:
     mk(name, o.mno, o.name^);
     chk_C_pref (o, name);

  |pc.ob_cons:
    <* IF TARGET_RISC THEN *>
     IF at.omark_procdesc IN o.marks THEN
       p :=  at.FindProcByDesc(o);
       IF (p.host # NIL) & (p.host.mode = pc.ty_record) THEN
         method_name(p, name);
         chk_C_pref (p, name);
       ELSE
         mk(name, o.mno, o.name^);
         chk_C_pref (o, name);
        END;
     ELSE
       mk(name, o.mno, o.name^);
       chk_C_pref (o, name);
     END;
    <* ELSE *>
     mk(name, o.mno, o.name^);
     chk_C_pref (o, name);
    <* END *>

  |pc.ob_module:
     IF (o^.mno=at.curr_mno) & at.main THEN
       IF at.GENDLL IN at.COMP_MODE THEN
         COPY(DLL_ENTR, name);
       ELSE
         COPY(MAIN_PROG_ENTR, name);
         chk_C_pref (o, name);
       END;
      <* IF TARGET_RISC THEN *>
       CheckPrefix(o, name);
      <* END*>
     ELSE
       module_body_name(o, name);
     END;
--ELSE ASSERT(FALSE,100h+o^.mode);
  END;
  replace_non_alpha (name);
END makename;

BEGIN
  CodeDef.makename := makename;
END ObjNames.
