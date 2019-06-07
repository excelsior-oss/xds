<* NEW ADB-      *>   (* allocate *)
<* NEW SDB-      *>   (* sweep *)
<* NEW MDB-      *>   (* mark  *)
<* NEW STAT-     *>   (* statistics *)
<* NEW FULLSTAT- *>   (* full statistics (STAT should be set) *)

(*----------------------------------------------------------------*)
<* IF XMM_DEBUG THEN *>
  <*+ checkindex   *>
  <*+ checkdindex  *>
  <*+ checknil     *>
  <*+ ioverflow    *>
  <*+ coverflow    *>
  <*+ assert	   *>
<* ELSE *>
  <*- checkindex   *>
  <*- checkdindex  *>
  <*- checknil     *>
  <*- ioverflow    *>
  <*- coverflow    *>
  <*- assert	   *>
  <*- ADB      	   *>
  <*- SDB          *>
  <*- MDB          *>
<* END *>

<*- checkrange   *>
<*- checkset     *>
<*- checkproc    *>
<*- ioverflow    *>
<*- coverflow    *>
<*- foverflow    *>
<*+ M2EXTENSIONS *>
<*+ M2ADDTYPES   *>
<*- M2BASE16     *>
<*- GENCTYPES    *>
<*+ WOFF310      *>

IMPLEMENTATION MODULE xrcO2MM; (*  08-15-95 11:01am *)

IMPORT
  SYSTEM
  ,rts:=xmRTS
  ,M2E:=M2EXCEPTION
  ,xrMM
  ,X2C
  ;

CONST
  AdrLss = X2C.X2C_adr_lss;
  AdrGtr = X2C.X2C_adr_gtr;

CONST
  LINK_SZ  = SIZE(rts.X2C_LINK_STR);

PROCEDURE [2] X2C_GUARDP_F(p: rts.X2C_pVOID; td: rts.X2C_TD): rts.X2C_pVOID;
  VAR ln: rts.X2C_LINK;
BEGIN
  IF p=NIL THEN rts.X2C_TRAP_F(ORD(M2E.invalidLocation)) END;
  <* IF XMM_DEBUG THEN *>
    IF NOT AdrGtr(p,xrMM.blk_min) THEN rts.X2C_ASSERT_F(100) END;
    IF NOT AdrLss(p,xrMM.blk_max) THEN rts.X2C_ASSERT_F(101) END;
  <* END *>
  ln:=SYSTEM.SUBADR(p,LINK_SZ);
  IF (ln^.td^.res#xrMM.MAGIC_T) THEN rts.X2C_ASSERT_F(102) END;
  IF ln^.td^.base[td^.level]#td THEN rts.X2C_TRAP_F(X2C.X2C_guardException) END;
  RETURN p;
END X2C_GUARDP_F;

PROCEDURE [2] X2C_GUARDV_F(vt,td: rts.X2C_TD): rts.X2C_TD;
BEGIN
  IF vt^.base[td^.level]#td THEN rts.X2C_TRAP_F(X2C.X2C_guardException) END;
  RETURN vt;
END X2C_GUARDV_F;

PROCEDURE [2] X2C_GUARDPE_F(p: rts.X2C_pVOID; td: rts.X2C_TD): rts.X2C_pVOID;
  VAR ln: rts.X2C_LINK;
BEGIN
  IF p=NIL THEN rts.X2C_TRAP_F(ORD(M2E.invalidLocation)) END;
  <* IF XMM_DEBUG THEN *>
    IF NOT AdrGtr(p,xrMM.blk_min) THEN rts.X2C_ASSERT_F(103) END;
    IF NOT AdrLss(p,xrMM.blk_max) THEN rts.X2C_ASSERT_F(104) END;
  <* END *>
  ln:=SYSTEM.SUBADR(p,LINK_SZ);
  IF ln^.td#td THEN rts.X2C_TRAP_F(X2C.X2C_guardException) END;
  RETURN p;
END X2C_GUARDPE_F;

PROCEDURE [2] X2C_GUARDVE_F(vt,td: rts.X2C_TD): rts.X2C_TD;
BEGIN
  IF vt#td THEN rts.X2C_TRAP_F(X2C.X2C_guardException) END;
  RETURN vt;
END X2C_GUARDVE_F;

END xrcO2MM.
