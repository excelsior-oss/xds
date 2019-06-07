--------------------------------------------------------------------------------
--                      Excelsior XDS Test Coverage System
--                          (c) 2015, Excelsior Ltd.
-- Module:   tcFEproxy
-- Mission:  Proxy to compiler front-end to unify usage of different versions of front-end.  
-- Synonym:  fe
-- Authors:  Lvov Konstantin
-- Created:  01-Nov-2006
--------------------------------------------------------------------------------
<* IF NOT DEFINED(dbg_tcs)        THEN *>  <* NEW dbg_tcs- *>        <* END *>
<* IF NOT DEFINED(TARGET_VAX_OLD) THEN *>  <* NEW TARGET_VAX_OLD- *> <* END *>
<* +o2addkwd *>

MODULE tcFEproxy;

IMPORT  pc := pcK,     env := xiEnv,        pcO
     ;

--------------------------------------------------------------------------------
<* IF TARGET_VAX_OLD THEN *>
TYPE
  -- language flags
  Lang * = SHORTINT;

  UTAG_SET * = SET;

CONST
  NullPos * = env.TPOS {MAX(LONGINT), MAX(INTEGER), MAX(INTEGER)};
  ZEROMno * = 0;

  LangsWithModuleConstructors * = SET{ pc.flag_o2, pc.flag_m2 };


PROCEDURE new_obj * (mode: pc.OB_MODE): pc.OBJECT;
VAR obj: pc.OBJECT;
BEGIN
  pcO.new_obj (obj, mode);
  RETURN obj;
END new_obj;

PROCEDURE new_type * (mode: pc.TY_MODE): pc.STRUCT;
VAR type: pc.STRUCT;
BEGIN
  pcO.new_type (type, mode);
  RETURN type;
END new_type;

PROCEDURE RefreshPrimitiveTypes * ();
BEGIN
END RefreshPrimitiveTypes;

--------------------------------------------------------------------------------
<* ELSE *> -- TARGET_VAX_OLD
TYPE
  -- language flags
  Lang * = pc.Lang;

  UTAG_SET * = pc.UTAG_SET;

CONST
  NullPos * = env.null_pos;
  ZEROMno * = pc.ZEROMno;

  LangsWithModuleConstructors * = pc.LangsWithModuleConstructors;

CONST
  new_obj  * = pcO.new_obj;
  new_type * = pcO.new_type;

  RefreshPrimitiveTypes * = pc.RefreshPrimitiveTypes;
<* END *>  -- TARGET_VAX_OLD

END tcFEproxy.
