(* Накопление TOC *)
MODULE TOC;

IMPORT pc   := pcK,
       cd   := CodeDef,
       at   := opAttrs,
       FormStr,
       Data := TOCData;

TYPE TOC_OFFSET* = INTEGER;

TYPE TocSegment  = POINTER TO TocDesc;
     TocCode*    = POINTER TO ARRAY OF pc.OBJECT;
     TocDesc*    = RECORD
                       code*      : TocCode;
                       code_len*  : TOC_OFFSET;
                   END;

VAR CurToc*: TocSegment;
    LTOCSegm : cd.CODE_SEGM;
    lastTOCentrance: INTEGER;  -- номер последней записи в TOC
TYPE
    TOC_OFFSET_ATTR  = POINTER TO toc_offs_rec;
    toc_offs_rec   = RECORD(at.attr_ext_rec)
                         offset : TOC_OFFSET;
                     END;
(* procedure to find object which should be used for fixiup in local TOC
  when an object 'o' is added to toc *)

VAR
  Obj4TOCRegister* : PROCEDURE (o : pc.OBJECT) : pc.OBJECT;
  gen_code : BOOLEAN;
  AfterFinish : BOOLEAN;

PROCEDURE Init*();
BEGIN
  NEW(CurToc);
  NEW(CurToc^.code,5);
  CurToc^.code_len := 0;
  Data.ModuleStartObjects := NIL;
  LTOCSegm := NIL;
  gen_code := FALSE;
  AfterFinish := FALSE;
END Init;

PROCEDURE Start*();
BEGIN
  gen_code := TRUE;
END Start;

PROCEDURE Finish*;
VAR old : cd.CODE_SEGM;
    i   : INTEGER;
    str: ARRAY 512 OF CHAR;
BEGIN
    cd.new_segm(LTOCSegm);
    cd.get_segm(old);
    cd.set_segm(LTOCSegm);
    IF CurToc.code_len = 0 THEN
      cd.gen_fixup(at.LocalTOC,0,cd.fx_obj32);
    ELSE
      FOR i := 0 TO CurToc.code_len-1 DO
        IF (at.CompModeSet{at.GENASM,at.debug} * at.COMP_MODE) = at.CompModeSet{at.GENASM,at.debug} THEN
          FormStr.print(str,"            # TOC offset = %d",i*4);
          cd.GenInstr(str);
        END;
        cd.gen_fixup(Obj4TOCRegister(CurToc.code[i]),0,cd.fx_obj32);
      END;
      lastTOCentrance:= CurToc.code_len-1;
    END;
    cd.set_segm(old);
    cd.set_ready(at.LocalTOC, LTOCSegm);
    AfterFinish := TRUE;
END Finish;

PROCEDURE add_to_toc(obj : pc.OBJECT);
  VAR i   : TOC_OFFSET;
      new : TocCode;
      a   : TOC_OFFSET_ATTR;
      sg,
      old : cd.CODE_SEGM;
      attr: at.ATTR_EXT;
      str : ARRAY 512 OF CHAR;
BEGIN
    IF CurToc^.code_len = LEN(CurToc^.code^) THEN
        NEW(new, CurToc^.code_len*2);
        FOR i := 0 TO CurToc^.code_len-1 DO new[i] := CurToc^.code[i] END;
        CurToc^.code := new;
    END;
    NEW(a);
    a^.offset := CurToc^.code_len;
    at.app_obj_attr(obj,a,at.a_TOCoffs);
    CurToc^.code[CurToc^.code_len] := obj;
    INC(CurToc^.code_len);
    IF AfterFinish THEN
        cd.get_segm(old);
        attr := at.attr(at.LocalTOC.ext, cd.a_ready_code);
        sg := attr(cd.CODE_SEGM);
        cd.set_segm(sg);
        IF (at.CompModeSet{at.GENASM,at.debug} * at.COMP_MODE) <> at.CompModeSet{} THEN
          INC(lastTOCentrance);
          FormStr.print(str,"            # TOC offset = %d", lastTOCentrance*4);
          cd.GenInstr(str);
        END;
        cd.gen_fixup(Obj4TOCRegister(obj),0,cd.fx_obj32);
        cd.set_segm(old);
    END;
END add_to_toc;

PROCEDURE HasTOCRecord*(obj : pc.OBJECT) : BOOLEAN;
BEGIN
  RETURN at.attr(obj^.ext,at.a_TOCoffs) <> NIL;
END HasTOCRecord;

PROCEDURE Add*(obj : pc.OBJECT);
BEGIN
    IF at.attr(obj^.ext,at.a_TOCoffs) = NIL THEN
      add_to_toc(obj);
    END;
END Add;

PROCEDURE AddBase*(obj : pc.OBJECT);
BEGIN
    IF Data.ModuleStartObjects = NIL THEN
        NEW(Data.ModuleStartObjects,pc.mod_cnt);
    END;
    ASSERT(Data.ModuleStartObjects[obj.mno] = NIL);
    Data.ModuleStartObjects[obj.mno] := obj;
    Add(obj);
END AddBase;

PROCEDURE GetModuleBase*(obj : pc.OBJECT) : pc.OBJECT;
BEGIN
    RETURN Data.ModuleStartObjects[obj.mno]
END GetModuleBase;

PROCEDURE Get*(obj : pc.OBJECT) : TOC_OFFSET;
VAR a : TOC_OFFSET_ATTR;
    ea : at.ATTR_EXT;
BEGIN
    ea := at.attr(obj^.ext,at.a_TOCoffs);
    a := ea(TOC_OFFSET_ATTR);
    RETURN a^.offset;
END Get;

(* ------------------------------------------------------------ *)

PROCEDURE GetExternalTOCOffset*(obj : pc.OBJECT) : TOC_OFFSET;
BEGIN
    IF (at.omark_allocated IN obj.marks) THEN
        obj := Data.ModuleStartObjects[obj.mno];
<* IF ~NODEBUG THEN *>
        ASSERT(obj <> NIL);
<* END *>
    END;
    RETURN Get(obj);
END GetExternalTOCOffset;

BEGIN
  Obj4TOCRegister := NIL;
END TOC.


