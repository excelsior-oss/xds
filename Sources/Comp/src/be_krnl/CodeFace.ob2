<*+ M2EXTENSIONS *>
<*+ O2EXTENSIONS *>
<*+ M2ADDTYPES *>
MODULE CodeFace;

IMPORT at:=opAttrs, pc:=pcK, nms:=ObjNames, cmd:=CodeDef, xfs:=xiFiles,
       env:=xiEnv, dstr:=DStrings, SYSTEM, io:=opIO;
IMPORT opt := Options;
IMPORT reg := Registry;

    TYPE OBJECT * = pc.OBJECT;
         STRING * = pc.STRING;

    CONST status_Unkown   = 0;
          status_Local  * = 1;
          status_Global * = 2;
          status_Extern * = 3;

    PROCEDURE ObjectStatus * (o: OBJECT): LONGINT;
    BEGIN
      IF (o.mno#at.curr_mno) OR (o.mode=pc.ob_eproc)  THEN
        RETURN status_Extern;
      ELSIF (o.mode=pc.ob_cproc) OR cmd.based_var(o) OR NOT((at.omark_gen_ready IN o.marks) OR (o.mode=pc.ob_var)&(o.lev=0)) THEN
        RETURN status_Unkown;
      ELSIF o.is_public() THEN
        RETURN status_Global;
      ELSE
        RETURN status_Local;
      END;
    END ObjectStatus;

    CONST class_Text   * = 0;
          class_Data   * = 1;
          class_ROData * = 2;
          class_BSS    * = 3;

    PROCEDURE ObjectClass * ( o: OBJECT): LONGINT;
    BEGIN
      ASSERT(ObjectStatus(o) # status_Unkown);
      CASE o.mode OF
        pc.ob_proc,pc.ob_xproc,
        pc.ob_lproc, pc.ob_module,
        pc.ob_eproc               : RETURN class_Text   |
        pc.ob_type                : RETURN class_Data   |
        pc.ob_cons                : RETURN class_Data   | -- class_ROData
        pc.ob_var                 : RETURN class_BSS    |
      ELSE           ASSERT(FALSE,100h+ORD(o^.mode));
      END;
    END ObjectClass;

    PROCEDURE ObjectName * (o: OBJECT; VAR name: ARRAY OF CHAR);
    BEGIN
      ASSERT(ObjectStatus(o) # status_Unkown);
      nms.makename(o, name);
    END ObjectName;

    PROCEDURE ObjectSize * (o: OBJECT): LONGINT;
        (* This procedure intendent to fit maximum possible needs.
           You, most likely, want externs to be zero-sized so, you
           can easely write some thunk to fit your needs, i didn't
           do that because sometimes it is desireble extern vars
           to have actual size.
         *)
      VAR rsegm: cmd.CODE_SEGM;
    BEGIN
      ASSERT(ObjectStatus(o) # status_Unkown);
      IF (o^.mode=pc.ob_var) THEN
        RETURN cmd.get_size(o);
      ELSIF ObjectStatus(o) = status_Extern THEN
        RETURN 0;
      ELSE
        rsegm := cmd.get_ready(o);
        RETURN rsegm.code_len;
      END;
    END ObjectSize;

    PROCEDURE FixupsNumber * (o: OBJECT): LONGINT;
      VAR rsegm: cmd.CODE_SEGM;
    BEGIN
      rsegm := cmd.get_ready(o);
      IF rsegm.fxup=NIL THEN RETURN 0 END;
      RETURN rsegm.fxup_len;
    END FixupsNumber;

    PROCEDURE ObjectAlignment * (o: OBJECT): LONGINT;
    BEGIN
      IF (o^.mode=pc.ob_var) OR (o^.mode=pc.ob_cons) THEN
        RETURN cmd.get_align(o);
      ELSE
        RETURN 1;
      END;
    END ObjectAlignment;

    PROCEDURE SourceFileName * (VAR s: ARRAY OF CHAR);
    BEGIN
      COPY(env.info.file^,s);
    END SourceFileName;

    PROCEDURE SourceFileNameLength * (): LONGINT;
    BEGIN
      RETURN LENGTH(env.info.file^);
    END SourceFileNameLength;


    PROCEDURE GenLineno * ():BOOLEAN;
    BEGIN
      RETURN at.CompModeSet{at.lineno,at.history}*at.COMP_MODE # at.CompModeSet{} ;
    END GenLineno;

    PROCEDURE GenDebug * ():BOOLEAN;
    BEGIN
      RETURN at.debug IN at.COMP_MODE;
    END GenDebug;
(*
 *********************************************
 *               File I/O                    *
 *********************************************
*)

      VAR obj_file  : xfs.RawFile;
          outbuf    : ARRAY 4096 OF SYSTEM.BYTE;
          outcnt    : LONGINT;
          file_offs : LONGINT; -- for alignment in file

          filename- : pc.STRING;

    PROCEDURE BeginOutput * ();
      CONST OBJ_EXT = ".obj";
      VAR  ext: pc.STRING;
    BEGIN
      env.config.Equation("OBJEXT", ext);
      IF ext = NIL THEN dstr.Assign(OBJ_EXT,ext) END;
      xfs.sys.Create("", at.curr_mod.name^, ext^, filename);
      xfs.sys.UseFirst(filename^, filename);
      xfs.raw.Open(filename^, TRUE);
      IF xfs.raw.file = NIL THEN
        env.errors.Fault(env.null_pos, 424, xfs.raw.msg^);
      END;
      obj_file := xfs.raw.file(xfs.RawFile);
      outcnt:=0;
      file_offs:=0;
    END BeginOutput;

    -->> AVY

    PROCEDURE flush;
    BEGIN
      IF outcnt > 0 THEN
        obj_file.WriteBlock(outbuf, 0, outcnt);
        outcnt := 0;
      END;
    END flush;

    PROCEDURE write (VAR data: ARRAY OF SYSTEM.BYTE; size: LONGINT);
    VAR
      req, i: LONGINT;
    BEGIN
      ASSERT((size>=0) & (size<=LEN(data)));
      IF size > LEN(outbuf) THEN
        flush;
        obj_file.WriteBlock(data, 0, size);
      ELSE
        req := LEN(outbuf)-outcnt;
        IF req > size THEN
          req := size;
        END;
        IF req > 0 THEN
          FOR i := 0 TO req-1 DO
            outbuf[outcnt] := data[i];
            INC(outcnt)
          END;
        END;
        DEC(size, req);
        IF size > 0 THEN
          flush;
          FOR i := 0 TO size-1 DO
            outbuf[outcnt] := data[req+i];
            INC(outcnt)
          END;
        END;
      END;
    END write;

    -- выдача в текущую позицию файла
    PROCEDURE out * (VAR data: ARRAY OF SYSTEM.BYTE; size: LONGINT);
    BEGIN
      write (data, size);
      INC(file_offs, size);
    END out;

    PROCEDURE out1 * (b: ARRAY 1 OF SYSTEM.BYTE);
    BEGIN
      out(b,1);
    END out1;

    PROCEDURE out2 * (b: ARRAY 2 OF SYSTEM.BYTE);
    BEGIN
      out(b,2);
    END out2;

    PROCEDURE out4 * (b: ARRAY 4 OF SYSTEM.BYTE);
    BEGIN
      out(b,4);
    END out4;

    -- получить текущую позицию в файле
    PROCEDURE get_pos * (): LONGINT;
    BEGIN
      flush;
      RETURN obj_file.GetPos();
    END get_pos;

    -- установить текущую позицию в файле
    PROCEDURE set_pos * (pos: LONGINT);
    BEGIN
      flush;
      obj_file.SetPos(pos);
    END set_pos;

    -- выдача в указанную позицию файла
    PROCEDURE ins * (pos: LONGINT; VAR data: ARRAY OF SYSTEM.BYTE; size: LONGINT);
    BEGIN
      set_pos (pos);
      write (data, size);
    END ins;

    --<< AVY


    PROCEDURE MakeAlign * (VAR s: LONGINT; al: LONGINT);
    BEGIN
      IF s # 0 THEN
        s:= (((s-1) DIV al)+1)*al;
      END;
    END MakeAlign;

    PROCEDURE align_file * (al: LONGINT);
      VAR i: LONGINT;
          empty: ARRAY 16 OF CHAR;
    BEGIN
      FOR i:=0 TO LEN(empty)-1 DO empty[i]:='w' END;
      i:=file_offs;
      MakeAlign(i,al);
      out(empty,i-file_offs);
    END align_file;

    PROCEDURE EndOutput * ();
      VAR err: pc.STRING;
    BEGIN
      flush;
      obj_file.CloseNew(TRUE, FALSE, err);
      IF err # NIL THEN
        env.errors.Fault(env.null_pos, 432, err^);
      END;
      obj_file := NIL;
    END EndOutput;

(*
 *********************************************
 *          Context Iterator                 *
 *********************************************
*)

    TYPE iter_proc_type   * = cmd.iterated_proc;
         filter_proc_type * = PROCEDURE(o: OBJECT):BOOLEAN;

      VAR  CurrentIterator : iter_proc_type;
           CurrentManager  : iter_proc_type;
           curr_f1,curr_f2 : filter_proc_type;
           DoProceedExterns: BOOLEAN;

      CONST omark_gen_passed    = SYSTEM.SUCC(at.omark_gen_marked);

      PROCEDURE ManagerIterate(o: OBJECT);
      BEGIN
        IF NOT(omark_gen_passed IN o.marks) THEN
          IF ( ObjectStatus(o) # status_Unkown ) AND
             ( (curr_f1=NIL) OR curr_f1(o) )       AND
             ( (curr_f2=NIL) OR curr_f2(o) )
          THEN
            CurrentIterator(o)
          END;
          INCL(o.marks,omark_gen_passed);
        END;
      END ManagerIterate;

      PROCEDURE ManagerClear(o: OBJECT);
      BEGIN
        EXCL(o.marks,omark_gen_passed);
      END ManagerClear;

      PROCEDURE do_iteration(o: OBJECT);
        VAR rsegm: cmd.CODE_SEGM;
            i: INTEGER;
      BEGIN
        IF ObjectStatus(o) IN { status_Local, status_Global } THEN
          CurrentManager(o);
          IF DoProceedExterns AND (at.omark_gen_ready IN o.marks) THEN
            rsegm:=cmd.get_ready(o);
            IF (rsegm.fxup#NIL) THEN
              FOR i:=0 TO rsegm.fxup_len-1 DO
                CurrentManager(rsegm.fxup[i].obj);
              END;
            END;
          END;
        END;
      END do_iteration;

    PROCEDURE IterateContext * (proc: iter_proc_type; f1: filter_proc_type; f2: filter_proc_type);
    BEGIN
      DoProceedExterns:=TRUE;
      CurrentIterator:=proc;
      CurrentManager:=ManagerIterate;
      curr_f1:=f1;
      curr_f2:=f2;
      cmd.iter_context(at.curr_mod,do_iteration);
      CurrentManager:=ManagerClear;
      cmd.iter_context(at.curr_mod,do_iteration);
    END  IterateContext;

(*******************************************************************)

    PROCEDURE SortFixups * (f: cmd.FIXUPs; f_len: LONGINT);

      PROCEDURE ExchangeFixups(i,j:LONGINT);
        VAR t: cmd.fixup_desc;
      BEGIN
        t.obj    :=f[i].obj;     f[i].obj    :=f[j].obj;     f[j].obj    :=t.obj;
        t.fx_offs:=f[i].fx_offs; f[i].fx_offs:=f[j].fx_offs; f[j].fx_offs:=t.offs;
        t.offs   :=f[i].offs;    f[i].offs   :=f[j].offs;    f[j].offs   :=t.offs;
        t.kind   :=f[i].kind;    f[i].kind   :=f[j].kind;    f[j].kind   :=t.kind;
      END ExchangeFixups;
      PROCEDURE do_qsort(l,r: LONGINT);
        VAR i,j,ix: LONGINT;
      BEGIN
        i:=l; j:=r;
        ix:=(l+r) DIV 2;
        REPEAT
          WHILE f[i].offs < f[ix].offs DO INC(i) END;
          WHILE f[ix].offs < f[j].offs DO DEC(j) END;
          IF i<=j THEN
            IF i#j THEN
              IF ix=i THEN ix:=j ELSIF ix=j THEN ix:=i END;
              ExchangeFixups(i,j)
            END;
            INC(i); DEC(j);
          END;
        UNTIL i>j;
        IF l<j THEN do_qsort(l,j) END;
        IF i<r THEN do_qsort(i,r) END;
      END do_qsort;
    BEGIN
      IF f=NIL THEN RETURN END;
      do_qsort(0,f_len-1);
    END SortFixups;

(************************************************************************)

    TYPE CARD32 * = ARRAY 4 OF SYSTEM.BYTE;

      TYPE
        ADDR_EXT = POINTER TO addr_ext_rec;
        addr_ext_rec = RECORD(at.attr_ext_rec)
                         c1,c2,c3,c4,c5,c6: CARD32;
                       END;
      CONST
        a_addr = cmd.a_ready_code + 1;


    PROCEDURE set_adr * (o: OBJECT; c1: CARD32; c2: CARD32; c3: CARD32;
                                    c4: CARD32; c5: CARD32; c6: CARD32 );
      VAR adr_ext: ADDR_EXT;
    BEGIN
      NEW(adr_ext);
      adr_ext.c1 := c1;
      adr_ext.c2 := c2;
      adr_ext.c3 := c3;
      adr_ext.c4 := c4;
      adr_ext.c5 := c5;
      adr_ext.c6 := c6;
      at.app_obj_attr(o, adr_ext, a_addr);
      INCL(o^.marks, at.omark_gen_marked);
    END set_adr;

    PROCEDURE get_adr * (o: OBJECT; VAR c1: CARD32; VAR c2: CARD32; VAR c3: CARD32;
                                    VAR c4: CARD32; VAR c5: CARD32; VAR c6: CARD32);
      VAR a: at.ATTR_EXT;
    BEGIN
      IF ~(at.omark_gen_marked IN o.marks) THEN
        io.needed := TRUE;
        io.print("\to.name = %s,o.allocated = %b\n",
                 o.name^,
                 at.omark_allocated IN o.marks);
        ASSERT(FALSE);
      END;
      --ASSERT(at.omark_gen_marked IN o.marks);
      a := at.attr(o.ext, a_addr);
      WITH a: ADDR_EXT DO
        c1:=a.c1;
        c2:=a.c2;
        c3:=a.c3;
        c4:=a.c4;
        c5:=a.c5;
        c6:=a.c6;
      END;
    END get_adr;


    PROCEDURE modify_adr * (o: OBJECT; c1: CARD32; c2: CARD32; c3: CARD32;
                                       c4: CARD32; c5: CARD32; c6: CARD32);
    VAR a: at.ATTR_EXT;
    BEGIN
      IF ~(at.omark_gen_marked IN o.marks) THEN
        io.needed := TRUE;
        io.print("\to.name = %s,o.allocated = %b\n",
                 o.name^,
                 at.omark_allocated IN o.marks);
        ASSERT(FALSE);
      END;

      a := at.attr(o.ext, a_addr);
      WITH a: ADDR_EXT DO
        a.c1 := c1;
        a.c2 := c2;
        a.c3 := c3;
        a.c4 := c4;
        a.c5 := c5;
        a.c6 := c6;
      END;
    END modify_adr;


TYPE

  FORM_OBJ *= POINTER TO formobj_rec;
  formobj_rec *= RECORD (reg.list_rec)
                   defaultFormat*: reg.ITEM;
                 END;

PROCEDURE (this: FORM_OBJ) SetDebugFormat* (): BOOLEAN;
BEGIN
  IF reg.SetFromEquation( opt.EQU_DBGFMT, this.defaultFormat.name,
                          opt.dbgFormat, this ) = NIL THEN
    env.errors.Warning(at.curr_mod.pos, 500);
    RETURN FALSE;
  ELSE
    RETURN TRUE;
  END;
END SetDebugFormat;

PROCEDURE (this: FORM_OBJ) generate*;
BEGIN
  ASSERT(FALSE);
END generate;

BEGIN
  reg.NewList(opt.objFormat);
END CodeFace.
(*
         TODO

    1. cproc is of what class ???

*)
