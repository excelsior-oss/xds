MODULE formCOFF;

IMPORT
    SYSTEM,
    cmd := CodeDef,
    env := xiEnv,
    str := Strings,
    fc	:= CodeFace,
    io	:= opIO,
    at	:= opAttrs,
    reg := Registry,
    opt := Options;

IMPORT dbg := DbgFace;


  TYPE
    CODE_SEGM = cmd.CODE_SEGM;
    OBJECT    = fc.OBJECT;
    STRING    = fc.STRING;

    INT32     = SYSTEM.INT32;
    INT16     = SYSTEM.INT16;
    INT8      = SYSTEM.INT8;
    CARD32    = SYSTEM.CARD32;
    CARD16    = SYSTEM.CARD16;
    CARD8     = SYSTEM.CARD8;
    UCHAR     = CARD8;

 VAR
      GenLineno
     ,LocalsNamed
     ,GenDebug
                  :BOOLEAN;



(*
 *********************************************
 *	   General coff description	     *
 *********************************************
*)


(*--  coff header  --*)

  TYPE
    COFF_HEADER = RECORD
      f_magic  : CARD16; -- magic nimber              0x14C for I386, 0x1F0 for PPCLE
      f_nscns  : CARD16; -- number of sections
      f_timdat : INT32;  -- time and date stamp
      f_symptr : INT32;  -- file pointer to symtab
      f_nsyms  : INT32;  -- number of symtab entries
      f_opthdr : CARD16; -- size of optional header   0
      f_flags  : CARD16; -- flags                     0
    END;

  CONST
    COFF_HEADER_SIZE = 20;

(*-- coff section header --*)

  TYPE
    SECTION_HEADER = RECORD
            s_name:    ARRAY 8 OF CHAR;   -- section name             -
            s_paddr:   INT32;             -- physical address         0
            s_vaddr:   INT32;             -- virtual address          0
            s_size:    INT32;             -- section size             -
            s_scnptr:  INT32;             -- file ptr to raw data     -
            s_relptr:  INT32;             -- file ptr to relocation   -
            s_lnnoptr: INT32;             -- file ptr to line numbers 0
            s_nreloc:  INT16;             -- # reloc entries          -
            s_nlnno:   INT16;            -- # line number entries    0
            s_flags:   INT32;             -- flags                    SEC_CODE_ATTR/SEC_DATA_ATTR/SEC_BSS_ATTR
    END;
  CONST
    SECTION_HEADER_SIZE=40;
    SEC_CODE_ATTR = 20H;
    SEC_DATA_ATTR = 40H;
    SEC_BSS_ATTR  = 80H;

    SEC_CODE_ATTR_DBG = 060500020H;
    SEC_DATA_ATTR_DBG = 0C0300040H;
    SEC_BSS_ATTR_DBG =  0C0300080H;

    SEC_DBGS_ATTR = 042100048H;
    SEC_DBGT_ATTR = 042100048H;

(*--  coff symtab entry  --*)

  TYPE
    SYMENT = RECORD
      n_name  : INT32;  -- offset in strtab, if zero then look into s_name;
      s_name  : ARRAY 8 OF CHAR;
      n_value : INT32;
      n_scnum : INT16;
      n_type  : CARD16; -- 0
      n_sclass: INT8;   -- Storage class, values defined bellow
      n_numaux: INT8;
    END;

  CONST -- Storage class values
      CLASS_EXTERNAL = 2;
      CLASS_STATIC   = 3;
      CALSS_FUNCTION = 101;
      CALSS_FILE     = 103;

  CONST SYMENT_SIZE = 18;


(*--  coff relocation entry  --*)

  TYPE
    RELOCATION_INFO = RECORD
       r_vaddr   : INT32;    -- address to apply relocation
       r_symndx  : INT32;    -- symtab index of referenced object
       type	 : INT16;    -- type, for I386 either R_PCRLONG or R_DIR32
                             --       for PPCLE - different
     END;

  CONST
    RELOCATION_INFO_SIZE = 10;

  CONST
<* IF TARGET_RISC THEN *>
    R_ADDR32   = 2;
    R_TOCREL16 = 8;
    R_REL24    = 6;
    R_IFGLUE   = 13;
    R_SECREL   = 11;
    R_SECTION  = 12;
<* ELSE *>
    R_PCRLONG  = 20;
    R_DIR32    = 6;
    R_SECREL   = 11;
    R_SECTION  = 10;
<* END *>

  TYPE
    LINENO = RECORD
      l_paddr : INT32;
      l_lnno  : CARD16;
    END;
  CONST LINENO_SIZE = 6;

  CONST TYPE_FUNCTION = 020H;

(*--  actual data  --*)

  CONST
    SEC_DBG_NDX  = -2;  --pseudo
    SEC_DUMB_NDX = 0;
    SEC_CODE_NDX = 1;
    SEC_DATA_NDX = 2;
    SEC_BSS_NDX  = 3;
    SEC_DBGS_NDX = 4;
    SEC_DBGT_NDX = 5;
    SEC_LAST_NDX = 5;

  VAR
    Header         : COFF_HEADER;
    SectionHeader  : ARRAY SEC_LAST_NDX+1 OF SECTION_HEADER;
    StrTabSize     : INT32;

    PROCEDURE InitHeader();
    BEGIN
<* IF TARGET_RISC THEN *>
      Header.f_magic   := 1F0H;   -- for PPCLE
<* ELSE *>
      Header.f_magic   := 14CH;   -- for I386
<* END *>
      Header.f_nscns   := SEC_LAST_NDX;
      Header.f_timdat  := 0;
      Header.f_symptr  := 0;
      Header.f_nsyms   := (fc.SourceFileNameLength() DIV SYMENT_SIZE) + 2;
      Header.f_opthdr  := 0; -- forever
      Header.f_flags   := 0; -- forever
      StrTabSize       := 4; -- at least
    END InitHeader;

    PROCEDURE InitSections();

      PROCEDURE Assign8(s-: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
        VAR i: INTEGER;
      BEGIN
        FOR i:=0 TO 7 DO d[i]:=s[i] END;
      END Assign8;

      VAR i, code_attr, data_attr, bss_attr: INT32;
    BEGIN
      FOR i:=0 TO SEC_LAST_NDX DO
        SectionHeader[i].s_paddr  :=0;
        SectionHeader[i].s_vaddr  :=0;
        SectionHeader[i].s_size   :=0;
        SectionHeader[i].s_scnptr :=0;
        SectionHeader[i].s_relptr :=0;
        SectionHeader[i].s_lnnoptr:=0;
        SectionHeader[i].s_nreloc :=0;
        SectionHeader[i].s_nlnno  :=0;
      END;
      IF GenLineno THEN
        code_attr:=SYSTEM.VAL(INT32,SEC_CODE_ATTR_DBG);
        data_attr:=SYSTEM.VAL(INT32,SEC_DATA_ATTR_DBG);
        bss_attr:=SYSTEM.VAL(INT32,SEC_BSS_ATTR_DBG);
      ELSE
        code_attr:=SEC_CODE_ATTR;
        data_attr:=SEC_DATA_ATTR;
        bss_attr:=SEC_BSS_ATTR;
      END;
      SectionHeader[SEC_CODE_NDX].s_name :=".text";
      SectionHeader[SEC_CODE_NDX].s_flags:=code_attr;
      SectionHeader[SEC_DATA_NDX].s_name :=".data";
      SectionHeader[SEC_DATA_NDX].s_flags:=data_attr;
      SectionHeader[SEC_BSS_NDX].s_name :=".bss";
      SectionHeader[SEC_BSS_NDX].s_flags:=bss_attr;

      Assign8(".debug$S",SectionHeader[SEC_DBGS_NDX].s_name);
      SectionHeader[SEC_DBGS_NDX].s_flags:=SEC_DBGS_ATTR;
      Assign8(".debug$T",SectionHeader[SEC_DBGT_NDX].s_name);
      SectionHeader[SEC_DBGT_NDX].s_flags:=SEC_DBGT_ATTR;
    END InitSections;


(*
 *********************************************
 *	    Objects attributes		     *
 *********************************************
*)

    PROCEDURE set_adr(o: OBJECT; sec_ndx: INT16;    symtab_idx: INT32;
                                 name_offs: INT32;  offs: INT32;
                                 lnno_offs: INT32);
    VAR sec_ndx32: INT32;
    BEGIN
      sec_ndx32:=sec_ndx;
      fc.set_adr(o,sec_ndx32,symtab_idx,name_offs,offs,lnno_offs,SYSTEM.VAL(fc.CARD32,153153));
    END set_adr;

    PROCEDURE get_adr(o: OBJECT; VAR sec_ndx: INT16;   VAR symtab_idx: INT32;
                                 VAR name_offs: INT32; VAR offs: INT32;
                                 VAR lnno_offs: INT32);
      VAR dummy: CARD32; sec_ndx32: INT32;
    BEGIN
      fc.get_adr(o,sec_ndx32,symtab_idx,name_offs,offs,lnno_offs,dummy);
      sec_ndx:=SYSTEM.VAL(INT16,sec_ndx32);
    END get_adr;


(*
 *********************************************
 *	    Objects classification	     *
 *********************************************
*)
  PROCEDURE TargetSection(o: OBJECT): INT16;  -- Which section object goes to?
  BEGIN
    IF fc.ObjectStatus(o) = fc.status_Extern THEN
      RETURN SEC_DUMB_NDX;
    ELSE
      CASE fc.ObjectClass(o) OF
        fc.class_Text   : RETURN SEC_CODE_NDX |
        fc.class_Data,
        fc.class_ROData : RETURN SEC_DATA_NDX |
        fc.class_BSS    : RETURN SEC_BSS_NDX  |
      END;
    END
  END TargetSection;

  PROCEDURE StorageClass(o: OBJECT): INT8;
  BEGIN
    CASE fc.ObjectStatus(o) OF
      fc.status_Local  : RETURN CLASS_STATIC   |
      fc.status_Global,
      fc.status_Extern : RETURN CLASS_EXTERNAL |
    END;
  END StorageClass;

  PROCEDURE FixupKind(ir_kind: SHORTINT): INT16; -- translate internal fixup type into COFF fixup types
  BEGIN
    CASE ir_kind OF
<* IF TARGET_RISC THEN *>
      cmd.fx_obj32   : RETURN R_ADDR32  |
      cmd.fx_rel24   : RETURN R_REL24   |
      cmd.fx_toc16   : RETURN R_TOCREL16|
      cmd.fx_ifglue  : RETURN R_IFGLUE  |
<* ELSE *>
      cmd.fx_obj32   : RETURN R_DIR32   |
      cmd.fx_relcall : RETURN R_PCRLONG |
<* END *>
    END;
  END FixupKind;

(*
 *********************************************
 *	    Context Iterator		     *
 *********************************************
*)

    PROCEDURE IterateContext(iterator: fc.iter_proc_type; user_filter: fc.filter_proc_type);
    BEGIN
      fc.IterateContext(iterator,user_filter,NIL);
    END IterateContext;

(*
    Set of filters for context iteration
    ------------------------------------
*)

    PROCEDURE FilterCode(o: OBJECT): BOOLEAN;
    BEGIN
      RETURN TargetSection(o)=SEC_CODE_NDX;
    END FilterCode;

    PROCEDURE FilterData(o: OBJECT): BOOLEAN;
    BEGIN
      RETURN TargetSection(o)=SEC_DATA_NDX;
    END FilterData;

(*
 *********************************************
 *	    Object Allocation		     *
 *********************************************
*)


      PROCEDURE ObjectName(o: OBJECT; VAR name: ARRAY OF CHAR);
      BEGIN
	IF (NOT LocalsNamed) AND (fc.ObjectStatus(o)=fc.status_Local) THEN
	  name[0]:=0X;
	ELSE
	  fc.ObjectName(o,name);
	END;
      END ObjectName;

      PROCEDURE ObjectNameSize(o: OBJECT):LONGINT;
	VAR s: ARRAY 512 OF CHAR;
	    l: LONGINT;
      BEGIN
	ObjectName(o,s);
	l:=LENGTH(s);
	IF l # 0 THEN l:=l+1 END;
	RETURN l;
      END ObjectNameSize;

      PROCEDURE XrefNumber(o: OBJECT): INT16;  --number of valid lineno entries for Object
        VAR rsegm: CODE_SEGM;
            i,predof,predln,of,ln,pos: LONGINT;
            fname: STRING;
            xrefs: cmd.XREFs;
            Number: INT16;
      BEGIN
        rsegm := cmd.get_ready(o);
        IF rsegm.xref=NIL THEN RETURN 0 END;
        ASSERT(rsegm.xref_len#0);
        xrefs:=rsegm.xref;
        Number:=0; predln:=-1; predof:=-1;
        FOR i:=0 TO rsegm.xref_len-1 DO
          xrefs[i].txtpos.unpack(fname, ln, pos);
          of := xrefs[i].offs;
          IF (ln # predln) & (of # predof) THEN
            predln := ln;
            predof := of;
            INC(Number);
          END;
        END ;
        RETURN Number;
      END XrefNumber;

      PROCEDURE Alloc(o: OBJECT); -- See Allocate()
        VAR symtab_idx,obj_offs,name_offs,name_size,lnno_offs: INT32;
            sec_ndx: INT16;
      BEGIN
        lnno_offs:=0;
        symtab_idx:=Header.f_nsyms;
        INC(Header.f_nsyms);
        sec_ndx:=TargetSection(o);
        IF sec_ndx=SEC_DUMB_NDX THEN
          obj_offs:=0;
        ELSE
          fc.MakeAlign(SectionHeader[sec_ndx].s_size,fc.ObjectAlignment(o));
          obj_offs:=SectionHeader[sec_ndx].s_size;
          INC(SectionHeader[sec_ndx].s_size,fc.ObjectSize(o));
          IF sec_ndx#SEC_BSS_NDX THEN
            INC(SectionHeader[sec_ndx].s_nreloc,fc.FixupsNumber(o));
          END;
        END;
        name_size:=ObjectNameSize(o);
        IF name_size<8 THEN
          name_offs:=0;
        ELSE
          name_offs:=StrTabSize;
          INC(StrTabSize,name_size);
        END;

        IF GenLineno AND ( sec_ndx = SEC_CODE_NDX ) THEN
          lnno_offs:=SectionHeader[sec_ndx].s_nlnno;
          INC( SectionHeader[sec_ndx].s_nlnno, XrefNumber(o)+1 );  -- +1 means host function reference
          INC(Header.f_nsyms,6);
        END;

        set_adr(o, sec_ndx, symtab_idx, name_offs, obj_offs, lnno_offs);
      END Alloc;

      PROCEDURE GenDebugSections;
        PROCEDURE CountFixups(rsegm: CODE_SEGM): INT16; --fx_obj32far is emalated in COFF by pair of fixups, that's why we have to count them in such manier
          VAR result: INT16;
              i: LONGINT;
        BEGIN
          result:=0;
          FOR i:=0 TO rsegm.fxup_len-1 DO 
            IF rsegm.fxup[i].kind=cmd.fx_obj32far THEN
              INC(result,2);
            ELSIF rsegm.fxup[i].kind=cmd.fx_obj32 THEN
              INC(result,1);
            ELSE
              ASSERT(FALSE);
            END;
          END;
          RETURN result;
        END CountFixups;
        VAR s: STRING;
            ext: ARRAY 16 OF CHAR;
      BEGIN
        env.config.Equation("OBJEXT", s);
        IF s = NIL THEN COPY(".obj", ext);
        ELSE
          IF s[0] = '.' THEN ext[0] := 0X;
          ELSE ext[0] := '.'; ext[1] := 0X;
          END;
          str.Append(s^, ext);
        END;
        IF dbg.generate (at.curr_mod.name^, ext, TRUE, at.DbgNestedProc IN at.COMP_MODE) THEN
          SectionHeader[SEC_DBGS_NDX].s_size   := dbg.symb_info.code_len;
          SectionHeader[SEC_DBGS_NDX].s_nreloc := CountFixups(dbg.symb_info);

          SectionHeader[SEC_DBGT_NDX].s_size   := dbg.type_info.code_len;
          SectionHeader[SEC_DBGT_NDX].s_nreloc := CountFixups(dbg.type_info);
        END;
      END GenDebugSections;


      PROCEDURE CountSectionsOffsets; -- Count sections offsets in file
        VAR fileptr: INT32;
      BEGIN
        fileptr:=COFF_HEADER_SIZE+SEC_LAST_NDX*SECTION_HEADER_SIZE;
        fc.MakeAlign(fileptr,16);
        SectionHeader[SEC_CODE_NDX].s_scnptr:=fileptr;
        INC(fileptr,SectionHeader[SEC_CODE_NDX].s_size);
        fc.MakeAlign(fileptr,16);
        SectionHeader[SEC_DATA_NDX].s_scnptr:=fileptr;
        INC(fileptr,SectionHeader[SEC_DATA_NDX].s_size);
        fc.MakeAlign(fileptr,16);
        SectionHeader[SEC_CODE_NDX].s_relptr:=fileptr;
        INC(fileptr,SYSTEM.VAL(INT32,SectionHeader[SEC_CODE_NDX].s_nreloc)*RELOCATION_INFO_SIZE);
        fc.MakeAlign(fileptr,16);
        SectionHeader[SEC_DATA_NDX].s_relptr:=fileptr;
        INC(fileptr,SYSTEM.VAL(INT32,SectionHeader[SEC_DATA_NDX].s_nreloc)*RELOCATION_INFO_SIZE);
        fc.MakeAlign(fileptr,16);
        IF GenLineno THEN
          SectionHeader[SEC_CODE_NDX].s_lnnoptr:=fileptr;
          INC(fileptr,SYSTEM.VAL(INT32,SectionHeader[SEC_CODE_NDX].s_nlnno)*LINENO_SIZE);
          fc.MakeAlign(fileptr,16);
        END;
        IF GenDebug THEN
          SectionHeader[SEC_DBGS_NDX].s_scnptr:=fileptr;
          INC(fileptr,SectionHeader[SEC_DBGS_NDX].s_size);
          fc.MakeAlign(fileptr,16);
          SectionHeader[SEC_DBGS_NDX].s_relptr:=fileptr;
          INC(fileptr,SYSTEM.VAL(INT32,SectionHeader[SEC_DBGS_NDX].s_nreloc)*RELOCATION_INFO_SIZE);
          fc.MakeAlign(fileptr,16);
          SectionHeader[SEC_DBGT_NDX].s_scnptr:=fileptr;
          INC(fileptr,SectionHeader[SEC_DBGT_NDX].s_size);
          fc.MakeAlign(fileptr,16);
          SectionHeader[SEC_DBGT_NDX].s_relptr:=fileptr;
          INC(fileptr,SYSTEM.VAL(INT32,SectionHeader[SEC_DBGT_NDX].s_nreloc)*RELOCATION_INFO_SIZE);
          fc.MakeAlign(fileptr,16);
        END;
        Header.f_symptr:=fileptr;
      END CountSectionsOffsets;

      PROCEDURE Allocate; -- Assign section, offset in it, name offset, lineno offset and symtab index to each object in internal representation
                          -- Also count sizes of each section, name table and symbol table
      BEGIN
        IterateContext(Alloc,NIL);
        INC(SectionHeader[SEC_BSS_NDX].s_size, 3); -- to to allow dword access to word sized variables at the very end of BSS
      END Allocate;

(*
 *********************************************
 *	    Writing to file 		     *
 *********************************************
*)

     CONST out=fc.out;

      PROCEDURE WriteContents(o: OBJECT); -- Write Object's code into file. 
                                          -- This implies that current positon in file  
                                          -- is equal to section_offset_of(section_of_object(Object))+objecto_ffset_in_section(Object)
        VAR rsegm: CODE_SEGM;
        PROCEDURE MakeAddends; -- Thtere are some thing to be chaged in code
          VAR                add, inf: INT32;
            CodeAddr, off4app, fx_adr: LONGINT;
                                    i: INTEGER;
            <* IF TARGET_RISC THEN *>
              z32, segm_offs_in_sec, offset_i: INT32;
                                      sec_ndx: INT16;
                               offset, opcode: SET;
            <* END *>
        BEGIN
          IF rsegm.fxup=NIL THEN RETURN END;
          <* IF TARGET_RISC THEN *>
          get_adr(o,sec_ndx,z32,z32,segm_offs_in_sec,z32);
          <* END *>
          FOR i:=0 TO rsegm.fxup_len-1 DO
            add     :=rsegm.fxup[i].fx_offs;  -- first add this one into fixup applying place
            off4app :=SYSTEM.VAL(LONGINT,rsegm.fxup[i].offs);
            CodeAddr:=SYSTEM.VAL(LONGINT,SYSTEM.ADR(rsegm.bcode[0]));
            fx_adr := CodeAddr + off4app;
            SYSTEM.GET(fx_adr, inf);
            inf := inf + add;
--            IF RelocationType(rsegm.fxup[i].kind) = R_386_PC32 THEN inf:=inf-4 END;
<* IF TARGET_RISC THEN *>
-- and if risc is the target then rel24 fixup's applying place should contain
-- distance from the beginning of current section to command, affected by fixup
            IF (sec_ndx=SEC_CODE_NDX) & (FixupKind(rsegm.fxup[i].kind)=R_REL24) THEN

              ASSERT(add=0);

              opcode := SYSTEM.VAL(SET,inf) - {2..25};
              offset := SYSTEM.VAL(SET,inf) - {0,1,26..31};
              offset_i := SYSTEM.VAL(INT32,offset) - (segm_offs_in_sec+off4app);
              offset := SYSTEM.VAL(SET,offset_i) - {0,1,26..31};
              inf:=SYSTEM.VAL(INT32,opcode+offset);

            END;
<* END *>
            SYSTEM.PUT(fx_adr, inf);
            rsegm.fxup[i].fx_offs:=0;
          END;
        END MakeAddends;
      BEGIN
        fc.align_file(fc.ObjectAlignment(o));
        rsegm:=cmd.get_ready(o);
        MakeAddends;   -- explicit addend
        out(rsegm.bcode^,rsegm.code_len);
      END WriteContents;

      PROCEDURE WriteRelocations(o: OBJECT); -- write down to file object's fixups
        VAR z16, fx_type: INT16;
            z32, i, ref_obj_symtab_ndx, obj_offs, offs_in_sec: INT32;
            rsegm: CODE_SEGM;
      BEGIN
        get_adr(o,z16,z32,z32,obj_offs,z32);
        rsegm:=cmd.get_ready(o);
        IF rsegm.fxup=NIL THEN RETURN END;
        FOR i:=0 TO rsegm.fxup_len-1 DO
          offs_in_sec:=rsegm.fxup[i].offs+obj_offs;
          out(offs_in_sec,4);
          get_adr(rsegm.fxup[i].obj,z16,ref_obj_symtab_ndx,z32,z32,z32);
          out(ref_obj_symtab_ndx,4);
          fx_type:=FixupKind(rsegm.fxup[i].kind);
          out(fx_type,2);
        END;
      END WriteRelocations;

        PROCEDURE WriteToSymTab(o: OBJECT); -- write info about object into symbol table
                                            -- This implies that current position in file is
                                            -- symtab_offset + object_symtab_index(Object)*symtabentrysize
          VAR sec_ndx, z16: INT16;
              i, symtab_ndx, name_offs, offs, z32, lnno_offs: INT32;
              begin_ln,begin_pos,end_ln,end_pos: INT32;
              fname: STRING;
              sc, z8: INT8;
              name: ARRAY 8 OF CHAR;
        BEGIN
          FOR i:=0 TO LEN(name)-1 DO name[i]:=0X END;
          z8:=0; z16:=0; z32:=0;
          get_adr(o,sec_ndx,symtab_ndx,name_offs,offs,lnno_offs);
        -- Symbol itself:
          IF name_offs=0 THEN
            ObjectName(o,name);
            out(name,8);
          ELSE
            out(z32,4);
            out(name_offs,4);
          END;
          out(offs,4);
          out(sec_ndx,2);
          IF sec_ndx=SEC_CODE_NDX THEN z16:= TYPE_FUNCTION END;
          out(z16,2);
          sc:=StorageClass(o);
          out(sc,1);
          IF GenLineno & (sec_ndx=SEC_CODE_NDX) THEN z8:=1 END;
          out(z8,1);

          IF GenLineno AND (sec_ndx = SEC_CODE_NDX) THEN

            --1st AUX entry:
            z32:=symtab_ndx+2;
            out(z32,4);
            z32:=fc.ObjectSize(o);
            out(z32,4);
            z32:=lnno_offs+SectionHeader[SEC_CODE_NDX].s_lnnoptr;
            out(z32,4);
            z32:=0;     -- i don't know where do i have to get that fucking index.
            out(z32,4);
            z16:=0;
            out(z16,2);

            o.pos.unpack(fname, begin_ln, begin_pos);
            o.end.unpack(fname, end_ln, end_pos);
            INC(begin_ln);INC(end_ln); -- Because FrontEnd counts lines from 0


            -- .bf entry:
            FOR i:=0 TO LEN(name)-1 DO name[i]:=0X END;
            name:=".bf";
            out(name,8);
            z32:=0;
            out(z32,4);
            z16:=sec_ndx;
            out(z16,2);
            z16:=0;
            out(z16,2);
            z8:=CALSS_FUNCTION;
            out(z8,1);
            z8:=1;
            out(z8,1);


            -- .bf AUX entry
            z16:=SYSTEM.VAL(INT16,begin_ln);
            z32:=0;
            out(z32,4);
            out(z16,2);
            z16:=0;
            out(z16,2);
            out(z32,4);
            out(z32,4);  -- still don't know where to get index
            out(z16,2);

            -- .lf entry:
            FOR i:=0 TO LEN(name)-1 DO name[i]:=0X END;
            name:=".lf";
            out(name,8);
            z32 := XrefNumber(o)+1;
            out(z32,4);
            z16 := sec_ndx;
            out(z16,2);
            z16 := 0;
            out(z16,2);
            z8:=CALSS_FUNCTION;
            out(z8,1);
            z8:=0;
            out(z8,1);

            -- .ef entry:
            FOR i:=0 TO LEN(name)-1 DO name[i]:=0X END;
            name:=".ef";
            out(name,8);
            z32:=fc.ObjectSize(o);
            out(z32,4);
            z16 := sec_ndx;
            out(z16,2);
            z16 := 0;
            out(z16,2);
            z8:=CALSS_FUNCTION;
            out(z8,1);
            z8:=1;
            out(z8,1);


            -- .ef AUX entry
            z16:=SYSTEM.VAL(INT16,end_ln);
            z32:=0;
            out(z32,4);
            out(z16,2);
            z16:=0;
            out(z16,2);
            out(z32,4);
            out(z32,4);
            out(z16,2);

          END;

        END WriteToSymTab;

      PROCEDURE WriteSymTab; -- write down into file special filename entry and entrys about all objects into symbol table
        VAR z16: INT16; i,z32: INT32;
            z8: INT8;
            name: ARRAY 8 OF CHAR;
            fname: POINTER TO ARRAY OF CHAR;
      BEGIN
        z32:=0;
        FOR i:=0 TO LEN(name)-1 DO name[i]:=0X END;
        name:=".file";
        out(name,8);
        out(z32,4);
        z16:= SEC_DBG_NDX;
        out(z16,2);
        z16:=0;
        out(z16,2);
        z8:=CALSS_FILE;
        out(z8,1);
        z8:=SYSTEM.VAL(INT8,(fc.SourceFileNameLength() DIV SYMENT_SIZE) + 1);
        out(z8,1);
        NEW(fname, z8 * SYMENT_SIZE);
        fc.SourceFileName(fname^);
        out(fname^,z8 * SYMENT_SIZE);
        IterateContext(WriteToSymTab,NIL);
      END WriteSymTab;


      PROCEDURE WriteToStrTab(o: OBJECT);  -- write objects names into file
                                        -- this implies that current position in file is
                                        -- strtab_offset+name_offset_in_strtab(Object)

        VAR z16: INT16; name_offs, z32: INT32;
            name: ARRAY 512 OF CHAR;
      BEGIN
        get_adr(o,z16,z32,name_offs,z32,z32);
        IF name_offs#0 THEN
          ObjectName(o,name);
          out(name,LENGTH(name)+1);
        END;
      END WriteToStrTab;

      PROCEDURE WriteXrefs(o: OBJECT);  -- write lineno entries into file
                                        -- this implies that current position in file is
                                        -- sections_linenos(sectionof(Object))+lineno_offset(Object)
        VAR rsegm: CODE_SEGM;
            i,predof,predln,of,ln,begin_ln,pos: LONGINT;
            fname: STRING;
            xrefs: cmd.XREFs;
            ln16, z16: INT16;
            symtab_ndx,seg_offs,z32: INT32;
      BEGIN
        get_adr(o,z16,symtab_ndx,z32,seg_offs,z32);
        out(symtab_ndx,4);
        ln16:=0;
        out(ln16,2);
        o.pos.unpack(fname, begin_ln, pos);
        rsegm := cmd.get_ready(o);
        IF rsegm.xref=NIL THEN RETURN END;
        ASSERT(rsegm.xref_len#0);
        xrefs:=rsegm.xref;
        predln:=-1; predof:=-1;
        FOR i:=0 TO rsegm.xref_len-1 DO
          xrefs[i].txtpos.unpack(fname, ln, pos);
          of := xrefs[i].offs;
          IF (ln # predln) & (of # predof) THEN
            predln := ln;
            predof := of;
            of:=of+seg_offs;
            out(of,4);
            ln16:=SYSTEM.VAL(INT16,ln-begin_ln);
            IF ln16<=0 THEN ln16:=1 END; --don't blame me, what else can i do here?
            out(ln16,2);
          END;
        END;
      END WriteXrefs;

      PROCEDURE WriteDBG; -- Write down debug info, which was generated by dbgCV module
        PROCEDURE MakeAdd(rsegm: CODE_SEGM);
          VAR                add, inf: INT32;
            CodeAddr, off4app, fx_adr: LONGINT;
                                    i: INTEGER;
        BEGIN
          IF rsegm.fxup=NIL THEN RETURN END;
          FOR i:=0 TO rsegm.fxup_len-1 DO
            add     :=rsegm.fxup[i].fx_offs;
            off4app :=SYSTEM.VAL(LONGINT,rsegm.fxup[i].offs);
            CodeAddr:=SYSTEM.VAL(LONGINT,SYSTEM.ADR(rsegm.bcode[0]));
            fx_adr := CodeAddr + off4app;
            SYSTEM.GET(fx_adr, inf);
            inf := inf + add;
            SYSTEM.PUT(fx_adr, inf);
            rsegm.fxup[i].fx_offs:=0;
          END;
        END MakeAdd;

        PROCEDURE WriteFx(rsegm: CODE_SEGM); -- special write fixup function needed
                                             -- because only in debug info one may meet obj32far fixup
          VAR z16, fx_type: INT16;
              z32, i, ref_obj_symtab_ndx, offs: INT32;
        BEGIN
          IF rsegm.fxup=NIL THEN RETURN END;
          FOR i:=0 TO rsegm.fxup_len-1 DO
            get_adr(rsegm.fxup[i].obj,z16,ref_obj_symtab_ndx,z32,z32,z32);
            CASE rsegm.fxup[i].kind OF
              cmd.fx_obj32:
                              out(rsegm.fxup[i].offs,4);
                              out(ref_obj_symtab_ndx,4);
                              fx_type:=FixupKind(rsegm.fxup[i].kind);
                              out(fx_type,2);           |
              cmd.fx_obj32far:
                              out(rsegm.fxup[i].offs,4);
                              out(ref_obj_symtab_ndx,4);
                              fx_type:= R_SECREL;
                              out(fx_type,2);

                              offs:=rsegm.fxup[i].offs+4;
                              out(offs,4);
                              out(ref_obj_symtab_ndx,4);
                              fx_type:= R_SECTION;
                              out(fx_type,2);            |
            END;
          END;
        END WriteFx;

      BEGIN
        MakeAdd(dbg.symb_info);
        out(dbg.symb_info.bcode^,dbg.symb_info.code_len);
        fc.align_file(16);
        WriteFx(dbg.symb_info);
        fc.align_file(16);

        MakeAdd(dbg.type_info);
        out(dbg.type_info.bcode^,dbg.type_info.code_len);
        fc.align_file(16);
        WriteFx(dbg.type_info);
        fc.align_file(16);
      END WriteDBG;

      PROCEDURE Write; -- all the writes all together
      BEGIN
        out(Header,COFF_HEADER_SIZE);
        out(SectionHeader[SEC_CODE_NDX],SECTION_HEADER_SIZE);
        out(SectionHeader[SEC_DATA_NDX],SECTION_HEADER_SIZE);
        out(SectionHeader[SEC_BSS_NDX],SECTION_HEADER_SIZE);
        out(SectionHeader[SEC_DBGS_NDX],SECTION_HEADER_SIZE);
        out(SectionHeader[SEC_DBGT_NDX],SECTION_HEADER_SIZE);
        fc.align_file(16);
        IterateContext(WriteContents,FilterCode);
        fc.align_file(16);
        IterateContext(WriteContents,FilterData);
        fc.align_file(16);
        IterateContext(WriteRelocations,FilterCode);
        fc.align_file(16);
        IterateContext(WriteRelocations,FilterData);
        fc.align_file(16);
        IF GenLineno THEN IterateContext(WriteXrefs,FilterCode); fc.align_file(16) END;
        IF GenDebug THEN WriteDBG END;
        WriteSymTab;
        out(StrTabSize,4);
        IterateContext(WriteToStrTab,NIL);
      END Write;

TYPE
  FORM_COFF *= POINTER TO formCOFF_rec;
  formCOFF_rec *= RECORD (fc.formobj_rec)
                  END;

PROCEDURE (this: FORM_COFF) generate*;
VAR
  dbg_is_set: BOOLEAN;
BEGIN
      LocalsNamed:=env.config.Option("LOCALSNAMED");
      dbg_is_set:= this.SetDebugFormat();
      GenDebug:= fc.GenDebug() & dbg_is_set;
      GenLineno:= GenDebug OR fc.GenLineno();

      InitHeader;
      InitSections;
      Allocate;
      IF GenDebug THEN GenDebugSections END;
      CountSectionsOffsets;
      fc.BeginOutput;
      Write;
      fc.EndOutput;
END generate;

VAR
  formCOFF: FORM_COFF;
  item: reg.ITEM;

BEGIN
  NEW(formCOFF);
  NEW(item);
  formCOFF.AddItem(opt.dbg_CV, item);
  formCOFF.defaultFormat := item;
  NEW(item);
  formCOFF.AddItem(opt.dbg_HLL, item);
  reg.Register(opt.objFormat, opt.objCOFF, formCOFF);
END formCOFF.

(*
   2DO:
   2. Iterating of fixups may make me reaching CodeFace.status_Unkown objects,
      wich should never be the case.
   3. Enormous ".lf" symbols may confuse UNIX's software (GNU binutils for instance)
      ... and may not. I don't know actually, i am just making a guess.
*)
