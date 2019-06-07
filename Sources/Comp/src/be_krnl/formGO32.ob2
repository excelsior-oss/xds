MODULE formGO32;

IMPORT
    SYSTEM,
    cmd := CodeDef,
    env := xiEnv,
    str := Strings,
    fc	:= CodeFace,
    io	:= opIO,
    at	:= opAttrs,
    pc  := pcK,
    cfd := coffDef,
    cfr := coffRoutine,
    dbg := DbgFace,
    reg := Registry,
    opt := Options;

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


 VAR GenLineno,
     LocalsNamed,
     GenDebugGO32	  :BOOLEAN;

 -- here indexes of section desc. entries are stored
 -- they are referenced from symtab
 VAR predefined_symbols : ARRAY cfr.SEC_BSS_NDX+1 OF INTEGER;

 PROCEDURE InitHeader();
 BEGIN

      predefined_symbols[cfr.SEC_CODE_NDX] := 2;
      predefined_symbols[cfr.SEC_DATA_NDX] := 4;
      predefined_symbols[cfr.SEC_BSS_NDX] := 6;

      cfr.InitCoffFile( cfr.SEC_BSS_NDX );
      IF NOT GenLineno THEN
      	cfr.FileHeader.f_flags := cfr.FileHeader.f_flags + cfd.F_LNNO;
      END;
(*      IF NOT GenDebugGO32 THEN
      	cfr.FileHeader.f_flags := cfr.FileHeader.f_flags + cfd.F_LSYMS;
      END; *) -- I don't know if it's true or not
 END InitHeader;

---------------------------- Context Iterator -----------------------------

 PROCEDURE IterateContext( iterator   : fc.iter_proc_type;
                           user_filter: fc.filter_proc_type);
 BEGIN
	fc.IterateContext(iterator,user_filter,NIL);
 END IterateContext;

------------------- Set of filters for context iteration ------------------

 PROCEDURE FilterCode(o: OBJECT): BOOLEAN;
 BEGIN
	RETURN cfr.TargetSection(o) = cfr.SEC_CODE_NDX;
 END FilterCode;

 PROCEDURE FilterData(o: OBJECT): BOOLEAN;
 BEGIN
 	RETURN cfr.TargetSection(o) = cfr.SEC_DATA_NDX;
 END FilterData;

-------------------------- Object Allocation -------------------------------		      

 PROCEDURE ObjectName(o: OBJECT; VAR name: ARRAY OF CHAR);
 BEGIN
	IF (NOT LocalsNamed) AND (fc.ObjectStatus(o)=fc.status_Local) THEN
		name[0]:=0X;
	ELSE
		fc.ObjectName(o,name);
	END;
 END ObjectName;

 PROCEDURE ObjectNameSize(o: OBJECT):LONGINT; -- in bytes
 VAR s: ARRAY 512 OF CHAR;
     l: LONGINT;
 BEGIN
	ObjectName(o,s);
	l:=LENGTH(s);
	IF l # 0 THEN l:=l+1 END;
	RETURN l;
 END ObjectNameSize;

 PROCEDURE XrefNumber(o: OBJECT):INT16; --number of valid lineno entries for Object
 VAR rsegm              : CODE_SEGM;
     i, predof, predln,
     of, ln, pos        : LONGINT;
     fname              : STRING;
     xrefs              : cmd.XREFs;
     Number             : INT16;
 BEGIN
	rsegm := cmd.get_ready(o);
        IF rsegm.xref=NIL THEN RETURN 0 END;
        ASSERT(rsegm.xref_len#0);
        xrefs  := rsegm.xref;
        Number := 0; predln := -1; predof := -1;
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

 (* the only rel32 internal fixup pointing to the object *)
 (* belongs to the same section isn't to be written      *)
 PROCEDURE FixupIsToBeWritten(o: OBJECT; rsegm: CODE_SEGM; i:INT32): BOOLEAN;
 BEGIN
       	IF cfr.FixupKind(rsegm.fxup[i].kind) # cfd.RELOC_REL32 THEN RETURN TRUE; END;
        IF fc.ObjectStatus( rsegm.fxup[i].obj ) = fc.status_Extern THEN
        	RETURN TRUE;
        END;
        IF cfr.TargetSection(o) # cfr.TargetSection( rsegm.fxup[i].obj ) THEN
        	RETURN TRUE;
        END;
      	RETURN FALSE;
 END FixupIsToBeWritten;

 (*  count all fixups but rel32&&internal  *)
 PROCEDURE WritableFixupsNumber(o:OBJECT): INTEGER;
 VAR i,n   : INTEGER;
     rsegm : CODE_SEGM;
 BEGIN
	n:=0;
	rsegm:=cmd.get_ready(o);
        FOR i:=0 TO rsegm.fxup_len-1 DO
            IF FixupIsToBeWritten(o,rsegm,i) THEN
                  n:=n+1;
            END;
        END;
        RETURN n;
 END WritableFixupsNumber;

 -- main changes are that we don't count symtab and strtab offsets here,
 -- GenSymbolAndStringTables is to do it.
 PROCEDURE Alloc(o: OBJECT); -- See Allocate()
 VAR symtab_idx, obj_offs,
     name_size,  lnno_offs         : INT32;
     sec_ndx: INT16;
 BEGIN
        lnno_offs:=0; 
      	symtab_idx:=0;
        sec_ndx:=cfr.TargetSection(o);
        IF sec_ndx = cfr.SEC_DUMB_NDX THEN
          obj_offs:=0;
        ELSE
          fc.MakeAlign( cfr.SectionHeaders[sec_ndx].s_size, fc.ObjectAlignment(o) );
          obj_offs := cfr.SectionHeaders[sec_ndx].s_size;
          INC( cfr.SectionHeaders[sec_ndx].s_size, fc.ObjectSize(o) );
          IF sec_ndx # cfr.SEC_BSS_NDX THEN
             	INC( cfr.SectionHeaders[sec_ndx].s_nreloc, WritableFixupsNumber(o) );
          END;
        END;

        IF GenLineno AND ( sec_ndx = cfr.SEC_CODE_NDX ) THEN
          lnno_offs := cfr.SectionHeaders[sec_ndx].s_nlnno;
          -- +1 here means host function reference
          INC( cfr.SectionHeaders[sec_ndx].s_nlnno, XrefNumber(o)+1 );
        END;

        cfr.set_adr(o, sec_ndx, symtab_idx, obj_offs, lnno_offs);
 END Alloc;

 PROCEDURE Allocate;
 -- Assign section, offset in it and name offset 
 -- to each object in internal representation
 -- Also count sizes of each section,
 BEGIN
       IterateContext(Alloc,NIL);
 END Allocate;

 PROCEDURE CountSectionsOffsets; -- Count sections offsets in file
 VAR fileptr, tosub, foo : INT32;
 BEGIN
        fileptr:=cfd.FILHSZ + cfr.SEC_LAST_NDX * cfd.SCNHSZ;
        fc.MakeAlign(fileptr,16);
        cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_scnptr:=fileptr;
        tosub:=fileptr;
        cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_paddr:=0;
        cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_vaddr:=0;

        INC(fileptr,cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_size);
        fc.MakeAlign(fileptr,16);
        cfr.SectionHeaders[cfr.SEC_DATA_NDX].s_scnptr:=fileptr;

        cfr.SectionHeaders[cfr.SEC_DATA_NDX].s_paddr:=fileptr-tosub;
        cfr.SectionHeaders[cfr.SEC_DATA_NDX].s_vaddr:=fileptr-tosub;

        INC(fileptr, cfr.SectionHeaders[cfr.SEC_DATA_NDX].s_size );
        fc.MakeAlign(fileptr,16);
        cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_relptr:=fileptr;

        cfr.SectionHeaders[cfr.SEC_BSS_NDX].s_paddr:=fileptr-tosub;
        cfr.SectionHeaders[cfr.SEC_BSS_NDX].s_vaddr:=fileptr-tosub;

        foo := SYSTEM.VAL( INT32, cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_nreloc);
        INC(fileptr, foo * cfd.RELSZ);
        fc.MakeAlign(fileptr,16);
        cfr.SectionHeaders[cfr.SEC_DATA_NDX].s_relptr:=fileptr;

        foo := SYSTEM.VAL( INT32, cfr.SectionHeaders[cfr.SEC_DATA_NDX].s_nreloc);
        INC( fileptr, foo * cfd.RELSZ);

        fc.MakeAlign(fileptr,16);

        IF GenLineno THEN
              cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_lnnoptr:=fileptr;
              foo := SYSTEM.VAL(INT32,cfr.SectionHeaders[cfr.SEC_CODE_NDX].s_nlnno);
              INC( fileptr, foo * cfd.LINESZ);
              fc.MakeAlign(fileptr,16);
        END;

        cfr.FileHeader.f_symptr:=fileptr;
 END CountSectionsOffsets;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

 PROCEDURE TouchObject( o : OBJECT );
 VAR rsegm: CODE_SEGM;

       	PROCEDURE GCoff_AddValue(i : INTEGER) : INT32;
 	VAR calling_object,           (* тот object, в котором relocation *)
	    target_object : OBJECT;   (* объект на который relocation    *)

	    foo32,
	    value_to_add : INT32;

	    code_addr,		       (* начало кода calling_object    *)
	    off_4_app,                 (* смещение внутри calling_object*)
	    adr_4_app: LONGINT;        (* адрес в памяти, по которому   *)
				       (* находятся эти чертовы 4 байта *)

	    calling_offset,            (* смещения точки relocation'a и *)
	    target_offset : LONGINT;   (* точки taget'a внутри файла    *)
				       (* относительно .text            *)

	    is_external: BOOLEAN;
	    is_rel32   : BOOLEAN;

	    target_section : INT16;
	    source_section : INT16;

	    source_ndx, target_ndx : INT16;

        BEGIN
	   calling_object := o;
	   target_object  := rsegm.fxup[i].obj;
	   IF cfr.FixupKind(rsegm.fxup[i].kind) = cfd.RELOC_REL32 THEN
	    is_rel32 := TRUE ELSE is_rel32 := FALSE;
	   END;
           IF fc.ObjectStatus(target_object) = fc.status_Extern THEN
             is_external := TRUE; ELSE is_external := FALSE;
	   END;

	   target_section := cfr.TargetSection(target_object);
	   source_section := cfr.TargetSection(calling_object);

	   off_4_app := SYSTEM.VAL(LONGINT,rsegm.fxup[i].offs);
	   code_addr := SYSTEM.VAL(LONGINT,SYSTEM.ADR(rsegm.bcode[0]));
	   adr_4_app := code_addr + off_4_app;

	   cfr.get_adr(calling_object,
		      source_ndx, (* sec_ndx *)
		      foo32, (* symtab_ndx *)
		      calling_offset, (* offs *)
		      foo32); (* lnno_offs *)
	   cfr.get_adr(target_object,
		      target_ndx, (* sec_ndx *)
		      foo32, (* symtab_ndx *)
		      target_offset, (* offs *)
		      foo32); (* lnno_offs *)

	   calling_offset := calling_offset + off_4_app;

           IF is_rel32 THEN
		IF is_external THEN
                        (* отнять адрес след. команды *)
			value_to_add := - calling_offset - 4;
		ELSE    (* положить смещение до цели *)
			value_to_add := target_offset - calling_offset - 4;
		END;
   	   ELSE         (* abs32 *)
		IF is_external THEN
			(* ничего не делать *)
			value_to_add := 0;
		ELSE    (* положить смещение цели относительно .text *)
                       	value_to_add := target_offset + cfr.SectionHeaders[ target_section ].s_paddr;
               	END;
   	   END;
       	   RETURN value_to_add;
	END GCoff_AddValue;

        PROCEDURE MakeAddends; -- Thtere are some thing to be chaged in code
        VAR add, inf                 : INT32;
            CodeAddr, off4app, fx_adr: LONGINT;
            i                        : INTEGER;
        BEGIN
          IF rsegm.fxup=NIL THEN RETURN END;
          FOR i:=0 TO rsegm.fxup_len-1 DO
            add     :=rsegm.fxup[i].fx_offs;  -- first add this one into fixup applying place
            off4app :=SYSTEM.VAL(LONGINT,rsegm.fxup[i].offs);
            CodeAddr:=SYSTEM.VAL(LONGINT,SYSTEM.ADR(rsegm.bcode[0]));
            fx_adr := CodeAddr + off4app;
            SYSTEM.GET(fx_adr, inf);

            inf := inf + add;
            inf := inf + GCoff_AddValue(i);

            SYSTEM.PUT(fx_adr, inf);
            rsegm.fxup[i].fx_offs:=0;
          END;
        END MakeAddends;

 BEGIN
        rsegm:=cmd.get_ready(o);
        MakeAddends;   
 END TouchObject;

 PROCEDURE DoChanges;
 BEGIN
       	IterateContext( TouchObject, FilterCode );
       	IterateContext( TouchObject, FilterData );
 END DoChanges;

--------------------------------------------------------------------------
-----------------------------Writing to file------------------------------
--------------------------------------------------------------------------

 CONST out=fc.out;

-- Write Object's code into file.
-- This implies that current positon in file is equal to
-- section_offset_of(section_of_object(Object))+objecto_ffset_in_section(Object)
 PROCEDURE WriteContents(o: OBJECT); 
 VAR rsegm: CODE_SEGM;
 BEGIN
	fc.align_file(fc.ObjectAlignment(o));
        rsegm:=cmd.get_ready(o);
        out(rsegm.bcode^,rsegm.code_len);
 END WriteContents;

--------------------------------------------------------------------------

 PROCEDURE WriteRelocations(o: OBJECT); -- write down to file object's fixups
 VAR z16, fx_type: INT16;
     z32, i, ref_obj_symtab_ndx, obj_offs, offs_in_sec: INT32;
     rsegm: CODE_SEGM;
 BEGIN
        cfr.get_adr(o,z16,z32,obj_offs,z32);
        rsegm:=cmd.get_ready(o);
        IF rsegm.fxup=NIL THEN RETURN END;
        FOR i:=0 TO rsegm.fxup_len-1 DO
          offs_in_sec:=rsegm.fxup[i].offs+obj_offs;
          cfr.get_adr(rsegm.fxup[i].obj,z16,ref_obj_symtab_ndx,z32,z32);
          fx_type:=cfr.FixupKind(rsegm.fxup[i].kind);

--          offs_in_sec := offs_in_sec + cfr.SectionHeaders[cfr.TargetSection(o)].s_paddr;
          IF (fx_type = cfd.RELOC_ADDR32) AND
                (fc.ObjectStatus( rsegm.fxup[i].obj ) # fc.status_Extern)
          THEN
             -- Replacing:
             -- fixup to resolved internal symbol -> reference to section
             ref_obj_symtab_ndx := predefined_symbols[cfr.TargetSection(rsegm.fxup[i].obj)];
          END;

          IF FixupIsToBeWritten(o, rsegm, i) THEN
               	out(offs_in_sec,4);
          	out(ref_obj_symtab_ndx,4);
          	out(fx_type,2);
          END;
        END;
 END WriteRelocations;

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

 PROCEDURE GenSymbols( o : OBJECT );
 VAR name : ARRAY 512 OF CHAR;
     symtab_ndx, offs, lnno_offs: INT32;
     sec_ndx: INT16;
 BEGIN
        cfr.get_adr(o, sec_ndx, symtab_ndx, offs, lnno_offs);
        IF sec_ndx > cfr.SEC_DUMB_NDX THEN
            offs := offs + cfr.SectionHeaders[sec_ndx].s_paddr;
        END;
        cfr.modify_adr(o, sec_ndx, symtab_ndx, offs, lnno_offs );

        IF GenDebugGO32 & (o.mno = at.curr_mno) &
           ( (o.mode IN pc.OB_SET{pc.ob_proc, pc.ob_xproc, pc.ob_lproc, pc.ob_module}) OR
             ((o.mode = pc.ob_var) & (o.lev = 0))
           )
        THEN RETURN END;

        ObjectName(o,name);
        IF GenLineno & (cfr.TargetSection(o) = cfr.SEC_CODE_NDX) THEN
            cfr.Add_procedure_entry( o, name, cfd.T_NULL + cfd.DT_FCN, 0 );
            cfr.Add_scope_entry( o, TRUE );
            cfr.Add_scope_entry( o, FALSE );
        ELSE
            cfr.Add_object_entry( o, name, cfd.T_NULL, 0 );
        END;
 END GenSymbols;

 PROCEDURE GenerateSymbolAndStringTables;
 VAR s: STRING;
   ext: ARRAY 16 OF CHAR;
   fname: POINTER TO ARRAY OF CHAR;
 BEGIN
        NEW(fname, fc.SourceFileNameLength() + 1 );
        fc.SourceFileName(fname^);
        cfr.Add_file_entry(fname^);
        fname := NIL;
        cfr.Add_section_entry(cfr.SEC_CODE_NDX);
        cfr.Add_section_entry(cfr.SEC_DATA_NDX);
        cfr.Add_section_entry(cfr.SEC_BSS_NDX);

        IterateContext( GenSymbols, NIL);

       	IF GenDebugGO32 THEN
            env.config.Equation("OBJEXT", s);
            IF s = NIL THEN COPY(".o", ext);
            ELSE
              IF s[0] = '.' THEN ext[0] := 0X;
              ELSE ext[0] := '.'; ext[1] := 0X; END;
              str.Append(s^, ext);
            END;
            IF dbg.generate (at.curr_mod.name^, ext, TRUE, TRUE) THEN END; -- FIXME
        END;
 END GenerateSymbolAndStringTables;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

 -- write lineno entries into file
 -- this implies that current position in file is
 -- sections_linenos(sectionof(Object))+lineno_offset(Object)
 PROCEDURE WriteXrefs(o: OBJECT);  

 	PROCEDURE WriteLineno( a : INT32; b : INT16 );
        BEGIN
        	out(a, 4);
                out(b, 2);
        END WriteLineno;

 VAR rsegm: CODE_SEGM;
     i,predof,predln,of,ln,begin_ln,pos: LONGINT;
     fname: STRING;
     ln16, z16: INT16;
     symtab_ndx, obj_offs, z32: INT32;
 BEGIN
        cfr.get_adr(o,z16,symtab_ndx,obj_offs,z32);
        WriteLineno( symtab_ndx, 0);  -- zero-number entry

        o.pos.unpack(fname, begin_ln, pos);
        rsegm := cmd.get_ready(o);
        IF rsegm.xref=NIL THEN RETURN END;
        ASSERT(rsegm.xref_len#0);
        predln:=-1; predof:=-1;

        FOR i:=0 TO rsegm.xref_len-1 DO
          rsegm.xref[i].txtpos.unpack(fname, ln, pos);
          of := rsegm.xref[i].offs;
          IF (ln # predln) & (of # predof) THEN
            predln := ln;
            predof := of;
            of:=of + obj_offs;
            ln16:=SYSTEM.VAL(INT16,ln-begin_ln);
            IF ln16<=0 THEN ln16:=1 END; --don't blame me, what else can i do here?
            WriteLineno( of, ln16+1 );
          END;
        END;
 END WriteXrefs;

 PROCEDURE WriteHeader;
 BEGIN
        out(cfr.FileHeader,cfd.FILHSZ);
        out(cfr.SectionHeaders[cfr.SEC_CODE_NDX], cfd.SCNHSZ);
        out(cfr.SectionHeaders[cfr.SEC_DATA_NDX], cfd.SCNHSZ);
        out(cfr.SectionHeaders[cfr.SEC_BSS_NDX], cfd.SCNHSZ);
        fc.align_file(16);
 END WriteHeader;

 PROCEDURE WriteCodeAndData;
 BEGIN
        IterateContext(WriteContents,FilterCode);
        fc.align_file(16);
        IterateContext(WriteContents,FilterData);
        fc.align_file(16);
        IterateContext(WriteRelocations,FilterCode);
        fc.align_file(16);
        IterateContext(WriteRelocations,FilterData);
        fc.align_file(16);
 END WriteCodeAndData;

 PROCEDURE WriteLinenos;
 BEGIN
 	IF GenLineno THEN
        	IterateContext(WriteXrefs,FilterCode);
                fc.align_file(16);
        END;
 END WriteLinenos;

 PROCEDURE WriteSymbolTable;
 VAR i, j : INT32;
 BEGIN
        FOR i:=0 TO cfr.FileHeader.f_nsyms-1 DO	
        	FOR j:=0 TO cfd.SYMESZ-1 DO
                	out( cfr.SymbolTable[i].whole[j], 1);
                END;
        END;
 END WriteSymbolTable;

 PROCEDURE WriteStringTable;
 VAR foo : INT32;
 BEGIN
        foo := cfr.StringTableSize + 4;
        out( foo , 4 ); -- first 4 means sizeof(strtabsize)
        IF cfr.StringTableSize # 0 THEN
            out( cfr.StringTable^, cfr.StringTableSize );
        END;
 END WriteStringTable;

TYPE
  FORM_GO32 *= POINTER TO formGO32_rec;
  formGO32_rec *= RECORD (fc.formobj_rec)
                  END;

PROCEDURE (this: FORM_GO32) generate*;
VAR
  item: reg.ITEM;
  GenDebug: BOOLEAN;
  dbg_is_set: BOOLEAN;
BEGIN
      LocalsNamed:=env.config.Option("LOCALSNAMED");

      dbg_is_set := this.SetDebugFormat();
      GenDebug := fc.GenDebug() & dbg_is_set;
      item := reg.GetActive(opt.dbgFormat);
      GenDebugGO32 := GenDebug & (item.name = opt.dbg_GO32);

--      GenLineno := GenDebug OR fc.GenLineno();
  -- Why OR GenDebug ? They are independent
      GenLineno := fc.GenLineno();

      InitHeader;
      Allocate;
      -- we have to count sections before we generate any symbols
      CountSectionsOffsets;
      DoChanges;                      
      GenerateSymbolAndStringTables;  -- not ready

      fc.BeginOutput;
      WriteHeader;                    
      WriteCodeAndData; -- it writes rellocs too 
      WriteLinenos;                                
      WriteSymbolTable;                           
      WriteStringTable;                          
      fc.EndOutput;

      IF GenDebug & (item.name = opt.dbg_TEXT) THEN
        IF dbg.generate(at.curr_mod.name^, ".o", TRUE, TRUE) THEN END;
      END;
END generate;

VAR
  formGO32: FORM_GO32;
  item: reg.ITEM;

BEGIN
  NEW(formGO32);
  NEW(item);
  formGO32.AddItem(opt.dbg_GO32, item);
  formGO32.defaultFormat := item;
  NEW(item);
  formGO32.AddItem(opt.dbg_TEXT, item);
  reg.Register(opt.objFormat, opt.objGO32, formGO32);
END formGO32.
