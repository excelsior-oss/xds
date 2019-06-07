<*+O2EXTENSIONS *>
MODULE coffRoutine;

-- coff file creation routine
-- written by Rainbow for XDS xc compiler

IMPORT SYSTEM, cfd := coffDef, Strings;
IMPORT pc := pcK, cmd := CodeDef, fc := CodeFace;

TYPE
    INT32     = SYSTEM.INT32;
    INT16     = SYSTEM.INT16;
    INT8      = SYSTEM.INT8;
    CARD32    = SYSTEM.CARD32;
    CARD16    = SYSTEM.CARD16;
    CARD8     = SYSTEM.CARD8;

CONST
    SEC_DBG_NDX  *= cfd.N_DEBUG;  --pseudo
    SEC_DUMB_NDX *= cfd.N_UNDEF;
    SEC_CODE_NDX *=  1;
    SEC_DATA_NDX *=  2;
    SEC_BSS_NDX  *=  3;

VAR SEC_LAST_NDX *: INTEGER;

------------------------------------------------------------------------------
------------------------------variables---------------------------------------
------------------------------------------------------------------------------
--FIleHeader is not a fake structure !----------------------------------------
--it is used to keep values of nSections and nSymbols at least----------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

VAR FileHeader    *: cfd.FILHDR;
    SectionHeaders*: POINTER TO ARRAY OF cfd.SCNHDR;
    
TYPE StringTableRef = POINTER TO ARRAY OF CHAR;
     SymbolTableRef = POINTER TO ARRAY OF cfd.ANYENT;

VAR SymbolTable   *: SymbolTableRef;
    StringTable   *: StringTableRef; 

VAR StringTableSize  *: INT32;

CONST StrTab_InitSize = 15342;
      SymTab_InitSize = 15342;

VAR prevBF  : INT32;
    prevFCN : INT32;
------------------------------------------------------------------------------
------------------------------procedures--------------------------------------
------------------------------------------------------------------------------
PROCEDURE AddToStrTab*( name : ARRAY OF CHAR ) : INT32;
VAR len, pos, newsize : INT32;
    newtable : StringTableRef;
BEGIN
        IF name[0] = 0C THEN RETURN 0; END; 
        len := Strings.Length(name) + 1;

        IF StringTable=NIL THEN
        	NEW( StringTable, StrTab_InitSize );
        END;
        ASSERT( StringTable # NIL ); -- Yeah! I've used it !!
        IF LEN( StringTable^ ) < StringTableSize+len THEN
		newsize := LEN( StringTable^ ) * 2;
                NEW( newtable, newsize );
                SYSTEM.MOVE( SYSTEM.ADR( StringTable[0] ),
                             SYSTEM.ADR( newtable[0] ),
                             StringTableSize );
                StringTable := newtable;
                newtable := NIL;
        END;
        SYSTEM.MOVE( SYSTEM.ADR( name[0] ),
                     SYSTEM.ADR( StringTable[StringTableSize] ),
                     len );
        pos := StringTableSize;
        INC( StringTableSize, len );
       	RETURN pos + 4;
END AddToStrTab;


----------------------- Clear_xxx ---------------------------------

PROCEDURE ClearSYMENT*(VAR foo : cfd.SYMENT);
BEGIN
       	SYSTEM.FILL(SYSTEM.ADR(foo), 0, cfd.SYMESZ);
END ClearSYMENT;

PROCEDURE ClearAUXENT*(VAR foo : cfd.AUXENT);
BEGIN
       	SYSTEM.FILL(SYSTEM.ADR(foo), 0, cfd.SYMESZ);
END ClearAUXENT;

---------------------- Get_xxx -------------------------------------

PROCEDURE GetSYMENT*( ndx : INT32; VAR foo : cfd.SYMENT);
BEGIN
        ASSERT( ndx < FileHeader.f_nsyms );
        foo := SymbolTable[ ndx ].sym;
END GetSYMENT;

PROCEDURE GetAUXENT*( ndx : INT32; VAR foo : cfd.AUXENT);
BEGIN
        ASSERT( ndx < FileHeader.f_nsyms );
        foo := SymbolTable[ ndx ].aux;
END GetAUXENT;

------------------------ Add_pure ----------------------------------

PROCEDURE EnlargeSymTab;
VAR newsize : INT32;
    newtable : SymbolTableRef;
BEGIN
        IF SymbolTable=NIL THEN
        	NEW( SymbolTable, SymTab_InitSize );
        END;
        ASSERT( SymbolTable # NIL );
        IF LEN( SymbolTable^ ) <= FileHeader.f_nsyms THEN
		newsize := LEN( SymbolTable^ ) * 2;
                NEW( newtable, newsize );
                SYSTEM.MOVE( SYSTEM.ADR( SymbolTable[0] ),
                             SYSTEM.ADR( newtable[0] ),
                             FileHeader.f_nsyms * cfd.SYMESZ );
                SymbolTable := newtable;
                newtable := NIL;
        END;
END EnlargeSymTab;

-- returns symtab ndx of the symbol has been put
PROCEDURE AddSYMENT_pure*( e : cfd.SYMENT );  
BEGIN
        EnlargeSymTab();
        SymbolTable[FileHeader.f_nsyms].sym := e;
       	INC( FileHeader.f_nsyms );
END AddSYMENT_pure;

-- returns symtab ndx of the symbol has been put
PROCEDURE AddAUXENT_pure*( e : cfd.AUXENT );
BEGIN
        EnlargeSymTab();
        SymbolTable[FileHeader.f_nsyms].aux := e;
	INC( FileHeader.f_nsyms );
END AddAUXENT_pure;

PROCEDURE AddSYMENT*(   name  : ARRAY OF CHAR;
			value : INT32;
			scnum : INT16;
		      	type  : CARD16;
		      	sclass: INT8;
		      	numaux: INT8 );
VAR len,i : INT32;
    e     : cfd.SYMENT;
BEGIN
        len := Strings.Length(name);
       	IF len > 8 THEN
        	e.e.e.e_zeroes := 0;
                e.e.e.e_offset := AddToStrTab( name );
        ELSE 
                FOR i := 0   TO len-1 DO e.e.e_name[i] := name[i]; END;
                FOR i := len TO 7     DO e.e.e_name[i] := 0C;      END;
        END;
        e.e_value := value;
      	e.e_scnum := scnum;
      	e.e_type  := type; 
      	e.e_sclass:= sclass;
      	e.e_numaux:= numaux;
	AddSYMENT_pure( e );
END AddSYMENT;

PROCEDURE AddAUXENT_fcn*(type_tag   : CARD32;
			size       : CARD32;
			lineno_ptr : CARD32;
                        ndx_after  : CARD32 );
VAR a   : cfd.AUXENT;
BEGIN
        a.x_sym.x_tagndx       := type_tag;
        a.x_sym.x_misc.x_fsize := size;
        a.x_sym.x_fcnary.x_fcn.x_lnnoptr := lineno_ptr;
        a.x_sym.x_fcnary.x_fcn.x_endndx  := ndx_after;
        a.x_sym.x_tvndx := 0;
       	AddAUXENT_pure( a );
END AddAUXENT_fcn;

-- .bb, .eb, .bf, .ef auxentries
PROCEDURE AddAUXENT_block*( lineno : CARD16;
			   endndx : CARD32 );
VAR a   : cfd.AUXENT;
BEGIN
        ClearAUXENT(a);
        a.x_sym.x_misc.x_lnsz.x_lnno := lineno;
        a.x_sym.x_fcnary.x_fcn.x_endndx := endndx;
       	AddAUXENT_pure( a );
END AddAUXENT_block;

PROCEDURE Add_file_entry*( name : ARRAY OF CHAR );
VAR len, i : INT32;
    a      : cfd.AUXENT;
BEGIN
        AddSYMENT('.file', 0, cfd.N_DEBUG, cfd.T_NULL, cfd.C_FILE, 1);
        ClearAUXENT( a );
        len := Strings.Length(name);
       	IF len > cfd.E_FILNMLEN THEN
        	a.x_file.x_n.x_zeroes := 0;
                a.x_file.x_n.x_offset := AddToStrTab( name );
        ELSE 
		FOR i := 0   TO len-1    DO a.x_file.x_fname[i] := name[i]; END;
		FOR i := len TO cfd.E_FILNMLEN-1 DO a.x_file.x_fname[i] := 0C;  END;
	END;
	AddAUXENT_pure(a);
END Add_file_entry;

PROCEDURE Add_section_entry*( nsec : INTEGER );
VAR a : cfd.AUXENT;
BEGIN
	AddSYMENT( SectionHeaders[nsec].s_name,
                   SectionHeaders[nsec].s_vaddr,
                   nsec, cfd.T_NULL, cfd.C_STAT, 1);
	ClearAUXENT( a );
	a.x_scn.x_scnlen := SectionHeaders[nsec].s_size;
	a.x_scn.x_nreloc := SectionHeaders[nsec].s_nreloc;
	a.x_scn.x_nlinno := SectionHeaders[nsec].s_nlnno;
       	AddAUXENT_pure(a);
END Add_section_entry;

PROCEDURE InitCoffFile* ( sec_last_ndx: INTEGER );
VAR i : INTEGER;
BEGIN
        SEC_LAST_NDX := sec_last_ndx;
        SYSTEM.FILL( SYSTEM.ADR(FileHeader), 0, cfd.FILHSZ );
        FileHeader.f_magic := cfd.I386MAGIC;
        FileHeader.f_flags := cfd.F_AR32WR;
        FileHeader.f_nscns := SEC_LAST_NDX;
        NEW( SectionHeaders, SEC_LAST_NDX + 1 );
        FOR i:= 1 TO SEC_LAST_NDX DO
        	SYSTEM.FILL( SYSTEM.ADR ( SectionHeaders[i] ), 0, cfd.SCNHSZ ); 
        END;
        SectionHeaders[SEC_CODE_NDX].s_name := cfd._TEXT;
        SectionHeaders[SEC_DATA_NDX].s_name := cfd._DATA;
        SectionHeaders[SEC_BSS_NDX].s_name := cfd._BSS;

        SectionHeaders[SEC_CODE_NDX].s_flags := cfd.STYP_TEXT;
        SectionHeaders[SEC_DATA_NDX].s_flags := cfd.STYP_DATA;
        SectionHeaders[SEC_BSS_NDX].s_flags := cfd.STYP_BSS;

	SymbolTable := NIL;
        StringTable := NIL;
        StringTableSize := 0;

        prevBF := -1;
        prevFCN := -1;
END InitCoffFile;

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------

------------------------- Objects classification ---------------------------

PROCEDURE TargetSection*(o: pc.OBJECT): INT16; -- Which section object goes to?
BEGIN
	IF fc.ObjectStatus(o) = fc.status_Extern THEN
		RETURN SEC_DUMB_NDX;
	ELSE
		CASE fc.ObjectClass(o) OF
			  fc.class_Text   : RETURN SEC_CODE_NDX
			| fc.class_Data,
			  fc.class_ROData : RETURN SEC_DATA_NDX
			| fc.class_BSS    : RETURN SEC_BSS_NDX
		END;
	END
END TargetSection;

PROCEDURE StorageClass*(o: pc.OBJECT): INT8;
BEGIN
 	CASE fc.ObjectStatus(o) OF
		  fc.status_Local  : RETURN cfd.C_STAT
		| fc.status_Global,
		  fc.status_Extern : RETURN cfd.C_EXT
	END;
END StorageClass;

PROCEDURE FixupKind*(ir_kind: SHORTINT): INT16;
-- translate internal fixup type into COFF fixup types
BEGIN
 	CASE ir_kind OF
		  cmd.fx_obj32   : RETURN cfd.RELOC_ADDR32
      		| cmd.fx_relcall : RETURN cfd.RELOC_REL32
    	END;
END FixupKind;

------------------------- Objects attributes ------------------------------

PROCEDURE set_adr*(o: pc.OBJECT; sec_ndx: INT16;    symtab_idx: INT32;
                                    offs: INT32;     lnno_offs: INT32);
VAR dummy: CARD32; sec_ndx32: INT32;
BEGIN
 	dummy:=153153;
	sec_ndx32:=sec_ndx;
	fc.set_adr(o,sec_ndx32,symtab_idx,dummy,offs,lnno_offs,dummy);
END set_adr;

PROCEDURE get_adr*(o: pc.OBJECT; VAR sec_ndx: INT16;   VAR symtab_idx: INT32;
                                 VAR    offs: INT32;   VAR  lnno_offs: INT32);
VAR dummy: CARD32; sec_ndx32: INT32;
BEGIN
	fc.get_adr(o,sec_ndx32,symtab_idx,dummy,offs,lnno_offs,dummy);
	sec_ndx:=SYSTEM.VAL(INT16,sec_ndx32);
END get_adr;

PROCEDURE modify_adr*(o: pc.OBJECT; sec_ndx: INT16;   symtab_idx: INT32;
                                       offs: INT32;    lnno_offs: INT32);
VAR dummy: CARD32; sec_ndx32: INT32;
BEGIN
 	dummy:=153153;
	sec_ndx32:=sec_ndx;
	fc.modify_adr(o,sec_ndx32,symtab_idx,dummy,offs,lnno_offs,dummy);
END modify_adr;

------------------------- ................. ------------------------------

PROCEDURE Add_object_entry*( o: pc.OBJECT;
				name: ARRAY OF CHAR;
                                type: INT16;
                                naux: INT8 );
VAR dummy32: INT32;
    offs, lnno_offs: INT32;
    sec_ndx: INT16;
BEGIN
    get_adr(o, sec_ndx, dummy32, offs, lnno_offs);
    modify_adr(o, sec_ndx, FileHeader.f_nsyms, offs, lnno_offs );
    AddSYMENT( name, offs, sec_ndx, type, StorageClass(o), naux );
END Add_object_entry;

PROCEDURE Add_procedure_entry*( o: pc.OBJECT;
				name: ARRAY OF CHAR;
                                type: INT16;
                                type_tag: INT32);
VAR dummy32: INT32;
    offs, lnno_offs: INT32;
    sec_ndx: INT16;
BEGIN
    IF prevFCN # -1 THEN
      SymbolTable[prevFCN+1].aux.x_sym.x_fcnary.x_fcn.x_endndx := FileHeader.f_nsyms;
    END;
    prevFCN := FileHeader.f_nsyms; 

    get_adr( o, sec_ndx, dummy32, offs, lnno_offs );
    ASSERT( sec_ndx = SEC_CODE_NDX );
    INC( lnno_offs , SYSTEM.VAL(INT32, SectionHeaders[sec_ndx].s_lnnoptr) );
    modify_adr( o, sec_ndx, FileHeader.f_nsyms, offs, lnno_offs );

    AddSYMENT( name, offs, sec_ndx, type, StorageClass(o), 1 );
    AddAUXENT_fcn( type_tag, fc.ObjectSize(o), lnno_offs, 0 );
END Add_procedure_entry;

PROCEDURE Add_scope_entry*( o: pc.OBJECT; is_open: BOOLEAN );
VAR dummy32, offset: INT32;
    sec_ndx: INT16;
    fname: pc.STRING;
    ln32, pos, begin_ln: INT32;
    sname : ARRAY 4 OF CHAR;
    sg: cmd.CODE_SEGM;
BEGIN
    get_adr( o, sec_ndx, dummy32, offset, dummy32 );
    ASSERT( sec_ndx = SEC_CODE_NDX );

    sg := cmd.get_ready(o);
    IF is_open THEN
        IF prevBF # -1 THEN
             SymbolTable[prevBF+1].aux.x_sym.x_fcnary.x_fcn.x_endndx := FileHeader.f_nsyms;
        END;
        prevBF := FileHeader.f_nsyms;

        sname := ".bf";
        INC( offset, sg.start );
        o.pos.unpack( fname, ln32, pos );
    ELSE
        sname := ".ef";
        INC( offset, sg.fin );
        o.end.unpack( fname, ln32, pos ); -- absolute lnno, we need relative
        o.pos.unpack( fname, begin_ln, pos );
        DEC(ln32, begin_ln);
    END;

    AddSYMENT(sname, offset, sec_ndx, cfd.T_NULL, cfd.C_FCN, 1 );
    AddAUXENT_block( SYSTEM.VAL( INT16, ln32+1 ), 0 );
    -- in GO32 coff lnno counted from 1;
END Add_scope_entry;

END coffRoutine.