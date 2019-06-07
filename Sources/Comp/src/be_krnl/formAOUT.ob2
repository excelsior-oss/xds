MODULE formAOUT;

IMPORT
    SYSTEM,
    cmd := CodeDef,
    nms := ObjNames,
    env := xiEnv,
    str := Strings,
    fc	:= CodeFace,
    io	:= opIO,
    at	:= opAttrs;

  TYPE
    CODE_SEGM = cmd.CODE_SEGM;
    OBJECT    = fc.OBJECT;
    STRING    = fc.STRING;

    INT32     = SYSTEM.INT32;
    CARD32    = SYSTEM.CARD32;
    CARD16    = SYSTEM.CARD16;
    CARD8     = SYSTEM.CARD8;
    UCHAR     = CARD8;

(*
 *********************************************
 *	   General a.out description	     *
 *********************************************
*)


(*--  a.out header  --*)

  TYPE
    AOUT_HEADER = RECORD
      a_midmag : INT32;  -- 0x00640107	=  no_flags<<3 + i386_arch<<2 + O_MAGIC ;
      a_text   : INT32;  -- text size
      a_data   : INT32;  -- data size
      a_bss    : INT32;  -- bss size
      a_syms   : INT32;  -- symtab size
      a_entry  : INT32;  -- entry point, 0 for o_magic
      a_trsize : INT32;  -- text relocation size
      a_drsize : INT32;  -- data relocation size
    END;

  CONST
    AOUT_HEADER_SIZE = 32;


(*--  a.out symtab entry  --*)

  TYPE
    NLIST = RECORD
      n_strx  : INT32;	   -- offset in strtab
      n_type  : CARD8;	   -- ( N_UNDF | N_TEXT | N_DATA | N_BSS ) ['|' N_EXT]
      n_other : INT8;	   -- 0
      n_desc  : INT16;	   -- 0
      n_value : INT32;	   -- N_UNDF -> size of common block or zero for pure external
			   -- N_TEXT, N_DATA -> offset in file from beginning of TEXT section
			   -- N_BSS -> same with previous, as if BSSs are located in file after DATA
    END;

  CONST
    NLIST_SIZE = 12;

  CONST
    N_EXT  = 1;
  CONST
    N_UNDF = 0;
    N_TEXT = 4;
    N_DATA = 6;
    N_BSS  = 8;
  -- stab types:
    N_FUN   = 024H;
    N_SLINE = 044H;
    N_SO    = 064H;


(*--  a.out relocation entry  --*)

  TYPE
    RELOCATION_INFO = RECORD
       r_address : INT32;    -- address to apply relocation
       more	 : CARD32;   -- (symbol_index << 1) + R_IS_EXTERN + R_SIZE_4
			     -- [ + R_IS_PCRELATIVE ] ;
     END;

  CONST
    RELOCATIO_INFO_SIZE = 8;

  CONST
    R_IS_EXTERN     = 8;  -- 'extern' actually means that fixup'll be done with normal symbol, not with section
    R_SIZE_4	    = 4;
    R_IS_PCRELATIVE = 1;


(*--  header itself  --*)

  VAR Header: AOUT_HEADER;
      strtabsize: INT32;

    PROCEDURE InitHeader();
    BEGIN
      Header.a_midmag := 00640107H;
      Header.a_text   := 0;
      Header.a_data   := 0;
      Header.a_bss    := 0;
      Header.a_syms   := 0;
      Header.a_entry  := 0; -- zero forever, others will be counted during Alloc()
      Header.a_trsize := 0;
      Header.a_drsize := 0;
      strtabsize      := 5; -- 4 for size itself and 1 for empty name
    END InitHeader;


(*
 *********************************************
 *	    Objects attributes		     *
 *********************************************
*)

    PROCEDURE set_adr(o: OBJECT; symtab_idx: INT32; name_offs: INT32;
                                 offs: INT32);
      VAR dummy: CARD32;
    BEGIN
      dummy:=153153;
      fc.set_adr(o,symtab_idx,name_offs,offs,dummy,dummy,dummy);
    END set_adr;

    PROCEDURE get_adr(o: OBJECT; VAR symtab_idx: INT32; VAR name_offs: INT32;
                                 VAR offs: INT32);
      VAR dummy: CARD32;
    BEGIN
      fc.get_adr(o,symtab_idx,name_offs,offs,dummy,dummy,dummy);
    END get_adr;


(*
 *********************************************
 *	    Objects classification	     *
 *********************************************
*)
    PROCEDURE ObjectType (o: OBJECT): CARD8;
      VAR type: CARD8;
    BEGIN
      IF fc.ObjectStatus(o) = fc.status_Extern THEN
        type := N_UNDF;
      ELSE
	CASE fc.ObjectClass(o) OF
          fc.class_Text   : type := N_TEXT |
	  fc.class_Data,
          fc.class_ROData : type := N_DATA |
          fc.class_BSS    : type := N_BSS  |
	END;
      END
    END ObjectType;

    PROCEDURE Get_N_EXT(o: OBJECT);
    BEGIN
      CASE fc.ObjectStatus(o) OF
        fc.status_Local  : RETURN N_EXT |
        fc.status_Global,
        fc.status_Extern : RETURN 0     |
      END;
    END Get_N_EXT;

    PROCEDURE Get_IsPcRelative(kind: SHORTINT): INT8;
    BEGIN
      CASE kind OF
	cmd.fx_obj32   : RETURN 0		|
	cmd.fx_relcall : RETURN R_IS_PCRELATIVE |
      END;
    END Get_IsPcRelative;


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
      RETURN ObjectType(o)=N_TEXT;
    END FilterCode;

    PROCEDURE FilterData(o: OBJECT): BOOLEAN;
    BEGIN
      RETURN ObjectType(o)=N_DATA;
    END FilterData;


(*
 *********************************************
 *	    Object Allocation		     *
 *********************************************
*)

    VAR LocalsNamed: BOOLEAN;


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


      PROCEDURE Alloc();
        PROCEDURE do_alloc(VAR ssize: INT32; o: OBJECT);
          VAR save: INT32;
        BEGIN
          fc.MakeAlign(ssize,fc.ObjectAlignment(o));
          save:=ssize;
          INC(ssize,fc.ObjectSize(o));
          RETURN save;
        END do_alloc;
        VAR symtab_idx,obj_offs,name_offs,name_offs: INT32;
      BEGIN
        symtab_idx:=Header.a_syms DIV NLIST_SIZE;
        INC(Header.a_syms,NLIST_SIZE);
        CASE ObjectType(o) OF
          N_TEXT: obj_offs := do_alloc(Header.a_text,o); -- obj_offs is only offset from the beginning of current section, actual virtual address is section_offset + obj_offs
                  INC(Header.a_trsize,fc.FixupsNumber(o)*RELOCATIO_INFO_SIZE)|
          N_DATA: obj_offs := do_alloc(Header.a_data,o);
                  INC(Header.a_drsize,fc.FixupsNumber(o)*RELOCATIO_INFO_SIZE)|
          N_BSS : obj_offs := do_alloc(Header.a_bss ,o)                      |
          N_UNDF: obj_offs := 0;                                             |
        END;
        name_size:=ObjectNameSize(o);
        IF name_size#0 THEN
          name_offs:=strtabsize;
          INC(strtabsize,name_size);
        ELSE
          name_offs:=0;
        END;
        set_adr(o, symtab_idx, name_offs, obj_offs);
      END Alloc;


END formAOUT.
