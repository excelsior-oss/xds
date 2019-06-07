<* Storage+ *>
<* ALIGNMENT="1" *>

IMPLEMENTATION MODULE ReadExp;

IMPORT sys := SYSTEM;
IMPORT rf  := RndFile;
IMPORT xfp := xFilePos;
IMPORT ioc := IOChan;
IMPORT rio := RawIO;
IMPORT fmt := FormStr;

IMPORT kt  := KrnTypes;
IMPORT lnx := Linux;

IMPORT Printf;

FROM Printf IMPORT printf;

(*
TYPE
  ENTRY_NAME = ARRAY [0..255] OF CHAR;

  EXPORTED = RECORD
               obj   : sys.CARD32;
               offset: sys.CARD32;
               name  : ENTRY_NAME;
             END;

  EXPORTS  = POINTER TO ARRAY OF EXPORTED;
*)


PROCEDURE ReadExport (full_name-: ARRAY OF CHAR; VAR exp: kt.EXPORTS): BOOLEAN;
TYPE
  SEGMENT = RECORD
    begin, end: CARDINAL;
    idx       : INTEGER;
  END;
  PSEGMENT_ARR = POINTER TO ARRAY OF SEGMENT;
  PBUFFER_TYPE = POINTER TO ARRAY OF CHAR;

VAR
  bfd: lnx.BFD;
  symaddr, flags: CARDINAL;
  i, j, number, buf_size: INTEGER;

  segments: PSEGMENT_ARR;
  cursegm_idx, segm_number, segm_idx: INTEGER;
  buffer: PBUFFER_TYPE;

  PROCEDURE get_segment_no (addr: CARDINAL): INTEGER;
  VAR
    j: INTEGER;
  BEGIN
    IF (segments^[cursegm_idx].begin <= addr) AND
       (addr <= segments^[cursegm_idx].end)
    THEN
      RETURN cursegm_idx;
    END;

    FOR j:=0 TO segm_number-1 DO
      IF (segments^[j].begin <= addr) AND
         (addr <= segments^[j].end)
      THEN
        cursegm_idx := j;
        RETURN cursegm_idx;
      END;
    END;
    RETURN 0;
  END get_segment_no;
  
BEGIN
  bfd := NIL;
  exp := NIL;

  bfd := lnx.my_bfd_open (full_name);
  IF bfd = NIL THEN
    RETURN FALSE;
  END;
--  printf ("*** ReadExport: %s *****\n", full_name);

  IF NOT lnx.my_bfd_check_format(bfd) THEN
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  buf_size := lnx.my_bfd_get_elf_phdr_upper_bound (bfd);

  IF buf_size <= 0 THEN
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  NEW (buffer, buf_size);
  IF buffer=NIL THEN
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  segm_number := lnx.my_bfd_get_elf_phdrs (bfd, buffer^);

  IF segm_number = 0 THEN
    DISPOSE (buffer);
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  NEW(segments, segm_number);

  j := 1;
  FOR i := 1 TO segm_number DO
    WITH segments^[j-1] DO
      IF lnx.my_bfd_get_segment_info (buffer^, i-1, 0, begin, end, flags)
      THEN
        idx := i;
        INC (j);
      END;
    END;
  END;
  segm_number := j;
  DISPOSE (buffer);


  buf_size := lnx.my_bfd_get_symtab_upper_bound (bfd);

  IF buf_size <= 0 THEN
    DISPOSE(segments);
    lnx.my_bfd_close (bfd);
    RETURN FALSE;
  END;

  NEW (buffer, buf_size);
  IF buffer=NIL THEN
    DISPOSE(segments);
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  number := lnx.my_bfd_canonicalize_symtab (bfd, buffer^);

  IF number = 0 THEN
    DISPOSE(segments);
    DISPOSE (buffer);
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  NEW(exp, number);
  cursegm_idx := 0;
  FOR i := 0 TO number-1 DO
    WITH exp^[i] DO
      lnx.my_bfd_get_symbol_info (buffer^, i, symaddr, name);
      segm_idx := get_segment_no (symaddr);

      offset := symaddr - segments^[segm_idx].begin;
      obj := segments^[segm_idx].idx;
      IF name = "" THEN
        fmt.print (name, " unnamed, offset=0x%x", symaddr);

      END;
--      printf ("*** offset: 0x%$8x, begin: 0x%$8x, %d, name: '%s'\n",
--              offset, segments^[segm_idx].begin, obj, name);
    END;
  END;

  DISPOSE(segments);
  DISPOSE (buffer);
  lnx.my_bfd_close(bfd);

  RETURN TRUE;

END ReadExport;


END ReadExp.

