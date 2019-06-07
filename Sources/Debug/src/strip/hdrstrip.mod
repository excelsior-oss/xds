MODULE HdrStrip;

FROM Printf IMPORT printf;

IMPORT hdr := Header;


VAR
  full_name: hdr.STR;

BEGIN
  IF hdr.GetFileName (full_name) THEN
    IF hdr.SaveHeader (full_name) THEN
      IF hdr.WriteHeader (full_name) THEN
        printf ("Information was successfully saved to file %s.\n", full_name);
        HALT (0);
      ELSE
        printf ("Error: can not write file %s.\n", full_name);
        HALT (1);
      END;
    ELSE
      printf ("Error: can not read file %s.\n", full_name);
      HALT (2);
    END;
  ELSE
    printf ("Error: file name is expected.\n");
    HALT (3);
  END;
END HdrStrip.
