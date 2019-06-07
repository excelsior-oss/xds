MODULE ir_def;

IMPORT SYSTEM;
IMPORT pc := pcK;

TYPE  LCNum    *= INTEGER;
      FLOAT    *= LONGREAL;
      VALUE    *= pc.VALUE;

(*----------------------------------------------------------------------------*)

TYPE TypeType *=
     (
       t_invalid,
       t_void,
       t_int,
       t_unsign,
       t_float,
       t_complex,
       t_ref,
       t_arr,
       t_rec,
       t_flxarr,
       t_ZZ,
       t_RR
     );

CONST MaxVarSize    *= 10;

TYPE TypeTypeSet    *= PACKEDSET OF TypeType;
     SizeType       *= SHORTINT;
     SizeTypeRange  *= SizeType[0..MaxVarSize];

TYPE ArrayTypeTypeOfString6 = ARRAY TypeType OF ARRAY 6 OF CHAR;

CONST TypeName* = ArrayTypeTypeOfString6
    { "inv",
      "void ",
      " int ",
      "unsg ",
      "float",
      "cplx ",
      " ref ",
      " arr ",
      " rec ",
      "flarr",
      "t_ZZ",
      "t_RR" };

END ir_def.
