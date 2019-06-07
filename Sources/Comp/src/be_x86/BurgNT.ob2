-- source grammar = e386.b
<*+WOFF*>
MODULE BurgNT;
IMPORT ir, SYSTEM;
TYPE NT *= (
     NTnowhere,
     NTstm,
     NTreg,
     NTrc,
     NTmrc,
     NTmem,
     NTbased,
     NTscaled,
     NTaddr,
     NTlocal,
     NTtos,
     NTconst,
     NTimem
     );
TYPE
	CostArray   *= ARRAY NT OF INTEGER;
	NTSet       *= PACKEDSET OF NT;
	Rule        *= INTEGER;
	RuleArray   *= ARRAY NT OF Rule;
	OpRange     *= ir.Operation[ir.o_invalid..ir.o_hiword];

<* IF ~nodebug THEN *>
TYPE ArrayOfString = ARRAY NT OF ARRAY 20 OF CHAR;
CONST
    NTName* = ArrayOfString {
      'XXX',
      'stm',
      'reg',
      'rc',
      'mrc',
      'mem',
      'based',
      'scaled',
      'addr',
      'local',
      'tos',
      'const',
      'imem'
    };
<* END *>

END BurgNT.
