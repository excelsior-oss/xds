MODULE TOCData;
IMPORT pc := pcK;

TYPE
  GetDataProc = PROCEDURE (VAR o: pc.OBJECT;
                           VAR offs: LONGINT;
                           VAR align: SHORTINT;
                           VAR const: BOOLEAN);
VAR
  GetFirstData* : GetDataProc;  -- set both by opDef
  GetNextData*  : GetDataProc;

VAR ModuleStartObjects* : POINTER TO ARRAY OF pc.OBJECT;


END TOCData.
