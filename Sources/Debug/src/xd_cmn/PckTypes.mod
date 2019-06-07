IMPLEMENTATION MODULE PckTypes;

IMPORT txt := Texts;

VAR
  i : CARDINAL;

BEGIN
  FOR i := 0 TO MaxPakets-1 DO
    WITH Pakets[i] DO
      Paket      := txt.nil;
      LastLine   := 0;
      LineNum    := 0;
      RetPaket   := 0;
      RetLineNum := 0;
      Reference  := FALSE;
    END;
  END;
  QuantityPaket := 0;
  CurrPaket     := 0;
  LastPaket     := MAX(CARDINAL);

  ActiveComponent := 0;
  ActiveModule    := 0;

  Mode       := SET_MODE_FLAGS {};
  ModePrefix := FALSE;
  ModeOn     := '';
  ModeOff    := '';
  TraceMode  := Source;
  ModeNames  := MODE_NAMES{ MODE_NAME{ '', '' } BY 32 };
  ModeBreak  := SET_MODE_FLAGS { User,     OutMem,       WrProt
                               , ProgInt,  Address,      Write
                               , WriteReg, Read,         ReadReg
                               , Call,     Ret,          If
                               , Line,     IO,           ModelProc
                               , System,   CompCreated,  CompDestroyed };

END PckTypes.
