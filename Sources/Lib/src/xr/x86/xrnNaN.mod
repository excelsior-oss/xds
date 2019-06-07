<*+ M2EXTENSIONS *>

IMPLEMENTATION MODULE xrnNaN;

IMPORT SYSTEM;

------------ float (4-byte)

PROCEDURE ["C"] X2C_is_aNF (f :float) :BOOLEAN;
CONST
  fullExpo = {23..30};  -- see IEEE 754 fp standard
BEGIN
  RETURN (BITSET(f)*fullExpo # fullExpo);
END X2C_is_aNF;


PROCEDURE ["C"] X2C_is_infF (f :float) :BOOLEAN;
CONST
  Significand = {0..22};  -- see IEEE 754 fp standard
BEGIN
  RETURN ~X2C_is_aNF (f) & (BITSET(f)*Significand = {});
END X2C_is_infF; 


PROCEDURE ["C"] X2C_is_NaNF (f :float) :BOOLEAN;
BEGIN
  RETURN ~( X2C_is_aNF (f) OR X2C_is_infF (f) );
END  X2C_is_NaNF;


PROCEDURE ["C"] X2C_is_EQF (f1, f2 :float) :BOOLEAN;
BEGIN
  RETURN X2C_is_aNF (f1) & X2C_is_aNF (f2) & (f1 = f2);
END X2C_is_EQF;

PROCEDURE ["C"] X2C_is_LESSF (f1, f2 :float) :BOOLEAN;
BEGIN
  RETURN X2C_is_aNF (f1) & X2C_is_aNF (f2) & (f1 < f2);
END X2C_is_LESSF; 


------------ double (8-byte)

PROCEDURE ["C"] X2C_is_aND (d :double) :BOOLEAN;
VAR
  setH :BITSET;
CONST
  fullExpo = {20..30};  -- see IEEE 754 fp standard
BEGIN
  SYSTEM.GET (SYSTEM.ADR(d)+4, setH);
  RETURN (setH*fullExpo # fullExpo);
END X2C_is_aND;


PROCEDURE ["C"] X2C_is_infD (d :double) :BOOLEAN;
VAR
  setL, setH :BITSET;
CONST
  SignificandH = {0..19};  -- see IEEE 754 fp standard
BEGIN
  SYSTEM.GET (SYSTEM.ADR(d), setL);
  SYSTEM.GET (SYSTEM.ADR(d)+4, setH);
  RETURN ~X2C_is_aND (d) & (setL = {}) & (setH*SignificandH = {});
END X2C_is_infD;


PROCEDURE ["C"] X2C_is_NaND (d :double) :BOOLEAN;
BEGIN
  RETURN ~( X2C_is_aND (d) OR X2C_is_infD (d) );
END X2C_is_NaND; 


PROCEDURE ["C"] X2C_is_EQD (d1, d2 :double) :BOOLEAN;
BEGIN
  RETURN X2C_is_aND (d1) & X2C_is_aND (d2) & (d1 = d2);
END X2C_is_EQD;


PROCEDURE ["C"] X2C_is_LESSD (d1, d2 :double) :BOOLEAN;
BEGIN
  RETURN X2C_is_aNF (d1) & X2C_is_aNF (d2) & (d1 < d2);
END X2C_is_LESSD;


------------ extended (10-byte)

PROCEDURE ["C"] X2C_is_aNE (e :extended) :BOOLEAN;
VAR
  setH :BITSET;
CONST
  fullExpo = {15..30};  -- see IEEE 754 fp standard
BEGIN
  SYSTEM.GET (SYSTEM.ADR(e)+4+2, setH);
  RETURN (setH*fullExpo # fullExpo);
END X2C_is_aNE; 


PROCEDURE ["C"] X2C_is_infE (e :extended) :BOOLEAN;
VAR
  setL :SYSTEM.SET16;
  setM :BITSET;
  setH :BITSET;
CONST
  SignificandH = {0..14};  -- see IEEE 754 fp standard
BEGIN
  SYSTEM.GET (SYSTEM.ADR(e), setL);
  SYSTEM.GET (SYSTEM.ADR(e)+2, setM);
  SYSTEM.GET (SYSTEM.ADR(e)+2+4, setH);

  RETURN ~X2C_is_aNE (e) & (setL = SYSTEM.SET16{}) &
         (setM = {}) & (setH*SignificandH = {});
END X2C_is_infE;


PROCEDURE ["C"] X2C_is_NaNE (e :extended) :BOOLEAN;
BEGIN
  RETURN ~( X2C_is_aNE (e) OR X2C_is_infE (e) );
END X2C_is_NaNE;


PROCEDURE ["C"] X2C_is_EQE (e1, e2 :extended) :BOOLEAN;
BEGIN
  RETURN X2C_is_aNE (e1) & X2C_is_aNE (e2) & (e1 = e2);
END X2C_is_EQE; 


PROCEDURE ["C"] X2C_is_LESSE (e1, e2 :extended) :BOOLEAN;
BEGIN
  RETURN X2C_is_aNE (e1) & X2C_is_aNE (e2) & (e1 < e2);
END X2C_is_LESSE;


END xrnNaN.
