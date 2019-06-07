-- Операции над 64-битными целыми числами

IMPLEMENTATION MODULE Int2Str;

IMPORT sys := SYSTEM;

IMPORT Int64;


TYPE
  POWER10VEC = ARRAY [0..19] OF Int64.INT64;


CONST
  Power10Vec = POWER10VEC { Int64.INT64 { 1          , 0          }
                          , Int64.INT64 { 10         , 0          }
                          , Int64.INT64 { 100        , 0          }
                          , Int64.INT64 { 1000       , 0          }
                          , Int64.INT64 { 10000      , 0          }
                          , Int64.INT64 { 100000     , 0          }
                          , Int64.INT64 { 1000000    , 0          }
                          , Int64.INT64 { 10000000   , 0          }
                          , Int64.INT64 { 100000000  , 0          }
                          , Int64.INT64 { 1000000000 , 0          }
                          , Int64.INT64 { 540BE400H  , 02H        }
                          , Int64.INT64 { 4876E800H  , 017H       }
                          , Int64.INT64 { 0D4A51000H , 0E8H       }
                          , Int64.INT64 { 4E72A000H  , 0918H      }
                          , Int64.INT64 { 107A4000H  , 05af3H     }
                          , Int64.INT64 { 0A4C68000H , 038d7EH    }
                          , Int64.INT64 { 06FC10000H , 02386f2H   }
                          , Int64.INT64 { 05D8A0000H , 01634578H  }
                          , Int64.INT64 { 0A7640000H , 0DE0B6B3H  }
                          , Int64.INT64 { 089E80000H , 08AC72304H } };


-- Преобразование числа в строку
PROCEDURE IntToStr (signed: BOOLEAN; HI, LO: CARDINAL; VAR s: ARRAY OF CHAR);
VAR
  POWER, COUNT, BASE_HI, BASE_LO: CARDINAL;
  k: CARDINAL;
  zero: BOOLEAN;
BEGIN
 <* PUSH *>
 <* -IOVERFLOW *>
 <* -COVERFLOW *>
  IF signed AND (INTEGER(HI) < 0) THEN
    ASM
     not LO
     not HI
     add LO, 1
     adc HI, 0
    END;
    s[0] := '-';
    k := 1;
  ELSE
    k := 0;
  END;
  zero := FALSE;
  -- GET MAX POWER
  POWER := HIGH(Power10Vec)+1;
  WHILE POWER > 0 DO
    -- GET THE APPROPRIATE POWER OF TEN VALUE (TWO WORDS).
    BASE_HI := Power10Vec[POWER-1].high;
    BASE_LO := Power10Vec[POWER-1].low;
    COUNT := 0;
    -- DETERMINE THE POWER OF THIS BASE NUMBER BY
    -- REPETITIVE SUBTRACTION OF ITS TWO'S COMPLEMENT.
    WHILE (HI > BASE_HI) OR ((HI = BASE_HI) & (LO >= BASE_LO)) DO
      INC(COUNT);
      ASM
        mov  eax, BASE_LO
        sub  LO, eax
        mov  edx, BASE_HI
        sbb  HI, edx 
      END;
    END;
    ASSERT(COUNT<10);
    DEC(POWER);
    IF COUNT = 0 THEN
      IF zero OR (POWER = 0) THEN
        s[k] := CHR(ORD('0') + COUNT);
        INC(k);
      END;
    ELSE
      s[k] := CHR(ORD('0') + COUNT);
      INC(k);
      zero := TRUE;
    END;
  END;
  s[k] := 0C;
 <* POP *>
END IntToStr;


END Int2Str.

