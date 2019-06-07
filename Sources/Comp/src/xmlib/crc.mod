<* +m2extensions *>
IMPLEMENTATION MODULE crc;
(*----------------------------------------------------------------------------*)
(* Модуль реализации для процедур подсчета контрольной суммы                  *)
(* Модуль содержит реализацию функций:                                        *)
(*   cksum32		- подсчет CRC32                                       *)
(*   cksum16		- подсчет CRC16                                       *)
(*   cksum32_tab	- подсчет CRC32 по таблице                            *)
(*   cksum16_tab	- подсчет CRC16 по таблице                            *)
(*----------------------------------------------------------------------------*)
(* ver 1.0	17.10.05	Краус                                         *)
(*----------------------------------------------------------------------------*)
FROM SYSTEM IMPORT CARD32, SET32, CARD16, SET16, CARD8, ADDADR, ADR, ADDRESS,
                   SHIFT;
(*FROM Printf IMPORT printf;*)

(* Алгоритм подсчета контрольной суммы cksum32 взят из MULTI (GHS) так же как *)
(* и полином *)

CONST CRC32_POLYNOMIAL = 010211021H;

(*----------------------------------------------------------------------------*)
(* Расчет контрольной суммы                                                   *)
(* Параметры:                                                                 *)
(*   initval	- начальное значение (если подсчет велся с начала = 0, если   *)
(*                нужно досчитывать КС, то равно значению, с которого будет   *)
(*                досчитывание.                                               *)
(*                                                                            *)
(*----------------------------------------------------------------------------*)
PROCEDURE cksum32 (initval: CARD32; staddr: ADDRESS; length: CARD32): CARD32;
VAR
  csum: CARD32;
  data: CARD32;
  p_byte: POINTER TO CARD8;
  i: CARDINAL;
BEGIN
  csum:=initval; p_byte:=staddr;
  WHILE length>0 DO
    data:=CARD32(SHIFT(SET32(VAL(CARD32,p_byte^)),24));  (* Только левый байт *)
    p_byte:=ADDADR(p_byte,1); DEC(length);
    FOR i:=0 TO 7 DO		      (* Цикл для каждого бита в байте *)
      IF ((SET32(data) / SET32(csum)) * SET32(080000000H)) = SET32{31} THEN
        (* Сдвинем CRC, выдвигая старший бит *)
	csum := CARD32(SHIFT(SET32(csum),1) / SET32(CRC32_POLYNOMIAL));
      ELSE
	csum := CARD32(SHIFT(SET32(csum),1));
      END;
      data := CARD32(SHIFT(SET32(data),1));
    END
  END;
  RETURN csum;
END cksum32;

(* Алгоритм подсчета контрольной суммы cksum16 взят из письма Буренина так же как и полином *)
(* Но - в письме алгоритм с ошибкой - нет сдвига данных и initval = 01D0FH *)

CONST CRC16_POLYNOMIAL = 01021H;

PROCEDURE cksum16(initval: CARD16; staddr: ADDRESS; length: CARD32): CARD16;
VAR
  csum: CARD16;
  data: CARD16;
  p_byte: POINTER TO CARD8;
  i: CARDINAL;
BEGIN
  csum:=initval; p_byte:=staddr;
  WHILE length>0 DO
    data:=CARD32(SHIFT(SET32(VAL(CARD16,p_byte^)),8));  (* Только левый байт *)
    p_byte:=ADDADR(p_byte,1); DEC(length);
    FOR i:=0 TO 7 DO		(* Цикл для каждого бита в байте *)
      IF ((SET16(data) / SET16(csum)) * SET16(08000H)) = SET16{15} THEN
        (* Сдвинем CRC, выдвигая старший бит *)
	csum := CARD16(SHIFT(SET16(csum),1) / SET16(CRC16_POLYNOMIAL));
      ELSE
	csum := CARD16(SHIFT(SET16(csum),1));
      END;
      data := CARD16(SHIFT(SET16(data),1));
    END
  END;
  RETURN csum;
END cksum16;

TYPE
  t_crctab = ARRAY [0..255] OF CARD32;

CONST
  crctab = t_crctab {
  000000000H, 010211021H, 020422042H, 030633063H, 040844084H, 050A550A5H, 060C660C6H, 070E770E7H,
  081088108H, 091299129H, 0A14AA14AH, 0B16BB16BH, 0C18CC18CH, 0D1ADD1ADH, 0E1CEE1CEH, 0F1EFF1EFH,
  012301231H, 002110210H, 032723273H, 022532252H, 052B452B5H, 042954294H, 072F672F7H, 062D762D6H,
  093389339H, 083198318H, 0B37AB37BH, 0A35BA35AH, 0D3BCD3BDH, 0C39DC39CH, 0F3FEF3FFH, 0E3DFE3DEH,
  024602462H, 034413443H, 004220420H, 014031401H, 064E464E6H, 074C574C7H, 044A644A4H, 054875485H,
  0A568A56AH, 0B549B54BH, 0852A8528H, 0950B9509H, 0E5ECE5EEH, 0F5CDF5CFH, 0C5AEC5ACH, 0D58FD58DH,
  036503653H, 026712672H, 016121611H, 006330630H, 076D476D7H, 066F566F6H, 056965695H, 046B746B4H,
  0B758B75BH, 0A779A77AH, 0971A9719H, 0873B8738H, 0F7DCF7DFH, 0E7FDE7FEH, 0D79ED79DH, 0C7BFC7BCH,
  048C048C4H, 058E158E5H, 068826886H, 078A378A7H, 008440840H, 018651861H, 028062802H, 038273823H,
  0C9C8C9CCH, 0D9E9D9EDH, 0E98AE98EH, 0F9ABF9AFH, 0894C8948H, 0996D9969H, 0A90EA90AH, 0B92FB92BH,
  05AF05AF5H, 04AD14AD4H, 07AB27AB7H, 06A936A96H, 01A741A71H, 00A550A50H, 03A363A33H, 02A172A12H,
  0DBF8DBFDH, 0CBD9CBDCH, 0FBBAFBBFH, 0EB9BEB9EH, 09B7C9B79H, 08B5D8B58H, 0BB3EBB3BH, 0AB1FAB1AH,
  06CA06CA6H, 07C817C87H, 04CE24CE4H, 05CC35CC5H, 02C242C22H, 03C053C03H, 00C660C60H, 01C471C41H,
  0EDA8EDAEH, 0FD89FD8FH, 0CDEACDECH, 0DDCBDDCDH, 0AD2CAD2AH, 0BD0DBD0BH, 08D6E8D68H, 09D4F9D49H,
  07E907E97H, 06EB16EB6H, 05ED25ED5H, 04EF34EF4H, 03E143E13H, 02E352E32H, 01E561E51H, 00E770E70H,
  0FF98FF9FH, 0EFB9EFBEH, 0DFDADFDDH, 0CFFBCFFCH, 0BF1CBF1BH, 0AF3DAF3AH, 09F5E9F59H, 08F7F8F78H,
  091809188H, 081A181A9H, 0B1C2B1CAH, 0A1E3A1EBH, 0D104D10CH, 0C125C12DH, 0F146F14EH, 0E167E16FH,
  010881080H, 000A900A1H, 030CA30C2H, 020EB20E3H, 0500C5004H, 0402D4025H, 0704E7046H, 0606F6067H,
  083B083B9H, 093919398H, 0A3F2A3FBH, 0B3D3B3DAH, 0C334C33DH, 0D315D31CH, 0E376E37FH, 0F357F35EH,
  002B802B1H, 012991290H, 022FA22F3H, 032DB32D2H, 0423C4235H, 0521D5214H, 0627E6277H, 0725F7256H,
  0B5E0B5EAH, 0A5C1A5CBH, 095A295A8H, 085838589H, 0F564F56EH, 0E545E54FH, 0D526D52CH, 0C507C50DH,
  034E834E2H, 024C924C3H, 014AA14A0H, 0048B0481H, 0746C7466H, 0644D6447H, 0542E5424H, 0440F4405H,
  0A7D0A7DBH, 0B7F1B7FAH, 087928799H, 097B397B8H, 0E754E75FH, 0F775F77EH, 0C716C71DH, 0D737D73CH,
  026D826D3H, 036F936F2H, 0069A0691H, 016BB16B0H, 0665C6657H, 0767D7676H, 0461E4615H, 0563F5634H,
  0D940D94CH, 0C961C96DH, 0F902F90EH, 0E923E92FH, 099C499C8H, 089E589E9H, 0B986B98AH, 0A9A7A9ABH,
  058485844H, 048694865H, 0780A7806H, 0682B6827H, 018CC18C0H, 008ED08E1H, 0388E3882H, 028AF28A3H,
  0CB70CB7DH, 0DB51DB5CH, 0EB32EB3FH, 0FB13FB1EH, 08BF48BF9H, 09BD59BD8H, 0ABB6ABBBH, 0BB97BB9AH,
  04A784A75H, 05A595A54H, 06A3A6A37H, 07A1B7A16H, 00AFC0AF1H, 01ADD1AD0H, 02ABE2AB3H, 03A9F3A92H,
  0FD20FD2EH, 0ED01ED0FH, 0DD62DD6CH, 0CD43CD4DH, 0BDA4BDAAH, 0AD85AD8BH, 09DE69DE8H, 08DC78DC9H,
  07C287C26H, 06C096C07H, 05C6A5C64H, 04C4B4C45H, 03CAC3CA2H, 02C8D2C83H, 01CEE1CE0H, 00CCF0CC1H,
  0EF10EF1FH, 0FF31FF3EH, 0CF52CF5DH, 0DF73DF7CH, 0AF94AF9BH, 0BFB5BFBAH, 08FD68FD9H, 09FF79FF8H,
  06E186E17H, 07E397E36H, 04E5A4E55H, 05E7B5E74H, 02E9C2E93H, 03EBD3EB2H, 00EDE0ED1H, 01EFF1EF0H
  };

(* Алгоритм подсчета контрольной суммы cksum32_tab взят из книги Ross N. Williams "Элементарное *)
(* руководство по CRC-алгоритмам обнаружения ошибок". Полином взят из MULTI (GHS). Таблица      *)
(* получена при помощи cksum32 для байта пробегающего значения от 0 до 255                      *)

PROCEDURE cksum32_tab (initval: CARD32; staddr: ADDRESS; length: CARD32): CARD32;
VAR
  csum: CARD32;
  data: CARD32;
  p_byte: POINTER TO CARD8;
BEGIN
  csum:=initval; p_byte:=staddr;
  WHILE length>0 DO
    data:=CARD32(p_byte^);
    p_byte:=ADDADR(p_byte,1); DEC(length);
    csum := CARD32(SET32(crctab[CARD32((SHIFT(SET32(csum),-24) / SET32(data))
            * SET32(0FFH))]) / SHIFT(SET32(csum),8));
  END;
  RETURN csum;
END cksum32_tab;

(* Алгоритм подсчета контрольной суммы cksum16_tab получен при модификации алгоритма cksum32_tab *)
(* а полином взят из книги Агурова "Последовательные интерфейсы ПК" (см. письмо Буренина) и, для *)
(* экономии места, доработан на использование таблицы из cksum32_tab                             *)

PROCEDURE cksum16_tab(initval: CARD16; staddr: ADDRESS; length: CARD32): CARD16;
VAR
  csum: CARD16;
  data: CARD16;
  p_byte: POINTER TO CARD8;
  crc16tab: POINTER TO ARRAY [0..255] OF ARRAY [0..1] OF CARD16;
BEGIN
  csum:=initval; p_byte:=staddr; crc16tab:=ADR(crctab);
  WHILE length>0 DO
    data:=CARD16(p_byte^);
    p_byte:=ADDADR(p_byte,1); DEC(length);
    csum := CARD16(SET16(crc16tab^[CARD16((SHIFT(SET16(csum),-8) / SET16(data))
            * SET16(0FFH))][0]) / SHIFT(SET16(csum),8));
  END;
  RETURN csum;
END cksum16_tab;

END crc.


