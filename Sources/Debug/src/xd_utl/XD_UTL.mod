-- Главный модуль компоненты xd_utl - вспомогательные модули
-- Компонента содержит разнообразные полезные инструменты
--
-- Проверяет срок работы пробной версии (TRIALVERSION)

MODULE XD_UTL;

IMPORT Sort;
IMPORT Options;
IMPORT Protocol;
IMPORT Dll;
IMPORT Int64;
IMPORT Int2Str;
IMPORT Real2Str;
IMPORT Unidata;
IMPORT Translit;
IMPORT OutDebug;
IMPORT List;


<* IF DEFINED(TRIALVERSION) & TRIALVERSION THEN *>

IMPORT TimeConv;
IMPORT COMPILER;
IMPORT Printf;

CONST
  Try_Days    = 90;
  Expiry_Date = COMPILER.TIMESTAMP + (Try_Days * 24 * 60 * 60);

BEGIN
  IF Expiry_Date <= TimeConv.time() THEN
    Printf.printf ('Trial version has expired.\n');
    HALT (0);
  END;

<* END *>

END XD_UTL.
