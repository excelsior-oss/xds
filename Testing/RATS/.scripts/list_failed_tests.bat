@echo off
rem ------------------------------------------------------------------------------
rem                          Excelsior Rats System Utility
rem                            (c) 2007, Excelsior Ltd.
rem
rem Module:       report_all
rem Author:       Lvov Konstantin
rem Modified By:  
rem Created:      16-March-2007
rem Purpose:      List all filed test in report subdirectories. 
rem Main:         
rem Required:     Pakages  - cygwin-b20: grep, cat
rem ------------------------------------------------------------------------------

set REP_DIR=%1

rem --------------- Create output file name
set LIST_FILE=failed_tests_list.txt
set LIST_FILE_BACKUP=failed_tests_list.txt.old

pushd "%REP_DIR%"

if exist "%LIST_FILE_BACKUP%" del "%LIST_FILE_BACKUP%"
if exist "%LIST_FILE%"        ren "%LIST_FILE%"  "%LIST_FILE_BACKUP%"

echo off > "%LIST_FILE%"

for /D %%i in (*) do call :lbl_parse_test_group %%i
popd

goto :EOF


:lbl_parse_test_group
if not exist %1\*.log  goto :EOF

echo Test group: '%1' >> "%LIST_FILE%"
echo ======================== >> "%LIST_FILE%"
for /D %%i in (%1\*) do call :lbl_pares_dir %%i
echo. >> "%LIST_FILE%"

goto :EOF


:lbl_pares_dir
echo. >> "%LIST_FILE%"
echo %1 >> "%LIST_FILE%"
echo ------------------------  >> "%LIST_FILE%"
for %%i in (%1\*) do  echo    %%i >> "%LIST_FILE%"
rem -- to take into account fast IEEE754 tests' specific 
if exist "%1.log"  grep "Total tests:" "%1.log" >> "%LIST_FILE%"
if exist "%1.log"  grep "Total errors:" "%1.log" >> "%LIST_FILE%"
if exist "%1.log"  grep "failed for" "%1.log" >> "%LIST_FILE%"
goto :EOF
