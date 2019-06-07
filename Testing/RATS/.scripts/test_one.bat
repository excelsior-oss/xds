@echo off
rem ------------------------------------------------------------------------------
rem                            RATS Test System
rem         Copyright (c) 2014 Excelsior LLC., Novosibirsk, Russia
rem 
rem Module:      test_one.bat
rem Author:      Lvov Konstantin
rem Modified by:  
rem Created:     02-Mar-2015
rem Purpose:     To run single RATS template (tg-file)
rem Usage:       test_one.bat <tg-file> [<report_dir>] 
rem ------------------------------------------------------------------------------

if "%1" == "" goto lbl_Help

if not "%2" == "%TESTDIR%" echo Testing %~n2

set TEMPLATE=%~1
set REP_DIR=%~2
set TESTDIR=%~n2

if "%REP_DIR%" == "" call :lbl_setup_temp_report_dir %~n1

set TEST_REP_FILE=%REP_DIR%\%~n1.rep
set TEST_LOG_FILE=%REP_DIR%\%~n1.log
set TEST_ERR_FILE=%REP_DIR%\%~n1.err

if not exist "%TEMPLATE%" goto lbl_Tepl_not_found

call "%~dp0\.config\setenv.bat"

call "%~dp0\clean.bat"

if not exist "%REP_DIR%" mkdir "%REP_DIR%"

echo   --- PROCESSING TEMPLATE %~n1 ---
call "%~dp0\..\bin\ctr\ctr.exe" "%TEMPLATE%" "%TEST_REP_FILE%" 1>"%TEST_LOG_FILE%" 2>&1
if errorlevel 1        goto lbl_FAIL
if exist tmp\mod\*.mod goto lbl_FAIL
if exist tmp\ob2\*.ob2 goto lbl_FAIL
if exist tmp\def\*.def goto lbl_FAIL

echo   --- OK ---
call :lbl_fast_IEEE754
del "%TEST_LOG_FILE%" 1>nul 2>nul
del "%TEST_ERR_FILE%" 1>nul 2>nul
goto :EOF

:lbl_FAIL
echo   *** FAIL ***
call :lbl_fast_IEEE754
mkdir %REP_DIR%\%~n1 1>nul 2>nul
for %%i in (tmp\mod\*.mod) do copy %%i "%REP_DIR%\%~n1" 1>nul 2>>"%TEST_ERR_FILE%"
for %%i in (tmp\def\*.def) do copy %%i "%REP_DIR%\%~n1" 1>nul 2>>"%TEST_ERR_FILE%"
for %%i in (tmp\ob2\*.ob2) do copy %%i "%REP_DIR%\%~n1" 1>nul 2>>"%TEST_ERR_FILE%"
goto :EOF

rem --------------- script finalization

:lbl_fast_IEEE754
rem -- to take into account fast IEEE754 tests' specific 
set TMP_CNT_FILE=%TEST_REP_FILE%.cnt.tmp
if exist "%TEST_LOG_FILE%"  grep "Total tests:"  "%TEST_LOG_FILE%" >  "%TMP_CNT_FILE%"
if exist "%TEST_LOG_FILE%"  grep "Total errors:" "%TEST_LOG_FILE%" >> "%TMP_CNT_FILE%"
for %%i in (%TMP_CNT_FILE%) do  set /A TMP_CNT_FILE_SIZE=%%~zi
if %TMP_CNT_FILE_SIZE% GTR 0  copy /Y "%TMP_CNT_FILE%"  "%TEST_REP_FILE%.cnt" 1>nul 2>nul
del /Q "%TMP_CNT_FILE%"
goto :EOF


:lbl_setup_temp_report_dir
set TESTDIR=%1
set REP_DIR=reports_tmp\%1

if exist "%REP_DIR%" rmdir /Q /S "%REP_DIR%" 1>nul 2>nul
mkdir "%REP_DIR%"
goto :EOF


:lbl_Tepl_not_found
echo *** Cannot find template: %TEMPLATE% ***

:lbl_Help
echo Usage:  test_one.bat ^<tg-file^> [^<report_dir^>] 
goto :EOF
