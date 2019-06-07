@echo off
rem Run tests for all folders defined in ".testing.bsc"
rem
rem Usage: test_all [remote] [Title]
rem   remote - setup and run tests in remote folder 
rem   Title  - optional description of the testing reason 

call "%~dp0\.scripts\get_rep_count.bat"

if not exist "%~dp0\.testing.bsc"  copy "%~dp0\.alltests.bsc"  "%~dp0\.testing.bsc"

if "%~1" == "remote" goto :lbl_remote

set TEST_ALL_DESC=
if not "%~1" == "" set TEST_ALL_DESC=%~1

set TEST_ALL_TITLE=XDS-x86 RATS tests
if not "%TEST_ALL_DESC%" == ""  set TEST_ALL_TITLE=%TEST_ALL_TITLE% (%TEST_ALL_DESC%)

call "%~dp0\.config\setenv.bat"
if not "%WINNOSENT%" == ""  call :lbl_notify_start %WINNOSENT%
echo Start %TEST_ALL_TITLE% at %TIME:~0,-3%

for /F "eol=# tokens=1*" %%i IN (%~dp0\.testing.bsc) do  call :lbl_run_tests %%i

echo ====================================================
echo End %TEST_ALL_TITLE% at %TIME:~0,-3%
if not "%WINSENT%" == "" call :lbl_notify_send "%TEST_ALL_TITLE% are complete"

if exist "%~dp0\.origin.bsc~" call :lbl_relocate_report

goto :EOF

rem ============================================= Run tests in given mode
:lbl_run_tests
echo ----------------------------------------------------
call test_one.bat %1 "%TEST_ALL_DESC%"
goto :EOF


rem ============================================= Run Winsent to receive messages
:lbl_notify_start
start /D "%~dp1" /B %~nx1
set POPUP_NOTIFY_LOC=yes
goto :EOF


:lbl_notify_send
call "%WINSENT%" /u:%USERNAME% %1 1>nul
set POPUP_NOTIFY_LOC=
goto :EOF


rem ============================================= Setup and Ru8n test in remote folder
:lbl_remote
call "%~dp0\.scripts\get_setup_dir.bat" RATS_TARGET_DIR || goto :lbl_Exit
call "%~dp0\.scripts\setup.bat" "%RATS_TARGET_DIR%"     || goto :lbl_Exit

echo ORIGIN_DIR=%~dp0> "%RATS_TARGET_DIR%\.origin.bsc~"

start "%RATS_TARGET_DIR%" /D "%RATS_TARGET_DIR%" test_all.bat %2

goto :EOF


rem ============================================= Relocate report
:lbl_relocate_report
set REP_COUNT=
ren "%RATS_TARGET_DIR%\.origin.bsc~" .origin.bsc
set /p "YesNo=Copy remote report into origin [Y/N]: "
set RelocateRep=no
if "%YesNo%" == "Y"  set RelocateRep=yes
if "%YesNo%" == "y"  set RelocateRep=yes
if "%YesNo%" == "≠"  set RelocateRep=yes
if "%YesNo%" == "ç"  set RelocateRep=yes
if "%RelocateRep%" == "yes" call "%~dp0\.scripts\remote2origin.bat" "%TEST_ALL_DESC%"
goto :EOF


rem ============================================= Error handling
:lbl_Exit
goto :EOF
