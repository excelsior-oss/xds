@echo off
rem Run specified tests 
rem 
rem Usage: test_one Test [remote] [Title]
rem   Test   - folder to run tests 
rem   remote - setup and run tests in remote folder 
rem   Title  - optional description of the testing reason 

if "%~2" == "remote" goto :lbl_remote

set TEST_DESC=
if not "%~2" == ""  set TEST_DESC=%~2

set TEST_TITLE=XDS-x86 RATS tests for '%1'
if not "%TEST_DESC%" == ""  set TEST_TITLE=%TEST_TITLE% (%TEST_DESC%)

echo Run %TEST_TITLE%
echo   start %TIME:~0,-3%

if not exist "%~dp0\%1" (
    echo Folder "%~dp0\%1" not found
    echo ******* Run %1 tests ******* Failed!
    exit /B 1
) 

call %~dp0\.config\setenv.bat
if not "%WINNOSENT%" == ""  call :lbl_notify_start %WINNOSENT%

if not exist "%~dp0\XDS" xcopy "%~dp0\..\..\XDS\*" "%~dp0\XDS\" /Y /E /Q

if "%REP_COUNT%" == "" call "%~dp0\.scripts\get_rep_count.bat"

call :lbl_run_tests %1 

echo   end   %TIME:~0,-3%
if not "%WINSENT%" == "" call :lbl_notify_send "%TEST_TITLE% are complete"

if exist "%~dp0\.origin.bsc" call :lbl_relocate_report

goto :EOF



rem ============================================= Run testing
:lbl_run_tests
pushd "%~dp0\%1"
if exist reports rename reports reports_%REP_COUNT%
call test_all.bat 1> test_all.log 2>&1
popd

call "%~dp0\report_one.bat" %1 "%TEST_DESC%" 1>> "%~dp0\%1\test_all.log" 2>>&1

goto :EOF


rem ============================================= Run Winsent to receive messages
:lbl_notify_start
if "%POPUP_NOTIFY_LOC%" == "yes"  goto :EOF
start /D "%~dp1" /B %~nx1
goto :EOF


:lbl_notify_send
if "%POPUP_NOTIFY_LOC%" == "yes"  goto :EOF
call "%WINSENT%" /u:%USERNAME% %1 1>nul
goto :EOF



rem ============================================= Setup and Run test in remote folder
:lbl_remote
call "%~dp0\.scripts\get_setup_dir.bat" RATS_TARGET_DIR || goto :lbl_Exit
call "%~dp0\.scripts\setup.bat" "%RATS_TARGET_DIR%" %1  || goto :lbl_Exit

echo ORIGIN_DIR=%~dp0> "%RATS_TARGET_DIR%\.origin.bsc"

start "%RATS_TARGET_DIR%" /D "%RATS_TARGET_DIR%" test_one.bat %1 %3

goto :EOF


rem ============================================= Relocate report
:lbl_relocate_report
set REP_COUNT=
set /p "YesNo=Copy remote report into origin [Y/N]: "
set RelocateRep=no
if "%YesNo%" == "Y"  set RelocateRep=yes
if "%YesNo%" == "y"  set RelocateRep=yes
if "%YesNo%" == "≠"  set RelocateRep=yes
if "%YesNo%" == "ç"  set RelocateRep=yes
if "%RelocateRep%" == "yes" call "%~dp0\.scripts\remote2origin.bat" "%TEST_DESC%"
goto :EOF


:lbl_Exit
goto :EOF
