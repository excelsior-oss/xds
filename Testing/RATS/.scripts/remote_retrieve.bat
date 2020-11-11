@echo off
rem Retrieve remote tests results into origin folder 

set REMOTE_DESC=%~1

set ORIGIN_CONFIG_FILE=%CD%\.origin.bsc


rem ============================================= Configure
if not exist "%ORIGIN_CONFIG_FILE%" (
    echo File "%ORIGIN_CONFIG_FILE%" not found
    echo ******* "%CD%" build configuration ******* Failed!
    exit /B 1
)
for /F "eol=# delims== tokens=1,2" %%i in (%ORIGIN_CONFIG_FILE%) do set %%i=%%j


rem ============================================= Main
for /F "eol=# tokens=1*" %%i IN (%~dp0\..\.testing.bsc) do  if exist %%i call :lbl_relocate %%i
echo -- Ok --
goto :EOF


rem ============================================= Relocate
:lbl_relocate
set ORIGIN_TEST_DIR=%ORIGIN_DIR%\%~1
echo Relocate '%~1' into "%ORIGIN_TEST_DIR%"

pushd "%ORIGIN_TEST_DIR%"
if exist reports call :lbl_save_prev_report
popd 

xcopy "%~1\reports\*"  "%ORIGIN_TEST_DIR%\reports\" /Y /E /Q 
del /Q "%ORIGIN_TEST_DIR%\reports\*.txt"  1>nul 2>nul

pushd "%ORIGIN_DIR%"
call report_one.bat %~1 "%REMOTE_DESC%"
popd 

goto :EOF


rem ============================================= Save old report
:lbl_save_prev_report
if not exist reports  goto :EOF

if "%REP_COUNT%" == "" call "%ORIGIN_DIR%\.scripts\get_rep_count.bat"
rename reports reports_%REP_COUNT%

goto :EOF
