@echo off
rem Setup remote folder to tests
rem Usage: remote_setup Target [Test]
rem   Target - remote folder to setup
rem   Test   - test folder to setup: 'xmwe_msvc', 'xmwe_mingw' etc

set RATS_SETUP_DIR=%~1
if "%RATS_SETUP_DIR%" == "" call "%~dp0\get_setup_dir.bat" RATS_SETUP_DIR 

echo Setup RATS in "%RATS_SETUP_DIR%"

if exist "%RATS_SETUP_DIR%" call :lbl_Error : "%RATS_SETUP_DIR%" already exists  
mkdir "%RATS_SETUP_DIR%"

rem ============================================= Setup tests' folders
set TEST_FOLDER=%~2
if not "%TEST_FOLDER%" == ""  call :lbl_Copy_test_folder "%TEST_FOLDER%"
if     "%TEST_FOLDER%" == ""  for /F "eol=# tokens=1*" %%i IN (%~dp0\..\.testing.bsc) do  call :lbl_Copy_test_folder %%i

rem ============================================= Setup test environment
for %%i in (.scripts,bin,lib,templates) do xcopy "%~dp0\..\%%i"  "%RATS_SETUP_DIR%\%%i\" /Q /E 
for %%i in (bsc,bat) do xcopy "%~dp0\..\*.%%i"  "%RATS_SETUP_DIR%\" /Q 

if not "%TEST_FOLDER%" == "" echo %TEST_FOLDER% > "%RATS_SETUP_DIR%\.testing.bsc"

if     exist "%~dp0\..\XDS" xcopy "%~dp0\..\XDS\*"       "%RATS_SETUP_DIR%\XDS\" /Y /E /Q
if not exist "%~dp0\..\XDS" xcopy "%~dp0\..\..\..\XDS\*" "%RATS_SETUP_DIR%\XDS\" /Y /E /Q

echo --- OK ---
goto :EOF


rem ============================================= Copy test folder
:lbl_Copy_test_folder
set SETUP_SRC_DIR=%~dp0\..\%~1
if not exist "%SETUP_SRC_DIR%" call :lbl_Error : Folder "%SETUP_SRC_DIR%" not found

mkdir "%RATS_SETUP_DIR%\%1"
xcopy "%SETUP_SRC_DIR%\*.bsc"  "%RATS_SETUP_DIR%\%1\" /Q 
xcopy "%SETUP_SRC_DIR%\*.bat"  "%RATS_SETUP_DIR%\%1\" /Q 
xcopy "%SETUP_SRC_DIR%\*.cfg"  "%RATS_SETUP_DIR%\%1\" /Q 
xcopy "%SETUP_SRC_DIR%\*.pkt"  "%RATS_SETUP_DIR%\%1\" /Q 
xcopy "%SETUP_SRC_DIR%\*.red"  "%RATS_SETUP_DIR%\%1\" /Q 
goto :EOF


rem ============================================= Error handling
:lbl_Error
echo ******* Setup RATS%* ******* Failed!
goto lbl_Exit
goto :EOF


:lbl_Exit
exit \B 1
