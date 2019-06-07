@echo off
rem Setup environment to run RATS-tests

set XDS_ENV_CONFIG_FILE=%~dp0\.env-%COMPUTERNAME%.bsc

if not exist %XDS_ENV_CONFIG_FILE% (
    echo File "%XDS_ENV_CONFIG_FILE%" does not exist
    echo *********** XDS-x86 Environment *********** Failed!
    exit /B 1
)

set IS_XDS_X86_RATS_ENV_DEFINED=no
call :lbl_check_env
if "%IS_XDS_X86_RATS_ENV_DEFINED%" ==  "yes" goto :EOF


rem ============================================= Set Environment
for /F "eol=# delims== tokens=1,2" %%i in (%XDS_ENV_CONFIG_FILE%) do call :lbl_set_envvar %%i "%%j"

if not exist "%MSVC_HOME%"        call :lbl_Error MSVC_HOME       "%MSVC_HOME%"     
rem if not exist "%LLVM_HOME%"        call :lbl_Error LLVM_HOME       "%LLVM_HOME%"     
if not exist "%MINGW%"            call :lbl_Error MINGW           "%MINGW%"         
if not exist "%MINGW_MYSYSDIR%"   call :lbl_Error MINGW_MYSYSDIR  "%MINGW_MYSYSDIR%"

rem === Setup 'PATH' to MinGW MySYS    
set PATH=%MINGW_MYSYSDIR%\bin;%PATH%

rem === Setup 'PATH' to Microsoft Visual C++
set PATH=%MSVC_HOME%\bin;%PATH%
call "%MSVC_HOME%\vcvarsall.bat"

rem === Setup 'PATH' to MinGW  
set PATH=%MINGW%\bin;%PATH%

rem === Setup 'PATH' to LLVM Compiler Infrastructure 
set PATH=%LLVM_HOME%\bin;%PATH%

set XDSDIR=%~dp0..\XDS
set PATH=%XDSDIR%\bin;%PATH%


set XDS_X86_RATS_ENV=%~dp0\setenv.bat
goto :EOF



rem ============================================= Check if the environment was already setuped
:lbl_check_env
if not defined MINGW_MYSYSDIR goto :EOF

if "%XDS_X86_RATS_ENV%" == "%~dp0\setenv.bat"  set IS_XDS_X86_RATS_ENV_DEFINED=yes

goto :EOF



rem ============================================= Set Environment Varibale
:lbl_set_envvar
set %1=%~2
goto :EOF



rem ============================================= Script Messages
:lbl_Error
echo *** Environment Error: variable "%1" is undefined or
echo *** invalid "%~2".
echo *** Please define it in the "%XDS_ENV_CONFIG_FILE%" 
exit /B 1
