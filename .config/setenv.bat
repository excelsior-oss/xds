@echo off
rem Setup environment to build XDS product

set XDS_PRODUCT=XDS-x86
set XDS_ENV_CONFIG_FILE=%~dp0\.env-%COMPUTERNAME%.bsc

if not exist %XDS_ENV_CONFIG_FILE% (
    echo File "%XDS_ENV_CONFIG_FILE%" does not exist
    echo *********** %XDS_PRODUCT% Environment *********** Failed!
    exit /B 1
)

set SILENT_MODE=no
if "%1" == "SILENT" set SILENT_MODE=yes

rem ============================================= Read Environment
for /F "eol=# delims== tokens=1,2" %%i in (%XDS_ENV_CONFIG_FILE%) do call :lbl_set_envvar %%i "%%j"

if not exist "%NASM_HOME%"        call :lbl_Error NASM_HOME         "%NASM_HOME%"
if not exist "%MSBUILD_HOME%"     call :lbl_Error MSBUILD_HOME      "%MSBUILD_HOME%" 
if not exist "%MSVC_HOME%"        call :lbl_Error MSVC_HOME         "%MSVC_HOME%"       
if not exist "%XDSDIR%"           call :lbl_Error XDSDIR            "%XDSDIR%"          
if not exist "%MINGW%"            call :lbl_Error MINGW             "%MINGW%"           
if not exist "%MSYS%"             call :lbl_Error MSYS              "%MSYS%"            
rem NOTE: docs generation disabled 
rem if not exist "%MS_HELP_WORKSHOP%" call :lbl_Error MS_HELP_WORKSHOP  "%MS_HELP_WORKSHOP%"
rem if not exist "%MIKTEX%"           call :lbl_Error MIKTEX            "%MIKTEX%"          


rem === Setup 'PATH' to Microsoft Build Engine
set PATH=%MSBUILD_HOME%\Bin;%PATH%

rem === Setup 'PATH' to Microsoft Visual C++
set PATH=%MSVC_HOME%\bin;%PATH%
call "%MSVC_HOME%\vcvarsall.bat"

rem === Setup 'PATH' to Instrumental Native XDS-x86 
set PATH=%XDSDIR%\bin;%PATH%

rem === Setup 'PATH' to the Netwide Assembler 
set PATH=%NASM_HOME%;%PATH%

rem === Setup 'PATH' to MinGW  
set PATH=%MINGW%\bin;%PATH%

rem === Setup 'PATH' to MSYS
set PATH=%PATH%;%MSYS%\bin

rem === Setup 'PATH' to Microsoft Help Workshop  
rem NOTE: docs generation disabled 
rem set PATH=%MS_HELP_WORKSHOP%;%PATH%

rem === Setup 'PATH' to MikTeX  
rem NOTE: docs generation disabled 
rem set PATH=%MIKTEX%\bin;%PATH%

goto :EOF


rem ============================================= Set Environment
:lbl_set_envvar
set %1=%~2
goto :EOF


rem ============================================= Script Messages
:lbl_Error
if "%SILENT_MODE%" == "yes"  goto :EOF
echo *** Environment Error: variable "%1" is undefined or
echo *** invalid "%~2".
echo *** Please define it in the "%XDS_ENV_CONFIG_FILE%" 
exit /B 1



