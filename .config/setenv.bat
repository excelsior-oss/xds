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

if not exist "%XDSDIR%"           call :lbl_Error XDSDIR            "%XDSDIR%"          
if not exist "%MSVC_HOME%"        call :lbl_Error MSVC_HOME         "%MSVC_HOME%"       
if not exist "%MSBUILD_HOME%"     call :lbl_Error MSBUILD_HOME      "%MSBUILD_HOME%"
if not exist "%NASM_HOME%"        call :lbl_Error NASM_HOME         "%NASM_HOME%"
rem if not exist "%MINGW%"            call :lbl_Error MINGW             "%MINGW%"           
rem if not exist "%LLVM_HOME%"        call :lbl_Error LLVM_HOME         "%LLVM_HOME%"       
rem NOTE: docs generation disabled 
rem if not exist "%MS_HELP_WORKSHOP%" call :lbl_Error MS_HELP_WORKSHOP  "%MS_HELP_WORKSHOP%"
rem if not exist "%MIKTEX%"           call :lbl_Error MIKTEX            "%MIKTEX%"          


rem === Setup 'PATH' to Microsoft Build Engine
if exist "%MSBUILD_HOME%"  set PATH=%MSBUILD_HOME%\Bin;%PATH%

rem === Setup 'PATH' to Microsoft Visual C++
set PATH=%MSVC_HOME%\bin;%PATH%
if exist "%MSVC_HOME%\vcvarsall.bat"                  call "%MSVC_HOME%\vcvarsall.bat"
if exist "%MSVC_HOME%\Auxiliary\Build\vcvarsall.bat"  call "%MSVC_HOME%\Auxiliary\Build\vcvarsall.bat" x86

rem === Setup 'PATH' to the Netwide Assembler 
set PATH=%NASM_HOME%;%PATH%

rem === Setup 'PATH' to MinGW  
if exist "%MINGW%"  set PATH=%MINGW%\bin;%PATH%

rem === Setup 'PATH' to LLVM Compiler Infrastructure 
if exist "%LLVM_HOME%"  set PATH=%LLVM_HOME%\bin;%PATH%

rem === Setup 'PATH' to Instrumental Native XDS-x86 
set PATH=%XDSDIR%\bin;%PATH%


rem === Setup 'PATH' to Microsoft Help Workshop  
if exist "%MS_HELP_WORKSHOP%"  set PATH=%MS_HELP_WORKSHOP%;%PATH%

rem === Setup 'PATH' to MikTeX  
if exist "%MIKTEX%"  set PATH=%MIKTEX%\bin;%PATH%
if exist "%MIKTEX%\bin\pdflatex.exe"      set PATH=%MIKTEX%\bin;%PATH%
if exist "%MIKTEX%\bin\x64\pdflatex.exe"  set PATH=%MIKTEX%\bin\x64;%PATH%

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



