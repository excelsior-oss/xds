@echo off
rem Usage: mkf [component] [ALL] [Release]
rem   component - component to make
rem   ALL       - rebuild mode
rem   Release   - compile in enduser mode
 
rem ============================================= Configure
set MAKE_SCRIPT_DIR=%~dp0\..\.scripts
set MAKE_CONFIG_FILE=%~dp0\.config.bsc

call "%MAKE_SCRIPT_DIR%\read_config.bat"  "%MAKE_CONFIG_FILE%"  MAKE  || exit /B 1 

set MAKE_TASK=%MAKE_DEFAULT_COMPONENT%
set MAKE_MODE_ALL=no
set MAKE_MODE_RELEASE=no

call :lbl_parse_argument %* || exit /B 1

call "%MAKE_SCRIPT_DIR%\setenv.bat" %MAKE_ENVIRONMENT%  || exit /B 1

if "%WORKPLACE_DIR%" == "" call "%MAKE_SCRIPT_DIR%\setenv_workplace.bat"

for %%i in (workplace,workplace\def\ob2) do if not exist %%i mkdir %%i 
for %%i in (workplace\lib\x86)           do if not exist %%i mkdir %%i 
for %%i in (ch\x86,sym\x86,lib\x86)      do if not exist %%i mkdir %%i 

set LIB_OPTIONS=-dbgfmt=HLL -decor=rt -woff+
set DLL_OPTIONS=-dbgfmt=HLL -decor=rt -woff+

for %%i in (obj,o) do if exist ch\x86\*.%%i del /Q ch\x86\*.%%i

if not exist src\TSlibs\IO_.* copy src\TSlibs\IO.*  src\TSlibs\IO_.*

goto lbl_%MAKE_TASK%
exit /B 0


rem ============================================= Make Single Thread TopSpeed Library
:lbl_x86_libts
echo.
echo Build single thread "libts.lib" 

set MAKE_OPTIONS=-multithread- %LIB_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p tslib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p tslib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success


rem ============================================= Make Multi Thread TopSpeed Library
:lbl_x86_libtsmt
echo.
echo Build multi-thread "libtsmt.lib" 

set MAKE_OPTIONS=-multithread+ %LIB_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p tslib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p tslib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success


rem ============================================= Make Single Thread ToopSpeed DLL
:lbl_tsstdll
echo.
echo Build single thread "xts25.dll" 

set MAKE_OPTIONS=-gendll+ -multithread- %DLL_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p tslib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p =a tslib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success

rem ============================================= Make Single Thread ToopSpeed Import Library
:lbl_tsstimplib
echo.
echo Build single thread xts25i.lib  

set MAKE_OPTIONS=MODE=work
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=MODE==enduser
set MAKE_OPTIONS=%MAKE_OPTIONS% XC_OPTIONS="%DLL_OPTIONS%"

echo xlib /implib /nobak workplace\lib\x86\xts25i.lib workplace\lib\x86\xts25.dll
xlib /implib /nobak workplace\lib\x86\xts25i.lib workplace\lib\x86\xts25.dll || goto lbl_Error
goto lbl_Success


rem ============================================= Make Multi Thread ToopSpeed DLL
:lbl_tsmtdll
echo.
echo Build multi thread "xts25m.dll" 

set MAKE_OPTIONS=-gendll+ -multithread+ %DLL_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p tslib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p =a tslib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success

rem ============================================= Make Multi Thread ToopSpeed Import Library
:lbl_tsmtimplib
echo.
echo Build multi thread xts25mi.lib  

set MAKE_OPTIONS=MODE=work
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=MODE==enduser
set MAKE_OPTIONS=%MAKE_OPTIONS% XC_OPTIONS="%DLL_OPTIONS%"

echo xlib /implib /nobak workplace\lib\x86\xts25mi.lib workplace\lib\x86\xts25m.dll
xlib /implib /nobak workplace\lib\x86\xts25mi.lib workplace\lib\x86\xts25m.dll || goto lbl_Error


rem ============================================= Parse Arguments
:lbl_parse_argument
if "%1" == "" goto :EOF

set IS_COMPONENT=no
for %%i in (%MAKE_ALL_COMPONENTS%) do if "%~1" == "%%i" set IS_COMPONENT=yes

if "%IS_COMPONENT%" == "yes" (
    set MAKE_TASK=%1
) else if "%~1" == "ALL" (
    set MAKE_MODE_ALL=yes
) else if "%~1" == "Release" (
    set MAKE_MODE_RELEASE=yes
) else ( 
    echo Incorrect arguments: %~nx0 %*
    echo.
    echo Usage: %~nx0 [component] [ALL] [Release]
    echo   component - component to make: %MAKE_ALL_COMPONENTS%
    echo   ALL       - rebuild mode
    echo   Release   - compile in enduser mode
    exit /B 1
)
shift /1
goto :lbl_parse_argument 


rem ============================================= Script Messages
:lbl_Success
call "%MAKE_SCRIPT_DIR%\copy_extdeps.bat" "%MAKE_EXTDEPS%" "%~dp0\workplace"  || goto lbl_Error
echo ============ %MAKE_TASK% ============ OK!
exit /B 0
goto :EOF


:lbl_Error
echo *********** %MAKE_TASK% *********** Failed!
exit /B 1
