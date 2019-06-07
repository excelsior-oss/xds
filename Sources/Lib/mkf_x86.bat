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

goto lbl_%MAKE_TASK%
exit /B 0


rem ============================================= Make Import32 Library
:lbl_import32	
echo.
echo Build import32.lib

set SYS=%WINDIR%\system32
set SYSTEM32_DLLs="%SYS%\ADVAPI32.dll" "%SYS%\KERNEL32.dll" "%SYS%\LZ32.dll"     "%SYS%\NETAPI32.dll"  ^
                  "%SYS%\VDMDBG.dll"   "%SYS%\WSOCK32.dll"  "%SYS%\COMCTL32.dll" "%SYS%\COMDLG32.dll"  ^
                  "%SYS%\CTL3D32.dll"  "%SYS%\GDI32.dll"    "%SYS%\SHELL32.dll"  "%SYS%\USER32.dll"    ^
                  "%SYS%\WINMM.dll"    "%SYS%\IMM32.dll"    "%SYS%\OLEDLG.dll"   "%SYS%\TAPI32.dll"    ^
                  "%SYS%\AVICAP32.dll" "%SYS%\MSACM32.dll"  "%SYS%\MSVFW32.dll"  "%SYS%\WINSPOOL.DRV"

echo xlib /implib /nobak workplace\import32 %SYSTEM32_DLLs%
xlib /implib /nobak workplace\lib\x86\import32 %SYSTEM32_DLLs% || goto lbl_Error
goto lbl_Success


rem ============================================= Make Startup Libraries
:lbl_xstart	
echo.
echo Build startup libraries: xstart.lib, xstartd.lib, xstartc.lib, xstartx.lib

set MAKE_OPTIONS=MODE=work 
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=MODE==enduser

pushd src\os\w95\startup
echo nmake /f startup.mkf %MAKE_OPTIONS%
nmake /f startup.mkf %MAKE_OPTIONS%  || (popd & goto lbl_Error)
popd
goto lbl_Success


rem ============================================= Make Single Thread Library
:lbl_x86_libxds
echo.
echo Build single thread "libxds.lib" 

set MAKE_OPTIONS=-multithread- %LIB_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success


rem ============================================= Make Multi Thread Library
:lbl_x86_libxdsmt
echo.
echo Build multi-thread "libxdsmt.lib" 

set MAKE_OPTIONS=-multithread+ %LIB_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success


rem ============================================= Make Single Thread DLL
:lbl_stdll
echo.
echo Build single thread "xds##.dll" 

set MAKE_OPTIONS=-gendll+ -multithread- %DLL_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success


rem ============================================= Make Multi Thread DLL
:lbl_mtdll
echo.
echo Build multi-thread "xds##m.dll"  

set MAKE_OPTIONS=-gendll+ -multithread+ %DLL_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p lib.prj %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success


rem ============================================= Make VTerm Library
:lbl_vterm
echo.
echo Build vterm.lib  

set MAKE_OPTIONS=%LIB_OPTIONS%
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser

echo "%WORKPLACE_DIR%\bin\xc.exe" =p src\vterm\vterm %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xc.exe" =p src\vterm\vterm %MAKE_OPTIONS% || goto lbl_Error
goto lbl_Success


rem ============================================= Make component
:lbl_stimplib
echo.
echo Build single thread xds##i.lib  

set MAKE_OPTIONS=MODE=work
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=MODE==enduser
set MAKE_OPTIONS=%MAKE_OPTIONS% XC_OPTIONS="%DLL_OPTIONS%"

echo xlib /implib /nobak workplace\lib\x86\xds25i.lib workplace\lib\x86\xds25.dll
xlib /implib /nobak workplace\lib\x86\xds25i.lib workplace\lib\x86\xds25.dll || goto lbl_Error

pushd src\os\w95
echo nmake /f startup.mkf %MAKE_OPTIONS%
nmake /a /f linkinto.mkf %MAKE_OPTIONS%  || (popd & goto lbl_Error)
popd

rem wlib -b -c workplace\lib\x86\xds25i.lib +lib\x86\li.lib || goto lbl_Error
xlib /nobak workplace\lib\x86\xds25i.lib +lib\x86\li.lib || goto lbl_Error
goto lbl_Success


rem ============================================= Make component
:lbl_mtimplib
echo.
echo Build single thread xds##mi.lib  

set MAKE_OPTIONS=MODE=work
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=MODE==enduser
set MAKE_OPTIONS=%MAKE_OPTIONS% XC_OPTIONS="%DLL_OPTIONS%"

echo xlib /implib /nobak workplace\lib\x86\xds25mi.lib workplace\lib\x86\xds25m.dll
xlib /implib /nobak workplace\lib\x86\xds25mi.lib workplace\lib\x86\xds25m.dll || goto lbl_Error

set WAS_STIMPLIB_ERROR=no

pushd src\os\w95
echo nmake /f startup.mkf %MAKE_OPTIONS%
nmake /a /f linkinto.mkf %MAKE_OPTIONS%  || (popd & goto lbl_Error)
popd

rem wlib -b -c workplace\lib\x86\xds25mi.lib +lib\x86\li.lib || goto lbl_Error
xlib /nobak workplace\lib\x86\xds25mi.lib +lib\x86\li.lib || goto lbl_Error
goto lbl_Success


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
