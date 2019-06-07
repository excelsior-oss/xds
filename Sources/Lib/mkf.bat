@echo off
rem Usage: mkf [component] [ALL] [Release]
rem   component - component to compile
rem   ALL       - rebuild mode
rem   Release   - compile in enduser mode
 
rem ============================================= Configure
set MAKE_SCRIPT_DIR=%~dp0\..\.scripts
set MAKE_CONFIG_FILE=%~dp0\.config.bsc

set MAKE_INCOMMING_ARGS=%*
 
call "%MAKE_SCRIPT_DIR%\read_config.bat"  "%MAKE_CONFIG_FILE%"  MAKE  || exit /B 1 

set MAKE_TASK=%MAKE_DEFAULT_COMPONENT%
set MAKE_MODE_ALL=no
set MAKE_MODE_RELEASE=no

call :lbl_parse_argument %* || exit /B 1

goto lbl_%MAKE_TASK% || goto lbl_Error
exit /B 0


rem ============================================= Make component
:lbl_import32
:lbl_xstart
:lbl_x86_libxds
:lbl_x86_libxdsmt
:lbl_stdll
:lbl_mtdll
:lbl_vterm
:lbl_stimplib
:lbl_mtimplib

call mkf_x86.bat %MAKE_INCOMMING_ARGS%  || goto lbl_Error
goto lbl_Success

rem ============================================= Make component
:lbl_x86_libts
:lbl_x86_libtsmt
:lbl_tsstdll
:lbl_tsstimplib
:lbl_tsmtdll
:lbl_tsmtimplib

call mkf_x86_ts.bat %MAKE_INCOMMING_ARGS%  || goto lbl_Error
goto lbl_Success

rem ============================================= Make component
:lbl_msvc_lib
:lbl_msvc_mkf
:lbl_unix_mkf
:lbl_watcom_mkf

call mkf_c.bat %MAKE_INCOMMING_ARGS%  || goto lbl_Error
goto lbl_Success


rem ============================================= Make component
:lbl_msvcts_lib
:lbl_msvcts_mkf
:lbl_watcomts_mkf
:lbl_unixts_mkf

call mkf_c_ts.bat %MAKE_INCOMMING_ARGS%  || goto lbl_Error
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
exit /B 0
goto :EOF


:lbl_Error
exit /B 1
