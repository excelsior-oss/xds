@echo off
rem Usage: build [ALL] [Release]
rem   ALL       - build all components
rem   Release   - build in enduser mode

rem ============================================= Configure
rem Set ###_SOURCE_DIR, ###_CONFIG_FILE, ###_LOG_DIR, ###_ACTION_FILE and read '.config.bsc'
call "%~dp0\setvars.bat"  BUILD  || exit /B 1

set BUILD_TASKS=%BUILD_PRIMARY_COMPONENTS%
set BUILD_MODE=

call :lbl_parse_argument %*  ||  exit /B 1


rem ============================================= Clean
call clean.bat
if not exist "%BUILD_LOG_DIR%" mkdir "%BUILD_LOG_DIR%"


rem ============================================= Build
set BUILD_PHASE=build
set BUILD_errorlevel=0
set BUILD_ERROR_LOG_FILE=

for %%i in (%BUILD_TASKS%) do call :lbl_build %%i 

set BUILD_TASK=%BUILD_UNIT_NAME%
set BUILD_LOG_FILE=%BUILD_ERROR_LOG_FILE%
if not "%BUILD_errorlevel%" == "0" goto lbl_Error


rem ============================================= Publish
set BUILD_PHASE=update
set BUILD_LOG_FILE=%BUILD_LOG_DIR%\%BUILD_UNIT_NAME%_update.out

call update.bat %BUILD_MODE% 1> "%BUILD_LOG_FILE%" 2>&1  || goto lbl_Error
goto lbl_Success



rem ============================================= Build actions
:lbl_build
set BUILD_TASK=%1
set BUILD_LOG_FILE=%BUILD_LOG_DIR%\%BUILD_UNIT_NAME%_%BUILD_TASK%.out

echo Build %BUILD_UNIT_NAME%: %BUILD_TASK%
call %~dp0\set_errorlevel.bat 0
call mkf.bat %BUILD_TASK% ALL %BUILD_MODE% 1> "%BUILD_LOG_FILE%" 2>&1  || goto lbl_Error
goto :EOF


rem ============================================= Parse Arguments
:lbl_parse_argument
if "%1" == "" goto :EOF

if "%~1" == "ALL" (
    set BUILD_TASKS=%BUILD_ALL_COMPONENTS%
) else if "%~1" == "Release" (
    set BUILD_MODE=Release
) else ( 
    echo Invalid argument: %~nx0 %*
    echo.
    echo Usage: %~nx0 [ALL] [Release]
    echo   ALL       - build all components
    echo   Release   - build in enduser mode
    exit /B 1
)
shift /1
goto :lbl_parse_argument 


rem ============================================= Script Messages
:lbl_Success
echo ======= %BUILD_TASK% ======= OK!
goto :EOF

:lbl_Error
echo ******* %BUILD_TASK% %BUILD_PHASE% ******* Failed! =^> "%BUILD_LOG_FILE%"
set BUILD_errorlevel=1
if "%BUILD_ERROR_LOG_FILE%" == "" set BUILD_ERROR_LOG_FILE=%BUILD_LOG_FILE%
exit /B 1

