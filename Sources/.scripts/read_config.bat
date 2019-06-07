@echo off
rem Read configure file and set environment variables according to its content
rem 
rem Usage: read_config <file> <prefix>
rem   file   - configuraton file in bsc-format
rem   prefix - unique prefix for created environment variables

set CONFIG_FILE=%~1
set CONFIG_PREFIX=%~2

rem ============================================= Read File
if not exist "%CONFIG_FILE%" (
    echo File "%CONFIG_FILE%" not found
    echo ******* "%CD%" configuration ******* Failed!
    exit /B 1
)

for /F "eol=# delims== tokens=1,2" %%i in (%CONFIG_FILE%) do call :lbl_setenv "%CONFIG_PREFIX%" "%%i" "%%j"
goto :EOF
 

rem ============================================= Set Environment
:lbl_setenv
set %~2=%~3
set %~1_%~2=%~3
goto :EOF
