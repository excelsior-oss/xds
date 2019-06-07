@echo off
rem Set basic environment variables for script: 
rem   <Prefix>_SOURCE_DIR  - path to component root directory
rem   <Prefix>_CONFIG_FILE - path to '.config.bsc'
rem   <Prefix>_LOG_DIR     - path to component log directory
rem   <Prefix>_ACTION_FILE - path to control bsc-file
rem   variables defined in the '.config.bsc'
rem 
rem Usage: setvars.bat <prefix> [<file> [<source_dir>]]
rem   prefix     - unique prefix common for created variables
rem   File       - task specific bsc-file 
rem   source_dir - path to component root directory (by default current directory)

rem ============================================= Parse Argument
set SETVARS_PREFIX=%~1
set SETVARS_BSC_FILE=%~2

set SETVARS_SOURCE_DIR=%~3
if "%SETVARS_SOURCE_DIR%" == "" set SETVARS_SOURCE_DIR=%CD%

if "%SETVARS_PREFIX%" == "" goto lbl_Error


rem ============================================= Set Environment Variables
set SETVARS_CONFIG_FILE=%SETVARS_SOURCE_DIR%\.config.bsc
set SETVARS_LOG_DIR=%SETVARS_SOURCE_DIR%\log
set SETVARS_ACTION_FILE=%SETVARS_SOURCE_DIR%\%SETVARS_BSC_FILE%

set %SETVARS_PREFIX%_SOURCE_DIR=%SETVARS_SOURCE_DIR%
set %SETVARS_PREFIX%_CONFIG_FILE=%SETVARS_CONFIG_FILE%

set %SETVARS_PREFIX%_LOG_DIR=%SETVARS_LOG_DIR%
if not "%XDS_GLOBAL_LOG_DIR%" == "" set %SETVARS_PREFIX%_LOG_DIR=%XDS_GLOBAL_LOG_DIR%

set %SETVARS_PREFIX%_ACTION_FILE=
if not "%SETVARS_BSC_FILE%" == "" set %SETVARS_PREFIX%_ACTION_FILE=%SETVARS_ACTION_FILE%


rem ============================================= Read Config
call "%~dp0\read_config.bat"  "%SETVARS_CONFIG_FILE%"  %SETVARS_PREFIX% || exit /B 1 

goto :EOF


rem ============================================= Script Messages
:lbl_Error
echo **** Usage: setvars.bat ^<prefix^> [^<file^> [^<source_dir^>]] *** Failed!
exit /B 1

