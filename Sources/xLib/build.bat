@echo off
rem Usage: build [ALL] [Release]
rem   ALL       - build all components
rem   Release   - build in enduser mode

set BUILD_CONFIG_FILE=%~dp0\.config.bsc
set BUID_LOG_DIR=%~dp0\log

call %~dp0\..\.scripts\build.bat %* || exit /B 1
