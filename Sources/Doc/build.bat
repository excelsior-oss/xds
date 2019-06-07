@echo off
rem Usage: build [ALL] [Release]
rem   ALL       - build all components
rem   Release   - build in enduser mode

call %~dp0\..\.scripts\build.bat %* || exit /B 1
