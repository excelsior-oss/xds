@echo off
rem Updates external workplace folder with built components by ".update.bsc"
rem Usage: update [Release] [Workplace]
rem   Release   - setup in enduser mode by ".update-release.bsc" if exist
rem   Workplace - target directory

call %~dp0\..\.scripts\update.bat %* || exit /B 1
