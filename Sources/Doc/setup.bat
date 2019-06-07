@echo off
rem Setups external workplace folder by ".setup.bsc" 
rem Usage: setup [Release] [Bootstrap] [Workplace]
rem   Release   - setup in enduser mode by ".setup-release.bsc" if exist
rem   Bootstrap - setup in themselves build mode
rem   Workplace - target directory

call %~dp0\..\.scripts\setup.bat %* || exit /B 1
