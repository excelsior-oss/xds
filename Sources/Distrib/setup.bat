@echo off
rem Setups external workplace folder by ".setup.bsc" 
rem Usage: setup [Release]
rem   Release   - setup in enduser mode 
rem   Workplace - target directory

call %~dp0\..\.scripts\setup.bat %* || exit /B 1
