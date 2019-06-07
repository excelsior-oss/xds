@echo off
rem Copies external dependences
rem Usage: copy_extdeps <DependencesList> <TargetDir>
rem   DependencesList - comma separated list of dependences
rem   TargetDir       - target folder to copy dependences

set EXTDEP_ERROR=no
set EXTDEP_TARGET_DIR=%~2

for %%i in (%~1) do call :lbl_process_extdep %%i 

if "%EXTDEP_ERROR%" == "yes" exit /B 1
goto :EOF


:lbl_process_extdep
set EXTDEP_SOURCE=%~1
set EXTDEP_FILE=%~nx1

if exist "%EXTDEP_TARGET_DIR%\%EXTDEP_FILE%"  goto :EOF 

echo copy "%EXTDEP_SOURCE%" "%EXTDEP_TARGET_DIR%\"
copy "%EXTDEP_SOURCE%" "%EXTDEP_TARGET_DIR%\" || set EXTDEP_ERROR=yes
goto :EOF


