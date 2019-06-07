@echo off

set REP_DIR=reports
set TEMPL_DIR=..\templates

rem The following error on langlims.tg occurs in case of absolute path to template folder:
rem Internal error: ASSERTion failed, file src\ctInterpret.c(compiled Oct  6 2014 at 11:15:10) line # 394
rem set TEMPL_DIR=%~dp0..\templates

if exist "%REP_DIR%" call :lbl_SavePreviousReports

call :lbl_PreocessTemplates %TEMPL_DIR% %REP_DIR%
goto :EOF

rem ------------------ Build tests from templates and run them
:lbl_PreocessTemplates
for /d %%i in ("%1\*")    do call :lbl_PreocessTemplates %%i %REP_DIR%\%%~ni
for    %%i in ("%1\*.tg") do if exist %%i  call "%~dp0\test_one.bat" "%%i" %2
goto :EOF

rem ------------------ Save report files generated in previous run
:lbl_SavePreviousReports
if "%REP_COUNT%" == "" call "%~dp0\.scripts\get_rep_count.bat"
rename "%REP_DIR%" "%REP_DIR%_%REP_COUNT%"
goto :EOF
