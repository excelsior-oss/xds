@echo off
rem Create reports on specified tests
rem
rem Usage: report_one Test [Title]
rem   Test   - folder to make tests report
rem   Title  - optional description of the testing reason 

rem --- Generate reports for "rep" files -------------------------------

for /D %%i in (%~dp0\templates\*) do  call "%~dp0\.scripts\report.bat" "%1\reports\%%~ni"

call "%~dp0\.scripts\summary.bat" "%~dp0\%1\reports"  %2

call "%~dp0\.scripts\list_failed_tests.bat" "%1\reports"

rem --- Generate reports for "mtx" files -------------------------------

set METRIX_HOME=%~dp0\bin\ctr_utils\metrix
set REPORT_HOME=%~f1\reports

for %%i in ("%REPORT_HOME%\*.mtx") do if exist %%i del %%i

for /d %%i in ("%REPORT_HOME%\*") do call :process_mtx_dir %%i %REPORT_HOME%\mtx_%%~ni.mtx
for %%i in ("%REPORT_HOME%\*.mtx") do call "%METRIX_HOME%\bin\mtx_sum.exe" %%i %%~ni >> %REPORT_HOME%\mtx_summary.tmp

rename %REPORT_HOME%\mtx_summary.tmp mtx_summary.mtx
rename %REPORT_HOME%\mtx_all.tmp mtx_all.mtx
goto :EOF

rem --- Agregate "mtx" files in dir ------------------------------------

:process_mtx_dir
if exist %2 del %2
for %%i in ("%1\*.mtx") do call "%METRIX_HOME%\bin\mtx_sum.exe" %%i %~n1\%%~ni >> %2
for %%i in ("%1\*.mtx") do call "%METRIX_HOME%\bin\mtx_cat.exe" %%i %~n1\%%~ni\ >> %REPORT_HOME%\mtx_all.tmp
goto :EOF

