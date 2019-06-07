@echo off

rem ============================================= Configure
if not exist %~dp0\.config.bsc (
    echo File "%~dp0\.config.bsc" not found
    echo ******* %~dp0 clean configuration ******* Failed!
    exit /B 1
)
for /F "eol=# delims== tokens=1,2" %%i in (%~dp0\.config.bsc) do set CLEAN_%%i=%%j


rem ============================================= Clean
for %%i in (log,workplace\html) do if exist %%i rmdir /Q /S %%i

for %%i in (hlp,pdf,cnt,chm)  do if exist workplace\*.%%i  del /F /Q workplace\*.%%i 


for %%i in (%CLEAN_ALL_COMPONENTS%) do  call :lbl_clean %%i 

goto :EOF 


rem ============================================= Clean component
:lbl_clean 
pushd "%~dp0\%1"
call clean.bat
popd
goto :EOF 
