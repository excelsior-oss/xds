@echo off
rem Cleaup source folders

set XDS_PRODUCT=XDS-x86
set SOURCE_DIR=%~dp0\..\Sources

if "%XDS_GLOBAL_LOG_DIR%" == "" set XDS_GLOBAL_LOG_DIR=%~dp0\log

if not "%1" == ""          set WORKPLACE_DIR=%1
if "%WORKPLACE_DIR%" == "" set WORKPLACE_DIR=%~dp0\%XDS_PRODUCT%

echo Clean %XDS_PRODUCT% source folders 

for /F "eol=# tokens=1*" %%i IN (%~dp0\.components.bsc) do  call :lbl_clean %%i 

for %%i in (log,%XDS_PRODUCT%) do if exist %%i rmdir /Q /S %%i

goto :EOF 


rem ============================================= Clean component
:lbl_clean 
if not exist "%SOURCE_DIR%\%1\clean.bat"  goto :EOF
echo Clean %1
pushd "%SOURCE_DIR%\%1"
call clean.bat
popd
goto :EOF 
