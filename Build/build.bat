@echo off
rem Usage: build [ALL] [Release]
rem   ALL       - build all components
rem   Release   - build in enduser mode

set XDS_PRODUCT=XDS-x86
set SOURCE_DIR=%~dp0\..\Sources
set XDS_BUILD_ARGS=%*

if "%XDS_GLOBAL_LOG_DIR%" == "" set XDS_GLOBAL_LOG_DIR=%~dp0\log

if "%WORKPLACE_DIR%" == "" set WORKPLACE_DIR=%~dp0\%XDS_PRODUCT%
if not exist "%WORKPLACE_DIR%"  (
    call setup.bat %XDS_BUILD_ARGS% || exit /B 1
    echo.
)

echo Build %XDS_PRODUCT% in "%WORKPLACE_DIR%" 
echo.

set XDS_BUILD_errorlevel=0
for /F "eol=# tokens=1*" %%i IN (%~dp0\.components.bsc) do  call :lbl_build %%i 
if not "%XDS_BUILD_errorlevel%" == "0" goto lbl_Error
goto lbl_Success


rem ============================================= Build component
:lbl_build 
if not exist "%SOURCE_DIR%\%1\build.bat"  goto :EOF
pushd "%SOURCE_DIR%\%1"
call build.bat %XDS_BUILD_ARGS% || set XDS_BUILD_errorlevel=1
popd
echo.
goto :EOF 


rem ============================================= Script Messages
:lbl_Success
echo %XDS_PRODUCT% workplace =^> "%WORKPLACE_DIR%"
echo ======= Build %XDS_PRODUCT% ======= OK!
goto :EOF

:lbl_Error
echo ******* Build %XDS_PRODUCT% ******* Failed!
exit /B 1

