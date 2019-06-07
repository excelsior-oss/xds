@echo off
rem Usage: build 

rem ============================================= Configure
set BUILDSUB_UNIT_NAME=XD-Documentation
set BUILDSUB_LOG_DIR=%~dp0\log
if not "%XDS_GLOBAL_LOG_DIR%" == "" set BUILDSUB_LOG_DIR=%XDS_GLOBAL_LOG_DIR%

call clean.bat
if not exist "%BUILDSUB_LOG_DIR%"  mkdir "%BUILDSUB_LOG_DIR%"

echo Build %BUILDSUB_UNIT_NAME%: xd_html
set BUILDSUB_LOG_FILE=%BUILDSUB_LOG_DIR%\%BUILDSUB_UNIT_NAME%-xd_html.out
call mkf.bat xd html  1> "%BUILDSUB_LOG_FILE%" 2>&1 || goto lbl_Error

echo Build %BUILDSUB_UNIT_NAME%: xd_pdf
set BUILDSUB_LOG_FILE=%BUILDSUB_LOG_DIR%\%BUILDSUB_UNIT_NAME%-xd_pdf.out
call mkf.bat xd pdf  1> "%BUILDSUB_LOG_FILE%" 2>&1 || goto lbl_Error

echo Build %BUILDSUB_UNIT_NAME%: xd_chm
set BUILDSUB_LOG_FILE=%BUILDSUB_LOG_DIR%\%BUILDSUB_UNIT_NAME%-xd_chm.out
call mkf.bat xd chm  1> "%BUILDSUB_LOG_FILE%" 2>&1 || goto lbl_Error

goto lbl_Success

rem ============================================= Script Messages
:lbl_Success
echo ======= %BUILDSUB_UNIT_NAME% ======= OK!
goto :EOF

:lbl_Error
echo ******* %BUILDSUB_UNIT_NAME% ******* Failed! =^> "%BUILDSUB_LOG_FILE%"
set BUILD_errorlevel=1
if "%BUILD_ERROR_LOG_FILE%" == "" set BUILD_ERROR_LOG_FILE=%BUILD_LOG_FILE%
exit /B 1



 