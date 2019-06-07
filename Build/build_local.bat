@echo off
rem Build XDS product from the working-copy

set XDS_PRODUCT=XDS-x86
set BUID_EXT_LOG_DIR=%~dp0\log
set BUID_EXT_LOG_FILE=%BUID_EXT_LOG_DIR%\build_local.log

call %~dp0\..\.config\config.bat XDS_BUILD_LOCAL_DIR || goto lbl_Error

echo Setup %XDS_PRODUCT% Build Environment in "%XDS_BUILD_LOCAL_DIR% 

if not exist "%BUID_EXT_LOG_DIR%" mkdir  "%BUID_EXT_LOG_DIR%"  || goto lbl_Error
if exist "%BUID_EXT_LOG_FILE%"    del /Q "%BUID_EXT_LOG_FILE%" || goto lbl_Error

if exist "%XDS_BUILD_LOCAL_DIR%" rmdir /Q /S "%XDS_BUILD_LOCAL_DIR%"
mkdir "%XDS_BUILD_LOCAL_DIR%"

for %%i in (.config,Build,Sources) do xcopy "%~dp0\..\%%i" "%XDS_BUILD_LOCAL_DIR%\%%i\" /Y /E 1>> "%BUID_EXT_LOG_FILE%" 2>>&1
if errorlevel 1 goto lbl_Error

copy /Y "%~dp0\..\xdswork.bat" "%XDS_BUILD_LOCAL_DIR%\"  1>> "%BUID_EXT_LOG_FILE%" 2>>&1

echo.

pushd "%XDS_BUILD_LOCAL_DIR%\Build"
call build.bat Release
popd

goto :EOF

:lbl_Error
echo ******* Build %XDS_PRODUCT% ******* Failed! =^> "%BUID_EXT_LOG_FILE%"
exit /B 1
