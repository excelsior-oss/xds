@echo off
rem Build XDS product from a Git repository
rem Usage: build_git.bat local|remote [branch]
rem   local  - clone local Git repository
rem   remote - clone remote Git repository
rem   brach  - branch name to checkout (by default "work")

set XDS_PRODUCT=XDS-x86
set BUID_EXT_LOG_DIR=%~dp0\log
set BUID_EXT_LOG_FILE=%BUID_EXT_LOG_DIR%\build_git.log

call %~dp0\..\.config\config.bat XDS_BUILD_GIT_DIR || goto lbl_Error

if "%1" == "local" (
    set GIT_REPOSITORY=%~dp0\..
) else if "%1" == "remote" (
    set GIT_REPOSITORY=https://github.com/excelsior-oss/xds.git
) else (
    goto :lbl_Error_Invalid_argument
)

set GIT_BRANCH=work
if not "%2" == "" set GIT_BRANCH=%2

echo Setup %XDS_PRODUCT% Build Environment in "%XDS_BUILD_GIT_DIR% 
echo    Repository: %GIT_REPOSITORY%
echo    Branch:     %GIT_BRANCH%

if not exist "%BUID_EXT_LOG_DIR%" mkdir  "%BUID_EXT_LOG_DIR%"  || goto lbl_Error
if exist "%BUID_EXT_LOG_FILE%"    del /Q "%BUID_EXT_LOG_FILE%" || goto lbl_Error

if exist "%XDS_BUILD_GIT_DIR%" rmdir /Q /S "%XDS_BUILD_GIT_DIR%"
mkdir "%XDS_BUILD_GIT_DIR%"

call git clone "%GIT_REPOSITORY%" "%XDS_BUILD_GIT_DIR%" 1> "%BUID_EXT_LOG_FILE%" 2>&1
if errorlevel 1 goto lbl_Error

rem Copy local configuration
xcopy "%~dp0\..\.config\*.env-*.bsc" "%XDS_BUILD_GIT_DIR%\.config\" /Y

pushd "%XDS_BUILD_GIT_DIR%\Build"

call git checkout %GIT_BRANCH% 1>> "%BUID_EXT_LOG_FILE%" 2>>&1
if errorlevel 1 goto lbl_Error

echo.
call build.bat Release

popd
goto :EOF


:lbl_Error_Invalid_argument
echo Invalid argument: %~nx0 %*
echo.
echo Usage: %~nx0 local^|remote [branch]
echo   local  - clone local Git repository
echo   remote - clone remote Git repository
echo   brach  - branch name to checkout (by default "work")
exit /B 1


:lbl_Error
echo ******* Build %XDS_PRODUCT% ******* Failed! =^> "%BUID_EXT_LOG_FILE%"
exit /B 1
