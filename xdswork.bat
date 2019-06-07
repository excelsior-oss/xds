@echo off
rem Usage: xdswork [Action] [Release]

call "%~dp0\Sources\.scripts\setenv_workplace.bat"
set XDS_GLOBAL_LOG_DIR=.\log

pushd %~dp0\Build
if "%1" == "setup" (
    call setup.bat %2
) else if "%1" == "build" (
    call build.bat %2
) else if "%1" == "clean" (
    call clean.bat
) else (
    call build.bat %1
)
popd

