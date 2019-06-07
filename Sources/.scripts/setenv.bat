@echo off
rem Usage: setenv [<EnvVare>,<EnvVar>]
rem   <EnvVare> - name of environment variable

if not "%BUILD_SYSTEM_ENV%" == "%~dp0\setenv.bat"  (call %~dp0\..\..\.config\setenv.bat || exit /B 1)

rem Check environment
for %%i in (%~1) do  if not defined %%i  call :lbl_Error %%i

set BUILD_SYSTEM_ENV=%~dp0\setenv.bat

goto :EOF


rem ============================================= Script Messages
:lbl_Error
echo *** Environment Error: variable "%1" is undefined
echo *** Please define it in the "%XDS_ENV_CONFIG_FILE%" 
exit /B 1
