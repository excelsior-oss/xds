@echo off
rem Setup environment to build XDS product

set XDS_PRODUCT=XDS-x86
set XDS_CONFIG_FILE=%~dp0\.config-%COMPUTERNAME%.bsc

if not exist %XDS_CONFIG_FILE% (
    echo File "%XDS_CONFIG_FILE%" does not exist
    echo *********** %XDS_PRODUCT% Configuration *********** Failed!
    exit /B 1
)

set XDS_CONFIG_VAR=
if not "%1" == "" set XDS_CONFIG_VAR=%1

rem ============================================= Parse Cofiguration File
set XDS_CONFIG_STATUS=
for /F "eol=# delims== tokens=1,2" %%i in (%XDS_CONFIG_FILE%) do call :lbl_set_config %%i "%%j"

if not "%XDS_CONFIG_STATUS%" == "ok" (
    echo Property '%XDS_CONFIG_VAR%' not found in the "%XDS_CONFIG_FILE%"
    echo *********** %XDS_PRODUCT% Configuration *********** Failed!
    exit /B 1
)

goto :EOF
 

rem ============================================= Set Environment
:lbl_set_config
set IS_XDS_CONFIG_VAR_ACCEPTED=no
if "%XDS_CONFIG_VAR%" == "" (
     set IS_XDS_CONFIG_VAR_ACCEPTED=yes
     set XDS_CONFIG_STATUS=ok
) else if "%XDS_CONFIG_VAR%" == "%1" (
     set IS_XDS_CONFIG_VAR_ACCEPTED=yes
     set XDS_CONFIG_STATUS=ok
) 
if "%IS_XDS_CONFIG_VAR_ACCEPTED%" == "yes" set %1=%~2
goto :EOF

