@echo off
if "%1" == "" goto lbl_Help

set TEMPLATE_DIR=%1

if not exist %TEMPLATE_DIR%       goto lbl_dir_not_found
if not exist %TEMPLATE_DIR%\*.tg  goto lbl_tg_not_found

for %%i in (%TEMPLATE_DIR%\*.tg) do  call :lbl_run %%i
goto :EOF

:lbl_run
call "%~dp0\test_one.bat" %1
goto :EOF


:lbl_dir_not_found
echo *** Cannot find directory: %TEMPLATE_DIR% ***
goto lbl_Help

:lbl_tg_not_found
echo *** No tg-files in directory: %TEMPLATE_DIR% ***
goto lbl_Help


:lbl_Help
echo Usage:  test_one_dir.bat ^<directory wuth tg-file^> 
goto :EOF

