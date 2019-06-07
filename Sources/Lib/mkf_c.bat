@echo off
rem Usage: mkf [component] [ALL] [Release]
rem   component - component to make
rem   ALL       - rebuild mode
rem   Release   - compile in enduser mode
 
rem ============================================= Configure
set MAKE_SCRIPT_DIR=%~dp0\..\.scripts
set MAKE_CONFIG_FILE=%~dp0\.config.bsc

call "%MAKE_SCRIPT_DIR%\read_config.bat"  "%MAKE_CONFIG_FILE%"  MAKE  || exit /B 1 

set MAKE_TASK=%MAKE_DEFAULT_COMPONENT%
set MAKE_MODE_ALL=no
set MAKE_MODE_RELEASE=no

call :lbl_parse_argument %* || exit /B 1

call "%MAKE_SCRIPT_DIR%\setenv.bat" %MAKE_ENVIRONMENT%  || exit /B 1

if "%WORKPLACE_DIR%" == "" call "%MAKE_SCRIPT_DIR%\setenv_workplace.bat"

for %%i in (workplace,workplace\def\ob2) do if not exist %%i mkdir %%i 
for %%i in (workplace\lib\C)             do if not exist %%i mkdir %%i 
for %%i in (ch\C,sym\C)                  do if not exist %%i mkdir %%i 

goto lbl_%MAKE_TASK%
exit /B 0


rem ============================================= Make MSVC library
:lbl_msvc_lib
set XDS_OPT=-cc=MSVC -env_target=winnt -decor=rht
set TARGET_OPT=-target_fs=unc -target_family=win32 -target_os=winnt 

echo.
echo Build XDS-C lib for MSVC x86 
call :lbl_libxds %XDS_OPT% %TARGET_OPT%  || goto lbl_Error
goto lbl_Success

rem ============================================= Generate MSVC makefile
:lbl_msvc_mkf
set LIB_MKF_NAME=msvc
set XDS_OPT=-cc=MSVC -env_target=winnt -decor=rht 
set TARGET_OPT=-target_fs=unc -target_family=win32 -target_os=winnt  
set MAKE_OPTIONS=-mkf_name:=%LIB_MKF_NAME% %XDS_OPT% %TARGET_OPT%

call :lbl_gen_mkf %LIB_MKF_NAME%  "%MAKE_OPTIONS%"  || goto lbl_Error

set CHECK_OPTIONS=nmake /f %LIB_MKF_NAME%.mkf
call :lbl_check_mkf %LIB_MKF_NAME% "%CHECK_OPTIONS%"  || goto lbl_Error
goto lbl_Success


rem ============================================= Generate Unix makefile
:lbl_unix_mkf
set LIB_MKF_NAME=unix
set XDS_OPT=-cc=GCC -env_target= -decor=rht
set TARGET_OPT=-target_fs=unix -target_family=unix -target_os=unix -libext=a 
set MAKE_OPTIONS=-mkf_name:=%LIB_MKF_NAME% %XDS_OPT% %TARGET_OPT%

call :lbl_gen_mkf %LIB_MKF_NAME%  "%MAKE_OPTIONS%"  || goto lbl_Error
goto lbl_Success


rem ============================================= Generate Watcom makefile
:lbl_watcom_mkf
set LIB_MKF_NAME=watcom
set XDS_OPT=-cc=Watcom -env_target=watcomnt -decor=rht
set TARGET_OPT=-target_family=win32 
set MAKE_OPTIONS=-mkf_name:=%LIB_MKF_NAME% %XDS_OPT% %TARGET_OPT%

call :lbl_gen_mkf %LIB_MKF_NAME%  "%MAKE_OPTIONS%"  || goto lbl_Error
goto lbl_Success


rem ============================================= Make MSVC TopSpeed library
:lbl_tsmsvc_lib
set XDS_OPT=-cc=MSVC -env_target=winnt -env_host=winnt
set TARGET_OPT=-target_fs=unc -target_family=win32 -target_os=winnt 

echo.
echo Build XDS-C TopSpeed lib for MSVC x86 
echo Update C-sources 

xcopy /R /Y src\TSlibs\POSIX\*.c  ch\C\     || exit /B 1 
copy src\TSlibs\IO.*  src\TSlibs\IO_.*

set MAKE_OPTIONS=%XDS_OPT% %TARGET_OPT% 
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser
echo.
echo "%WORKPLACE_DIR%\bin\xm.exe" =p tslib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xm.exe" =p tslib.prj %MAKE_OPTIONS%  || exit /B 1
goto lbl_Success


rem ============================================= Make library
:lbl_libxds
for %%i in (obj,o) do if exist ch\c\*.%%i del /Q ch\c\*.%%i

echo.
echo Update C-sources 

xcopy /R /Y src\os\posix\*.c      ch\C\     || exit /B 1 
xcopy /R /Y src\xr\C\*.c          ch\C\     || exit /B 1
xcopy /R /Y src\os\posix\*.h      include\  || exit /B 1
xcopy /R /Y src\xr\C\*.h          include\  || exit /B 1
xcopy /R /Y ..\API\src\Win32\*.h  include\  || exit /B 1

set MAKE_OPTIONS= 
if "%MAKE_MODE_ALL%" == "yes"     set MAKE_OPTIONS=%MAKE_OPTIONS% =a
if "%MAKE_MODE_RELEASE%" == "yes" set MAKE_OPTIONS=%MAKE_OPTIONS% -mode:=enduser
set MAKE_OPTIONS=%MAKE_OPTIONS% %*

echo.
echo "%WORKPLACE_DIR%\bin\xm.exe" =p lib.prj %MAKE_OPTIONS%
"%WORKPLACE_DIR%\bin\xm.exe" =p lib.prj %MAKE_OPTIONS%  || exit /B 1

goto :EOF



rem ============================================= Generate makefile
:lbl_gen_mkf
set GEN_MKF_NAME=%1
set GEN_MKF_OPTIONS=..\..\..\lib.prj =g -mode:=make_file %~2

echo.
echo --- Build %GEN_MKF_NAME%.mkf ---------------------------
echo Update C-sources 

for %%i in (workplace\C,workplace\include) do if not exist %%i mkdir %%i 

xcopy /R /Y src\os\posix\*.c      workplace\C\        || exit /B 1 
xcopy /R /Y src\xr\C\*.c          workplace\C\        || exit /B 1
xcopy /R /Y src\os\posix\*.h      workplace\include\  || exit /B 1
xcopy /R /Y src\xr\C\*.h          workplace\include\  || exit /B 1
xcopy /R /Y ..\API\src\Win32\*.h  workplace\include\  || exit /B 1

pushd workplace\lib\C
echo "%WORKPLACE_DIR%\bin\xm.exe" %GEN_MKF_OPTIONS% 
"%WORKPLACE_DIR%\bin\xm.exe" %GEN_MKF_OPTIONS%  || (popd & exit /B 1)
popd

for %%i in (workplace\C,workplace\include) do if exist %%i rmdir /Q /S %%i

goto :EOF


rem ============================================= Check makefile
:lbl_check_mkf
set CHECK_MKF_NAME=%1
set CHECK_MKF_CMD=%~2
set CHECK_MKF_DIR=tmp\%CHECK_KMF_NAME%

echo.
echo --- Check %GEN_MKF_NAME%.mkf ---------------------------
for %%i in (%CHECK_MKF_DIR%) do if exist %%i rmdir /Q /S %%i

for %%i in (tmp,%CHECK_MKF_DIR%) do if not exist %%i  mkdir %%i 
for %%i in (C,include,lib\C)     do mkdir %CHECK_MKF_DIR%\%%i 

xcopy /R /Y ch\C\*.c               %CHECK_MKF_DIR%\C\        || exit /B 1
xcopy /R /Y include\*.h            %CHECK_MKF_DIR%\include\  || exit /B 1
xcopy /R /Y workplace\lib\C\*.mkf  %CHECK_MKF_DIR%\lib\C\    || exit /B 1

pushd %CHECK_MKF_DIR%\lib\C
echo %CHECK_MKF_CMD%
%CHECK_MKF_CMD%  || (popd & exit /B 1) 
popd

goto :EOF



rem ============================================= Parse Arguments
:lbl_parse_argument
if "%1" == "" goto :EOF

set IS_COMPONENT=no
for %%i in (%MAKE_ALL_COMPONENTS%) do if "%~1" == "%%i" set IS_COMPONENT=yes

if "%IS_COMPONENT%" == "yes" (
    set MAKE_TASK=%1
) else if "%~1" == "ALL" (
    set MAKE_MODE_ALL=yes
) else if "%~1" == "Release" (
    set MAKE_MODE_RELEASE=yes
) else ( 
    echo Incorrect arguments: %~nx0 %*
    echo.
    echo Usage: %~nx0 [component] [ALL] [Release]
    echo   component - component to make: %MAKE_ALL_COMPONENTS%
    echo   ALL       - rebuild mode
    echo   Release   - compile in enduser mode
    exit /B 1
)
shift /1
goto :lbl_parse_argument 


rem ============================================= Script Messages
:lbl_Success
call "%MAKE_SCRIPT_DIR%\copy_extdeps.bat" "%MAKE_EXTDEPS%" "%~dp0\workplace"  || goto lbl_Error
echo ============ %MAKE_TASK% ============ OK!
exit /B 0
goto :EOF


:lbl_Error
echo *********** %MAKE_TASK% *********** Failed!
exit /B 1
