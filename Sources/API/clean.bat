@echo off

for %%i in (log,ch,sym) do if exist %%i rmdir /Q /S %%i

for %%i in (exe,obj)      do if exist src\Win32\build\*.%%i  del /Q src\Win32\build\*.%%i 
