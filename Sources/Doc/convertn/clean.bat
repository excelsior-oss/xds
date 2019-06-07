@echo off

for %%i in (log,ch) do if exist %%i rmdir /Q /S %%i

for %%i in (pdb,ilk,bsc) do  if exist workplace\*.%%i del /Q workplace\*.%%i 
for %%i in (exe)         do  if exist workplace\*.%%i del /Q workplace\*.%%i 
