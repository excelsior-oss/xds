@echo off

for %%i in (log,ch,sym) do if exist %%i rmdir /Q /S %%i
for %%i in (Debug,Release) do if exist src\xpview\Win32\%%i rmdir /Q /S src\xpview\Win32\%%i

for %%i in (mkf,$$$,lnk,out)  do  if exist *.%%i  del /Q *.%%i 

for %%i in (dll,exe,lib,pdb,ilk) do  if exist workplace\*.%%i  del /Q workplace\*.%%i 
