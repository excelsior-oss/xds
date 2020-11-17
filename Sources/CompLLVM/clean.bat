@echo off

for %%i in (log) do if exist %%i rmdir /Q /S %%i

for %%i in (ch,sym) do if not exist %%i mkdir %%i 

for %%i in (ch,sym)  do  del /Q %%i\*.* 
for %%i in (mkf,$$$,lnk,out)  do  if exist *.%%i  del /Q *.%%i 

for %%i in (dll,exe) do  if exist workplace\*.%%i  del /Q workplace\*.%%i 

if not exist "src"  mklink /D /J "src"  "..\Comp\src"
