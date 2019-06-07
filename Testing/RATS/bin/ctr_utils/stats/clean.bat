@echo off

for %%i in (bin,sym)  do  if exist %%i\*.* del /Q %%i\*.* 
for %%i in (mkf,lnk,exe,$$$)  do  if exist *.%%i  del /Q *.%%i 
