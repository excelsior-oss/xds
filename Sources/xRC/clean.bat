@echo off

for %%i in (log,ch,sym) do if exist %%i rmdir /Q /S %%i

for %%i in (mkf,$$$,lnk,out)  do  if exist *.%%i  del /Q *.%%i

if exist workplace\xrc.*  del /Q workplace\xrc.* 
