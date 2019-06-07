@echo off

for %%i in (ch\C,ch\x86,ch)         do if exist %%i rmdir /Q /S %%i
for %%i in (sym\C,sym\x86,sym)      do if exist %%i rmdir /Q /S %%i
for %%i in (lib\x86,lib,tmp)        do if exist %%i rmdir /Q /S %%i
for %%i in (include,workplace,log)  do if exist %%i rmdir /Q /S %%i

for %%i in (mkf,$$$,lnk,out)  do  if exist *.%%i  del /Q *.%%i 

for %%i in (*.pdb) do if exist %%i  del /Q %%i
