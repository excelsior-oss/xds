@echo off

for %%i in (ch) do if exist %%i  rmdir /S /Q %%i 
for %%i in (ilk,pdb)  do  if exist *.%%i del /Q *.%%i 

