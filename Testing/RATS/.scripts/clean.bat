@echo off

if not exist tmp  mkdir tmp

for /D %%i in (tmp\*) do rmdir /Q /S %%i
if exist tmp\*  del /Q tmp\*

del *.log *.mkf *.lnk oct.ses comp.log comp1.log 1>nul 2>nul
for %%i in (prg,map,err)  do  if exist *.%%i del /Q *.%%i 

for %%i in (def,mod,ob2,ref,sym,ch,obj,exe,log) do if not exist tmp\%%i  mkdir tmp\%%i 

goto :EOF
