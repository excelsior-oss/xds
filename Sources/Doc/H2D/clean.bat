@echo off

for %%i in (log,workplace\html) do if exist %%i rmdir /Q /S %%i

for %%i in (hlg,rlg,rtf,log,php) do  if exist *.%%i del /F /Q *.%%i
for %%i in (aux,ids,idx,ilg,ind,toc,out) do  if exist *.%%i del /F /Q *.%%i

for %%i in (hlp,pdf,cnt,html)  do if exist *.%%i  del /F /Q *.%%i 
for %%i in (hlp,pdf,cnt)  do if exist workplace\*.%%i  del /F /Q workplace\*.%%i 

for %%i in (chm,html,clg,hha,hhc)  do if exist *.%%i  del /F /Q *.%%i 
