@echo off
copy src\OS2\*.c src
copy src\OS2\*.cfg
copy src\OS2\*.mkf
nmake /f ctr.mkf
nmake /f ctrmake.mkf
