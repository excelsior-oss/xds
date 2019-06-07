@echo off
rem *fsa 2014: msvc42 may make it
set MSVC42=T:\msvc42

set PATH=%MSVC42%\bin;%PATH%
set INCLUDE=%MSVC42%\include;%MSVC42%\mfc\include;%INCLUDE%
set LIB=%MSVC42%\lib;%LIB%


rem copy src\Windows\*.c src
rem copy src\Windows\*.cfg
rem copy src\Windows\*.mkf

if not exist ch  mkdir ch

nmake /f ctr.mkf
nmake /f ctrmake.mkf
