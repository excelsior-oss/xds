@echo off
cl   pack.c ..\zlib\compress.c ..\zlib\deflate.c ..\zlib\adler32.c ..\zlib\trees.c ..\zlib\zutil.c ..\zlib\crc32.c /I..\zlib /Fepack.exe
del /Q *.obj