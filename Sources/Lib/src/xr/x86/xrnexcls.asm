; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; X2C_EXCLs constant array

                cpu 386
                bits 32

%ifdef OS2
group           DGROUP _DATA
                section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group           DGROUP _DATA
                section _DATA  use32  align=16  public 'DATA' 
%endif

                global X2C_EXCLs
                global X2C_EXCLs_HI

X2C_EXCLs:
                dd 0fffffffeh,0fffffffdh,0fffffffbh,0fffffff7h
                dd 0ffffffefh,0ffffffdfh,0ffffffbfh,0ffffff7fh
                dd 0fffffeffh,0fffffdffh,0fffffbffh,0fffff7ffh
                dd 0ffffefffh,0ffffdfffh,0ffffbfffh,0ffff7fffh
                dd 0fffeffffh,0fffdffffh,0fffbffffh,0fff7ffffh
                dd 0ffefffffh,0ffdfffffh,0ffbfffffh,0ff7fffffh
                dd 0feffffffh,0fdffffffh,0fbffffffh,0f7ffffffh
                dd 0efffffffh,0dfffffffh,0bfffffffh,07fffffffh

                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh


X2C_EXCLs_HI:
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh
                dd 0ffffffffh, 0ffffffffh, 0ffffffffh, 0ffffffffh

                dd 0fffffffeh,0fffffffdh,0fffffffbh,0fffffff7h
                dd 0ffffffefh,0ffffffdfh,0ffffffbfh,0ffffff7fh
                dd 0fffffeffh,0fffffdffh,0fffffbffh,0fffff7ffh
                dd 0ffffefffh,0ffffdfffh,0ffffbfffh,0ffff7fffh
                dd 0fffeffffh,0fffdffffh,0fffbffffh,0fff7ffffh
                dd 0ffefffffh,0ffdfffffh,0ffbfffffh,0ff7fffffh
                dd 0feffffffh,0fdffffffh,0fbffffffh,0f7ffffffh
                dd 0efffffffh,0dfffffffh,0bfffffffh,07fffffffh

