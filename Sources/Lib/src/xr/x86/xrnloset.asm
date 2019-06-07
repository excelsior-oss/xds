; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; X2C_LOSETs constant array

                cpu 386
                bits 32

%ifdef OS2
group           DGROUP _DATA
                section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group           DGROUP _DATA
                section _DATA  use32  align=16  public 'DATA' 
%endif

                global X2C_LOSETs


X2C_LOSETs:
                dd 00000001h, 00000003h, 00000007h, 0000000Fh
                dd 0000001Fh, 0000003Fh, 0000007Fh, 000000FFh
                dd 000001FFh, 000003FFh, 000007FFh, 00000FFFh
                dd 00001FFFh, 00003FFFh, 00007FFFh, 0000FFFFh
                dd 0001FFFFh, 0003FFFFh, 0007FFFFh, 000FFFFFh
                dd 001FFFFFh, 003FFFFFh, 007FFFFFh, 00FFFFFFh
                dd 01FFFFFFh, 03FFFFFFh, 07FFFFFFh, 0FFFFFFFh
                dd 1FFFFFFFh, 3FFFFFFFh, 7FFFFFFFh,0FFFFFFFFh

