; COPYRIGHT (c) 1996,2002 XDS. All Rights Reserved.

                cpu 386
                bits 32

%ifdef OS2
group           DGROUP  _DATA
                section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group           DGROUP  _DATA
                section _DATA  use32  align=4  public 'DATA' 
%endif

%ifdef OS2
                section .text  use32  public  align=4  FLAT  public 'CODE'  
%else
                section .text  use32  public  align=16  public 'CODE'   
%endif

                global  X2C_controlfp

X2C_controlfp:
                mov     eax, dword [esp+8]
                push    ecx
                fstcw   [esp]
                wait
                test    eax, eax
                jz      control87_ret
                mov     ecx, eax
                not     ecx
                and     dword [esp], ecx
                and     eax, dword [esp+8]
                or      dword [esp], eax
                fldcw   [esp]
                wait
                fstcw   [esp]
                wait
control87_ret:  xor     eax, eax
                mov     ax, word [esp]
                pop     ecx
                ret     8
