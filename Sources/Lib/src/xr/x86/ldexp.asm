; Copyright 2002, Excelsior LLC. All rights reserved

                cpu 386
      bits 32

%ifdef OS2
group       DGROUP _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group       DGROUP _DATA
      section _DATA  use32  align=4  public 'DATA' 
%endif

%ifdef OS2
      section .text  use32  align=4  FLAT  public 'CODE' 
%else
      section .text  use32  align=16  public 'CODE'
%endif


realValueException equ  7

;                assume  cs: .text, ds: DGROUP, ss: DGROUP, gs: nothing, fs: nothing

                extern  X2C_TRAP_F
                global  X2C_ldexp

X2C_ldexp:
                mov     eax, dword [esp+8]
                test    eax, eax
                je      ldexp_3
                mov     eax, dword [esp+10]
                mov     edx, dword [esp+12]
                and     eax, 00007FF0H
                sar     eax, 4
                cmp     edx, 00003E80H
                jle     ldexp_1
                mov     edx, 00003E80H
ldexp_1:        cmp     edx, 0FFFFC180H
                jge     ldexp_2
                mov     edx, 0FFFFC180H
ldexp_2:        add     eax, edx
                jle     ldexp_5
                cmp     eax,000007FFH
                jge     ldexp_4
                mov     edx, dword [esp+10]
                and     edx, 0000800FH
                shl     eax, 4
                or      eax, edx
                mov     word [esp+10], ax
ldexp_3:        fld     qword [esp+4]
                ret     12
ldexp_4:        push    byte realValueException
                call    X2C_TRAP_F
ldexp_5:        fldz
                ret     12
;X2C_ldexp       endp

