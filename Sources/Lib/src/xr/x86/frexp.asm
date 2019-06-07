; COPYRIGHT (c) 1996,99,2002 XDS. All Rights Reserved.

      cpu 386
      bits 32

%ifdef OS2
group    DGROUP   _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group    DGROUP   _DATA
      section _DATA  use32  align=4  public 'DATA' 
%endif

%ifdef OS2
      section .text  use32  public  align=4  FLAT  public 'CODE'  
%else
      section .text  use32  public  align=16  public 'CODE'   
%endif

;                assume  cs: .text, ds: DGROUP, ss: DGROUP, gs: nothing, fs: nothing

                global  X2C_frexp

X2C_frexp:
                mov     ecx, [esp+12]
                mov     edx, [esp+8]
                xor     eax, eax
                test    edx, edx
                je      frexp_ret
                mov     eax, [esp+10]
                and     eax, 00007FF0H
                mov     edx, [esp+10]
                and     edx, 0000800FH
                sar     eax, 4
                or      edx, 00003FE0H
                sub     eax, 000003FEH
                mov     [esp+10], dx
frexp_ret:      fld     qword [esp+4]
                mov     [ecx], eax
                ret     12
