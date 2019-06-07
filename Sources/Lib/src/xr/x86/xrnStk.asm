; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

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

;                assume  cs: .text, ds: DGROUP, gs: nothing, fs: nothing

;PROCEDURE ["C"] X2C_StackTop(): SYSTEM.ADDRESS;
                global  X2C_StackTop
X2C_StackTop:
      mov   eax, esp
      sub   eax, 4
      ret
;X2C_StackTop    endp
