; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.
; COPYRIGHT (c) 2002 Excelsior LLC. All Rights Reserved.

; Implementation for X2C_adr_lss and X2C-adr_gtr functions

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
      section .text  use32  public  align=4  FLAT  public 'CODE'
%else
      section .text  use32  public  align=16  public 'CODE'
%endif

;                assume  cs: .text, ds: DGROUP, gs: nothing, fs: nothing

;PROCEDURE [2] X2C_adr_lss(x,y: SYSTEM.ADDRESS): BOOLEAN;
                global  X2C_adr_lss
X2C_adr_lss:
                mov     eax, [esp+4]
                cmp     eax, [esp+8]
                setb    al
                ret
;X2C_adr_lss     endp

;PROCEDURE [2] X2C_adr_gtr(x,y: SYSTEM.ADDRESS): BOOLEAN;
                global  X2C_adr_gtr
X2C_adr_gtr:
                mov     eax, [esp+4]
                cmp     eax, [esp+8]
                seta    al
                ret
;X2C_adr_gtr     endp

