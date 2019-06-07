; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for rem & reml functions

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

;PROCEDURE ["StdCall"] X2C_rem (x, y: REAL): REAL

                global  X2C_rem
X2C_rem:
      fld   dword [esp+8]  ;arg2
      fld   dword [esp+4]  ;arg1
                fprem     
X2C_rem_More:
                fprem                       ;get partial remainder
                fstsw   ax
                test    ah, 004H            ;C2 = 0 if reduction is complete
                jnz     X2C_rem_More        ;reduction is incomplete 
                fstp    st1
                ret     8h
;X2C_rem         endp


;PROCEDURE ["StdCall"] X2C_reml (x, y: LONGREAL): LONGREAL

                global  X2C_reml
X2C_reml:
      fld   qword [esp+12]  ;arg2
      fld   qword [esp+4]   ;arg1
X2C_reml_More:
                fprem                       ;get partial remainder
                fstsw   ax
                test    ah, 004H            ;C2 = 0 if reduction is complete
                jnz     X2C_reml_More       ;reduction is incomplete 
                fstp    st1
                ret     10h
;X2C_reml   endp

