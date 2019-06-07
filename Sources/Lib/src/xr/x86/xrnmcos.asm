; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for cos & cosl functions

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

;PROCEDURE ["StdCall"] X2C_cos(x: REAL): REAL
                global  X2C_cos
X2C_cos:
      fld     dword [esp+4]
      fcos
                fstsw     ax
                test      ah, 004H
                jz        X2C_cos_L2
                fld1                        ; cos argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      st1
                fxch      st1
X2C_cos_L1:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_cos_L1
                fstp      ST1
                fcos      
X2C_cos_L2:
      ret       4h
;X2C_cos      endp


;PROCEDURE [2] X2C_cosl(x: LONGREAL): LONGREAL
                global  X2C_cosl
X2C_cosl:
      fld     qword [esp+4]
      fcos
                fstsw     ax
                test      ah, 004H
                jz        X2C_cosl_L4
                fld1                        ; cos argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      st1
                fxch      st1
X2C_cosl_L3:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_cosl_L3
                fstp      st1
                fcos      
X2C_cosl_L4:
      ret       8h
;X2C_cosl   endp

