; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for sin & sinl functions

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

;PROCEDURE ["StdCall"] X2C_sin(x: REAL): REAL
                global  X2C_sin
X2C_sin:
      fld     dword [esp+4]
      fsin      
                fstsw     ax              
                test      ah, 004H
                jz        X2C_sin_L2
                fld1                      ; sin argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      st1
                fxch      st1
X2C_sin_L1:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_sin_L1
                fstp      st1
                fsin      
X2C_sin_L2:
      ret       4h
;X2C_sin    endp
 

;PROCEDURE ["StdCall"] X2C_sinl(x: LONGREAL): LONGREAL
                global  X2C_sinl
X2C_sinl:
      fld     qword [esp+4]
      fsin      
                fstsw     ax              
                test      ah, 004H
                jz        X2C_sinl_L4
                fld1                      ; sin argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      st1
                fxch      st1
X2C_sinl_L3:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_sinl_L3
                fstp      st1
                fsin      
X2C_sinl_L4:
      ret     8h
;X2C_sinl   endp

