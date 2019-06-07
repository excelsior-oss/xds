; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for tan & tanl functions

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

;PROCEDURE ["StdCall"] X2C_tan(x: REAL): REAL
                global  X2C_tan
X2C_tan:
      fld     dword [esp+4]
      fptan
                fstsw     ax
                test      ah, 004H
                jz        X2C_tan_L1 
                fld1                       ; tan argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      st1
                fxch      st1
X2C_tan_L2:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_tan_L2 
                fstp      st1
                fptan     
X2C_tan_L1:
      fstp       st0
      ret        4h
;X2C_tan    endp


;PROCEDURE ["StdCall"] X2C_tanl(x: LONGREAL): LONGREAL
                global  X2C_tanl
X2C_tanl:
_X2C_tanl:
      fld     qword [esp+4]
      fptan     
  ;                fstsw     ax
;                test      ah, 004H
;                jz        X2C_tanl_L4 
;                fld1                       ; tan argument is out of -2^63..+2^63
;                fldpi     
;                fscale    
;                fstp      st1
;                fxch      st1
;X2C_tanl_L3:
;                fprem1    
;                fstsw     ax
;                test      ah, 004H
;                jnz       X2C_tanl_L3 
;                fstp      st1
;                fptan     
;X2C_tanl_L4:
      fstp    st0
      ret     8h
;X2C_tanl   endp

