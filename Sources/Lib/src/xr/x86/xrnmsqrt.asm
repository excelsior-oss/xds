; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for sqrt & sqrtl functions

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

;PROCEDURE ["StdCall"] X2C_sqrt(x: REAL): REAL
                global  X2C_sqrt
X2C_sqrt:
      fld   dword [esp+4]
      fsqrt
                ret     4h
;X2C_sqrt        endp


;PROCEDURE ["StdCall"] X2C_sqrtl(x: LONGREAL): LONGREAL
                global  X2C_sqrtl
X2C_sqrtl:
      fld   qword [esp+4]
      fsqrt
                ret     8h
;X2C_sqrtl  endp

