; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for floor & floorl functions

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

;PROCEDURE ["StdCall"] X2C_floor(x: REAL): REAL
                global  X2C_floor
X2C_floor:
      fld   dword [esp+4]
      frndint
                ret     4h
;X2C_floor       endp


;PROCEDURE ["StdCall"] X2C_floorl(x: LONGREAL): LONGREAL
                global  X2C_floorl
X2C_floorl:
      fld   qword [esp+4]
      frndint
                ret     8h
;X2C_floorl endp

