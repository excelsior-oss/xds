; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for lg & lgl functions

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

;PROCEDURE ["StdCall"] X2C_lg(x: REAL): REAL
                global  X2C_lg
X2C_lg:
      fld   dword [esp+4]
      fldlg2
      fxch    st1
      fyl2x
                ret     4h
;X2C_lg     endp


;PROCEDURE ["StdCall"] X2C_lgl(x: LONGREAL): LONGREAL
                global  X2C_lgl
X2C_lgl:
      fld   qword [esp+4]
      fldlg2
      fxch    st1
      fyl2x
                ret     8h
;X2C_lgl endp

