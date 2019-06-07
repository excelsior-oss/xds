; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for exp & expl functions

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

do_exp:
      fld1
      fldl2e
      fmul    st0, st2
      fst     st2
      fprem
      f2xm1
      faddp   st1, st0
      fscale
      fstp    st1
      ret
;do_exp     endp

;PROCEDURE ["StdCall"] X2C_exp(x: REAL): REAL
                global  X2C_exp
X2C_exp:
      fld   dword [esp+4]
      call  do_exp
                ret     4h
;X2C_exp    endp


;PROCEDURE ["StdCall"] X2C_expl(x: LONGREAL): LONGREAL
                global  X2C_expl
X2C_expl:
      fld   qword [esp+4]
      call  do_exp
                ret     8h
;X2C_expl   endp

