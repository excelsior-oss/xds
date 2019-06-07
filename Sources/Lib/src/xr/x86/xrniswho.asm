; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for isWholeNum function

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

;PROCEDURE [2] X2C_isWholeNum(x: LONGREAL): BOOLEAN;
      global   X2C_isWholeNum
X2C_isWholeNum:
      fld   qword [esp+4]
      frndint
      fcomp qword [esp+4]
      fnstsw ax
      sahf
                sete    al
                ret
;X2C_isWholeNum   endp


;PROCEDURE [2] X2C_isOdd(x: LONGREAL): BOOLEAN;
      global   X2C_isOdd
X2C_isOdd:
      mov   eax,2
      push  eax
      fild  dword [esp]
      fdivr qword [esp+8]
      frndint
      sub   esp,8
      fstp  qword [esp]
      fild  dword [esp+8]
      fmul  qword [esp]
      add   esp,0ch
      fcomp qword [esp+4]
      fnstsw ax
      sahf
                setne   al
      ret
;X2C_isOdd  endp

