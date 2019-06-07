; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for arctan2 & arctan2ll functions

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

;PROCEDURE ["StdCall"] X2C_arctan2(y,x: REAL): REAL;
                global  X2C_arctan2
X2C_arctan2:
      fld   dword [esp+4]  ; load "y"
      fld   dword [esp+8]  ; load "x"
L0:      fpatan
                ret     8H
;X2C_arctan2   endp

;PROCEDURE ["StdCall] X2C_arctan(x: REAL): REAL
                global  X2C_arctan
X2C_arctan:
      fld   dword [esp+4]
      fld1
                fpatan
                ret     4H
;X2C_arctan    endp

;PROCEDURE ["StdCall"] X2C_arctan2l(y,x: LONGREAL): LONGREAL;
                global  X2C_arctan2l
X2C_arctan2l:
      fld   qword [esp+4]  ; load "y"
      fld   qword [esp+0ch]  ; load "x"
LL0:     fpatan
                ret     10H
;X2C_arctan2l  endp

;PROCEDURE [2] X2C_arctanl(x: LONGREAL): LONGREAL
                global  X2C_arctanl
X2C_arctanl:
      fld   qword [esp+4]
      fld1
                fpatan
                ret     8H
;X2C_arctanl   endp

