; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for long, ulong, longl & ulongl functions

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

;PROCEDURE ["StdCall"] X2C_long(x: REAL): INTEGER;
                global  X2C_long
X2C_long:
      fld   dword [esp+4]
                fistp   dword [esp+4]
      mov   eax, [esp+4]
                ret     4h
;X2C_long        endp

;PROCEDURE ["StdCall"] X2C_longl(x: LONGREAL): INTEGER;
                global  X2C_longl
X2C_longl:
_X2C_longl:
      fld   qword [esp+4]
                fistp   dword [esp+4]
      mov   eax, [esp+4]
                ret     8h
;X2C_longl  endp

;PROCEDURE ["StdCall"] X2C_ulong(x: REAL): CARDINAL;
                global  X2C_ulong
X2C_ulong:
      fld   dword [esp+4]
                sub     esp, 8
                fistp   qword [esp]
      mov   eax, [esp]
      add   esp, 8
                ret     4h
;X2C_ulong       endp

;PROCEDURE ["StdCall"] X2C_ulongl(x: LONGREAL): CARDINAL;
                global  X2C_ulongl
X2C_ulongl:
      fld   qword [esp+4]
                sub     esp, 8
                fistp   qword [esp]
      mov   eax, [esp]
      add   esp, 8
                ret     8h
;X2C_ulongl endp

