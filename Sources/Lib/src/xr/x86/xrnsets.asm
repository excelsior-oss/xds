; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

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

;PROCEDURE X2C_doROTL(x: BITSET; bits: CARDINAL): BITSET;
      global   X2C_doROTL
X2C_doROTL:
      push  ecx
      mov   eax, [esp+8h]
      mov   ecx, [esp+0ch]
      shl   eax, cl
      pop   ecx
      ret
;X2C_doROTL endp


;PROCEDURE X2C_doROTR(x: BITSET; bits: CARDINAL): BITSET;
      global   X2C_doROTR
X2C_doROTR:
      push  ecx
      mov   eax, [esp+8h]
      mov   ecx, [esp+0ch]
      shr   eax,cl
      pop   ecx
      ret
;X2C_doROTR endp


;PROCEDURE X2C_doASHR(x: BITSET; bits: CARDINAL): BITSET;
                global  X2C_doASHR
X2C_doASHR:
      push  ecx
      mov   eax, [esp+8h]
      mov   ecx, [esp+0ch]
      sar   eax, cl
      pop   ecx
      ret
;X2C_doASHR endp

