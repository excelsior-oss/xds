; COPYRIGHT (c) 1999,2002 Excelsior. All Rights Reserved.
; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.
;
; Implementation for X2C_setjmp and X2C_longjmp functions
;

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

;PROCEDURE X2C_setjmp(VAR point: X2C_jmp_buf): INTEGER;

      global   X2C_setjmp
X2C_setjmp:
      mov   eax, [esp+4]
      mov   [eax], ebx
      mov   [eax+4h], ecx
      mov   [eax+8h], edx
      mov   [eax+0ch], esi
      mov   [eax+10h], edi
      mov   [eax+14h], ebp
      pop   dword [eax+18h]
      mov   ecx, esp
      add   ecx, 4
      mov   [eax+1ch], ecx
      push  dword [eax+18h]
      mov     [eax+20H], es
      mov     [eax+22H], ds
      mov     [eax+24H], cs
      mov     [eax+26H], fs
      mov     [eax+28H], gs
      mov     [eax+2aH], ss
                xor     eax, eax
      ret
;X2C_setjmp endp


;PROCEDURE X2C_longjmp(VAR point: X2C_jmp_buf; n: INTEGER);
                global  X2C_longjmp
X2C_longjmp:
                pop     eax     ; drop return EIP
                pop     eax     ; jmp_buf pointer
                pop     edx     ; argument
                mov     ss, [eax+2aH]        ; switch to new stack
      mov     esp, [eax+1cH]       ;
      push  eax          ;
      push    dword [eax+18H] ; return EIP
      or      edx, edx
      jne     L1
      inc     edx
   L1:   push    edx
      mov     ebx, [eax]
      mov     ecx, [eax+4H]
      mov     esi, [eax+0cH]
      mov     edi, [eax+10H]
      mov     ebp, [eax+14H]
      mov     dx,  [eax+20H]
      verr    dx
      je      L2
      xor     edx, edx
   L2:   mov     es, dx
      mov     edx, [eax+8H]
      mov     ds, [eax+22H]
      pop     eax
      ret


