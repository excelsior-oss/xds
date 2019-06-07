; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for X2C_LENGTH function

                cpu 386
      bits 32

%ifdef OS2
      section .text  use32  global   align=4  FLAT  global  'CODE'  
%else
      section .text  use32  global   align=16  global  'CODE'   
%endif


;PROCEDURE [2] X2C_LENGTH (a: xmRTS.X2C_pVOID; alen: size_t): size_t;

                global  X2C_LENGTH
X2C_LENGTH:
                mov     ecx, [esp+4]
                mov     edx, [esp+8]
                push    esi
                test    ecx, 3
                jz      LLoop
ALoop:
                mov     al, [ecx]
                inc     ecx
                test    al, al
                jz      ret_m1
                dec     edx
                js      near ret_len
                test    cl, 3
                jne     ALoop
LLoop:
                sub     edx, 4
                js      tail
                mov     esi, [ecx]
                mov     eax, 0FEFEFEFFH
                add     eax, esi
                not     esi
                and     eax, esi
                add     ecx, 4
                test    eax, 80808080H
                jz      LLoop

                test    al, al
                js      ret_m4
                test    ah, ah
                js      ret_m3
                test    eax, 00800000H
                jnz     ret_m2
ret_m1:
                lea     eax, [ecx-1]
                mov     ecx, [esp+8]
                sub     eax, ecx
                pop     esi
                ret     
ret_m2:
                lea     eax, [ecx-2]
                mov     ecx, [esp+8]
                sub     eax, ecx
                pop     esi
                ret     
ret_m3:
                lea     eax, [ecx-3]
                mov     ecx, [esp+8]
                sub     eax, ecx
                pop     esi
                ret     
ret_m4:
                lea     eax, [ecx-4]
                mov     ecx, [esp+8]
                sub     eax, ecx
                pop     esi
                ret
tail:
                add     edx, 4
                jz      ret_len
TLoop:
                mov     al, [ecx]
                inc     ecx
                test    al, al
                jz      ret_m1
                dec     edx
                jne     TLoop
ret_len:
                mov     eax, [esp+12]
                pop     esi
                ret
