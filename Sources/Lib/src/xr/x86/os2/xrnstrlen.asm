                .386p

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for X2C_LENGTH function

ifdef OS2
                .model FLAT
endif

ifdef OS2
_TEXT           segment public dword use32 'CODE'
else
_TEXT		segment public para use32 'CODE'
endif

;PROCEDURE [2] X2C_LENGTH (a: xmRTS.X2C_pVOID; alen: size_t): size_t;

                public  X2C_LENGTH

X2C_LENGTH      proc    near
                mov     ecx, dword ptr 4[esp]
                mov     edx, dword ptr 8[esp]
                push    esi
                test    ecx, 3
                jz      short LLoop
ALoop:
                mov     al, byte ptr [ecx]
                inc     ecx
                test    al, al
                jz      short ret_m1
                dec     edx
                js      short ret_len
                test    cl, 3
                jne     short ALoop
LLoop:
                sub     edx, 4
                js      short tail
                mov     esi, dword ptr [ecx]
                mov     eax, 0FEFEFEFFH
                add     eax, esi
                not     esi
                and     eax, esi
                add     ecx, 4
                test    eax, 80808080H
                jz      short LLoop

                test    al, al
                js      short ret_m4
                test    ah, ah
                js      short ret_m3
                test    eax, 00800000H
                jnz     short ret_m2
ret_m1:
                lea     eax, -1[ecx]
                mov     ecx, dword ptr 8[esp]
                sub     eax, ecx
                pop     esi
                ret     
ret_m2:
                lea     eax, -2[ecx]
                mov     ecx, dword ptr 8[esp]
                sub     eax, ecx
                pop     esi
                ret     
ret_m3:
                lea     eax, -3[ecx]
                mov     ecx, dword ptr 8[esp]
                sub     eax, ecx
                pop     esi
                ret     
ret_m4:
                lea     eax, -4[ecx]
                mov     ecx, dword ptr 8[esp]
                sub     eax, ecx
                pop     esi
                ret
tail:
                add     edx, 4
                jz      short ret_len
TLoop:
                mov     al, byte ptr [ecx]
                inc     ecx
                test    al, al
                jz      short ret_m1
                dec     edx
                jne     short TLoop
ret_len:
                mov     eax, dword ptr 12[esp]
                pop     esi
                ret

X2C_LENGTH      endp

_TEXT           ends

                end

