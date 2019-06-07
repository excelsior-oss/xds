                .386p

; COPYRIGHT (c) 1996,99. All Rights Reserved.

; Implementation for X2C_MEMSET function

ifdef OS2
                .model FLAT
endif

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif

;PROCEDURE ["C"] X2C_ZEROMEM (p :ADDRESS; quatroNum: CARDINAL);

                public   X2C_ZEROMEM

X2C_ZEROMEM     proc     near

                push     edi
                mov      edi, +8[esp]  ;p
                mov      ecx, +12[esp]  ;octaNum

                xor      eax, eax
                rep      stosd
                pop      edi
                ret

X2C_ZEROMEM     endp


;PROCEDURE [2] X2C_MEMSET (p: xmRTS.X2C_pVOID; c: CHAR; n: size_t);

                public   X2C_MEMSET

X2C_MEMSET      proc    near
                mov     dl,  byte  ptr [esp+8]
                mov     ecx, dword ptr [esp+12]
                test    ecx, ecx
                je      short sb_ret
                mov     eax, dword ptr [esp+4]
sb_beg:
                test    al, 3
                je      short sb_main
                mov     byte ptr [eax],dl
                inc     eax
                dec     ecx
                jne     short sb_beg
                ret
sb_main:
                mov     dh, dl
                shl     edx, 8
                mov     dl, dh
                shl     edx, 8
                mov     dl, dh
                push    ecx
                shr     ecx, 2
                call    X2C_STOSD
                pop     ecx
                and     ecx, 3
                je      short sb_ret
                mov     byte ptr [eax], dl
                dec     ecx
                je      short sb_ret
                mov     byte ptr [eax+1], dl
                dec     ecx
                je      short sb_ret
                mov     byte ptr [eax+2], dl
sb_ret:
                ret
X2C_MEMSET      endp

ifdef OS2
                align   dword
else
                align   16, 90h
endif

; На входе:
;       eax - куда писать
;       ecx - сколько слов
;       edx - что писать
; На выходе:
;       eax - продвинутый указатель
X2C_STOSD       proc    near
                test    ecx, ecx
                je      short sd_ret
sd_beg:
                test    al, 1Fh
                je      short sd_main
                mov     dword ptr [eax], edx
                add     eax, 4
                dec     ecx
                jne     short sd_beg
                ret
sd_main:
                push    ecx
                shr     ecx, 2
                je      short sd_zero
                dec     ecx
                je      short sd_tail
sd_loop:
                mov     dword ptr [eax],   edx
                mov     dword ptr [eax+4], edx
                dec     ecx
                mov     dword ptr [eax+8],  edx
                mov     dword ptr [eax+12], edx
                je      short sd_fin
                mov     dword ptr [eax+16], edx
                mov     dword ptr [eax+20], edx
                dec     ecx
                mov     dword ptr [eax+24], edx
                mov     dword ptr [eax+28], edx
                lea     eax, [eax+32]
                jne     short sd_loop
                sub     eax, 16
sd_fin:
                add     eax, 16
sd_tail:
                mov     dword ptr [eax],    edx
                mov     dword ptr [eax+4],  edx
                mov     dword ptr [eax+8],  edx
                mov     dword ptr [eax+12], edx
                add     eax, 16
sd_zero:
                pop     ecx
                and     ecx, 3
                je      short sd_ret
                mov     dword ptr [eax], edx
                add     eax, 4
                dec     ecx
                je      short sd_ret
                mov     dword ptr [eax], edx
                add     eax, 4
                dec     ecx
                je      short sd_ret
                mov     dword ptr [eax],edx
                add     eax, 4
sd_ret:
                ret
X2C_STOSD       endp

_TEXT           ends

                end
