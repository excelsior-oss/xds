; COPYRIGHT (c) 1996,99,2002. All Rights Reserved.

; Implementation for X2C_MEMSET function

                cpu 386
                bits 32

%ifdef OS2
                section .text  use32  align=4  FLAT  public 'CODE' 
%else
                section .text  use32  align=16  public 'CODE'
%endif


;PROCEDURE ["C"] X2C_ZEROMEM (p :ADDRESS; quatroNum: CARDINAL);

                global   X2C_ZEROMEM

X2C_ZEROMEM:

                push     edi
                mov      edi, [esp+8]  ;p
                mov      ecx, [esp+12]  ;octaNum

                xor      eax, eax
                rep      stosd
                pop      edi
                ret

;X2C_ZEROMEM     endp


;PROCEDURE [2] X2C_MEMSET (p: xmRTS.X2C_pVOID; c: CHAR; n: size_t);

                global   X2C_MEMSET

X2C_MEMSET:
                mov     dl,  byte  [esp+8]
                mov     ecx, dword [esp+12]
                test    ecx, ecx
                je      sb_ret
                mov     eax, dword [esp+4]
sb_beg:
                test    al, 3
                je      sb_main
                mov     byte [eax],dl
                inc     eax
                dec     ecx
                jne     sb_beg
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
                je      sb_ret
                mov     byte [eax], dl
                dec     ecx
                je      sb_ret
                mov     byte [eax+1], dl
                dec     ecx
                je      sb_ret
                mov     byte [eax+2], dl
sb_ret:
                ret
;X2C_MEMSET      endp

%ifdef OS2
                align   4
%else
                align   16, db 90h
%endif

; In input:
;       eax - where to write
;       ecx - how many words
;       edx - what to write
; In output:
;       eax - moved pointer

X2C_STOSD:
                test    ecx, ecx
                je      near sd_ret
sd_beg:
                test    al, 1Fh
                je      sd_main
                mov     dword [eax], edx
                add     eax, 4
                dec     ecx
                jne     sd_beg
                ret
sd_main:
                push    ecx
                shr     ecx, 2
                je      sd_zero
                dec     ecx
                je      sd_tail
sd_loop:
                mov     dword [eax],   edx
                mov     dword [eax+4], edx
                dec     ecx
                mov     dword [eax+8],  edx
                mov     dword [eax+12], edx
                je      sd_fin
                mov     dword [eax+16], edx
                mov     dword [eax+20], edx
                dec     ecx
                mov     dword [eax+24], edx
                mov     dword [eax+28], edx
                lea     eax, [eax+32]
                jne     sd_loop
                sub     eax, 16
sd_fin:
                add     eax, 16
sd_tail:
                mov     dword [eax],    edx
                mov     dword [eax+4],  edx
                mov     dword [eax+8],  edx
                mov     dword [eax+12], edx
                add     eax, 16
sd_zero:
                pop     ecx
                and     ecx, 3
                je      sd_ret
                mov     dword [eax], edx
                add     eax, 4
                dec     ecx
                je      sd_ret
                mov     dword [eax], edx
                add     eax, 4
                dec     ecx
                je      sd_ret
                mov     dword [eax],edx
                add     eax, 4
sd_ret:
                ret
;X2C_STOSD       endp

