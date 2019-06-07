                .386p

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for X2C_STRCMP_PROC function

ifdef OS2
                .model FLAT
endif

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs:_TEXT

;PROCEDURE [2] X2C_STRCMP_PROC(a: xmRTS.X2C_pVOID; alen: size_t;
;                              b: xmRTS.X2C_pVOID; blen: size_t): int;
                public  X2C_STRCMP_PROC
X2C_STRCMP_PROC proc    near
                push    ebx
                push    esi
                mov     ebx,dword ptr 12[esp]
                mov     edx,dword ptr 20[esp]
                mov     esi,dword ptr 16[esp]
                cmp     esi,dword ptr 24[esp]
                jle     short _loop
                mov     esi,dword ptr 24[esp]
_loop:
                cmp     esi,4
                jc      str_end
                mov     eax,dword ptr [ebx]
                mov     ecx,dword ptr [edx]
                cmp     ecx,eax
                jne     str_neq
                not     ecx
                add     eax,0FEFEFEFFH
                and     eax,ecx
                and     eax,80808080H
                jne     short str_equal
                cmp     esi,8
                jc      str_end4
                mov     eax,dword ptr 4[ebx]
                mov     ecx,dword ptr 4[edx]
                cmp     ecx,eax
                jne     short str_neq
                not     ecx
                add     eax,0FEFEFEFFH
                and     eax,ecx
                and     eax,80808080H
                jne     short str_equal
                cmp     esi,12
                jc      short str_end8
                mov     eax,dword ptr 8[ebx]
                mov     ecx,dword ptr 8[edx]
                cmp     ecx,eax
                jne     short str_neq
                not     ecx
                add     eax,0FEFEFEFFH
                and     eax,ecx
                and     eax,80808080H
                jne     short str_equal
                cmp     esi,16
                jc      short str_end12
                mov     eax,dword ptr 12[ebx]
                mov     ecx,dword ptr 12[edx]
                cmp     ecx,eax
                jne     short str_neq
                add     ebx,16
                add     edx,16
                sub     esi,16
                not     ecx
                add     eax,0FEFEFEFFH
                and     eax,ecx
                and     eax,80808080H
                je      _loop
str_equal:
                pop     esi
                sub     eax,eax
                pop     ebx
                ret
str_neq:
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      short str_equal
                cmp     ah,ch
                jne     short byte_neq
                cmp     ah,00H
                je      short str_equal
                shr     eax,16
                shr     ecx,16
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      short str_equal
                cmp     ah,ch
byte_neq:
                pop     esi
                sbb     eax,eax
                pop     ebx
                or      al,01H
                ret

; Осталось сравнить менее 4 байтов

str_end12:
                add     edx,4
                add     ebx,4
                sub     esi,4
str_end8:
                add     edx,4
                add     ebx,4
                sub     esi,4
str_end4:
                add     edx,4
                add     ebx,4
                sub     esi,4
str_end:
                test    esi,esi
                je      short end_end

                mov     al,byte ptr [ebx]
                mov     cl,byte ptr [edx]
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      short str_equal
                inc     ebx
                inc     edx
                cmp     esi,1
                je      short end_end

                mov     al,byte ptr [ebx]
                mov     cl,byte ptr [edx]
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      short str_equal
                inc     ebx
                inc     edx
                cmp     esi,2
                je      short end_end

                mov     al,byte ptr [ebx]
                mov     cl,byte ptr [edx]
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      short str_equal
                inc     ebx
                inc     edx
end_end:
                mov     esi,dword ptr 16[esp]
                cmp     esi,dword ptr 24[esp]
                je      str_equal
                jg      short first_longer
                cmp     byte ptr [edx],0
                je      str_equal
                pop     esi
                mov     eax,-1
                pop     ebx
                ret
first_longer:
                cmp     byte ptr [ebx],0
                je      str_equal
                pop     esi
                mov     eax,1
                pop     ebx
                ret
X2C_STRCMP_PROC endp

_TEXT           ends

                end
