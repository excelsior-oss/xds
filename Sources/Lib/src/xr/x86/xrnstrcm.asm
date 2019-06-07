                cpu 386
      bits 32

; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for X2C_STRCMP_PROC function

                cpu 386

%ifdef OS2
      section .text  use32  align=4  FLAT  public 'CODE' 
%else
      section .text  use32  align=16  public 'CODE'
%endif

;                assume  cs:.text

;PROCEDURE [2] X2C_STRCMP_PROC(a: xmRTS.X2C_pVOID; alen: size_t;
;                              b: xmRTS.X2C_pVOID; blen: size_t): int;
                global  X2C_STRCMP_PROC
X2C_STRCMP_PROC:
                push    ebx
                push    esi
                mov     ebx,dword [esp+12]
                mov     edx,dword [esp+20]
                mov     esi,dword [esp+16]
                cmp     esi,dword [esp+24]
                jle     short _loop
                mov     esi,dword [esp+24]
_loop:
                cmp     esi,4
                jc      near str_end
                mov     eax,dword [ebx]
                mov     ecx,dword [edx]
                cmp     ecx,eax
                jne     near str_neq
                not     ecx
                add     eax,0FEFEFEFFH
                and     eax,ecx
                and     eax,80808080H
                jne     near str_equal
                cmp     esi,8
                jc      near str_end4
                mov     eax,dword [ebx+4]
                mov     ecx,dword [edx+4]
                cmp     ecx,eax
                jne     near str_neq
                not     ecx
                add     eax,0FEFEFEFFH
                and     eax,ecx
                and     eax,80808080H
                jne     short str_equal
                cmp     esi,12
                jc      near str_end8
                mov     eax,dword [ebx+8]
                mov     ecx,dword [edx+8]
                cmp     ecx,eax
                jne     short str_neq
                not     ecx
                add     eax,0FEFEFEFFH
                and     eax,ecx
                and     eax,80808080H
                jne     short str_equal
                cmp     esi,16
                jc      short str_end12
                mov     eax,dword [ebx+12]
                mov     ecx,dword [edx+12]
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
                je      str_equal
                cmp     ah,ch
                jne     short byte_neq
                cmp     ah,00H
                je      str_equal
                shr     eax,16
                shr     ecx,16
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      str_equal
                cmp     ah,ch
byte_neq:
                pop     esi
                sbb     eax,eax
                pop     ebx
                or      al,01H
                ret

; there are less than 4 bytes left to compare

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

                mov     al,byte [ebx]
                mov     cl,byte [edx]
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      str_equal
                inc     ebx
                inc     edx
                cmp     esi,1
                je      short end_end

                mov     al,byte [ebx]
                mov     cl,byte [edx]
                cmp     al,cl
                jne     short byte_neq
                cmp     al,00H
                je      str_equal
                inc     ebx
                inc     edx
                cmp     esi,2
                je      short end_end

                mov     al,byte [ebx]
                mov     cl,byte [edx]
                cmp     al,cl
                jne     near byte_neq
                cmp     al,00H
                je      str_equal
                inc     ebx
                inc     edx
end_end:
                mov     esi,dword [esp+16]
                cmp     esi,dword [esp+24]
                je      str_equal
                jg      short first_longer
                cmp     byte [edx],0
                je      str_equal
                pop     esi
                mov     eax,-1
                pop     ebx
                ret
first_longer:
                cmp     byte [ebx],0
                je      str_equal
                pop     esi
                mov     eax,1
                pop     ebx
                ret
;X2C_STRCMP_PROC endp

