                .386p
                .387

ifdef OS2
                .model FLAT
endif

DGROUP          group   _DATA

_DATA           segment use32 dword public 'DATA'
_DATA           ends

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs: _TEXT, ds: DGROUP, ss: DGROUP, gs: nothing, fs: nothing

                public  X2C_modf
X2C_modf        proc    near
                push    ebp
                push    esi
                push    edi
                push    ebx
                mov     esi, 28[esp]
                lea     ebp, 20[esp]
                mov     eax, dword ptr  [ebp]
                mov     edx, dword ptr 4[ebp]
                xchg    ebp, esi
                mov     dword ptr  [ebp], eax
                mov     dword ptr 4[ebp], edx
                mov     ecx, edx
                and     ecx, 7FF00000H
                je      short modf_1
                cmp     ecx, 43300000H
                jb      short modf_2
                xchg    esi, ebp
modf_1:         xor     eax, eax
                mov     dword ptr  [ebp], eax
                mov     dword ptr 4[ebp], eax
                pop     ebx
                pop     edi
                pop     esi
                pop     ebp
                fld     qword ptr 4[esp]
                ret     12
modf_2:         mov     edi, ecx
                shr     ecx, 20
                sub     cx, 03FFH
                jb      short modf_1
                push    edx
                xor     eax, eax
                mov     edx, 0FFF00000H
                cmp     cl, 20
                jbe     short modf_3
                sar     edx, 21
                rcr     eax, 1
                sub     cl, 21
                sar     eax, cl
                jmp     short modf_4
modf_3:         sar     edx, cl
modf_4:         and     dword ptr  [ebp], eax
                and     dword ptr 4[ebp], edx
                not     edx
                not     eax
                mov     ebp, esi
                and     eax, dword ptr  [ebp]
                and     edx, dword ptr 4[ebp]
                xor     esi, esi
                or      esi, eax
                or      esi, edx
                je      short modf_13
                test    edx, 0FFF00000H
                jne     short modf_10
modf_9:         sub     edi, 00100000h
                add     eax, eax
                adc     edx, edx
                test    edx, 0FFF00000H
                je      short modf_9
                jmp     short modf_12
modf_10:        test    edx, 0FFE00000H
                je      short modf_12
modf_11:        add     edi, 00100000h
                shr     edx, 1
                rcr     eax, 1
                rcr     esi, 1
                test    edx, 0FFE00000H
                jne     short modf_11
                adc     esi, esi
                adc     eax, 00000000h
                adc     edx, 00000000h
modf_12:        and     edx, 000FFFFFH
                or      edx, edi
modf_13:        pop     esi
                test    edx, edx
                je      short modf_5
                and     esi, 80000000H
                or      edx, esi
modf_5:         mov     dword ptr  [ebp], eax
                mov     dword ptr 4[ebp], edx
                pop     ebx
                pop     edi
                pop     esi
                pop     ebp
                fld     qword ptr 4[esp]
                ret     0CH
X2C_modf        endp

_TEXT           ends

                end