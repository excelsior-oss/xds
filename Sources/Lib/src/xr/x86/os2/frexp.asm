                .386p
                .387

; COPYRIGHT (c) 1996,99 XDS. All Rights Reserved.

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
                assume  cs: _TEXT, ds: DGROUP, ss: DGROUP, gs: nothing, fs: nothing

                public  X2C_frexp

X2C_frexp       proc    near
                mov     ecx, dword ptr 12[esp]
                mov     edx, dword ptr 8[esp]
                xor     eax, eax
                test    edx, edx
                je      short frexp_ret
                mov     eax, dword ptr 10[esp]
                and     eax, 00007FF0H
                mov     edx, dword ptr 10[esp]
                and     edx, 0000800FH
                sar     eax, 4
                or      edx, 00003FE0H
                sub     eax, 000003FEH
                mov     word ptr 10[esp], dx
frexp_ret:      fld     qword ptr 4[esp]
                mov     dword ptr [ecx], eax
                ret     12
X2C_frexp       endp

_TEXT           ends

                end
