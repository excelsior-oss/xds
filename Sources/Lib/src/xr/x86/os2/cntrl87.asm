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

                public  X2C_controlfp

X2C_controlfp   proc    near
                mov     eax, dword ptr 8[esp]
                push    ecx
                fstcw   [esp]
                wait
                test    eax, eax
                jz      short control87_ret
                mov     ecx, eax
                not     ecx
                and     dword ptr [esp], ecx
                and     eax, dword ptr 8[esp]
                or      dword ptr [esp], eax
                fldcw   [esp]
                wait
                fstcw   [esp]
                wait
control87_ret:  xor     eax, eax
                mov     ax, word ptr [esp]
                pop     ecx
                ret     8
X2C_controlfp   endp

_TEXT           ends

                end
