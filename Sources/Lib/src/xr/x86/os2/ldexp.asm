                .386p
                .387

ifdef OS2
                .model FLAT
endif

realValueException equ  7

DGROUP          group   _DATA

_DATA           segment use32 dword public 'DATA'
_DATA           ends

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs: _TEXT, ds: DGROUP, ss: DGROUP, gs: nothing, fs: nothing

                extrn   X2C_TRAP_F: near
                public  X2C_ldexp

X2C_ldexp       proc    near
                mov     eax, dword ptr 8[esp]
                test    eax, eax
                je      near ptr ldexp_3
                mov     eax, dword ptr 10[esp]
                mov     edx, dword ptr 12[esp]
                and     eax, 00007FF0H
                sar     eax, 4
                cmp     edx, 00003E80H
                jle     short ldexp_1
                mov     edx, 00003E80H
ldexp_1:        cmp     edx, 0FFFFC180H
                jge     short ldexp_2
                mov     edx, 0FFFFC180H
ldexp_2:        add     eax, edx
                jle     short ldexp_5
                cmp     eax,000007FFH
                jge     short ldexp_4
                mov     edx, dword ptr 10[esp]
                and     edx, 0000800FH
                shl     eax, 4
                or      eax, edx
                mov     word ptr 10[esp], ax
ldexp_3:        fld     qword ptr 4[esp]
                ret     12
ldexp_4:        push    realValueException
                call    X2C_TRAP_F
ldexp_5:        fldz
                ret     12
X2C_ldexp       endp

_TEXT           ends

                end
