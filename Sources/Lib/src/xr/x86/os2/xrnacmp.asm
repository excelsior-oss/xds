                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for X2C_adr_lss and X2C-adr_gtr functions

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
;                assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing

;PROCEDURE [2] X2C_adr_lss(x,y: SYSTEM.ADDRESS): BOOLEAN;
                public  X2C_adr_lss
X2C_adr_lss     proc    near
                mov     eax,+4[esp]
                cmp     eax,+8[esp]
                setb    al
                ret
X2C_adr_lss     endp

;PROCEDURE [2] X2C_adr_gtr(x,y: SYSTEM.ADDRESS): BOOLEAN;
                public  X2C_adr_gtr
X2C_adr_gtr     proc    near
                mov     eax,+4[esp]
                cmp     eax,+8[esp]
                seta    al
                ret
X2C_adr_gtr     endp

_TEXT           ends

                end
