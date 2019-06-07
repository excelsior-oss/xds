                .386p

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

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

;PROCEDURE ["C"] X2C_StackTop(): SYSTEM.ADDRESS;
                public  X2C_StackTop
X2C_StackTop    proc    near
		mov	eax,esp
		sub	eax,4
		ret
X2C_StackTop    endp

_TEXT           ends

                end
