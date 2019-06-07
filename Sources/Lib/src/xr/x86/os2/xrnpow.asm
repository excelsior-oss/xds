                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; jump to X2C_EXPRR function named "pow".

ifdef SYMANTEC  ; =========== Rename publics
pow             equ     _pow
endif

ifdef OS2
                .model FLAT
endif

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
                assume  cs: _TEXT

                public  pow
                extrn   X2C_EXPRR: near
pow             proc    near
                jmp     X2C_EXPRR
pow             endp

_TEXT           ends

                end
