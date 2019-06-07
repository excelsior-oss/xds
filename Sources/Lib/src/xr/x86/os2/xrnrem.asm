                .386p
                .387

; COPYRIGHT (c) 1998 XDS Ltd. All Rights Reserved.

; Implementation for rem & reml functions

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

;PROCEDURE ["StdCall"] X2C_rem (x, y: REAL): REAL

                public  X2C_rem
X2C_rem         proc    near
		fld	dword ptr +8[esp]  ;arg2
		fld	dword ptr +4[esp]  ;arg1
                fprem     
X2C_rem_More:
                fprem                       ;get partial remainder
                fstsw   ax
                test    ah, 004H            ;C2 = 0 if reduction is complete
                jnz     X2C_rem_More        ;reduction is incomplete 
                fstp    ST(1)
                ret     8h
X2C_rem         endp


;PROCEDURE ["StdCall"] X2C_reml (x, y: LONGREAL): LONGREAL

                public  X2C_reml
X2C_reml	proc	near
		fld	qword ptr +12[esp]  ;arg2
		fld	qword ptr +4[esp]   ;arg1
X2C_reml_More:
                fprem                       ;get partial remainder
                fstsw   ax
                test    ah, 004H            ;C2 = 0 if reduction is complete
                jnz     X2C_reml_More       ;reduction is incomplete 
                fstp    ST(1)
                ret     10h
X2C_reml	endp

_TEXT		ends

                end
