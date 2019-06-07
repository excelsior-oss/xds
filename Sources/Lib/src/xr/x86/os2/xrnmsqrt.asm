                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for sqrt & sqrtl functions

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

;PROCEDURE ["StdCall"] X2C_sqrt(x: REAL): REAL
                public  X2C_sqrt
X2C_sqrt        proc    near
		fld	dword ptr +4[esp]
		fsqrt
                ret     4h
X2C_sqrt        endp

;PROCEDURE ["StdCall"] X2C_sqrtl(x: LONGREAL): LONGREAL
                public  X2C_sqrtl
X2C_sqrtl	proc	near
		fld	qword ptr +4[esp]
		fsqrt
                ret     8h
X2C_sqrtl	endp

_TEXT		ends

                end
