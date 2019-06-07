                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for floor & floorl functions

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

;PROCEDURE ["StdCall"] X2C_floor(x: REAL): REAL
                public  X2C_floor
X2C_floor       proc    near
		fld	dword ptr +4[esp]
		frndint
                ret     4h
X2C_floor       endp

;PROCEDURE ["StdCall"] X2C_floorl(x: LONGREAL): LONGREAL
                public  X2C_floorl
X2C_floorl	proc	near
		fld	qword ptr +4[esp]
		frndint
                ret     8h
X2C_floorl	endp

_TEXT		ends

                end
