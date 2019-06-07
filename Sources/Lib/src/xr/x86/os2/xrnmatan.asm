                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for arctan2 & arctan2ll functions

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

;PROCEDURE ["StdCall"] X2C_arctan2(y,x: REAL): REAL;
                public  X2C_arctan2
X2C_arctan2	proc	near
		fld	dword ptr +4[esp]  ; load "y"
		fld	dword ptr +8[esp]  ; load "x"
L0:		fpatan
                ret     8H
X2C_arctan2	endp

;PROCEDURE ["StdCall] X2C_arctan(x: REAL): REAL
                public  X2C_arctan
X2C_arctan		proc	near
		fld	dword ptr +4[esp]
		fld1
                fpatan
                ret     4H
X2C_arctan		endp

;PROCEDURE ["StdCall"] X2C_arctan2l(y,x: LONGREAL): LONGREAL;
                public  X2C_arctan2l
X2C_arctan2l	proc	near
		fld	qword ptr +4[esp]  ; load "y"
		fld	qword ptr +0ch[esp]  ; load "x"
LL0:		fpatan
                ret     10H
X2C_arctan2l	endp

;PROCEDURE [2] X2C_arctanl(x: LONGREAL): LONGREAL
                public  X2C_arctanl
X2C_arctanl	proc	near
		fld	qword ptr +4[esp]
		fld1
                fpatan
                ret     8H
X2C_arctanl	endp

_TEXT		ends

                end
