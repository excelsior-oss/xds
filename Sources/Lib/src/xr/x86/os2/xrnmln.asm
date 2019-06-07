                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for ln & lnl functions

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

;PROCEDURE ["StdCall"] X2C_ln(x: REAL): REAL
                public  X2C_ln
X2C_ln		proc	near
		fld	dword ptr +4[esp]
		fldln2
		fxch    st(1)
		fyl2x
                ret     4h
X2C_ln		endp

;PROCEDURE ["StdCall"] X2C_lnl(x: LONGREAL): LONGREAL
                public  X2C_lnl
X2C_lnl	proc	near
		fld	qword ptr +4[esp]
		fldln2
		fxch    st(1)
		fyl2x
                ret     8h
X2C_lnl         endp

_TEXT		ends

                end
