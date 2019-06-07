                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for exp & expl functions

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

do_exp		proc	near
		fld1
		fldl2e
		fmul    st,st(2)
		fst     st(2)
		fprem
		f2xm1
		faddp   st(1),st
		fscale
		fstp    st(1)
		ret
do_exp		endp

;PROCEDURE ["StdCall"] X2C_exp(x: REAL): REAL
                public  X2C_exp
X2C_exp		proc	near
		fld	dword ptr +4[esp]
		call	do_exp
                ret     4h
X2C_exp		endp

;PROCEDURE ["StdCall"] X2C_expl(x: LONGREAL): LONGREAL
                public  X2C_expl
X2C_expl	proc	near
		fld	qword ptr +4[esp]
		call	do_exp
                ret     8h
X2C_expl	endp

_TEXT		ends

                end
