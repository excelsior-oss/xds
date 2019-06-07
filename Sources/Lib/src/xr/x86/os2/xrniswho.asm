                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for isWholeNum function

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

;PROCEDURE [2] X2C_isWholeNum(x: LONGREAL): BOOLEAN;
		public	X2C_isWholeNum
X2C_isWholeNum	proc	near
		fld	qword ptr +4[esp]
		frndint
		fcomp	qword ptr +4[esp]
		fstsw	ax
		sahf
                sete    al
                ret
X2C_isWholeNum	endp

;PROCEDURE [2] X2C_isOdd(x: LONGREAL): BOOLEAN;
		public	X2C_isOdd
X2C_isOdd	proc	near
		mov	eax,2
		push	eax
		fild	dword ptr [esp]
		fdivr	qword ptr +8[esp]
		frndint
		sub	esp,8
		fstp	qword ptr [esp]
		fild	dword ptr +8[esp]
		fmul	qword ptr [esp]
		add	esp,0ch
		fcomp	qword ptr +4[esp]
		fstsw	ax
		sahf
                setne   al
		ret
X2C_isOdd	endp

_TEXT		ends
                end
