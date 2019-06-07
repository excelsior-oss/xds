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

;PROCEDURE X2C_doROTL(x: BITSET; bits: CARDINAL): BITSET;
		public	X2C_doROTL
X2C_doROTL	proc	near
		push	ecx
		mov	eax,+8h[esp]
		mov	ecx,+0ch[esp]
		shl	eax,cl
		pop	ecx
		ret
X2C_doROTL	endp

;PROCEDURE X2C_doROTR(x: BITSET; bits: CARDINAL): BITSET;
		public	X2C_doROTR
X2C_doROTR	proc	near
		push	ecx
		mov	eax,+8h[esp]
		mov	ecx,+0ch[esp]
		shr	eax,cl
		pop	ecx
		ret
X2C_doROTR	endp

;PROCEDURE X2C_doASHR(x: BITSET; bits: CARDINAL): BITSET;
                public  X2C_doASHR
X2C_doASHR	proc	near
		push	ecx
		mov	eax,+8h[esp]
		mov	ecx,+0ch[esp]
		sar	eax,cl
		pop	ecx
		ret
X2C_doASHR	endp

_TEXT		ends
                end
