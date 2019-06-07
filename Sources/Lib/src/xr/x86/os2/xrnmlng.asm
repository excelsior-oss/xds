                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for long, ulong, longl & ulongl functions

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

;PROCEDURE ["StdCall"] X2C_long(x: REAL): INTEGER;
                public  X2C_long
X2C_long        proc    near
		fld	dword ptr +4[esp]
                fistp   dword ptr +4[esp]
		mov	eax,+4[esp]
                ret     4h
X2C_long        endp

;PROCEDURE ["StdCall"] X2C_longl(x: LONGREAL): INTEGER;
                public  X2C_longl
X2C_longl	proc	near
_X2C_longl:
		fld	qword ptr +4[esp]
                fistp   dword ptr +4[esp]
		mov	eax,+4[esp]
                ret     8h
X2C_longl	endp

;PROCEDURE ["StdCall"] X2C_ulong(x: REAL): CARDINAL;
                public  X2C_ulong
X2C_ulong       proc    near
		fld	dword ptr +4[esp]
                sub     esp,8
                fistp   qword ptr [esp]
		mov	eax,[esp]
		add	esp,8
                ret     4h
X2C_ulong       endp

;PROCEDURE ["StdCall"] X2C_ulongl(x: LONGREAL): CARDINAL;
                public  X2C_ulongl
X2C_ulongl	proc	near
		fld	qword ptr +4[esp]
                sub     esp,8
                fistp   qword ptr [esp]
		mov	eax,[esp]
		add	esp,8
                ret     8h
X2C_ulongl	endp

_TEXT		ends

                end
