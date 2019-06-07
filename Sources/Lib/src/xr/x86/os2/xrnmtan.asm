                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for tan & tanl functions

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

;PROCEDURE ["StdCall"] X2C_tan(x: REAL): REAL
                public  X2C_tan
X2C_tan		proc	near
		fld	dword ptr +4[esp]
		fptan
                fstsw     ax
                test      ah, 004H
                jz        X2C_tan_L1 
                fld1                       ; tan argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      ST(1)
                fxch      ST(1)
X2C_tan_L2:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_tan_L2 
                fstp      ST(1)
                fptan     
X2C_tan_L1:
		fstp	st(0)
                ret     4h
X2C_tan		endp

;PROCEDURE ["StdCall"] X2C_tanl(x: LONGREAL): LONGREAL
                public  X2C_tanl
X2C_tanl	proc	near
_X2C_tanl:
		fld	qword ptr +4[esp]
                fptan     
                fstsw     ax
                test      ah, 004H
                jz        X2C_tanl_L4 
                fld1                       ; tan argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      ST(1)
                fxch      ST(1)
X2C_tanl_L3:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_tanl_L3 
                fstp      ST(1)
                fptan     
X2C_tanl_L4:
		fstp	st(0)
                ret     8h
X2C_tanl	endp

_TEXT		ends

                end
