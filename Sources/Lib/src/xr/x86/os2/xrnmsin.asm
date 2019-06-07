                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for sin & sinl functions

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

;PROCEDURE ["StdCall"] X2C_sin(x: REAL): REAL
                public  X2C_sin
X2C_sin		proc	near
		fld	dword ptr +4[esp]
                fsin      
                fstsw     ax              
                test      ah, 004H
                jz        X2C_sin_L2
                fld1                      ; sin argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      ST(1)
                fxch      ST(1)
X2C_sin_L1:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_sin_L1
                fstp      ST(1)
                fsin      
X2C_sin_L2:
                ret     4h
X2C_sin		endp

;PROCEDURE ["StdCall"] X2C_sinl(x: LONGREAL): LONGREAL
                public  X2C_sinl
X2C_sinl	proc	near
		fld	qword ptr +4[esp]
                fsin      
                fstsw     ax              
                test      ah, 004H
                jz        X2C_sinl_L4
                fld1                      ; sin argument is out of -2^63..+2^63
                fldpi     
                fscale    
                fstp      ST(1)
                fxch      ST(1)
X2C_sinl_L3:
                fprem1    
                fstsw     ax
                test      ah, 004H
                jnz       X2C_sinl_L3
                fstp      ST(1)
                fsin      
X2C_sinl_L4:
                ret     8h
X2C_sinl	endp


_TEXT		ends

                end
