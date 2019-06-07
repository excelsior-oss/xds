                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

; Implementation for FPU interface.

ifdef SYMANTEC  ; =========== Rename publics
X2C_FLT_USED    equ     _X2C_FLT_USED
endif

ifdef OS2
                .model FLAT
endif

DGROUP          group   _DATA
ifdef OS2
_DATA           segment use32 dword public 'DATA'
else
_DATA           segment use32 para public 'DATA'
endif
                public  X2C_FLT_USED
X2C_FLT_USED    dd      0
FPP_init        dd      0
fp_env          db      28 dup (?)
_DATA           ends

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing

                public  X2C_ClearFPP
X2C_ClearFPP    proc    near
                cmp     byte ptr X2C_FLT_USED, 0
                jne     short DO
                cmp     byte ptr FPP_init, 0
                je      short L1
        DO:     fnstenv fp_env
                and     word ptr fp_env,    0FFC0h      ; Unmask all exceptions
                or      word ptr fp_env,    00032h      ; Mask precision loss,
                                                        ;  underflow,
                                                        ;  denormalized operand
                and     word ptr fp_env+4H, 0C700h      ; Clear exceptions,
                                                        ;  reset TOS to 0
                mov     word ptr fp_env+8H, 0FFFFh      ; Empty stack
                fldenv  fp_env
        L1:     ret
X2C_ClearFPP    endp

; Clear exception flags, empty FP stack & set control word.
; Set FPP control word:
;     - exceptions (except precision loss, underflow and denormalized operand)
;       enabled
;     - presision control set to 11 -- 64 bit extended presision
;     - rounding control set to 00 -- round to nearest or even
;  Control word: 0000001100110010B, 0332H

                public  X2C_InitFPP
X2C_InitFPP     proc    near
                cmp     byte ptr FPP_init, 0
                jne     short L2
                fninit
                push    0332h
                fldcw   [esp]
                pop     eax
                inc     byte ptr FPP_init
        L2:     ret
X2C_InitFPP     endp


; Set FPP control word:
;    
; - exceptions disabled (it conforms to Java spec): 
;
;                        precision loss
;                        underflow
;                        overflow
;                        denormalized operand
;                        zero divide
;
; - the other exceptions are enabled
;
; - presision control set to 10 -- 53 bit double precision
; - rounding control set to 00 -- round to nearest or even
;
;  Control word: 0000_0010_0011_1111B

                public  X2C_InitFPP4NaN
X2C_InitFPP4NaN proc    near
                push    023fh
                fldcw   [esp]
                pop     eax
                ret
X2C_InitFPP4NaN endp


_TEXT           ends

                end
