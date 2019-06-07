; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; Implementation for FPU interface.

                cpu 386
      bits 32


%ifdef SYMANTEC  ; =========== Rename publics
X2C_FLT_USED    equ     _X2C_FLT_USED
%endif

%ifdef OS2
group    DGROUP   _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group    DGROUP   _DATA
      section _DATA  use32  align=16  public 'DATA' 
%endif

                global  X2C_FLT_USED

X2C_FLT_USED:   dd      0
FPP_init:       dd      0
fp_env:         times 28 db 0


%ifdef OS2
      section .text  use32  public  align=4  FLAT  public 'CODE'  
%else
      section .text  use32  public  align=16  public 'CODE'   
%endif


;                assume  cs: .text, ds: DGROUP, gs: nothing, fs: nothing

                global  X2C_ClearFPP
X2C_ClearFPP:
                cmp     byte [X2C_FLT_USED], 0
                jne     DO
                cmp     byte [FPP_init], 0
                je      L1
        DO:     fnstenv [fp_env]
                and     word [fp_env],    0FFC0h      ; Unmask all exceptions
                or      word [fp_env],    00032h      ; Mask precision loss,
                                                        ;  underflow,
                                                        ;  denormalized operand
                and     word [fp_env+4H], 0C700h      ; Clear exceptions,
                                                        ;  reset TOS to 0
                mov     word [fp_env+8H], 0FFFFh      ; Empty stack
                fldenv  [fp_env]
        L1:     ret
;X2C_ClearFPP    endp


; Clear exception flags, empty FP stack & set control word.
; Set FPP control word:
;     - exceptions (except precision loss, underflow and denormalized operand)
;       enabled
;     - presision control set to 11 -- 64 bit extended presision
;     - rounding control set to 00 -- round to nearest or even
;  Control word: 0000001100110010B, 0332H

                global  X2C_InitFPP
X2C_InitFPP:
                cmp     byte [FPP_init], 0
                jne     short L2
                fninit
                push    0332h
                fldcw   [esp]
                pop     eax
                inc     byte [FPP_init]
        L2:     ret
;X2C_InitFPP     endp


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

                global  X2C_InitFPP4NaN
X2C_InitFPP4NaN:
                push    023fh
                fldcw   [esp]
                pop     eax
                ret
;X2C_InitFPP4NaN endp

