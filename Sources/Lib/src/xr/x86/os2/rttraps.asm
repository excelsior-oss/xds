                .386p

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

ifdef OS2
                .model FLAT
endif

X2C_INDEX       equ     0
X2C_RANGE       equ     1
X2C_NIL         equ     3
X2C_OVERFLOW    equ     5
X2C_DIVISION    equ     6
X2C_FLT_OVERFL  equ     7
X2C_FLT_DIV     equ     8

X2C_USER_BREAK  equ    21               

ifdef SYMANTEC  ; =========== Rename publics & externs

X2C_ClearFPP    equ     _X2C_ClearFPP
X2C_TRAP_G      equ     _X2C_TRAP_G

X2C_TRAP_INDEX  equ     _X2C_TRAP_INDEX
X2C_TRAP_RANGE  equ     _X2C_TRAP_RANGE
X2C_TRAP_NIL    equ     _X2C_TRAP_NIL
X2C_TRAP_DIV    equ     _X2C_TRAP_DIV
X2C_TRAP_OVERFL equ     _X2C_TRAP_OVERFL

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

                public  X2C_TRAP_OVERFL
                public  X2C_TRAP_RANGE
                public  X2C_TRAP_INDEX
                public  X2C_TRAP_NIL
                public  X2C_TRAP_DIV
                public  X2C_TRAP_BREAK
                public  X2C_TrapFJump

                extrn   X2C_TRAP_G: near
                extrn   ExcInfo:    near

StoreExceptionInfo  proc near
                    ; [esp+4] = EIP
                    ; esp+8   = ESP

                    push eax
                    mov  eax, offset ExcInfo
                    mov  [eax +  4], ebx
                    mov  [eax +  8], ecx
                    mov  [eax + 12], edx
                    mov  [eax + 16], esi
                    mov  [eax + 20], edi
                    mov  [eax + 24], ebp
                    pop  eax

                    mov  ecx, offset ExcInfo
                    mov  [ecx], eax

                    mov  eax, [esp+4]
                    mov  [ecx + 32], eax  ; EIP

                    lea  eax, [esp + 8]
                    mov  [ecx + 28], eax  ; ESP

                    push esi
                    push edi
                    mov  esi, eax
                    lea  edi, [ecx + 36]  ; stk
                    mov  ecx, 32
                    rep  movsd
                    pop  esi
                    pop  edi
                    ret
StoreExceptionInfo  endp

X2C_TRAP_RANGE  proc    near
                call    StoreExceptionInfo
                pop     eax
                push    X2C_RANGE
                push    eax
                jmp     X2C_TRAP_G
X2C_TRAP_RANGE  endp

X2C_TRAP_OVERFL proc    near
                call    StoreExceptionInfo
                pop     eax
                push    X2C_OVERFLOW
                push    eax
                jmp     X2C_TRAP_G
X2C_TRAP_OVERFL endp

X2C_TRAP_NIL    proc    near
                call    StoreExceptionInfo
                pop     eax
                push    X2C_NIL
                push    eax
                jmp     X2C_TRAP_G
X2C_TRAP_NIL    endp

X2C_TRAP_DIV    proc    near
                call    StoreExceptionInfo
                pop     eax
                push    X2C_DIVISION
                push    eax
                jmp     X2C_TRAP_G
X2C_TRAP_DIV    endp


X2C_TRAP_INDEX  proc    near
                call    StoreExceptionInfo
                pop     eax
                push    X2C_INDEX
                push    eax
                jmp     X2C_TRAP_G
X2C_TRAP_INDEX  endp

X2C_TRAP_BREAK  proc    near
                pop     eax   ; return address
                pop     eax   ; ErrEIP
                pop     esp   ; ErrESP
                push    X2C_USER_BREAK
                push    eax
                jmp     X2C_TRAP_G
X2C_TRAP_BREAK  endp

                extrn   X2C_ClearFPP: near

X2C_TrapFJump   proc    near
                push    eax     ;dword ptr X2C_ErrCode
                push    edx     ;dword ptr X2C_ErrEIP
                call    X2C_ClearFPP
                jmp     X2C_TRAP_G
X2C_TrapFJump   endp

_TEXT           ends

                end
