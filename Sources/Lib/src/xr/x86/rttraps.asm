; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.
; COPYRIGHT (c) 2002 Excelsior LLC. All Rights Reserved.

                cpu 386
                bits 32

%ifdef OS2
group    DGROUP   _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group    DGROUP   _DATA
      section _DATA  use32  align=16  public 'DATA' 
%endif

%ifdef OS2
      section .text  use32  align=4  FLAT  public 'CODE' 
%else
      section .text  use32  align=16  public 'CODE'
%endif


X2C_INDEX       equ     0
X2C_RANGE       equ     1
X2C_NIL         equ     3
X2C_OVERFLOW    equ     5
X2C_DIVISION    equ     6
X2C_FLT_OVERFL  equ     7
X2C_FLT_DIV     equ     8

X2C_USER_BREAK  equ    21               

%ifdef SYMANTEC  ; =========== Rename publics & externs

X2C_ClearFPP    equ     _X2C_ClearFPP
X2C_TRAP_G      equ     _X2C_TRAP_G

X2C_TRAP_INDEX  equ     _X2C_TRAP_INDEX
X2C_TRAP_RANGE  equ     _X2C_TRAP_RANGE
X2C_TRAP_NIL    equ     _X2C_TRAP_NIL
X2C_TRAP_DIV    equ     _X2C_TRAP_DIV
X2C_TRAP_OVERFL equ     _X2C_TRAP_OVERFL

%endif

;                assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing

                global  X2C_TRAP_OVERFL
                global  X2C_TRAP_RANGE
                global  X2C_TRAP_INDEX
                global  X2C_TRAP_NIL
                global  X2C_TRAP_DIV

                global  X2C_TRAP_OVERFL_C
                global  X2C_TRAP_RANGE_C
                global  X2C_TRAP_INDEX_C
                global  X2C_TRAP_NIL_C
                global  X2C_TRAP_DIV_C

                global  X2C_TRAP_FC

                global  X2C_TRAP_BREAK
                global  X2C_TrapFJump

                extern   X2C_TRAP_G
                extern   X2C_TRAP_FC_IMPL
                extern   ExcInfo

StoreExceptionInfo:
                    ; [esp+4] = EIP
                    ; esp+8   = ESP

                    push eax
                    mov  eax, ExcInfo
                    mov  [eax +  4], ebx
                    mov  [eax +  8], ecx
                    mov  [eax + 12], edx
                    mov  [eax + 16], esi
                    mov  [eax + 20], edi
                    mov  [eax + 24], ebp
                    pop  eax

                    mov  ecx, ExcInfo
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
;StoreExceptionInfo  endp

X2C_TRAP_FC:
                call    StoreExceptionInfo
                jmp     X2C_TRAP_FC_IMPL
;X2C_TRAP_FC  endp

X2C_TRAP_RANGE:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_RANGE
                push    eax
                jmp     X2C_TRAP_G
;X2C_TRAP_RANGE  endp

X2C_TRAP_RANGE_C:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_RANGE
                push    eax
                jmp     X2C_TRAP_FC_IMPL
;X2C_TRAP_RANGE_C  endp

X2C_TRAP_OVERFL:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_OVERFLOW
                push    eax
                jmp     X2C_TRAP_G
;X2C_TRAP_OVERFL endp

X2C_TRAP_OVERFL_C:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_OVERFLOW
                push    eax
                jmp     X2C_TRAP_FC_IMPL
;X2C_TRAP_OVERFL_C endp

X2C_TRAP_NIL:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_NIL
                push    eax
                jmp     X2C_TRAP_G
;X2C_TRAP_NIL    endp

X2C_TRAP_NIL_C:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_NIL
                push    eax
                jmp     X2C_TRAP_FC_IMPL
;X2C_TRAP_NIL_C    endp

X2C_TRAP_DIV:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_DIVISION
                push    eax
                jmp     X2C_TRAP_G
;X2C_TRAP_DIV    endp

X2C_TRAP_DIV_C:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_DIVISION
                push    eax
                jmp     X2C_TRAP_FC_IMPL
;X2C_TRAP_DIV_C    endp


X2C_TRAP_INDEX:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_INDEX
                push    eax
                jmp     X2C_TRAP_G
;X2C_TRAP_INDEX  endp

X2C_TRAP_INDEX_C:
                call    StoreExceptionInfo
                pop     eax
                push    byte X2C_INDEX
                push    eax
                jmp     X2C_TRAP_FC_IMPL
;X2C_TRAP_INDEX_C  endp

X2C_TRAP_BREAK:
                pop     eax   ; return address
                pop     eax   ; ErrEIP
                pop     esp   ; ErrESP
                push    byte X2C_USER_BREAK
                push    eax
                jmp     X2C_TRAP_G
;X2C_TRAP_BREAK  endp

                extern   X2C_ClearFPP

X2C_TrapFJump:
                push    eax     ;dword ptr X2C_ErrCode
                push    edx     ;dword ptr X2C_ErrEIP
                call    X2C_ClearFPP
                jmp     X2C_TRAP_G
;X2C_TrapFJump   endp

