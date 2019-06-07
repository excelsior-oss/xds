                .386p
                .387

; COPYRIGHT (c) 1998 XDS Ltd. All Rights Reserved.

ifdef OS2
                .model FLAT
endif

DGROUP          group   _DATA

_DATA           segment use32 dword public 'DATA'
minint64    dd  0,          080000000h
maxint64    dd  0ffffffffh, 07fffffffh
half        dd  0, 0c8h
result      dd  0,0
_DATA           ends

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["StdCall"] X2J_double2long(x: LONGREAL): SYSTEM.INT64;
            public  X2J_double2long
X2J_double2long   proc    near
_X2C_longl:
            fld      qword ptr +4[esp]
            fistp    qword ptr +4[esp]
            mov      eax,+4[esp]
            mov      edx,+8[esp]
            ret      8
X2J_double2long   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;PROCEDURE ["StdCall"] X2J_long2double (VAR i :SYSTEM.INT64) :LONGREAL;
                public  X2J_long2double

X2J_long2double   proc    near

                  mov     eax, [esp+4]
                  fild    qword ptr [eax]
                  ret     4
X2J_long2double   endp


;PROCEDURE ["C"] X2J_TRUNCI64 (x :LONGREAL) :SYSTEM.INT64;
;VAR
;  l :SYSTEM.INT64;
;BEGIN
;  IF    (x < VAL (LONGREAL, MIN (SYSTEM.INT64)) (*- 0.5*)) THEN
;    RETURN MIN (SYSTEM.INT64)
;  ELSIF (x > VAL (LONGREAL, MIN (SYSTEM.INT64)) (*+ 0.5*)) THEN
;    RETURN MAX (SYSTEM.INT64)
;  END;
;
;  l := X2J_double2long (x);
;  IF (x > 0.0) THEN
;    IF VAL(LONGREAL,l)>x THEN DEC(l) END;
;  ELSE
;    IF VAL(LONGREAL,l)<x THEN INC(l) END;
;  END;
;  RETURN i;
;END X2C_TRUNCI;

            public X2J_TRUNCI64
X2J_TRUNCI64 proc near
        ;
            fld     qword ptr +4[esp]
                fild    qword ptr [minint64]
                fcomp    
            fstsw   ax
            test    ax, 0100h
            jnz j1                            ; min >= X - return min;
            mov     edx, dword ptr +4[minint64]
            mov     eax, dword ptr   [minint64]
            mov     dword ptr +4[result], edx
            mov     dword ptr   [result], eax
            jmp return
j1:
               fild    qword ptr [maxint64]
               fcomp
            fstsw   ax
            test    ax, 4500h
            jz j2
            mov     edx, dword ptr +4[maxint64]
            mov     eax, dword ptr   [maxint64]
            mov     dword ptr +4[result], edx
            mov     dword ptr   [result], eax
            jmp return
j2:
            sub     esp, 8
            fst     qword ptr [esp] 
            call    X2J_double2long

            mov     dword ptr +4[result], edx
            mov     dword ptr   [result], eax

            test    edx, 80000000h
            jz  positive

                fild     qword ptr [result]
                fcomp    
            fstsw   ax
            test    ax, 0100h
            jz   return
            add     dword ptr   [result], 1
            adc     dword ptr +4[result], 0
            jmp return
            
positive:   
                fild     qword ptr [result]
                fcomp
            fstsw   ax
            test    ax, 4500h
            jnz   return
            sub     dword ptr   [result], 1
            sbb     dword ptr +4[result], 0
return:
            mov     edx, dword ptr +4[result]
            mov     eax, dword ptr   [result]
            fstp    st
        ret 
X2J_TRUNCI64 endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_INC64 (VAR a: SYSTEM.INT64; b:SYSTEM.INT64): SYSTEM.INT64
            public  X2J_INC64
X2J_INC64   proc    near

incB        EQU     [esp + 12]      ; stack address of b
incA        EQU     [esp + 8]       ; stack address of a

            push    ebx            
            mov     ecx, incA
            mov     eax, dword ptr [ecx]
            mov     edx, dword ptr +4[ecx]
            mov     ebx, incB
            add     dword ptr [ecx], ebx
            mov     ebx, incB+4
            adc     dword ptr +4[ecx], ebx
            pop     ebx
            ret

X2J_INC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_INC64_1 (VAR a: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_INC64_1
X2J_INC64_1   proc    near

addB        EQU     [esp + 12]      ; stack address of b
addA        EQU     [esp + 4]       ; stack address of a

            mov     ecx, addA
            mov     eax, dword ptr [ecx]
            mov     edx, dword ptr +4[ecx]
            add     dword ptr [ecx], 1
            adc     dword ptr +4[ecx], 0
            ret

X2J_INC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_DEC64 (VAR a: SYSTEM.INT64; b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_DEC64
X2J_DEC64   proc    near

            push    ebx
            mov     ecx, incA
            mov     eax, dword ptr [ecx]
            mov     edx, dword ptr +4[ecx]
            mov     ebx, incB
            sub     dword ptr [ecx], ebx
            mov     ebx, incB+4
            sbb     dword ptr +4[ecx], ebx
            pop     ebx
            ret

X2J_DEC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_DEC64_1 (VAR a: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_DEC64_1
X2J_DEC64_1   proc    near

            mov     ecx, addA
            mov     eax, dword ptr [ecx]
            mov     edx, dword ptr +4[ecx]
            sub     dword ptr [ecx], 1
            sbb     dword ptr +4[ecx], 0
            ret

X2J_DEC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREINC64 (a: SYSTEM.INT64; b:SYSTEM.INT64): SYSTEM.INT64
            public  X2J_PREINC64
X2J_PREINC64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            add     eax, addB
            adc     edx, addB+4
            ret


X2J_PREINC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREINC64_1 (a: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_PREINC64_1
X2J_PREINC64_1   proc    near

            mov     eax, addA
            mov     edx, addA+4
            add     eax, 1
            adc     edx, 0
            ret


X2J_PREINC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREDEC64 (a: SYSTEM.INT64; b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_PREDEC64
X2J_PREDEC64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            sub     eax, addB
            sbb     edx, addB+4
            ret

X2J_PREDEC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREDEC64_1 (a: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_PREDEC64_1
X2J_PREDEC64_1   proc    near

            mov     eax, addA
            mov     edx, addA+4
            sub     eax, 1
            sbb     edx, 0
            ret

X2J_PREDEC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_ADD64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_ADD64
X2J_ADD64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            add     eax, addB
            adc     edx, addB+4
            ret     

X2J_ADD64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_SUB64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_SUB64
X2J_SUB64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            sub     eax, addB
            sbb     edx, addB+4
            ret     

X2J_SUB64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_LESS64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            public  X2J_LESS64
X2J_LESS64   proc    near

            mov     edx, addA+4
            cmp     edx, addB+4
            jl      lessyes
            jg      lessno
            mov     eax, addA
            cmp     eax, addB
            jae     lessno
lessyes:
            mov     al,1
            ret
lessno:
            mov     al,0
            ret     

X2J_LESS64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_LEQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            public  X2J_LEQ64
X2J_LEQ64   proc    near

            mov     edx, addA+4
            cmp     edx, addB+4
            jl      leqyes
            jg      leqno
            mov     eax, addA
            cmp     eax, addB
            ja      leqno
leqyes:
            mov     al,1
            ret
leqno:
            mov     al,0
            ret     

X2J_LEQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_GTR64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            public  X2J_GTR64
X2J_GTR64   proc    near

            mov     edx, addA+4
            cmp     edx, addB+4
            jg      gtryes
            jl      gtrno
            mov     eax, addA
            cmp     eax, addB
            jbe     gtrno
gtryes:
            mov     al,1
            ret
gtrno:
            mov     al,0
            ret     

X2J_GTR64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_GEQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            public  X2J_GEQ64
X2J_GEQ64   proc    near

            mov     edx, addA+4
            cmp     edx, addB+4
            jg      geqyes
            jl      geqno
            mov     eax, addA
            cmp     eax, addB
            jb      geqno
geqyes:
            mov     al,1
            ret
geqno:
            mov     al,0
            ret     

X2J_GEQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_NEQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            public  X2J_NEQ64
X2J_NEQ64   proc    near

            mov     edx, addA+4
            cmp     edx, addB+4
            jne      neqyes
            mov     eax, addA
            cmp     eax, addB
            je      neqno
neqyes:
            mov     al,1
            ret
neqno:
            mov     al,0
            ret     

X2J_NEQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_EQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            public  X2J_EQ64
X2J_EQ64   proc    near

            mov     edx, addA+4
            cmp     edx, addB+4
            jne     eqno
            mov     eax, addA
            cmp     eax, addB
            jne     eqno
eqyes:
            mov     al,1
            ret
eqno:
            mov     al,0
            ret     

X2J_EQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_XOR64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_XOR64
X2J_XOR64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            xor     eax, addB
            xor     edx, addB+4
            ret     

X2J_XOR64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_OR64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_OR64
X2J_OR64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            or      eax, addB
            or      edx, addB+4
            ret     

X2J_OR64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_AND64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_AND64
X2J_AND64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            and     eax, addB
            and     edx, addB+4
            ret     

X2J_AND64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_NOT64 (a: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_NOT64
X2J_NOT64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            not     eax
            not     edx
            ret    

X2J_NOT64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_MINUS64 (a: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_MINUS64
X2J_MINUS64   proc    near

            mov     eax, addA
            mov     edx, addA+4
            neg     eax
            adc     edx,0
            neg     edx
            ret     

X2J_MINUS64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_MUL64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_MUL64
X2J_MUL64   proc    near

;       AHI, BHI : upper 32 bits of A and B
;       ALO, BLO : lower 32 bits of A and B
;
;             ALO * BLO
;       ALO * BHI
; +     BLO * AHI
; ---------------------

mulB        EQU     [esp + 12]      ; stack address of b
mulA        EQU     [esp + 4]       ; stack address of a

            mov     eax, mulA+4
            mov     ecx, mulB+4
            or      ecx,eax         ;test for both hiwords zero.
            mov     ecx, mulB
            jnz     short hard      ;both are zero, just mult ALO and BLO

            mov     eax, mulA
            mul     ecx

            ret                   ; callee restores the stack

hard:
            push    ebx

; must redefine A and B since esp has been altered

mulB2       EQU     [esp + 16]      ; stack address of b
mulA2       EQU     [esp + 8]       ; stack address of a

            mul     ecx             ;eax has AHI, ecx has BLO, so AHI * BLO
            mov     ebx,eax         ;save result

            mov     eax, mulA2
            mul     dword ptr mulB2+4 ; ALO * BHI
            add     ebx,eax             ;ebx = ((ALO * BHI) + (AHI * BLO))

            mov     eax, mulA2      ;eax = ALO; ecx = BLO
            mul     ecx             ;so edx:eax = ALO*BLO
            add     edx,ebx         ;now edx has all the LO*HI stuff

            pop     ebx

            ret                   ; callee restores the stack


X2J_MUL64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_DIV64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_DIV64
X2J_DIV64   proc    near

            push    edi
            push    esi
            push    ebx

; Set up the local stack and save the index registers.  When this is done
; the stack frame will look as follows (assuming that the expression a/b will
; generate a call to lldiv(a, b)):
;
;               -----------------
;               |               |
;               |---------------|
;               |               |
;               |--divisor (b)--|
;               |               |
;               |---------------|
;               |               |
;               |--dividend (a)-|
;               |               |
;               |---------------|
;               | return addr** |
;               |---------------|
;               |      EDI      |
;               |---------------|
;               |      ESI      |
;               |---------------|
;       ESP---->|      EBX      |
;               -----------------
;

divB    equ     +24[esp]      ; stack address of divisor (b)  DVSR
divA    equ     +16[esp]      ; stack address of dividend (a)  DVDN


; Determine sign of the result (edi = 0 if result is positive, non-zero
; otherwise) and make operands positive.

            xor     edi,edi         ; result sign assumed positive

            mov     eax, divA+4
            or      eax,eax         ; test to see if signed
            jge     short L1        ; skip rest if a is already positive
            inc     edi             ; complement result sign flag
            mov     edx, divA        ; lo word of a
            neg     eax             ; make a positive
            neg     edx
            sbb     eax,0
            mov     divA+4,eax ; save positive value
            mov     divA,edx
L1:
            mov     eax,divB+4      ; hi word of b
            or      eax,eax         ; test to see if signed
            jge     short L2        ; skip rest if b is already positive
            inc     edi             ; complement the result sign flag
            mov     edx,divB        ; lo word of a
            neg     eax             ; make b positive
            neg     edx
            sbb     eax,0
            mov     divB+4,eax ; save positive value
            mov     divB,edx
L2:

;
; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;
; NOTE - eax currently contains the high order word of DVSR
;

            or      eax,eax         ; check to see if divisor < 4194304K
            jnz     short L3        ; nope, gotta do this the hard way
            mov     ecx,divB        ; load divisor
            mov     eax,divA+4      ; load high word of dividend
            xor     edx,edx
            div     ecx             ; eax <- high order bits of quotient
            mov     ebx,eax         ; save high bits of quotient
            mov     eax,divA        ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; eax <- low order bits of quotient
            mov     edx,ebx         ; edx:eax <- quotient
            jmp     short L4        ; set sign, restore stack and return
            
;
; Here we do it the hard way.  Remember, eax contains the high word of DVSR
;

L3:
            mov     ebx,eax         ; ebx:ecx <- divisor
            mov     ecx,divB
            mov     edx,divA+4      ; edx:eax <- dividend
            mov     eax,divA
L5: 
            shr     ebx,1           ; shift divisor right one bit
            rcr     ecx,1
            shr     edx,1           ; shift dividend right one bit
            rcr     eax,1
            or      ebx,ebx
            jnz     short L5        ; loop until divisor < 4194304K
            div     ecx             ; now divide, ignore remainder
            mov     esi,eax         ; save quotient
            
;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mul     dword ptr divB+4 ; QUOT * HIWORD(DVSR)
            mov     ecx,eax
            mov     eax,divB
            mul     esi             ; QUOT * LOWORD(DVSR)
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      short L6        ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we are ok, otherwise
; subtract one (1) from the quotient.
;

            cmp     edx,divA+4      ; compare hi words of result and original
            ja      short L6        ; if result > original, do subtract
            jb      short L7        ; if result < original, we are ok
            cmp     eax,divA ; hi words are equal, compare lo words
            jbe     short L7        ; if less or equal we are ok, else subtract
L6:
            dec     esi             ; subtract 1 from quotient
L7:
            xor     edx,edx         ; edx:eax <- quotient
            mov     eax,esi

;
; Just the cleanup left to do.  edx:eax contains the quotient.  Set the sign
; according to the save value, cleanup the stack, and return.
;

L4:
            dec     edi             ; check to see if result is negative
            jnz     short L8        ; if EDI == 0, result should be negative
            neg     edx             ; otherwise, negate the result
            neg     eax
            sbb     edx,0

;
; Restore the saved registers and return.
;

L8:
            pop     ebx
            pop     esi
            pop     edi

            ret     

X2J_DIV64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_REM64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_REM64
X2J_REM64   proc    near

            push    ebx
            push    edi

; Set up the local stack and save the index registers.  When this is done
; the stack frame will look as follows (assuming that the expression a%b will
; generate a call to lrem(a, b)):
;
;               -----------------
;               |               |
;               |---------------|
;               |               |
;               |--divisor (b)--|
;               |               |
;               |---------------|
;               |               |
;               |--dividend (a)-|
;               |               |
;               |---------------|
;               | return addr** |
;               |---------------|
;               |       EBX     |
;               |---------------|
;       ESP---->|       EDI     |
;               -----------------
;

remB    equ     [esp + 20]      ; stack address of divisor (b)  DVSR
remA    equ     [esp + 12]      ; stack address of dividend (a) DVND


; Determine sign of the result (edi = 0 if result is positive, non-zero
; otherwise) and make operands positive.

            xor     edi,edi         ; result sign assumed positive

            mov     eax,remA+4      ; hi word of a
            or      eax,eax         ; test to see if signed
            jge     short L_R1        ; skip rest if a is already positive
            inc     edi             ; complement result sign flag bit
            mov     edx,remA        ; lo word of a
            neg     eax             ; make a positive
            neg     edx
            sbb     eax,0
            mov     remA+4,eax ; save positive value
            mov     remA,edx
L_R1:
            mov     eax,remB+4 ; hi word of b
            or      eax,eax         ; test to see if signed
            jge     short L_R2        ; skip rest if b is already positive
            mov     edx,remB ; lo word of b
            neg     eax             ; make b positive
            neg     edx
            sbb     eax,0
            mov     remB+4,eax ; save positive value
            mov     remB,edx
L_R2:

;
; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;
; NOTE - eax currently contains the high order word of DVSR
;

            or      eax,eax         ; check to see if divisor < 4194304K
            jnz     short L_R3        ; nope, gotta do this the hard way
            mov     ecx,remB        ; load divisor
            mov     eax,remA+4      ; load high word of dividend
            xor     edx,edx
            div     ecx             ; edx <- remainder
            mov     eax,remA ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; edx <- final remainder
            mov     eax,edx         ; edx:eax <- remainder
            xor     edx,edx
            dec     edi             ; check result sign flag
            jns     short L_R4        ; negate result, restore stack and return
            jmp     short L_R8        ; result sign ok, restore stack and return

;
; Here we do it the hard way.  Remember, eax contains the high word of DVSR
;

L_R3:
            mov     ebx,eax         ; ebx:ecx <- divisor
            mov     ecx,remB
            mov     edx,remA+4      ; edx:eax <- dividend
            mov     eax,remA
L_R5:
            shr     ebx,1           ; shift divisor right one bit
            rcr     ecx,1
            shr     edx,1           ; shift dividend right one bit
            rcr     eax,1
            or      ebx,ebx
            jnz     short L_R5        ; loop until divisor < 4194304K
            div     ecx             ; now divide, ignore remainder

;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mov     ecx,eax         ; save a copy of quotient in ECX
            mul     dword ptr remB+4
            xchg    ecx,eax         ; save product, get quotient in EAX
            mul     dword ptr remB
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      short L_R6        ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we are ok, otherwise
; subtract the original divisor from the result.
;

            cmp     edx,remA+4      ; compare hi words of result and original
            ja      short L_R6        ; if result > original, do subtract
            jb      short L_R7        ; if result < original, we are ok
            cmp     eax,remA        ; hi words are equal, compare lo words
            jbe     short L_R7        ; if less or equal we are ok, else subtract
L_R6:
            sub     eax,remB        ; subtract divisor from result
            sbb     edx,remB+4
L_R7:

;
; Calculate remainder by subtracting the result from the original dividend.
; Since the result is already in a register, we will do the subtract in the
; opposite direction and negate the result if necessary.
;

            sub     eax,remA        ; subtract dividend from result
            sbb     edx,remA+4

;
; Now check the result sign flag to see if the result is supposed to be positive
; or negative.  It is currently negated (because we subtracted in the 'wrong'
; direction), so if the sign flag is set we are done, otherwise we must negate
; the result to make it positive again.
;

            dec     edi             ; check result sign flag
            jns     short L_R8        ; result is ok, restore stack and return
L_R4:
            neg     edx             ; otherwise, negate the result
            neg     eax
            sbb     edx,0

;
; Just the cleanup left to do.  edx:eax contains the quotient.
; Restore the saved registers and return.
;

L_R8:
            pop     edi
            pop     ebx

            ret     

X2J_REM64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_SHL64 (a: SYSTEM.INT64, b: SYSTEM.INT8): SYSTEM.INT64
            public  X2J_SHL64
X2J_SHL64   proc    near

shlB    equ     [esp + 12]     
shlA    equ     [esp + 4]      

            mov     edx ,shlA+4
            mov     eax ,shlA
            mov     cl, shlB
            and     cl, 3fh

;
; Handle shifts of between 0 and 31 bits
;
            cmp     cl, 32
            jae     short MORE32
            shld    edx,eax,cl
            shl     eax,cl
            ret

;
; Handle shifts of between 32 and 63 bits
;
MORE32:
            mov     edx,eax
            xor     eax,eax
            and     cl,31
            shl     edx,cl
            ret

X2J_SHL64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_SHR64 (a: SYSTEM.INT64, b: SYSTEM.INT8): SYSTEM.INT64
            public  X2J_SHR64
X2J_SHR64   proc    near

shrB    equ     [esp + 12]
shrA    equ     [esp + 4]

            mov     edx,shrA+4
            mov     eax,shrA
            mov     cl,shrB
            and     cl, 3fh
;
;
; Handle shifts of between 0 and 31 bits
;
            cmp     cl, 32
            jae     short MORE32_2
            shrd    eax,edx,cl
            sar     edx,cl
            ret

;
; Handle shifts of between 32 and 63 bits
;
MORE32_2:
            mov     eax,edx
            sar     edx,31
            and     cl,31
            sar     eax,cl
            ret

X2J_SHR64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_UDIV64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_UDIV64
X2J_UDIV64   proc    near



            push    ebx
            push    esi

; Set up the local stack and save the index registers.  When this is done
; the stack frame will look as follows (assuming that the expression a/b will
; generate a call to uldiv(a, b)):
;
;               -----------------
;               |               |
;               |---------------|
;               |               |
;               |--divisor (b)--|
;               |               |
;               |---------------|
;               |               |
;               |--dividend (a)-|
;               |               |
;               |---------------|
;               | return addr** |
;               |---------------|
;               |      EBX      |
;               |---------------|
;       ESP---->|      ESI      |
;               -----------------
;

udivB    equ     +20[esp]      ; stack address of divisor (b)  DVSR
udivA    equ     +12[esp]      ; stack address of dividend (a)  DVDN

;
; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;

            mov     eax,udivB       ; check to see if divisor < 4194304K
            or      eax,eax
            jnz     short UL1        ; nope, gotta do this the hard way
            mov     ecx,udivB+4     ; load divisor
            mov     eax,udivA+4     ; load high word of dividend
            xor     edx,edx
            div     ecx             ; get high order bits of quotient
            mov     ebx,eax         ; save high bits of quotient
            mov     eax,udivA       ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; get low order bits of quotient
            mov     edx,ebx         ; edx:eax <- quotient hi:quotient lo
            jmp     short UL2        ; restore stack and return

;
; Here we do it the hard way.  Remember, eax contains DVSRHI
;

UL1:
            mov     ecx,eax         ; ecx:ebx <- divisor
            mov     ebx,udivB
            mov     edx,udivA+4     ; edx:eax <- dividend
            mov     eax,udivA
UL3:
            shr     ecx,1           ; shift divisor right one bit; hi bit <- 0
            rcr     ebx,1
            shr     edx,1           ; shift dividend right one bit; hi bit <- 0
            rcr     eax,1
            or      ecx,ecx
            jnz     short UL3        ; loop until divisor < 4194304K
            div     ebx             ; now divide, ignore remainder
            mov     esi,eax         ; save quotient

;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mul     dword ptr udivB+4 ; QUOT * HIWORD(DVSR)
            mov     ecx,eax
            mov     eax,udivB
            mul     esi             ; QUOT * LOWORD(DVSR)
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      short UL4        ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we are ok, otherwise
; subtract one (1) from the quotient.
;

            cmp     edx,udivA+4      ; compare hi words of result and original
            ja      short UL4        ; if result > original, do subtract
            jb      short UL5        ; if result < original, we are ok
            cmp     eax,udivA        ; hi words are equal, compare lo words
            jbe     short UL5        ; if less or equal we are ok, else subtract
UL4:
            dec     esi             ; subtract 1 from quotient
UL5:
            xor     edx,edx         ; edx:eax <- quotient
            mov     eax,esi

;
; Just the cleanup left to do.  edx:eax contains the quotient.
; Restore the saved registers and return.
;

UL2:

            pop     esi
            pop     ebx

            ret     

X2J_UDIV64  endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_UREM64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            public  X2J_UREM64
X2J_UREM64   proc    near


            push    ebx

; Set up the local stack and save the index registers.  When this is done
; the stack frame will look as follows (assuming that the expression a%b will
; generate a call to ullrem(a, b)):
;
;               -----------------
;               |               |
;               |---------------|
;               |               |
;               |--divisor (b)--|
;               |               |
;               |---------------|
;               |               |
;               |--dividend (a)-|
;               |               |
;               |---------------|
;               | return addr** |
;               |---------------|
;       ESP---->|      EBX      |
;               -----------------
;

uremB    equ     [esp + 16]      ; stack address of divisor (b)  DVSR
uremA    equ     [esp + 8]      ; stack address of dividend (a) DVND


; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;

            mov     eax,uremB+4     ; check to see if divisor < 4194304K
            or      eax,eax
            jnz     short RL1       ; nope, gotta do this the hard way
            mov     ecx,uremB        ; load divisor
            mov     eax,uremA+4     ; load high word of dividend
            xor     edx,edx
            div     ecx             ; edx <- remainder, eax <- quotient
            mov     eax,uremA       ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; edx <- final remainder
            mov     eax,edx         ; edx:eax <- remainder
            xor     edx,edx
            jmp     short RL2        ; restore stack and return

;
; Here we do it the hard way.  Remember, eax contains DVSRHI
;

RL1:
            mov     ecx,eax         ; ecx:ebx <- divisor
            mov     ebx,uremB
            mov     edx,uremA+4     ; edx:eax <- dividend
            mov     eax,uremA
RL3:     
            shr     ecx,1           ; shift divisor right one bit; hi bit <- 0
            rcr     ebx,1
            shr     edx,1           ; shift dividend right one bit; hi bit <- 0
            rcr     eax,1
            or      ecx,ecx
            jnz     short RL3        ; loop until divisor < 4194304K
            div     ebx             ; now divide, ignore remainder

;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mov     ecx,eax         ; save a copy of quotient in ECX
            mul     dword ptr uremB+4
            xchg    ecx,eax         ; put partial product in ECX, get quotient in EAX
            mul     dword ptr uremB
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      short RL4       ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we're ok, otherwise
; subtract the original divisor from the result.
;

            cmp     edx,uremA+4      ; compare hi words of result and original
            ja      short RL4        ; if result > original, do subtract
            jb      short RL5        ; if result < original, we're ok
            cmp     eax,uremA        ; hi words are equal, compare lo words
            jbe     short RL5        ; if less or equal we're ok, else subtract
RL4:
            sub     eax,uremB       ; subtract divisor from result
            sbb     edx,uremB+4
RL5:

;
; Calculate remainder by subtracting the result from the original dividend.
; Since the result is already in a register, we will perform the subtract in
; the opposite direction and negate the result to make it positive.
;

            sub     eax,uremA       ; subtract original dividend from result
            sbb     edx,uremA+4
            neg     edx             ; and negate it
            neg     eax
            sbb     edx,0

;
; Just the cleanup left to do.  dx:ax contains the remainder.
; Restore the saved registers and return.
;

RL2:

            pop     ebx

            ret     


X2J_UREM64  endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_USHR64 (a: SYSTEM.INT64, b: SYSTEM.INT8): SYSTEM.INT64
            public  X2J_USHR64
X2J_USHR64   proc    near

ushrB    equ     [esp + 12]
ushrA    equ     [esp + 4]

            mov     edx,ushrA+4
            mov     eax,ushrA
            mov     cl,ushrB
            and     cl, 3fh

;
; Handle shifts of 64 bits or more (if shifting 64 bits or more, the result
; depends only on the high order bit of edx).
;
            cmp     cl,64
            jae     short RETZERO_2

;
; Handle shifts of between 0 and 31 bits
;
            cmp     cl, 32
            jae     short MORE32_3
            shrd    eax,edx,cl
            shr     edx,cl
            ret

;
; Handle shifts of between 32 and 63 bits
;
MORE32_3:
            mov     eax,edx
            xor     edx,edx
            and     cl,31
            shr     eax,cl
            ret

;
; return 0 in edx:eax
;
RETZERO_2:
            xor     eax,eax
            xor     edx,edx
            ret     

X2J_USHR64  endp



_TEXT       ends

end

