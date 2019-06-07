; COPYRIGHT (c) 1998,2002 XDS Ltd. All Rights Reserved.

                cpu 386
      bits 32

%ifdef OS2
group       DGROUP _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group       DGROUP _DATA
      section _DATA  use32  align=4  public 'DATA' 
%endif


minint64:   dd  0,          080000000h
maxint64:   dd  0ffffffffh, 07fffffffh
half:       dd  0, 0c8h
result:     dd  0,0

delta:      dd 5f800000h ; (float)(2^64)

; Extended precision FPP control word:
;   - ALL exceptions disabled
;   - precision control set to 11 -- 64 bit extended precision
;   - rounding control set to 00 -- round to nearest or even
; Control word: 0000_0011_0011_1111B

FPPCW_extended:      dd    033fh



%ifdef OS2
      section .text  use32  align=4  FLAT  public 'CODE' 
%else
      section .text  use32  align=16  public 'CODE'
%endif

;                assume  cs: .text, ds: DGROUP, gs: nothing, fs: nothing


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["StdCall"] X2J_double2long(x: LONGREAL): SYSTEM.INT64;
            global  X2J_double2long
X2J_double2long:
_X2C_longl:
            fld      qword [esp+4]
            fistp    qword [esp+4]
            mov      eax, [esp+4]
            mov      edx, [esp+8]
            ret      8
;X2J_double2long   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;PROCEDURE ["StdCall"] X2J_long2double (VAR i :SYSTEM.INT64) :LONGREAL;
            global  X2J_long2double

X2J_long2double:

            mov     eax, [esp+4]
            fild    qword [eax]
            ret     4
;X2J_long2double   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["StdCall"] X2J_ulong2double (VAR v: SYSTEM.CARD64): LONGREAL;
            global  X2J_ulong2double

X2J_ulong2double:
            mov     eax, [esp+4]
            fild    qword [eax]
            cmp     dword [eax+4], 0
            jnl     lbl_ulong2double_positive
            ; save current control word
            push    eax
            fstcw   [esp]
            ; set extended precision
            fldcw   [FPPCW_extended]
            ; do
            fld     dword [delta]
            fadd
            ; restore current control word
            fldcw   [esp]
            pop     eax
lbl_ulong2double_positive:
            ret         4   
;X2J_ulong2double   endp


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

            global X2J_TRUNCI64
X2J_TRUNCI64:
        ;
            fld     qword [esp+4]
            fild    qword [minint64]
            fcomp   st1    
            fnstsw  ax
            test    ax, 0100h
            jnz j1                            ; min >= X - return min;
            mov     edx, dword [minint64+4]
            mov     eax, dword [minint64]
            mov     dword [result+4], edx
            mov     dword [result], eax
            jmp return
j1:
            fild    qword [maxint64]
            fcomp   st1
            fnstsw  ax
            test    ax, 4500h
            jz j2
            mov     edx, dword [maxint64+4]
            mov     eax, dword [maxint64]
            mov     dword [result+4], edx
            mov     dword [result], eax
            jmp short return
j2:
            sub     esp, 8
            fst     qword [esp] 
            call    X2J_double2long

            mov     dword [result+4], edx
            mov     dword [result], eax

            test    edx, 80000000h
            jz  short positive

            fild     qword [result]
            fcomp   st1
            fnstsw  ax
            test    ax, 0100h
            jz   short return
            add     dword [result], 1
            adc     dword [result+4], 0
            jmp short return
            
positive:   
            fild     qword [result]
            fcomp   st1
            fnstsw  ax
            test    ax, 4500h
            jnz   short return
            sub     dword [result], 1
            sbb     dword [result+4], 0
return:
            mov     edx, dword [result+4]
            mov     eax, dword [result]
            fstp    st0
            ret 
;X2J_TRUNCI64 endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_INC64 (VAR a: SYSTEM.INT64; b:SYSTEM.INT64): SYSTEM.INT64
            global  X2J_INC64
X2J_INC64:

%define incB_ [esp + 12]      ; stack address of b
%define incA_ [esp + 8]       ; stack address of a

%define incB(x) [esp + 12 + x]
%define incA(x) [esp + 8 + x]

            push    ebx            
            mov     ecx, incA_
            mov     eax, dword [ecx]
            mov     edx, dword [ecx+4]
            mov     ebx, incB_
            add     dword [ecx], ebx
            mov     ebx, incB(4)
            adc     dword [ecx+4], ebx
            pop     ebx
            ret

;X2J_INC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_INC64_1 (VAR a: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_INC64_1
X2J_INC64_1:

%define addB_ [esp + 12]      ; stack address of b
%define addA_ [esp + 4]       ; stack address of a

%define addB(x) [esp + 12 + x]
%define addA(x) [esp + 4 + x]

            mov     ecx, addA_
            mov     eax, dword [ecx]
            mov     edx, dword [ecx+4]
            add     dword [ecx], 1
            adc     dword [ecx+4], 0
            ret

;X2J_INC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_DEC64 (VAR a: SYSTEM.INT64; b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_DEC64
X2J_DEC64:

            push    ebx
            mov     ecx, incA_
            mov     eax, dword [ecx]
            mov     edx, dword [ecx+4]
            mov     ebx, incB_
            sub     dword [ecx], ebx
            mov     ebx, incB(4)
            sbb     dword [ecx+4], ebx
            pop     ebx
            ret

;X2J_DEC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_DEC64_1 (VAR a: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_DEC64_1
X2J_DEC64_1:

            mov     ecx, addA_
            mov     eax, dword [ecx]
            mov     edx, dword [ecx+4]
            sub     dword [ecx], 1
            sbb     dword [ecx+4], 0
            ret

;X2J_DEC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREINC64 (a: SYSTEM.INT64; b:SYSTEM.INT64): SYSTEM.INT64
            global  X2J_PREINC64
X2J_PREINC64:

            mov     eax, addA_
            mov     edx, addA(4)
            add     eax, addB_
            adc     edx, addB(4)
            ret

;X2J_PREINC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREINC64_1 (a: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_PREINC64_1
X2J_PREINC64_1:

            mov     eax, addA_
            mov     edx, addA(4)
            add     eax, 1
            adc     edx, 0
            ret

;X2J_PREINC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREDEC64 (a: SYSTEM.INT64; b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_PREDEC64
X2J_PREDEC64:

            mov     eax, addA_
            mov     edx, addA(4)
            sub     eax, addB_
            sbb     edx, addB(4)
            ret

;X2J_PREDEC64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_PREDEC64_1 (a: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_PREDEC64_1
X2J_PREDEC64_1:

            mov     eax, addA_
            mov     edx, addA(4)
            sub     eax, 1
            sbb     edx, 0
            ret

;X2J_PREDEC64_1   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_ADD64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_ADD64
X2J_ADD64:

            mov     eax, addA_
            mov     edx, addA(4)
            add     eax, addB_
            adc     edx, addB(4)
            ret     

;X2J_ADD64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_SUB64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_SUB64
X2J_SUB64:

            mov     eax, addA_
            mov     edx, addA(4)
            sub     eax, addB_
            sbb     edx, addB(4)
            ret     

;X2J_SUB64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_LESS64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            global  X2J_LESS64
X2J_LESS64:

            mov     edx, addA(4)
            cmp     edx, addB(4)
            jl      lessyes
            jg      lessno
            mov     eax, addA_
            cmp     eax, addB_
            jae     lessno
lessyes:
            mov     al, 1
            ret
lessno:
            mov     al, 0
            ret     

;X2J_LESS64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_LEQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            global  X2J_LEQ64
X2J_LEQ64:

            mov     edx, addA(4)
            cmp     edx, addB(4)
            jl      leqyes
            jg      leqno
            mov     eax, addA_
            cmp     eax, addB_
            ja      leqno
leqyes:
            mov     al, 1
            ret
leqno:
            mov     al, 0
            ret     

;X2J_LEQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_GTR64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            global  X2J_GTR64
X2J_GTR64:

            mov     edx, addA(4)
            cmp     edx, addB(4)
            jg      gtryes
            jl      gtrno
            mov     eax, addA_
            cmp     eax, addB_
            jbe     gtrno
gtryes:
            mov     al, 1
            ret
gtrno:
            mov     al, 0
            ret     

;X2J_GTR64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_GEQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            global  X2J_GEQ64
X2J_GEQ64:

            mov     edx, addA(4)
            cmp     edx, addB(4)
            jg      geqyes
            jl      geqno
            mov     eax, addA_
            cmp     eax, addB_
            jb      geqno
geqyes:
            mov     al, 1
            ret
geqno:
            mov     al, 0
            ret     

;X2J_GEQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_NEQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            global  X2J_NEQ64
X2J_NEQ64:

            mov     edx, addA(4)
            cmp     edx, addB(4)
            jne      neqyes
            mov     eax, addA_
            cmp     eax, addB_
            je      neqno
neqyes:
            mov     al, 1
            ret
neqno:
            mov     al, 0
            ret     

;X2J_NEQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_EQ64 (a: SYSTEM.INT64, b: SYSTEM.INT64): BOOLEAN
            global  X2J_EQ64
X2J_EQ64:

            mov     edx, addA(4)
            cmp     edx, addB(4)
            jne     eqno
            mov     eax, addA_
            cmp     eax, addB_
            jne     eqno
eqyes:
            mov     al, 1
            ret
eqno:
            mov     al, 0
            ret     

;X2J_EQ64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_XOR64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_XOR64
X2J_XOR64:

            mov     eax, addA_
            mov     edx, addA(4)
            xor     eax, addB_
            xor     edx, addB(4)
            ret     

;X2J_XOR64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_OR64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_OR64
X2J_OR64:

            mov     eax, addA_
            mov     edx, addA(4)
            or      eax, addB_
            or      edx, addB(4)
            ret     

;X2J_OR64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_AND64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_AND64
X2J_AND64:

            mov     eax, addA_
            mov     edx, addA(4)
            and     eax, addB_
            and     edx, addB(4)
            ret     

;X2J_AND64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_NOT64 (a: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_NOT64
X2J_NOT64:

            mov     eax, addA_
            mov     edx, addA(4)
            not     eax
            not     edx
            ret    

;X2J_NOT64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_MINUS64 (a: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_MINUS64
X2J_MINUS64:

            mov     eax, addA_
            mov     edx, addA(4)
            neg     eax
            adc     edx,0
            neg     edx
            ret     

;X2J_MINUS64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_MUL64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_MUL64
X2J_MUL64:

;       AHI, BHI : upper 32 bits of A and B
;       ALO, BLO : lower 32 bits of A and B
;
;             ALO * BLO
;       ALO * BHI
; +     BLO * AHI
; ---------------------

%define mulB_ [esp + 12]      ; stack address of b
%define mulA_ [esp + 4]       ; stack address of a

%define mulB(x) [esp + 12 + x]
%define mulA(x) [esp + 4 + x]

            mov     eax, mulA(4)
            mov     ecx, mulB(4)
            or      ecx,eax         ;test for both hiwords zero.
            mov     ecx, mulB_
            jnz     hard      ;both are zero, just mult ALO and BLO

            mov     eax, mulA_
            mul     ecx

            ret                   ; callee restores the stack

hard:
            push    ebx

; must redefine A and B since esp has been altered

%define mulB2_ [esp + 16]      ; stack address of b
%define mulA2_ [esp + 8]       ; stack address of a
%define mulB2(x) [esp + 16 + x]
%define mulA2(x) [esp + 8 + x]

            mul     ecx             ;eax has AHI, ecx has BLO, so AHI * BLO
            mov     ebx,eax         ;save result

            mov     eax, mulA2_
            mul     dword mulB2(4) ; ALO * BHI
            add     ebx,eax             ;ebx = ((ALO * BHI) + (AHI * BLO))

            mov     eax, mulA2_      ;eax = ALO; ecx = BLO
            mul     ecx             ;so edx:eax = ALO*BLO
            add     edx,ebx         ;now edx has all the LO*HI stuff

            pop     ebx

            ret                   ; callee restores the stack

;X2J_MUL64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_DIV64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_DIV64
X2J_DIV64:

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

%define divB_ [esp+24]      ; stack address of divisor (b)  DVSR
%define divA_ [esp+16]      ; stack address of dividend (a)  DVDN
%define divB(x) [esp+24+x]
%define divA(x) [esp+16+x]


; Determine sign of the result (edi = 0 if result is positive, non-zero
; otherwise) and make operands positive.

            xor     edi,edi         ; result sign assumed positive

            mov     eax, divA(4)
            or      eax,eax         ; test to see if signed
            jge     L1              ; skip rest if a is already positive
            inc     edi             ; complement result sign flag
            mov     edx, divA_       ; lo word of a
            neg     eax             ; make a positive
            neg     edx
            sbb     eax,0
            mov     divA(4),eax      ; save positive value
            mov     divA_,edx
L1:
            mov     eax,divB(4)     ; hi word of b
            or      eax,eax         ; test to see if signed
            jge     L2              ; skip rest if b is already positive
            inc     edi             ; complement the result sign flag
            mov     edx,divB_        ; lo word of a
            neg     eax             ; make b positive
            neg     edx
            sbb     eax,0
            mov     divB(4),eax ; save positive value
            mov     divB_,edx
L2:

;
; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;
; NOTE - eax currently contains the high order word of DVSR
;

            or      eax,eax         ; check to see if divisor < 4194304K
            jnz     L3              ; nope, gotta do this the hard way
            mov     ecx,divB_        ; load divisor
            mov     eax,divA(4)      ; load high word of dividend
            xor     edx,edx
            div     ecx             ; eax <- high order bits of quotient
            mov     ebx,eax         ; save high bits of quotient
            mov     eax,divA_        ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; eax <- low order bits of quotient
            mov     edx,ebx         ; edx:eax <- quotient
            jmp     short L4        ; set sign, restore stack and return
            
;
; Here we do it the hard way.  Remember, eax contains the high word of DVSR
;

L3:
            mov     ebx,eax         ; ebx:ecx <- divisor
            mov     ecx,divB_
            mov     edx,divA(4)      ; edx:eax <- dividend
            mov     eax,divA_
L5: 
            shr     ebx,1           ; shift divisor right one bit
            rcr     ecx,1
            shr     edx,1           ; shift dividend right one bit
            rcr     eax,1
            or      ebx,ebx
            jnz     L5              ; loop until divisor < 4194304K
            div     ecx             ; now divide, ignore remainder
            mov     esi,eax         ; save quotient
            
;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mul     dword divB(4) ; QUOT * HIWORD(DVSR)
            mov     ecx,eax
            mov     eax,divB_
            mul     esi             ; QUOT * LOWORD(DVSR)
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      L6              ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we are ok, otherwise
; subtract one (1) from the quotient.
;

            cmp     edx,divA(4)      ; compare hi words of result and original
            ja      L6        ; if result > original, do subtract
            jb      L7        ; if result < original, we are ok
            cmp     eax,divA_ ; hi words are equal, compare lo words
            jbe     L7        ; if less or equal we are ok, else subtract
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
            jnz     L8        ; if EDI == 0, result should be negative
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

;X2J_DIV64   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_REM64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_REM64
X2J_REM64:

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

%define remB_ [esp + 20]      ; stack address of divisor (b)  DVSR
%define remA_ [esp + 12]      ; stack address of dividend (a) DVND
%define remB(x) [esp + 20 + x]
%define remA(x) [esp + 12 + x]


; Determine sign of the result (edi = 0 if result is positive, non-zero
; otherwise) and make operands positive.

            xor     edi,edi         ; result sign assumed positive

            mov     eax,remA(4)     ; hi word of a
            or      eax,eax         ; test to see if signed
            jge     L_R1        ; skip rest if a is already positive
            inc     edi             ; complement result sign flag bit
            mov     edx,remA_       ; lo word of a
            neg     eax             ; make a positive
            neg     edx
            sbb     eax,0
            mov     remA(4),eax ; save positive value
            mov     remA_,edx
L_R1:
            mov     eax,remB(4)     ; hi word of b
            or      eax,eax         ; test to see if signed
            jge     L_R2        ; skip rest if b is already positive
            mov     edx,remB_       ; lo word of b
            neg     eax             ; make b positive
            neg     edx
            sbb     eax,0
            mov     remB(4),eax ; save positive value
            mov     remB_,edx
L_R2:

;
; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;
; NOTE - eax currently contains the high order word of DVSR
;

            or      eax,eax         ; check to see if divisor < 4194304K
            jnz     L_R3        ; nope, gotta do this the hard way
            mov     ecx,remB_       ; load divisor
            mov     eax,remA(4)     ; load high word of dividend
            xor     edx,edx
            div     ecx             ; edx <- remainder
            mov     eax,remA_ ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; edx <- final remainder
            mov     eax,edx         ; edx:eax <- remainder
            xor     edx,edx
            dec     edi             ; check result sign flag
            jns     L_R4        ; negate result, restore stack and return
            jmp     short L_R8  ; result sign ok, restore stack and return

;
; Here we do it the hard way.  Remember, eax contains the high word of DVSR
;

L_R3:
            mov     ebx,eax         ; ebx:ecx <- divisor
            mov     ecx,remB_
            mov     edx,remA(4)      ; edx:eax <- dividend
            mov     eax,remA_
L_R5:
            shr     ebx,1           ; shift divisor right one bit
            rcr     ecx,1
            shr     edx,1           ; shift dividend right one bit
            rcr     eax,1
            or      ebx,ebx
            jnz     L_R5        ; loop until divisor < 4194304K
            div     ecx             ; now divide, ignore remainder

;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mov     ecx,eax         ; save a copy of quotient in ECX
            mul     dword remB(4)
            xchg    ecx,eax         ; save product, get quotient in EAX
            mul     dword remB_
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      L_R6        ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we are ok, otherwise
; subtract the original divisor from the result.
;

            cmp     edx,remA(4)      ; compare hi words of result and original
            ja      L_R6        ; if result > original, do subtract
            jb      L_R7        ; if result < original, we are ok
            cmp     eax,remA_        ; hi words are equal, compare lo words
            jbe     L_R7        ; if less or equal we are ok, else subtract
L_R6:
            sub     eax,remB_        ; subtract divisor from result
            sbb     edx,remB(4)
L_R7:

;
; Calculate remainder by subtracting the result from the original dividend.
; Since the result is already in a register, we will do the subtract in the
; opposite direction and negate the result if necessary.
;

            sub     eax,remA_        ; subtract dividend from result
            sbb     edx,remA(4)

;
; Now check the result sign flag to see if the result is supposed to be positive
; or negative.  It is currently negated (because we subtracted in the 'wrong'
; direction), so if the sign flag is set we are done, otherwise we must negate
; the result to make it positive again.
;

            dec     edi             ; check result sign flag
            jns     L_R8        ; result is ok, restore stack and return
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

;X2J_REM64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_SHL64 (a: SYSTEM.INT64, b: SYSTEM.INT8): SYSTEM.INT64
            global  X2J_SHL64
X2J_SHL64:

%define shlB_ [esp + 12]     
%define shlA_ [esp + 4]      
%define shlB(x) [esp + 12 + x]
%define shlA(x) [esp + 4 + x]

            mov     edx, shlA(4)
            mov     eax, shlA_
            mov     cl, shlB_
            and     cl, 3fh

;
; Handle shifts of between 0 and 31 bits
;
            cmp     cl, 32
            jae     MORE32
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

;X2J_SHL64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_SHR64 (a: SYSTEM.INT64, b: SYSTEM.INT8): SYSTEM.INT64
            global  X2J_SHR64
X2J_SHR64:

%define shrB_ [esp + 12]
%define shrA_ [esp + 4]
%define shrB(x) [esp + 12 + x]
%define shrA(x) [esp + 4 + x]

            mov     edx, shrA(4)
            mov     eax, shrA_
            mov     cl, shrB_
            and     cl, 3fh
;
;
; Handle shifts of between 0 and 31 bits
;
            cmp     cl, 32
            jae     MORE32_2
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

;X2J_SHR64   endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_UDIV64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_UDIV64
X2J_UDIV64:

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

%define udivB_ [esp+20]      ; stack address of divisor (b)  DVSR
%define udivA_ [esp+12]      ; stack address of dividend (a)  DVDN
%define udivB(x) [esp+20+x]
%define udivA(x) [esp+12+x]

;
; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;

            mov     eax,udivB(4)     ; check to see if divisor < 4194304K
            or      eax,eax
            jnz     UL1        ; nope, gotta do this the hard way
            mov     ecx,udivB_       ; load divisor
            mov     eax,udivA(4)     ; load high word of dividend
            xor     edx,edx
            div     ecx             ; get high order bits of quotient
            mov     ebx,eax         ; save high bits of quotient
            mov     eax,udivA_       ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; get low order bits of quotient
            mov     edx,ebx         ; edx:eax <- quotient hi:quotient lo
            jmp     short UL2     ; restore stack and return

;
; Here we do it the hard way.  Remember, eax contains DVSRHI
;

UL1:
            mov     ecx,eax         ; ecx:ebx <- divisor
            mov     ebx,udivB_
            mov     edx,udivA(4)     ; edx:eax <- dividend
            mov     eax,udivA_
UL3:
            shr     ecx,1           ; shift divisor right one bit; hi bit <- 0
            rcr     ebx,1
            shr     edx,1           ; shift dividend right one bit; hi bit <- 0
            rcr     eax,1
            or      ecx,ecx
            jnz     UL3        ; loop until divisor < 4194304K
            div     ebx             ; now divide, ignore remainder
            mov     esi,eax         ; save quotient

;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mul     dword udivB(4) ; QUOT * HIWORD(DVSR)
            mov     ecx,eax
            mov     eax,udivB_
            mul     esi             ; QUOT * LOWORD(DVSR)
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      UL4        ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we are ok, otherwise
; subtract one (1) from the quotient.
;

            cmp     edx,udivA(4)      ; compare hi words of result and original
            ja      UL4        ; if result > original, do subtract
            jb      UL5        ; if result < original, we are ok
            cmp     eax,udivA_        ; hi words are equal, compare lo words
            jbe     UL5        ; if less or equal we are ok, else subtract
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

;X2J_UDIV64  endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_UREM64 (a: SYSTEM.INT64, b: SYSTEM.INT64): SYSTEM.INT64
            global  X2J_UREM64
X2J_UREM64:


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

%define uremB_ [esp + 16]      ; stack address of divisor (b)  DVSR
%define uremA_ [esp + 8]      ; stack address of dividend (a) DVND
%define uremB(x) [esp + 16 + x]
%define uremA(x) [esp + 8 + x]


; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.
;

            mov     eax,uremB(4)     ; check to see if divisor < 4194304K
            or      eax,eax
            jnz     RL1       ; nope, gotta do this the hard way
            mov     ecx,uremB_        ; load divisor
            mov     eax,uremA(4)     ; load high word of dividend
            xor     edx,edx
            div     ecx             ; edx <- remainder, eax <- quotient
            mov     eax,uremA_       ; edx:eax <- remainder:lo word of dividend
            div     ecx             ; edx <- final remainder
            mov     eax,edx         ; edx:eax <- remainder
            xor     edx,edx
            jmp     short RL2        ; restore stack and return

;
; Here we do it the hard way.  Remember, eax contains DVSRHI
;

RL1:
            mov     ecx,eax         ; ecx:ebx <- divisor
            mov     ebx,uremB_
            mov     edx,uremA(4)     ; edx:eax <- dividend
            mov     eax,uremA_
RL3:     
            shr     ecx,1           ; shift divisor right one bit; hi bit <- 0
            rcr     ebx,1
            shr     edx,1           ; shift dividend right one bit; hi bit <- 0
            rcr     eax,1
            or      ecx,ecx
            jnz     RL3        ; loop until divisor < 4194304K
            div     ebx             ; now divide, ignore remainder

;
; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.
;

            mov     ecx,eax         ; save a copy of quotient in ECX
            mul     dword uremB(4)
            xchg    ecx,eax         ; put partial product in ECX, get quotient in EAX
            mul     dword uremB_
            add     edx,ecx         ; EDX:EAX = QUOT * DVSR
            jc      RL4       ; carry means Quotient is off by 1

;
; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we're ok, otherwise
; subtract the original divisor from the result.
;

            cmp     edx,uremA(4)      ; compare hi words of result and original
            ja      RL4        ; if result > original, do subtract
            jb      RL5        ; if result < original, we're ok
            cmp     eax,uremA_        ; hi words are equal, compare lo words
            jbe     RL5        ; if less or equal we're ok, else subtract
RL4:
            sub     eax,uremB_       ; subtract divisor from result
            sbb     edx,uremB(4)
RL5:

;
; Calculate remainder by subtracting the result from the original dividend.
; Since the result is already in a register, we will perform the subtract in
; the opposite direction and negate the result to make it positive.
;

            sub     eax,uremA_       ; subtract original dividend from result
            sbb     edx,uremA(4)
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


;X2J_UREM64  endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2J_USHR64 (a: SYSTEM.INT64, b: SYSTEM.INT8): SYSTEM.INT64
            global  X2J_USHR64
X2J_USHR64:

%define ushrB_ [esp + 12]
%define ushrA_ [esp + 4]
%define ushrB(x) [esp + 12 + x]
%define ushrA(x) [esp + 4 + x]

            mov     edx,ushrA(4)
            mov     eax,ushrA_
            mov     cl,ushrB_
            and     cl, 3fh

;
; Handle shifts of 64 bits or more (if shifting 64 bits or more, the result
; depends only on the high order bit of edx).
;
            cmp     cl,64
            jae     RETZERO_2

;
; Handle shifts of between 0 and 31 bits
;
            cmp     cl, 32
            jae     MORE32_3
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

;X2J_USHR64  endp



