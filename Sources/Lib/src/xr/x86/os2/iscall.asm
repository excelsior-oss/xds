                .386p

; COPYRIGHT (c) 1995,97,99 XDS. All Rights Reserved.

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
;                assume  cs: _TEXT, ds: FLAT, gs: nothing, fs: nothing

                public  X2C_IS_CALL

X2C_IS_CALL     proc    near

; ASSUME : it's guaranteed that the address passed is always in the code segment
;          and is GREATER than the beginning of codeseg + 6
;          ( checking before a call to this proc )

                mov     eax, 4[esp]

; Сначала проверяем на call reg и call [reg]
;
; don't test (eax-i) if i>7 ( see X2C_IS_CODESEG - VitVit )
;
check:          dec     eax
                dec     eax
                cmp     byte ptr cs: [eax], 0FFh
                jne     short not0
                mov     cl, byte ptr cs: 1[eax]
                cmp     cl, 14h                 ; не бывает [esp]
                je      short not0
                cmp     cl, 15h                 ; и [ebp]
                je      short not0
                cmp     cl,0D4h                 ; и esp
                je      short not0
                and     cl, 0F8h
                cmp     cl, 0D0h
                je      short true
                cmp     cl, 10h
                je      short true

; Теперь проверяем на call d8 [reg]

not0:           dec     eax
                cmp     byte ptr cs: [eax], 0FFh
                jne     short not1
                mov     cl, byte ptr cs: 1[eax]
                cmp     cl, 54h                 ; не бывает d8 [esp]
                je      short not1
                and     cl, 0F8h
                cmp     cl, 50h
                je      short true

; Проверяем на call [reg1 + scale * reg2]

                cmp     byte ptr cs: 1[eax], 14h
                jne     short not1
                mov     cl, byte ptr cs: 2[eax]
                and     cl, 7
                cmp     cl, 5                   ; не бывает [ebp + scale * reg2]
                jne     short true

; Теперь проверяем на call d8 [reg1 + scale * reg2]

not1:           dec     eax
                cmp     byte ptr cs: [eax], 0FFh
                jne     short not2
                mov     cl, byte ptr cs: 1[eax]
                and     cl, 0F8h
                cmp     cl, 50h
                je      short true

; Теперь проверяем на call relative

not2:           dec     eax
                cmp     byte ptr cs: [eax], 0E8h
                je      short true

; Теперь проверяем на call d32 и на d32 [reg]

                dec     eax
                cmp     byte ptr cs: [eax], 0FFh
                jne     short not3
                mov     cl, byte ptr cs: 1[eax]
                cmp     cl, 94h                 ; не бывает в шестибайтовой команде
                je      short not3
                cmp     cl, 15h                 ; call [disp32]
                je      short true
                and     cl, 0F8h
                cmp     cl, 90h
                je      short true

; И наконец проверяем на call d32 [reg1 + scale * reg2]

not3:           dec     eax
                cmp     byte ptr cs: [eax], 0FFh
                jne     short false
                cmp     byte ptr cs: 1[eax], 94h
                je      short true

false:          xor     eax, eax
true:           ret
X2C_IS_CALL     endp

_TEXT           ends

                end
