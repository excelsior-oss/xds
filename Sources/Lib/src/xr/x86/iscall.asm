; COPYRIGHT (c) 1995,97,99,2002 XDS. All Rights Reserved.

                cpu 386
      bits 32

%ifdef OS2
group    DGROUP   _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA'
%else
group    DGROUP   _DATA
      section _DATA  use32  align=4  public 'DATA'
%endif

%ifdef OS2
      section .text  use32  public  align=4  FLAT  public 'CODE'
%else
      section .text  use32  public  align=16  public 'CODE'
%endif

                global  X2C_IS_CALL

X2C_IS_CALL:

; ASSUME : it's guaranteed that the address passed is always in the code segment
;          and is GREATER than the beginning of codeseg + 6
;          ( checking before a call to this proc )

                mov     eax, [esp+4]

; Сначала проверяем на call reg и call [reg]
;
; don't test (eax-i) if i>7 ( see X2C_IS_CODESEG - VitVit )
;
check:          dec     eax
                dec     eax
                cmp     byte [cs:eax], 0FFh
                jne     not0
                mov     cl, byte [cs:eax+1]
                cmp     cl, 14h                 ; не бывает [esp]
                je      not0
                cmp     cl, 15h                 ; и [ebp]
                je      not0
                cmp     cl,0D4h                 ; и esp
                je      not0
                and     cl, 0F8h
                cmp     cl, 0D0h
                je      near true
                cmp     cl, 10h
                je      near true

; Теперь проверяем на call d8 [reg]

not0:           dec     eax
                cmp     byte [cs:eax], 0FFh
                jne     not1
                mov     cl, byte [cs:eax+1]
                cmp     cl, 54h                 ; не бывает d8 [esp]
                je      not1
                and     cl, 0F8h
                cmp     cl, 50h
                je      true

; Проверяем на call [reg1 + scale * reg2]

                cmp     byte [cs:eax+1], 14h
                jne     not1
                mov     cl, byte [cs:eax+2]
                and     cl, 7
                cmp     cl, 5                   ; не бывает [ebp + scale * reg2]
                jne     true

; Теперь проверяем на call d8 [reg1 + scale * reg2]

not1:           dec     eax
                cmp     byte [cs:eax], 0FFh
                jne     not2
                mov     cl, byte [cs:eax+1]
                and     cl, 0F8h
                cmp     cl, 50h
                je      true

; Теперь проверяем на call relative

not2:           dec     eax
                cmp     byte [cs:eax], 0E8h
                je      true

; Теперь проверяем на call d32 и на d32 [reg]

                dec     eax
                cmp     byte [cs:eax], 0FFh
                jne     not3
                mov     cl, byte [cs:eax+1]
                cmp     cl, 94h                 ; не бывает в шестибайтовой команде
                je      not3
                cmp     cl, 15h                 ; call [disp32]
                je      true
                and     cl, 0F8h
                cmp     cl, 90h
                je      true

; И наконец проверяем на call d32 [reg1 + scale * reg2]

not3:           dec     eax
                cmp     byte [cs:eax], 0FFh
                jne     false
                cmp     byte [cs:eax+1], 94h
                je      true

false:          xor     eax, eax
true:           ret

