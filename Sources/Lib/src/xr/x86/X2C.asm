; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.
; COPYRIGHT (c) 2002 Excelsior LLC. All Rights Reserved.

                cpu 386
      bits 32

%ifdef OS2
group       DGROUP _DATA _BSS
      section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group       DGROUP _DATA _BSS
      section _DATA  use32  align=4  public 'DATA' 
%endif

                global X2C_HisList

  libInitFlag:  dd      0    ; 0/1 - native lib not yet/already initialized 
  XEntryFlag:   dd      0
  X2C_HisList:  dd      0

  minint64:     dd  0,          080000000h
  maxint64:     dd  0ffffffffh, 07fffffffh
  result:       dd  0,0


%ifdef OS2
      section _BSS  use32  align=4  FLAT  public 'BSS'
%else
      section _BSS  use32  align=4  public 'BSS'
%endif

                global  X2C_argc
                global  _X2C_argc
                global  X2C_argv
                global  _X2C_argv
                global  X2C_sysarg
                global  _X2C_sysarg
  _X2C_argc:
  X2C_argc:     resd    1
  _X2C_argv:
  X2C_argv:     resd    1
  _X2C_sysarg:
  X2C_sysarg:   resd    1


%ifdef OS2
      section .text  use32  align=4  FLAT  public 'CODE' 
%else
      section .text  use32  align=16  public 'CODE'
%endif

;                assume  cs: .text, ds: DGROUP, gs: nothing, fs: nothing



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PROCEDURE ["C"] X2C_TRUNCI64 (x :LONGREAL; VAR retlong :SYSTEM.INT64);
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

            extern   X2J_double2long

%define     retlong [esp+4+8]       ; stack address of "retlong"

            global X2C_TRUNCI64
X2C_TRUNCI64:
            fld     qword [esp+4]
            fild    qword [minint64]
            fcomp   st1        ;;;
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
            fcomp   st1       ;;;
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
            jz  positive

            fild     qword [result]
            fcomp   st1       ;;;
            fnstsw  ax
            test    ax, 0100h
            jz   return
            add     dword [result], 1
            adc     dword [result+4], 0
            jmp short return
            
positive:   
            fild     qword [result]
            fcomp   st1      ;;;
            fnstsw  ax
            test    ax, 4500h
            jnz   short return
            sub     dword [result], 1
            sbb     dword [result+4], 0
return:
            mov     eax, dword [result]
            mov     edx, dword [result+4]
            mov     ecx, retlong
            mov     dword [ecx], eax
            mov     dword [ecx+4], edx 
            fstp    st0
          ret 
;X2C_TRUNCI64 endp



;PROCEDURE ["C"] X2C_assign_acav(ac: INTEGER; av: SYSTEM.ADDRESS);

                global  X2C_assign_acav

X2C_assign_acav:
                mov     eax, [esp+4]     ; argc
                mov     [X2C_argc], eax
                mov     eax, [esp+8]     ; argv
                mov     [X2C_argv], eax
                ret
;X2C_assign_acav endp



;PROCEDURE ["C"] / X2C_wasNLibInit() :BOOLEAN;

                global  X2C_wasNLibInit

X2C_wasNLibInit:
                mov     eax, [libInitFlag]
                ret
;X2C_wasNLibInit endp


;PROCEDURE ["C"] / X2C_makeNLibInit();

                global  X2C_makeNLibInit

X2C_makeNLibInit:
                mov     dword [libInitFlag], 1
                ret
;X2C_makeNLibInit endp



;PROCEDURE ["C"] / X2C_wasXEntry() :BOOLEAN;

                global  X2C_wasXEntry

X2C_wasXEntry:
                mov     eax, [XEntryFlag]
                ret
;X2C_wasXEntry   endp


;PROCEDURE ["C"] / X2C_makeXEntry();

                global  X2C_makeXEntry

X2C_makeXEntry:
                mov     dword [XEntryFlag], 1
                ret
;X2C_makeXEntry   endp

