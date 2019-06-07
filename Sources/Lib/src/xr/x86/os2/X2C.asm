                .386p
                .387

; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.

ifdef OS2
                .model FLAT
endif

DGROUP          group   _DATA,_BSS

_DATA           segment use32 dword public 'DATA'

                public  X2C_HisList

  libInitFlag   dd      0    ; 0/1 - native lib not yet/already initialized 
  XEntryFlag    dd      0
  X2C_HisList   dd      0

  minint64      dd  0,          080000000h
  maxint64      dd  0ffffffffh, 07fffffffh
  result        dd  0,0

_DATA           ends


_BSS            segment use32 dword public 'BSS'

                public  X2C_argc
                public  _X2C_argc
                public  X2C_argv
                public  _X2C_argv
                public  X2C_sysarg
                public  _X2C_sysarg
  _X2C_argc:
  X2C_argc      dd      0
  _X2C_argv:
  X2C_argv      dd      0
  _X2C_sysarg:
  X2C_sysarg    dd      0

_BSS            ends


_DATA           segment use32 dword public 'DATA'
_DATA           ends

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing



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

            extrn   X2J_double2long  :near

retlong     EQU     [esp+4+8]       ; stack address of "retlong"

            public X2C_TRUNCI64
X2C_TRUNCI64 proc near
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
            mov     eax, dword ptr   [result]
            mov     edx, dword ptr +4[result]
            mov     ecx, retlong
            mov     dword ptr [ecx], eax
            mov     dword ptr +4[ecx], edx 
          fstp    st
          ret 
X2C_TRUNCI64 endp



;PROCEDURE ["C"] X2C_assign_acav(ac: INTEGER; av: SYSTEM.ADDRESS);

                public  X2C_assign_acav

X2C_assign_acav proc    near
                mov     eax,+4[esp]     ; argc
                mov     X2C_argc,eax
                mov     eax,+8[esp]     ; argv
                mov     X2C_argv,eax
                ret
X2C_assign_acav endp



;PROCEDURE ["C"] / X2C_wasNLibInit() :BOOLEAN;

                public  X2C_wasNLibInit

X2C_wasNLibInit proc    near
                mov     eax, libInitFlag
                ret
X2C_wasNLibInit endp


;PROCEDURE ["C"] / X2C_makeNLibInit();

                 public  X2C_makeNLibInit

X2C_makeNLibInit proc    near
                 mov     libInitFlag, 1
                 ret
X2C_makeNLibInit endp



;PROCEDURE ["C"] / X2C_wasXEntry() :BOOLEAN;

                public  X2C_wasXEntry

X2C_wasXEntry   proc    near
                mov     eax, XEntryFlag
                ret
X2C_wasXEntry   endp


;PROCEDURE ["C"] / X2C_makeXEntry();

                 public  X2C_makeXEntry

X2C_makeXEntry   proc    near
                 mov     XEntryFlag, 1
                 ret
X2C_makeXEntry   endp



_TEXT           ends

                end
