; COPYRIGHT (c) 1995,99,2002 XDS. All Rights Reserved.

; jump to X2C_EXPRR function named "pow".

                cpu 386
      bits 32


%ifdef SYMANTEC  ; =========== Rename publics
%define pow _pow
%endif


%ifdef OS2
      section .text  use32  align=4  FLAT  public 'CODE' 
%else
      section .text  use32  align=16  public 'CODE'
%endif

;                assume  cs: .text

                global  pow
                extern  X2C_EXPRR
pow:
                jmp     X2C_EXPRR
;pow             endp

