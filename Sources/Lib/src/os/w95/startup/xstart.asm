; COPYRIGHT (c) 1995 xTech Ltd. All Rights Reserved.
; COPYRIGHT (c) 2002, 2003 Excelsior LLC. All Rights Reserved.
;

                cpu 386
                bits 32


                section .data  use32  public  align=4  public 'DATA'

myHandle:       dd      0
string:         db      "QuQu",10,12
nbytes:         dd      0

                section .text  use32  public  align=4  public 'CODE'


;EXPORT
;------
                global  xStart
                
                global  X2C_INIT_HISTORY
                
                global  X2C_FINALLY

                global  X2C_MODULE
                
                global  X2C_GetMyHandle
                

;//////////////////////// Entry point /////////////////////////////

                extern   X2C_xStart
                extern   X2C_EstablishMain
                extern   main


;PROCEDURE ["StdCall"] GetModuleHandleA ( lpszModule :ARRAY OF CHAR );

                extern   GetModuleHandleA

..start:
xStart:

                push    dword 0                ;NULL - GetMyHandle
                call    GetModuleHandleA ;->eax
                mov     [myHandle], eax

                push    main
                call    X2C_EstablishMain
                sub     esp,-04H

                push    xFilter
                xor     eax, eax
                push    dword [fs:eax]
                mov     [fs:eax], esp
                jmp     X2C_xStart

;xStart          endp

                extern   X2C_xFilter

xFilter:
                mov     eax, [esp+4]          ; exception_record *
                mov     edx, [esp+12]          ; context_record *
                push    edx
                push    eax
                test    dword [eax+4], 6
                jnz     Unhandled
                mov     eax, esp
                push    eax
                call    X2C_xFilter
                test    eax, eax
                jge     Unhandled
                add     esp, 8
                xor     eax, eax
                ret
Unhandled:      xor     eax, eax
                mov     ecx, [fs:eax]
                mov     ecx, [ecx]
                mov     [fs:eax], ecx
                add     esp, 8
                inc     eax
                ret
;xFilter         endp



;////////////////////////// Finalization //////////////////////////

;PROCEDURE ["C"] X2C_FINALEXE ( proc :PROC );

                 extern   X2C_FINALEXE

;PROCEDURE ["C"] X2C_FINALLY ( p :PROC );

X2C_FINALLY:
                 jmp     X2C_FINALEXE
;X2C_FINALLY      endp


;///////////////////////////// History ///////////////////////////////


;PROCEDURE ["C"] X2C_HISTORY_REG (hmod, someAddr :ADDRESS);

                 extern   X2C_HISTORY_REG

;PROCEDURE ["C"] X2C_HISTORY_ON ();

                 extern   X2C_HISTORY_ON



;PROCEDURE ["C"] X2C_INIT_HISTORY();

X2C_INIT_HISTORY:
                 push    dword xStart    ;arbitrary addr from this code segment
                 push    dword [myHandle]
                 call    X2C_HISTORY_REG
                 call    X2C_HISTORY_ON   ; to make showable 
                 sub     esp,-08H
                 ret
;X2C_INIT_HISTORY endp


;////////////////////// O2 modules unloading support /////////////////////


                 extern   X2C_MODULEXE

;PROCEDURE ["C"] X2C_MODULEXE ( md :xmRTS.X2C_MD; hmod :ADDRESS );


;PROCEDURE ["C"] X2C_MODULE ( md :xmRTS.X2C_MD );

X2C_MODULE:
                 push    dword [myHandle]
                 mov     eax,[esp+8]      ;md 
                 push    eax
                 call    X2C_MODULEXE
                 sub     esp,-08H
                 ret
;X2C_MODULE       endp


;//////////////////////  DLLRTS support ///////////////////////////////////


;PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

X2C_GetMyHandle:
                 mov     eax, [myHandle]
                 ret
;X2C_GetMyHandle  endp


;                 end xStart
