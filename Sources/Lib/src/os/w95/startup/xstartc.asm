; COPYRIGHT (c) 1995 xTech Ltd. All Rights Reserved.
; COPYRIGHT (c) 2002 Excelsior LLC. All Rights Reserved.
;

                cpu 386
                bits 32


                section .data  use32  public  align=4  public 'DATA'
                
myHandle:       dd      0
string:         db      'QuQu',10,12
nbytes:         dd      0

                section .text  use32  public  align=4  public 'CODE'



;EXPORT
;------
      
                global  X2C_INIT_HISTORY
      
                global  X2C_FINALLY

                global  X2C_MODULE
      
      global  X2C_GetMyHandle
      

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


;PROCEDURE ["StdCall"] GetModuleHandleA ( lpszModule :ARRAY OF CHAR );

                 extern   GetModuleHandleA 




;PROCEDURE ["C"] X2C_INIT_HISTORY();

X2C_INIT_HISTORY:
                 push    X2C_INIT_HISTORY  ;arbitrary addr from this code segment
                 push    dword 0                  ;NULL - GetMyHandle
                 call    GetModuleHandleA         ;->eax
            push    eax
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
                 push    dword 0          ;NULL - GetMyHandle
                 call    GetModuleHandleA ;->eax
                 push    eax
                 mov     eax,[esp+8]      ;md 
                 push    eax
                 call    X2C_MODULEXE
                 sub     esp,-08H
                 ret
;X2C_MODULE       endp


;//////////////////////  DLLRTS support ///////////////////////////////////


;PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

X2C_GetMyHandle:
                 push    dword 0          ;NULL - GetMyHandle
                 jmp     GetModuleHandleA ;->eax
;X2C_GetMyHandle  endp

