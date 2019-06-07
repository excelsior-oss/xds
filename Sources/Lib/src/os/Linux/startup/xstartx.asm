; COPYRIGHT (c) 1997 xTech Ltd. All Rights Reserved.
; COPYRIGHT (c) 2002, 2003 Excelsior LLC. All Rights Reserved.
;
; PE DLL initialization/termination routine

                cpu 386
                bits 32

                section .data  use32  public  align=4  public 'DATA'
myHandle:       dd    0
finals:         dd    0         ; DLL FINALLY list head
O2modules:      dd    0         ; O2 type descriptors from modules of this DLL


; Representation of the Dl_info structure

info    : dd 0 ; File name of defining object.
          dd 0 ; Load address of that object.
          dd 0 ; Name of nearest symbol.
          dd 0 ; Exact value of nearest symbol.


                section .text  use32  public  align=4  public 'CODE'


;EXPORT
;------
                global  _init
                global  _fini

                global xDLLinit
                
                global X2C_FINALLY
                
                global X2C_INIT_HISTORY  

                global X2C_MODULE

                global X2C_GetMyHandle

;//////////////////////// Entry point /////////////////////////////


                extern  _dllmain
                extern  X2C_EXITDLL
                
                extern  _IO_printf

;PROCEDURE ["C"] X2C_DISABLE_COMPONENT ( component :xmRTS.X2C_MD );

                extern  X2C_DISABLE_COMPONENT

;PROCEDURE ["C"] X2C_HISTORY_REG (hmod, someAddr :ADDRESS);

                extern  X2C_HISTORY_REG


;PROCEDURE ["C"] X2C_EXIT_HISTORY   ( hmod :ADDRESS );
                extern  X2C_EXIT_HISTORY

;PROCEDURE ["C"] X2C_EXIT_PROFILER (isExe: BOOLEAN);
                extern  X2C_EXIT_PROFILER

;PROCEDURE ["C"] X2C_FINALIZE_RT ();
                extern  X2C_FINALIZE_RT

                extern  xosMalloc_init

                extern  dlopen
                extern  dladdr
                extern  dlclose

..start:
xDLLinit:

_init:          
                push   info
                push   _init           
                call   dladdr          
                add    esp, 8
                
                push   dword 1         
                push   dword [info]    
                call   dlopen          
                add    esp, 8
                mov    [myHandle], eax
                push   eax
                call   dlclose
                add    esp, 4
                call   xosMalloc_init    ; initialize malloc/free as soon as possible
                push   dword xDLLinit    ; arbitrary addr from this code segment
                push   dword [myHandle]
                call   X2C_HISTORY_REG   ; register before _dllmain to may work when DLL initialization
                sub    esp, -08H
                call   _dllmain
                mov    eax, 1            ; rc is always TRUE
                ret

_fini:
                push   00H
                call   X2C_EXIT_PROFILER
                push   dword finals
                call   X2C_EXITDLL
                push   dword [myHandle]
                call   X2C_EXIT_HISTORY      ;unregister
                push   dword [O2modules]
                call   X2C_DISABLE_COMPONENT ;invalidate O2 types
                sub    esp, -10H
                call   X2C_FINALIZE_RT
                mov    eax, 1                ; rc is always TRUE
                ret

;xDLLinit       endp


;////////////////////////// Finalization //////////////////////////


;PROCEDURE ["C"] X2C_FINALDLL ( VAR finalHead :ADDRESS; proc :PROC );

                extern  X2C_FINALDLL

;PROCEDURE ["C"] X2C_FINALLY ( p :PROC );

X2C_FINALLY:
                mov     eax, [esp+4]
                push    eax
                push    dword finals
                call    X2C_FINALDLL
                sub     esp, -08H
                ret
;X2C_FINALLY    endp


;///////////////////////////// History ///////////////////////////////


;PROCEDURE ["C"] X2C_HISTORY_ON ();

                 extern  X2C_HISTORY_ON


;PROCEDURE ["C"] X2C_INIT_HISTORY();

X2C_INIT_HISTORY:
                 jmp    X2C_HISTORY_ON  ; to make showable
;X2C_INIT_HISTORY endp



;////////////////////// O2 modules unloading support ///////////////////// 

                 extern  X2C_MODULEDLL

;PROCEDURE X2C_MODULEDLL (VAR component :xmRTS.X2C_MD;
;                                    md :xmRTS.X2C_MD
;                                  hmod :ADDRESS );


;PROCEDURE ["C"] X2C_MODULE ( md :xmRTS.X2C_MD );

X2C_MODULE:
                 push    dword [myHandle]
                 mov     eax, [esp+8]      ;md 
                 push    eax
                 push    dword O2modules
                 call    X2C_MODULEDLL
                 sub     esp, -0CH
                 ret
;X2C_MODULE      endp


;//////////////////////  DLLRTS support ///////////////////////////////////


;PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

X2C_GetMyHandle:
                 mov     eax, [myHandle]
                 ret



