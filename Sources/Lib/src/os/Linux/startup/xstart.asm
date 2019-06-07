; COPYRIGHT (c) 2002 Excelsior LLC. All Rights Reserved.
;
                cpu 386
                bits 32

                section .data use32 dword public 'DATA'

myHandle:       dd 0

                section .text use32 dword public 'CODE'

;EXPORT
;------
                global  xStart
                
                global  X2C_INIT_HISTORY
                
                global  X2C_FINALLY

                global  X2C_MODULE

                global  X2C_GetMyHandle         


;//////////////////////// Entry point /////////////////////////////

                extern X2C_EstablishMain
                extern X2C_stkScan
                extern X2C_hisShow
                extern X2C_EXIT

                extern  xosMalloc_init

                extern main
                extern __libc_start_main
                extern environ

                global _start


..start:
_start:
xStart:
                 call xosMalloc_init    ; initialize malloc/free as soon as possible

                 push main
                 call X2C_EstablishMain
                 sub esp, -04H

                 mov dword [X2C_stkScan], 0
                 mov dword [X2C_hisShow], 0

        ; partly grabbed from glibc's sysdeps/i386/elf/start.S
        ;

                 xor ebp, ebp
         
                 ; Extract the arguments as encoded on the stack and set up
                 ; the arguments for `main': argc, argv.  envp will be determined
                 ; later in __libc_start_main.

                 pop esi              ; Pop the argument count.
                 mov ecx, esp         ; argv starts just at the current stack top.

                 lea eax, [esp+esi*4+4]
                 mov [environ], eax
         
                 ; Before pushing the arguments align the stack to a 16-byte
                 ; (SSE needs 16-byte alignment) boundary to avoid penalties from
                 ; misaligned accesses.  Thanks to Edward Seidl <seidl@janed.com>
                 ; for pointing this out.

                 and esp, 0xfffffff0
                 push eax                 ; Push garbage because we allocate
                                          ; 28 more bytes.
         
                 ; Provide the highest stack address to the user code (for stacks
                 ; which grow downwards).

                 push esp
         
                 push edx                 ; Push address of the shared library
                                          ; termination function.
         
                 ; Push address of our own entry points to .fini and .init.
                 push _fini
                 push _init
         
                 push ecx                 ; Push second argument: argv.
                 push esi                 ; Push first argument: argc.

;                 mov eax, [environ]
;                 mov eax, [__environ]

                 push main_wrapper

                 call __libc_start_main         




;calls main and returns zero
main_wrapper:
                 push dword [esp+8] ;argv
                 push dword [esp+8] ;argc

                 call main

                 call X2C_EXIT

                 hlt ;if something crashed


                 

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


X2C_INIT_HISTORY:
;                 push    xStart    ;arbitrary addr in this code segment
;                 push    dword 0 ; [myHandle]
;                 call    X2C_HISTORY_REG
;                 call    X2C_HISTORY_ON   ; to make showable 
;                 sub     esp,-08H
                 ret
;X2C_INIT_HISTORY endp


;////////////////////// O2 modules unloading support /////////////////////


                 extern   X2C_MODULEXE

;PROCEDURE ["C"] X2C_MODULEXE ( md :xmRTS.X2C_MD; hmod :ADDRESS );


;PROCEDURE ["C"] X2C_MODULE ( md :xmRTS.X2C_MD );

X2C_MODULE:
                 push    dword 0 ; [myHandle]
                 mov     eax, [esp+8]      ;md 
                 push    eax
                 call    X2C_MODULEXE
                 sub     esp,-08H
                 ret
;X2C_MODULE       endp


;//////////////////////  DLLRTS support ///////////////////////////////////


;PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

X2C_GetMyHandle:
                 mov     eax, 0 ; [myHandle]
                 ret
;X2C_GetMyHandle  endp


_init:           ret
_fini:           ret


