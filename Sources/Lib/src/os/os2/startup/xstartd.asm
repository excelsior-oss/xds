                .386p
                .387

; COPYRIGHT (c) 1997 xTech Ltd. All Rights Reserved.
;
; LX DLL initialization/termination routine

                model FLAT

DGROUP          group   _DATA

_DATA           segment use32 dword public 'DATA'
myHandle        dd    0
finals          dd    0         ; DLL FINALLY list head
O2modules       dd    0         ; O2 type descriptors from modules of this DLL
_DATA           ends


_TEXT           segment use32 dword public 'CODE'

;               assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing

;EXPORT
;------
                public xDLLinit
		
                public X2C_FINALLY
		
                public X2C_INIT_HISTORY  

                public X2C_MODULE

                public X2C_GetMyHandle

;//////////////////////// Entry point /////////////////////////////

; Library initialization registers are defined as follows.
;
;           EBP = 0
;
;           [ESP+0] = Return address to system, (EAX) = return code.
;
;           [ESP+4] = Module handle for library module.
;
;           [ESP+8] = 0/1 - initialization/termination


                extrn  _dllmain    :near
                extrn  X2C_EXITDLL :near
		
;PROCEDURE ["C"] X2C_DISABLE_COMPONENT ( component :xmRTS.X2C_MD );		
		
                extrn  X2C_DISABLE_COMPONENT :near

;PROCEDURE ["C"] X2C_HISTORY_REG (hmod, someAddr :ADDRESS);

                 extrn   X2C_HISTORY_REG :near


;PROCEDURE ["C"] X2C_EXIT_HISTORY   ( hmod :ADDRESS );
                extrn  X2C_EXIT_HISTORY :near

;PROCEDURE ["C"] X2C_EXIT_PROFILER (isExe: BOOLEAN);
                extrn  X2C_EXIT_PROFILER :near


xDLLinit         proc   near
                 mov    eax,+8[esp]
                 or     eax,eax
                 jnz    short dllterm
;dllinit
                 mov    eax,+4[esp]
                 mov    myHandle,eax
                 push   offset xDLLinit     ;arbitrary addr in this code segment
                 push   myHandle
                 call   X2C_HISTORY_REG  ;register before _dllmain to may work when DLL initialization
                 sub    esp,-08H
                 call   _dllmain
                 mov    eax,1      ; rc is always TRUE
                 ret

dllterm:         push  00H
                 call  X2C_EXIT_PROFILER
                 push  offset finals
                 call  X2C_EXITDLL
                 push  myHandle
                 call  X2C_EXIT_HISTORY
                 push  O2modules
		 call  X2C_DISABLE_COMPONENT ;invalidate O2 types
                 sub   esp,-10H
                 mov   eax,1                  ; rc is always TRUE
                 ret

xDLLinit         endp


;////////////////////////// Finalization //////////////////////////

;PROCEDURE ["C"] X2C_FINALDLL ( VAR finalHead :ADDRESS; proc :PROC );

                 extrn   X2C_FINALDLL  :near

;PROCEDURE ["C"] X2C_FINALLY ( p :PROC );

X2C_FINALLY      proc    near
                 mov     eax,+4[esp]
                 push    eax
                 push    offset finals
                 call    X2C_FINALDLL
                 sub     esp,-08H
                 ret
X2C_FINALLY      endp


;///////////////////////////// History ///////////////////////////////

;PROCEDURE ["C"] X2C_HISTORY_ON ();

                 extrn   X2C_HISTORY_ON :near


;PROCEDURE ["C"] X2C_INIT_HISTORY();

X2C_INIT_HISTORY proc    near
                 jmp     X2C_HISTORY_ON  ; to make showable
X2C_INIT_HISTORY endp


;////////////////////// O2 modules unloading support ///////////////////// 

                 extrn   X2C_MODULEDLL :near

;PROCEDURE X2C_MODULEDLL (VAR component :xmRTS.X2C_MD;
;                                    md :xmRTS.X2C_MD
;                                  hmod :ADDRESS );


;PROCEDURE ["C"] X2C_MODULE ( md :xmRTS.X2C_MD );

X2C_MODULE       proc    near
                 push    myHandle
                 mov     eax,+8[esp]      ;md 
                 push    eax
                 push    offset O2modules
                 call    X2C_MODULEDLL
                 sub     esp,-0CH
                 ret
X2C_MODULE       endp


;//////////////////////  DLLRTS support ///////////////////////////////////


;PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

X2C_GetMyHandle  proc    near
                 mov     eax, myHandle
		 ret
X2C_GetMyHandle  endp



_TEXT            ends

                 end xDLLinit            ;entry point


