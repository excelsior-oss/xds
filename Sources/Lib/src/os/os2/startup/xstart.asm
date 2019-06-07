                .386p

; COPYRIGHT (c) 1995,97 xTech Ltd. All Rights Reserved.
;

                 model FLAT

DGROUP           group   _DATA

_DATA            segment use32 dword public 'DATA'
myHandle         dd 0
_DATA            ends


_TEXT            segment use32 dword public 'CODE'
                 assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing

;EXPORT
;------
                public  xStart
		
                public  X2C_INIT_HISTORY
		
                public  X2C_FINALLY

                public  X2C_MODULE

		public  X2C_GetMyHandle		

;//////////////////////// Entry point /////////////////////////////

                 extrn   X2C_xStart        :near
                 extrn   X2C_EstablishMain :near
                 extrn   main              :near

xStart           proc    near
                 mov     eax,+4[esp]
                 mov     myHandle,eax
                 push    offset main
                 call    X2C_EstablishMain
                 sub     esp,-04H
                 jmp     X2C_xStart
xStart           endp


;////////////////////////// Finalization //////////////////////////

;PROCEDURE ["C"] X2C_FINALEXE ( proc :PROC );

                 extrn   X2C_FINALEXE :near  

;PROCEDURE ["C"] X2C_FINALLY ( p :PROC );

X2C_FINALLY      proc    near
                 jmp     X2C_FINALEXE
X2C_FINALLY      endp


;///////////////////////////// History ///////////////////////////////

;PROCEDURE ["C"] X2C_HISTORY_REG (hmod, someAddr :ADDRESS);

                 extrn   X2C_HISTORY_REG :near

;PROCEDURE ["C"] X2C_HISTORY_ON ();

                 extrn   X2C_HISTORY_ON :near


X2C_INIT_HISTORY proc    near
                 push    offset xStart    ;arbitrary addr in this code segment
                 push    myHandle
                 call    X2C_HISTORY_REG
                 call    X2C_HISTORY_ON   ; to make showable 
                 sub     esp,-08H
                 ret
X2C_INIT_HISTORY endp


;////////////////////// O2 modules unloading support /////////////////////


                 extrn   X2C_MODULEXE :near

;PROCEDURE ["C"] X2C_MODULEXE ( md :xmRTS.X2C_MD; hmod :ADDRESS );


;PROCEDURE ["C"] X2C_MODULE ( md :xmRTS.X2C_MD );

X2C_MODULE       proc    near
                 push    myHandle
                 mov     eax,+8[esp]      ;md 
                 push    eax
                 call    X2C_MODULEXE
                 sub     esp,-08H
                 ret
X2C_MODULE       endp

;//////////////////////  DLLRTS support ///////////////////////////////////


;PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

X2C_GetMyHandle  proc    near
                 mov     eax, myHandle
		 ret
X2C_GetMyHandle  endp


_TEXT            ends

                 end xStart   ; entry point
