                .386p
                .387

; COPYRIGHT (c) 1998 XDS Ltd. All Rights Reserved.
;
; seg/selector -> flat address conversion

               .model FLAT

DGROUP         group   _DATA

_DATA          segment use32 dword public 'DATA'
_DATA          ends



_TEXT          segment use32 dword public 'CODE'

;              assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing


               public X2C_SEL2FLAT

               extrn   __SelToFlat: near

;PROCEDURE X2C_SEL2FLAT (seloffs: SYSTEM.CARD32) :SYSTEM.CARD32;

X2C_SEL2FLAT   proc    near
               mov     eax,+4[esp] 
               jmp     __SelToFlat
X2C_SEL2FLAT   endp


_TEXT          ends

               end 




