                .386p
                .387

; COPYRIGHT (c) 1997 xTech Ltd. All Rights Reserved.
;
; DLL initialization stub

ifdef OS2
                .model FLAT
endif

DGROUP          group   _DATA

_DATA           segment use32 dword public 'DATA'
_DATA           ends



_TEXT           segment use32 dword public 'CODE'

;               assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing


; stub for "weak extern"

               public _dllmain
_dllmain       proc   near
               ret
_dllmain       endp


_TEXT          ends

               end 

