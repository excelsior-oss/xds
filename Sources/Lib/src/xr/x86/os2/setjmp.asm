                .386p
                .387

; COPYRIGHT (c) 1999 Excelsior. All Rights Reserved.
; COPYRIGHT (c) 1995,99 XDS. All Rights Reserved.
;
; Implementation for X2C_setjmp and X2C_longjmp functions
;

ifdef OS2
                .model FLAT
endif

DGROUP          group   _DATA

_DATA           segment use32 dword public 'DATA'
_DATA           ends

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs: _TEXT, ds: DGROUP, gs: nothing, fs: nothing

;PROCEDURE X2C_setjmp(VAR point: X2C_jmp_buf): INTEGER;
		public	X2C_setjmp
X2C_setjmp	proc	near
		mov	eax,+4[esp]
		mov	[eax],ebx
		mov	+4h[eax],ecx
		mov	+8h[eax],edx
		mov	+0ch[eax],esi
		mov	+10h[eax],edi
		mov	+14h[eax],ebp
		pop	dword ptr +18h[eax]
		mov	ecx,esp
		add	ecx,4
		mov	+1ch[eax],ecx
		push	dword ptr +18h[eax]
		mov     +20H[eax],es
		mov     +22H[eax],ds
		mov     +24H[eax],cs
		mov     +26H[eax],fs
		mov     +28H[eax],gs
		mov     +2aH[eax],ss
ifdef OS2
                mov     ecx, fs:[00h]
                mov     +2cH[eax],ecx
endif
                xor     eax,eax
		ret
X2C_setjmp	endp

;PROCEDURE X2C_longjmp(VAR point: X2C_jmp_buf; n: INTEGER);
                public  X2C_longjmp
X2C_longjmp	proc	near
ifdef OS2
                extrn   DosUnwindException: near
                mov     eax,+4[esp]
                push    00h
                push    offset L0
                push    dword ptr +2cH[eax]
                call    DosUnwindException
        L0:     add     esp,0cH
endif
                pop     eax     ; drop return EIP
                pop     eax     ; jmp_buf pointer
                pop     edx     ; argument
                mov     ss,+2aH[eax]        ; switch to new stack
		mov     esp,+1cH[eax]       ;
		push	eax		    ;
		push    dword ptr +18H[eax] ; return EIP
		or      edx,edx
		jne     L1
		inc     edx
	L1:	push    edx
		mov     ebx,[eax]
		mov     ecx,+4H[eax]
		mov     esi,+0cH[eax]
		mov     edi,+10H[eax]
		mov     ebp,+14H[eax]
		mov     dx,+20H[eax]
		verr    dx
		je      L2
		xor     edx,edx
	L2:	mov     es,dx
;                mov     dx,+26H[eax]
;                verr    dx
;                je      L3
;                xor     edx,edx
;        L3:     mov     fs,dx
;                mov     dx,+28H[eax]
;                verr    dx
;                je      L4
;                xor     edx,edx
;        L4:     mov     gs,dx
		mov     edx,+8H[eax]
		mov     ds,+22H[eax]
		pop     eax
		ret
X2C_longjmp	endp

_TEXT		ends

                end
