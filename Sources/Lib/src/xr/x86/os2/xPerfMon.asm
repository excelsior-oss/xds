                .586p

; COPYRIGHT (c) 1999 XDS Ltd. All Rights Reserved.
; 31.03.99 paul: created

; Implementation for performance monitoring functions

ifdef OS2
                .model FLAT
endif

DGROUP          group   _DATA

_DATA           segment use32 dword public 'DATA'

PUBLIC proc_dirty_overhead_rec, proc_dirty_overhead_norec, proc_pure_overhead
PUBLIC nest_dirty_overhead, nest_pure_overhead
PUBLIC brackets_sum_overhead_lo, brackets_sum_overhead_hi

    proc_dirty_overhead_rec	dd 0
    proc_dirty_overhead_norec	dd 0
    proc_pure_overhead		dd 0
    nest_dirty_overhead		dd 0
    nest_pure_overhead		dd 0
    brackets_sum_overhead_lo	dd 0
    brackets_sum_overhead_hi	dd 0

PUBLIC X2C_prof_enabled
    X2C_prof_enabled		dd 0


_DATA           ends

ifdef OS2
_TEXT           segment use32 dword public 'CODE'
else
_TEXT           segment use32 para public 'CODE'
endif
;                assume  cs:_TEXT

;PROCEDURE ["StdCall"] X2C_Read_TSC(VAR timestamp: INT64);
X2C_Read_TSC proc    near
public X2C_Read_TSC
        mov	ecx, +4[esp]
        rdtsc
        mov	[ecx], eax
        mov	+4[ecx], edx
        ret 4
X2C_Read_TSC endp


;PROCEDURE ["StdCall"] X2C_Open_TSC_Scope(VAR timestamp: INT64);
X2C_Open_TSC_Scope proc    near
public X2C_Open_TSC_Scope
        mov	ecx, +4[esp]
        rdtsc
        sub	[ecx], eax
        sbb	+4[ecx], edx
        ret 4
X2C_Open_TSC_Scope endp


;PROCEDURE ["StdCall"] X2C_Close_TSC_Scope(VAR timestamp: INT64);
X2C_Close_TSC_Scope proc    near
public X2C_Close_TSC_Scope
        mov	ecx, +4[esp]
        rdtsc
        add	[ecx], eax
        adc	+4[ecx], edx
        ret 4
X2C_Close_TSC_Scope endp


CPU_Info struc
    CPUID_supported dd 0
    max_CPUID_call dd 0

;   vendor: ARRAY 12 OF CHAR;
    vendor_lo	dd 0
    vendor_med	dd 0
    vendor_hi	dd 0

    cputype	dd 0
    family	dd 0
    model	dd 0
    stepping	dd 0
    features	dd 0
CPU_Info ends

; returns: eax == ( CPUID is present ? 1 : 0 )
is_CPUID_supported proc near
        xor	eax, eax
        pushfd
        btr	dword ptr [esp], 21
        popfd
        pushfd
        bt	dword ptr [esp], 21
        jc	there_isnt_CPUID
        bts	dword ptr [esp], 21
        popfd
        pushfd
        bt	dword ptr [esp], 21
        jnc	there_isnt_CPUID
; CPUID supported
        inc	eax
there_isnt_CPUID:
        popfd
        ret
is_CPUID_supported endp

;PROCEDURE ["StdCall"] X2C_GetCPUInfo( VAR info: CPU_Info );
X2C_GetCPUInfo proc    near
public X2C_GetCPUInfo
        mov	eax, +4[esp]
        pusha
        mov	esi, eax

        call    is_CPUID_supported
        mov	CPU_Info[esi].CPUID_supported, eax
        test    eax, eax
        jnz	do_CPUID

        mov	ebx, eax
        mov	ecx, eax
        mov	edx, eax
        jmp	set_info
do_CPUID:
        xor	eax, eax
        cpuid
set_info:
        mov	CPU_Info[esi].max_CPUID_call, eax
        mov	CPU_Info[esi].vendor_lo, ebx
        mov	CPU_Info[esi].vendor_med, edx
        mov	CPU_Info[esi].vendor_hi, ecx

        xor	edx, edx
        test    eax, eax
        jz	no_features_info
        mov	eax, 1
        cpuid
no_features_info:
        mov	CPU_Info[esi].features, edx
        mov	ebx, eax
        shr	ebx, 4
        and	ebx, 030fh
        movzx	ecx, bh     ; ecx == type
        mov	bh, 0       ; ebx == model
        and	eax, 0f0fh
        movzx	edx, ah     ; edx == family
        mov	ah, 0       ; eax == stepping
        mov	CPU_Info[esi].cputype, ecx
        mov	CPU_Info[esi].family, edx
        mov	CPU_Info[esi].model, ebx
        mov	CPU_Info[esi].stepping, eax

        popa
        ret 4
X2C_GetCPUInfo endp

; ---------------------------------------------------
; !!!!! taken from xmRTS. _MUST_ be synchronized !!!!!
; ---------------------------------------------------
;
;    X2C_CALL_TYPE_PTR = POINTER TO X2C_CALL_TYPE;
;    X2C_CALL_TYPE = RECORD
;      ip                : ADDRESS;
;      count             : CARD32;
;      next              : X2C_CALL_TYPE_PTR;
;    END;
;
;    X2C_Profile_STR = RECORD
;      pure_dur_lo       : CARD32;
;      pure_dur_hi       : CARD32;
;      dirty_dur_lo      : CARD32;
;      dirty_dur_hi      : CARD32;
;      saved_overhead_lo : CARD32;
;      saved_overhead_hi : CARD32;
;      total_entry_count : CARD32;
;      norec_entry_count : CARD32;
;      rec_level         : CARD32;
;      call_list         : X2C_CALL_TYPE_PTR;
;    END;

CallType struc
	ct_ip		dd 0
	ct_count	dd 0
	ct_next		dd 0
CallType ends

ProfStr struc
	pure_dur_lo	dd 0
	pure_dur_hi	dd 0
	dirty_dur_lo	dd 0
	dirty_dur_hi	dd 0
	saved_overhead_lo dd 0
	saved_overhead_hi dd 0
	total_entry_count dd 0
	norec_entry_count dd 0
	rec_level	dd 0
	call_list	dd 0
ProfStr ends
; ---------------------------------------------------
; !!!!! taken from xmRTS. _MUST_ be syncronized !!!!!
; ---------------------------------------------------


;PROCEDURE ["StdCall"] / X2C_PROFILE_PROC_START ( VAR procDesc: ProfSTR );
; {
;   tsc := rdtsc();
;   procDesc->pure_dur -= tsc;
;   ++procDesc->total_entry_count;
;   if( (++ procDesc->rec_level) == 1 ) {
;     ++procDesc->norec_entry_count;
;     procDesc->dirty_dur -= tsc;
;     procDesc->saved_overhead := brackets_sum_overhead;
;     brackets_sum_overhead := proc_dirty_overhead_norec;
;   } else {
;     brackets_sum_overhead += proc_dirty_overhead_rec;
;   }
; }
     
	align 4
X2C_PROFILE_PROC_START proc  near
public X2C_PROFILE_PROC_START
;local isRecurse1
        push ecx
        push edx
        push eax

        mov	ecx, +16[esp]		; ecx := procDesc
        rdtsc
        sub	ProfStr[ecx].pure_dur_lo, eax	; pure_dur -= tsc
        sbb	ProfStr[ecx].pure_dur_hi, edx

        inc	ProfStr[ecx].total_entry_count
        inc	ProfStr[ecx].rec_level
        cmp	ProfStr[ecx].rec_level, 1
        ja	isRecurse1

        inc	ProfStr[ecx].norec_entry_count
        sub	ProfStr[ecx].dirty_dur_lo, eax
        sbb	ProfStr[ecx].dirty_dur_hi, edx

; procDesc->saved_overhead := brackets_sum_overhead;
; brackets_sum_overhead := proc_dirty_overhead_norec;
	mov	eax, proc_dirty_overhead_norec
        xor	edx, edx
        xchg	eax, brackets_sum_overhead_lo
        xchg	edx, brackets_sum_overhead_hi
        mov	ProfStr[ecx].saved_overhead_lo, eax
        mov	ProfStr[ecx].saved_overhead_hi, edx

        pop eax
        pop edx
        pop ecx
        ret 4

isRecurse1:
	mov	eax, proc_dirty_overhead_rec
        add	brackets_sum_overhead_lo, eax
        adc	brackets_sum_overhead_hi, 0

        pop eax
        pop edx
        pop ecx
        ret 4
X2C_PROFILE_PROC_START endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_PROC_START_C ( VAR procDesc: ProfSTR;
;                                                        callerIP: ADDRESS );
extrn X2C_malloc: PROC
	align 4
X2C_PROFILE_PROC_START_C proc  near
public X2C_PROFILE_PROC_START_C
        push esi
        push edi
        push eax

        mov	esi, +16[esp]		; esi := procDesc
        lea	eax, ProfStr[esi].call_list - ct_next
        mov	esi, [esp]+20		; esi := caller's IP
cont:
        mov	edi, eax		; edi := &last_elem
        mov	eax, CallType[eax].ct_next
        test	eax, eax
        jz	end_of_list
        cmp	esi, CallType[eax].ct_ip
        jne	cont
        inc	CallType[eax].ct_count
        jmp	exit
end_of_list:                
        push ecx
        push edx
        push	12
        call	X2C_malloc
        pop	edx	; drop '12'
        pop edx
        pop ecx

        mov	CallType[edi].ct_next, eax	; last_elem->next == &new_elem
        mov	CallType[eax].ct_ip, esi
        mov	CallType[eax].ct_count, 1
        mov	CallType[eax].ct_next, 0

exit:
        pop eax
        pop edi
        pop esi
;        ret 4
	jmp	X2C_PROFILE_PROC_START
X2C_PROFILE_PROC_START_C endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_PROC_END ( VAR procDesc: ProfSTR );
; {
;   tsc := rdtsc();
;   procDesc->pure_dur += (tsc - proc_pure_overhead);
;   if( (-- procDesc->rec_level) == 0 ) {
;     procDesc->dirty_dur += (tsc - brackets_sum_overhead);
;     brackets_sum_overhead += procDesc->saved_overhead;
;   }
; }

        align 4
X2C_PROFILE_PROC_END proc    near
public X2C_PROFILE_PROC_END
;local isRecurse3
        push ecx
        push edx
        push eax
        push esi

        mov	ecx, +20[esp]
        rdtsc
        add	ProfStr[ecx].pure_dur_lo, eax
        adc	ProfStr[ecx].pure_dur_hi, edx
        mov	esi, proc_pure_overhead
        sub	ProfStr[ecx].pure_dur_lo, esi
        sbb	ProfStr[ecx].pure_dur_hi, 0

        dec	ProfStr[ecx].rec_level
        jnz	isRecurse3

;     procDesc->dirty_dur += (tsc - brackets_sum_overhead);
;     brackets_sum_overhead += procDesc->saved_overhead;
        sub	eax, brackets_sum_overhead_lo
        sbb	edx, brackets_sum_overhead_hi
        add	ProfStr[ecx].dirty_dur_lo, eax
        adc	ProfStr[ecx].dirty_dur_hi, edx
        mov	eax, ProfStr[ecx].saved_overhead_lo
        mov	edx, ProfStr[ecx].saved_overhead_hi
        add	brackets_sum_overhead_lo, eax
        adc	brackets_sum_overhead_hi, edx

isRecurse3:
        pop esi
        pop eax
        pop edx
        pop ecx
        ret 4
X2C_PROFILE_PROC_END endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_NEST_START ( VAR procDesc: ProfSTR );
; {
;   tsc := rdtsc();
;   procDesc->pure_dur += tsc;
; }

	align 4
X2C_PROFILE_NEST_START proc    near
public X2C_PROFILE_NEST_START
        push ecx
        push edx
        push eax
        mov	ecx, +16[esp]
        rdtsc
        add	ProfStr[ecx].pure_dur_lo, eax
        adc	ProfStr[ecx].pure_dur_hi, edx
        pop eax
        pop edx
        pop ecx
        ret 4
X2C_PROFILE_NEST_START endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_NEST_END ( VAR procDesc: ProfSTR );
; {
;   tsc := rdtsc();
;   procDesc->pure_dur -= (tsc + nest_pure_overhead);
;   brackets_sum_overhead += nest_dirty_overhead;
; }

	align 4
X2C_PROFILE_NEST_END proc    near
public X2C_PROFILE_NEST_END
        push ecx
        push edx
        push eax

        mov	ecx, +16[esp]
        rdtsc
        add	eax, nest_pure_overhead
        adc	edx,0
        sub	ProfStr[ecx].pure_dur_lo, eax
        sbb	ProfStr[ecx].pure_dur_hi, edx
        mov	eax, nest_dirty_overhead
        add	brackets_sum_overhead_lo, eax
        adc	brackets_sum_overhead_hi, 0

        pop eax
        pop edx
        pop ecx
        ret 4
X2C_PROFILE_NEST_END endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_TAIL_OVERHEAD ( VAR procDesc: ProfSTR;
;                                    bracket: BracketProc; VAR tsc: INT64 );

	align 4
X2C_PROFILE_TAIL_OVERHEAD proc    near
public X2C_PROFILE_TAIL_OVERHEAD
        push ecx
        push edx
        push eax

        mov	eax, +20[esp]	; eax := bracket
        mov	edx, +16[esp]	; edx := procDesc
        push	edx
        call	eax
        rdtsc
        mov	ecx, +24[esp]	; ecx := tsc
        mov	[ecx], eax
        mov	[ecx]+4, edx

        pop eax
        pop edx
        pop ecx
        ret 12
X2C_PROFILE_TAIL_OVERHEAD endp


; PROCEDURE ["StdCall"] / X2C_PROFILE_HEAD_OVERHEAD ( VAR procDesc: ProfSTR;
;                                    bracket: BracketProc; VAR tsc: INT64 );

	align 4
X2C_PROFILE_HEAD_OVERHEAD proc    near
public X2C_PROFILE_HEAD_OVERHEAD
        push ecx
        push edx
        push eax
        push esi

        mov	esi, +24[esp]	; esi := bracket
        mov	ecx, +20[esp]	; ecx := procDesc
        rdtsc
        push	ecx
        call	esi
        mov	ecx, +28[esp]	; ecx := tsc
        mov	[ecx], eax
        mov	[ecx]+4, edx

        pop esi
        pop eax
        pop edx
        pop ecx
        ret 12
X2C_PROFILE_HEAD_OVERHEAD endp


_TEXT           ends

                end
