; COPYRIGHT (c) 1999 XDS Ltd. All Rights Reserved.
; COPYRIGHT (c) 2002 Excelsior LLC. All Rights Reserved.
; 31.03.99 paul: created
; 23.11.2002 jek: converted to NASM

; Implementation for performance monitoring functions

                cpu 586
      bits 32

global proc_dirty_overhead_rec, proc_dirty_overhead_norec, proc_pure_overhead
global nest_dirty_overhead, nest_pure_overhead
global brackets_sum_overhead_lo, brackets_sum_overhead_hi


%ifdef OS2
group       DGROUP _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA' 
      section _DATA
%else
group       DGROUP _DATA
      section _DATA  use32  align=4  public 'DATA' 
      section _DATA
%endif

    proc_dirty_overhead_rec:  dd 0
    proc_dirty_overhead_norec:   dd 0
    proc_pure_overhead:    dd 0
    nest_dirty_overhead:   dd 0
    nest_pure_overhead:    dd 0
    brackets_sum_overhead_lo: dd 0
    brackets_sum_overhead_hi: dd 0

global X2C_prof_enabled
    X2C_prof_enabled:      dd 0

struc CPU_Info
    .CPUID_supported    :resd 1
    .max_CPUID_call  :resd 1

;   vendor ARRAY 12 OF CHAR;

    .vendor_lo    :resd 1
    .vendor_med      :resd 1
    .vendor_hi    :resd 1

    .cputype      :resd 1
    .family    :resd 1
    .model     :resd 1
    .stepping     :resd 1
    .features     :resd 1
endstruc


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

struc CallType
   .ct_ip      :resd 1
   .ct_count   :resd 1
   .ct_next    :resd 1
endstruc

struc ProfStr
   .pure_dur_lo      :resd 1
   .pure_dur_hi      :resd 1
   .dirty_dur_lo     :resd 1
   .dirty_dur_hi     :resd 1
   .saved_overhead_lo   :resd 1
   .saved_overhead_hi   :resd 1
   .total_entry_count   :resd 1
   .norec_entry_count   :resd 1
   .rec_level     :resd 1
   .call_list     :resd 1
endstruc


%ifdef OS2
      section .text  use32  public  align=4  FLAT  public 'CODE'
%else
      section .text  use32  public  align=16  public 'CODE'
%endif

;                assume  cs:.text

;PROCEDURE ["StdCall"] X2C_Read_TSC(VAR timestamp: INT64);
global X2C_Read_TSC
X2C_Read_TSC:
        mov ecx, [esp+4]
        rdtsc
        mov [ecx], eax
        mov [ecx+4], edx
        ret 4
;X2C_Read_TSC endp


;PROCEDURE ["StdCall"] X2C_Open_TSC_Scope(VAR timestamp: INT64);
global X2C_Open_TSC_Scope
X2C_Open_TSC_Scope:
        mov ecx, [esp+4]
        rdtsc
        sub [ecx], eax
        sbb [ecx+4], edx
        ret 4
;X2C_Open_TSC_Scope endp


;PROCEDURE ["StdCall"] X2C_Close_TSC_Scope(VAR timestamp: INT64);
global X2C_Close_TSC_Scope
X2C_Close_TSC_Scope:
        mov ecx, [esp+4]
        rdtsc
        add [ecx], eax
        adc [ecx+4], edx
        ret 4
;X2C_Close_TSC_Scope endp


; returns: eax == ( CPUID is present ? 1 : 0 )
is_CPUID_supported:
        xor eax, eax
        pushfd
        btr dword [esp], 21
        popfd
        pushfd
        bt  dword [esp], 21
        jc  there_isnt_CPUID
        bts dword [esp], 21
        popfd
        pushfd
        bt  dword [esp], 21
        jnc there_isnt_CPUID
; CPUID supported
        inc eax
there_isnt_CPUID:
        popfd
        ret
;is_CPUID_supported endp

;PROCEDURE ["StdCall"] X2C_GetCPUInfo( VAR info: CPU_Info );
global X2C_GetCPUInfo
X2C_GetCPUInfo:
        mov eax, [esp+4]
        pusha
        mov esi, eax

        call    is_CPUID_supported
        mov [esi+CPU_Info.CPUID_supported], eax
        test    eax, eax
        jnz do_CPUID

        mov ebx, eax
        mov ecx, eax
        mov edx, eax
        jmp short set_info
do_CPUID:
        xor eax, eax
        cpuid
set_info:
        mov [esi+CPU_Info.max_CPUID_call], eax
        mov [esi+CPU_Info.vendor_lo], ebx
        mov [esi+CPU_Info.vendor_med], edx
        mov [esi+CPU_Info.vendor_hi], ecx

        xor edx, edx
        test    eax, eax
        jz  no_features_info
        mov eax, 1
        cpuid
no_features_info:
        mov [esi+CPU_Info.features], edx
        mov ebx, eax
        shr ebx, 4
        and ebx, 030fh
        movzx  ecx, bh     ; ecx == type
        mov bh, 0       ; ebx == model
        and eax, 0f0fh
        movzx  edx, ah     ; edx == family
        mov ah, 0       ; eax == stepping
        mov [esi+CPU_Info.cputype], ecx
        mov [esi+CPU_Info.family], edx
        mov [esi+CPU_Info.model], ebx
        mov [esi+CPU_Info.stepping], eax

        popa
        ret 4
;X2C_GetCPUInfo endp

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
global X2C_PROFILE_PROC_START
X2C_PROFILE_PROC_START:
;local isRecurse1
        push ecx
        push edx
        push eax

        mov ecx, [esp+16]     ; ecx := procDesc
        rdtsc
        sub [ecx+ProfStr.pure_dur_lo], eax   ; pure_dur -= tsc
        sbb [ecx+ProfStr.pure_dur_hi], edx
                            
        inc dword [ecx+ProfStr.total_entry_count]
        inc dword [ecx+ProfStr.rec_level]
        cmp dword [ecx+ProfStr.rec_level], 1
        ja  isRecurse1

        inc dword [ecx+ProfStr.norec_entry_count]
        sub [ecx+ProfStr.dirty_dur_lo], eax
        sbb [ecx+ProfStr.dirty_dur_hi], edx

; procDesc->saved_overhead := brackets_sum_overhead;
; brackets_sum_overhead := proc_dirty_overhead_norec;
   mov   eax, [proc_dirty_overhead_norec]
        xor edx, edx
        xchg   eax, [brackets_sum_overhead_lo]
        xchg   edx, [brackets_sum_overhead_hi]
        mov [ecx+ProfStr.saved_overhead_lo], eax
        mov [ecx+ProfStr.saved_overhead_hi], edx

        pop eax
        pop edx
        pop ecx
        ret 4

isRecurse1:
   mov   eax, [proc_dirty_overhead_rec]
        add [brackets_sum_overhead_lo], eax
        adc dword [brackets_sum_overhead_hi], 0

        pop eax
        pop edx
        pop ecx
        ret 4
;X2C_PROFILE_PROC_START endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_PROC_START_C ( VAR procDesc: ProfSTR;
;                                                        callerIP: ADDRESS );
extern X2C_malloc
   align 4

global X2C_PROFILE_PROC_START_C
X2C_PROFILE_PROC_START_C:
        push esi
        push edi
        push eax

        mov esi, [esp+16]     ; esi := procDesc
   lea   eax, [esi+ProfStr.call_list - CallType.ct_next]
        mov esi, [esp+20]     ; esi := caller's IP
cont:
        mov edi, eax    ; edi := &last_elem
        mov eax, [eax+CallType.ct_next]
        test   eax, eax
        jz  end_of_list
        cmp esi, [eax+CallType.ct_ip]
        jne cont
        inc dword [eax+CallType.ct_count]
        jmp short exit
end_of_list:                
        push ecx
        push edx
        push byte  12
        call   X2C_malloc
        pop edx   ; drop '12'
        pop edx
        pop ecx

        mov [edi+CallType.ct_next], eax   ; last_elem->next == &new_elem
        mov [eax+CallType.ct_ip], esi
        mov dword [eax+CallType.ct_count], 1
        mov dword [eax+CallType.ct_next], 0

exit:
        pop eax
        pop edi
        pop esi
;        ret 4
   jmp   X2C_PROFILE_PROC_START
;X2C_PROFILE_PROC_START_C endp

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
global X2C_PROFILE_PROC_END
X2C_PROFILE_PROC_END:
;local isRecurse3
        push ecx
        push edx
        push eax
        push esi

        mov ecx, [esp+20]
        rdtsc
        add [ecx+ProfStr.pure_dur_lo], eax
        adc [ecx+ProfStr.pure_dur_hi], edx
        mov esi, [proc_pure_overhead]
        sub [ecx+ProfStr.pure_dur_lo], esi
        sbb dword [ecx+ProfStr.pure_dur_hi], 0

        dec dword [ecx+ProfStr.rec_level]
        jnz isRecurse3

;     procDesc->dirty_dur += (tsc - brackets_sum_overhead);
;     brackets_sum_overhead += procDesc->saved_overhead;
        sub eax, [brackets_sum_overhead_lo]
        sbb edx, [brackets_sum_overhead_hi]
        add [ecx+ProfStr.dirty_dur_lo], eax
        adc [ecx+ProfStr.dirty_dur_hi], edx
        mov eax, [ecx+ProfStr.saved_overhead_lo]
        mov edx, [ecx+ProfStr.saved_overhead_hi]
        add [brackets_sum_overhead_lo], eax
        adc [brackets_sum_overhead_hi], edx

isRecurse3:
        pop esi
        pop eax
        pop edx
        pop ecx
        ret 4
;X2C_PROFILE_PROC_END endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_NEST_START ( VAR procDesc: ProfSTR );
; {
;   tsc := rdtsc();
;   procDesc->pure_dur += tsc;
; }

   align 4
global X2C_PROFILE_NEST_START
X2C_PROFILE_NEST_START:
        push ecx
        push edx
        push eax
        mov ecx, [esp+16]
        rdtsc
        add [ecx+ProfStr.pure_dur_lo], eax
        adc [ecx+ProfStr.pure_dur_hi], edx
        pop eax
        pop edx
        pop ecx
        ret 4
;X2C_PROFILE_NEST_START endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_NEST_END ( VAR procDesc: ProfSTR );
; {
;   tsc := rdtsc();
;   procDesc->pure_dur -= (tsc + nest_pure_overhead);
;   brackets_sum_overhead += nest_dirty_overhead;
; }

   align 4
global X2C_PROFILE_NEST_END
X2C_PROFILE_NEST_END:
        push ecx
        push edx
        push eax

        mov ecx, [esp+16]
        rdtsc
        add eax, [nest_pure_overhead]
        adc edx, 0
        sub [ecx+ProfStr.pure_dur_lo], eax
        sbb [ecx+ProfStr.pure_dur_hi], edx
        mov eax, [nest_dirty_overhead]
        add [brackets_sum_overhead_lo], eax
        adc dword [brackets_sum_overhead_hi], 0

        pop eax
        pop edx
        pop ecx
        ret 4
;X2C_PROFILE_NEST_END endp

; PROCEDURE ["StdCall"] / X2C_PROFILE_TAIL_OVERHEAD ( VAR procDesc: ProfSTR;
;                                    bracket: BracketProc; VAR tsc: INT64 );

   align 4
global X2C_PROFILE_TAIL_OVERHEAD
X2C_PROFILE_TAIL_OVERHEAD:
        push ecx
        push edx
        push eax

        mov eax, [esp+20]  ; eax := bracket
        mov edx, [esp+16]  ; edx := procDesc
        push   edx
        call   eax
        rdtsc
        mov ecx, [esp+24]  ; ecx := tsc
        mov [ecx], eax
        mov [ecx+4], edx

        pop eax
        pop edx
        pop ecx
        ret 12
;X2C_PROFILE_TAIL_OVERHEAD endp


; PROCEDURE ["StdCall"] / X2C_PROFILE_HEAD_OVERHEAD ( VAR procDesc: ProfSTR;
;                                    bracket: BracketProc; VAR tsc: INT64 );

   align 4
global X2C_PROFILE_HEAD_OVERHEAD
X2C_PROFILE_HEAD_OVERHEAD:
        push ecx
        push edx
        push eax
        push esi

        mov esi, [esp+24]  ; esi := bracket
        mov ecx, [esp+20]  ; ecx := procDesc
        rdtsc               
        push   ecx         
        call   esi         
        mov ecx, [esp+28]  ; ecx := tsc
        mov [ecx], eax
        mov [ecx+4], edx

        pop esi
        pop eax
        pop edx
        pop ecx
        ret 12
;X2C_PROFILE_HEAD_OVERHEAD endp


