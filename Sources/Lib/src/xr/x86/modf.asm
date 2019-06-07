                cpu 386
      bits 32

%ifdef OS2
group       DGROUP _DATA
      section _DATA  use32  align=4  FLAT  public 'DATA' 
%else
group       DGROUP _DATA
      section _DATA  use32  align=4  public 'DATA' 
%endif

%ifdef OS2
      section .text  use32  align=4  FLAT  public 'CODE' 
%else
      section .text  use32  align=16  public 'CODE'
%endif

;                assume  cs: .text, ds: DGROUP, ss: DGROUP, gs: nothing, fs: nothing

                global  X2C_modf
X2C_modf:
                push    ebp
                push    esi
                push    edi
                push    ebx
                mov     esi, [esp+28]
                lea     ebp, [esp+20]
                mov     eax, dword [ebp]
                mov     edx, dword [ebp+4]
                xchg    ebp, esi
                mov     dword [ebp], eax
                mov     dword [ebp+4], edx
                mov     ecx, edx
                and     ecx, 7FF00000H
                je      modf_1
                cmp     ecx, 43300000H
                jb      modf_2
                xchg    esi, ebp
modf_1:         xor     eax, eax
                mov     dword [ebp], eax
                mov     dword [ebp+4], eax
                pop     ebx
                pop     edi
                pop     esi
                pop     ebp
                fld     qword [esp+4]
                ret     12
modf_2:         mov     edi, ecx
                shr     ecx, 20
                sub     cx, 03FFH
                jb      modf_1
                push    edx
                xor     eax, eax
                mov     edx, 0FFF00000H
                cmp     cl, 20
                jbe     modf_3
                sar     edx, 21
                rcr     eax, 1
                sub     cl, 21
                sar     eax, cl
                jmp     short modf_4
modf_3:         sar     edx, cl
modf_4:         and     dword [ebp], eax
                and     dword [ebp+4], edx
                not     edx
                not     eax
                mov     ebp, esi
                and     eax, dword [ebp]
                and     edx, dword [ebp+4]
                xor     esi, esi
                or      esi, eax
                or      esi, edx
                je      modf_13
                test    edx, 0FFF00000H
                jne     modf_10
modf_9:         sub     edi, 00100000h
                add     eax, eax
                adc     edx, edx
                test    edx, 0FFF00000H
                je      modf_9
                jmp     short modf_12
modf_10:        test    edx, 0FFE00000H
                je      modf_12
modf_11:        add     edi, 00100000h
                shr     edx, 1
                rcr     eax, 1
                rcr     esi, 1
                test    edx, 0FFE00000H
                jne     modf_11
                adc     esi, esi
                adc     eax, 00000000h
                adc     edx, 00000000h
modf_12:        and     edx, 000FFFFFH
                or      edx, edi
modf_13:        pop     esi
                test    edx, edx
                je      modf_5
                and     esi, 80000000H
                or      edx, esi
modf_5:         mov     dword [ebp], eax
                mov     dword [ebp+4], edx
                pop     ebx
                pop     edi
                pop     esi
                pop     ebp
                fld     qword [esp+4]
                ret     0CH
;X2C_modf        endp

