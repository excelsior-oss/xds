# Run-time support for XDS optimizing compiler (Linux ELF version)

.equ    _X2C_INDEX      ,     0
.equ    _X2C_RANGE      ,     1
.equ    _X2C_NIL                ,     3
.equ    _X2C_OVERFLOW   ,     5
.equ    _X2C_DIVISION   ,     6
.equ    _X2C_FLT_OVERFL ,     7
.equ    _X2C_FLT_DIV    ,     8


                .global  _X2C_FLT_USED
                .global  SEEK_SET
                .global  SEEK_CUR
                .global  SEEK_END
                .global  P_WAIT
                .global  O_BINARY
                .global  O_CREAT
                .global  O_RDONLY
                .global  O_WRONLY
                .global  O_RDWR
                .global  S_IREAD
                .global  S_IWRITE


.data
.align 4

_X2C_FLT_USED:  .long      0
SEEK_SET    :  .long      0
SEEK_CUR    :  .long      1
SEEK_END    :  .long      2
P_WAIT      :  .long      0
O_BINARY    :  .long      00000  # don't know what to put here, there is no such flag in linux
O_CREAT     :  .long      00100
O_RDONLY    :  .long      00000
O_WRONLY    :  .long      00001
O_RDWR      :  .long      00002
S_IREAD     :  .long      00400
S_IWRITE    :  .long      00200
fp_env      :  .zero      28 
ErrCode     :  .long      0
ErrEIP      :  .long      0
UnableStr   :  .asciz    "Unable to process exception\020"
FPP_Init    :  .long      0
code_from   :  .long      1  # when code_from>code_to no pointer referencing
code_to     :  .long      0  # occurs in _X2C_IS_CALL.


.text
.align 4


# ==============================================================================

               .global  setjmp
setjmp:   jmp     _setjmp

# ==============================================================================

#               .global  _X2C_FINALLY
#_X2C_FINALLY:  jmp _X2C_FINALEXE

# ==============================================================================

                .global  _X2C_TRAP_NIL

_X2C_TRAP_NIL:
                popl     %eax
                pushl    $_X2C_NIL
                pushl    %eax
                jmp      _X2C_TRAP_F

# ==============================================================================

                .global  _X2C_TRAP_DIV

_X2C_TRAP_DIV  :
                popl     %eax
                pushl    $_X2C_DIVISION
                pushl    %eax
                jmp      _X2C_TRAP_F

# ==============================================================================

                .global  _X2C_TRAP_OVERFL

_X2C_TRAP_OVERFL: 
                popl     %eax
                pushl    $_X2C_OVERFLOW
                pushl    %eax
                jmp      _X2C_TRAP_F

# ==============================================================================

                .global  _X2C_TRAP_RANGE

_X2C_TRAP_RANGE :
                popl     %eax
                pushl    $_X2C_RANGE
                pushl    %eax
                jmp      _X2C_TRAP_F

# ==============================================================================

                .global  _X2C_TRAP_INDEX

_X2C_TRAP_INDEX:
                popl     %eax
                pushl    $_X2C_INDEX
                pushl    %eax
                jmp      _X2C_TRAP_F

# ==============================================================================

Exception:
                cmpl    $0, _X2C_FLT_USED
                je      L1
                fstenv  fp_env
                andw    $0x0c700,  fp_env+4
                movw    $0x0ffff,  fp_env+8
                fldenv  fp_env
L1:             pushl   ErrCode
                pushl   ErrEIP
                jmp     _X2C_TRAP_F

# ==============================================================================


filter:
                pushl   %ebx
                pushl   %esi
                pushl   %edi
                movl    16(%esp), %ebx           # ExceptionInfo
                movl    (%ebx), %eax             # ExceptionRecord
                movl    4(%eax), %edi            # ExceptionFlags
                testl   %edi, %edi
                je      filter_cont

                pushl    $UnableStr
                call    _printf
                addl    $4, %esp
                jmp     filter_err
filter_cont:
                movl   (%eax), %eax              # ExceptionCode

                movl    $_X2C_FLT_DIV, %edi
                cmpl    $0x0c000008e, %eax
                je      filter_ok

                movl     $_X2C_FLT_OVERFL, %edi
                cmpl     $0x0c000008d, %eax
                je       filter_ok
                cmpl     $0x0c000008f, %eax
                je       filter_ok
                cmpl     $0x0c0000090, %eax
                je       filter_ok
                cmpl     $0x0c0000091, %eax
                je       filter_ok
                cmpl     $0x0c0000092, %eax
                je       filter_ok
                cmpl     $0x0c0000093, %eax
                je       filter_ok

                movl     $_X2C_DIVISION, %edi
                cmpl     $0x0c0000094, %eax
                je       filter_ok

                movl     $_X2C_OVERFLOW, %edi
                cmpl     $0x0c0000095, %eax
                je       filter_ok

                movl     $_X2C_INDEX, %edi
                cmpl     $0x0c000008c, %eax
                je       filter_ok
filter_err:
                movl     $1, %eax
                jmp      filter_ret
filter_ok:
                movl     %edi, ErrCode
                movl     4(%ebx),%eax                 # ContextRecord
                movl     0xb8(%eax),%edx                 # Eip
                movl     %edx,ErrEIP
                movl     $Exception,  0x0B8(%eax)
                movl     $0x0ffffffff, %eax
filter_ret:
                popl     %edi
                popl     %esi
                popl     %ebx
                ret      $4

# ==============================================================================

                .global  _X2C_NATIVE_BEGIN
_X2C_NATIVE_BEGIN :

                ret

# ==============================================================================
# Determines wether given pointer points to a procedure call or not.
# Returns pointer to call instruction if any, otherwise zero
# void*  _X2C_IS_CALL ( void* )

.global  _X2C_IS_CALL
.global __X2C_IS_CALL
.global  _X2C_SET_CODE_EXTENT
.global __X2C_SET_CODE_EXTENT


#PROCEDURE / _X2C_SET_CODE_EXTENT(from,to: SYSTEM.ADDRESS);
 _X2C_SET_CODE_EXTENT:
__X2C_SET_CODE_EXTENT:
                        movl    4(%esp), %eax
                        addl    $12,%eax        #to be sure that x2c_is_call won't attempt to access memory ot of [code_from..code_to] range.
                        movl    %eax,code_from
                        movl    8(%esp),%eax
                        movl    %eax,code_to
                        ret

#PROCEDURE / _X2C_IS_CALL(i: LONGINT): LONGINT;
 _X2C_IS_CALL:
__X2C_IS_CALL:
                movl     4(%esp), %eax

                movl     code_from, %ecx
                cmpl     %ecx, %eax
                jb       quit
                cmpl     code_to, %eax
                jbe      check
quit:           xorl     %eax, %eax
                ret

check:          decl     %eax
                decl     %eax
                cmpb    $0x0FF, (%eax)
                jne     not0
                movb    1(%eax),%cl
                cmpb    $0x14, %cl                 # не бывает [esp]
                je      not0
                cmpb    $0x15, %cl                 # и [ebp]
                je      not0
                cmpb    $0x0D4h, %cl
                je      not0
                andb    $0x0F8, %cl
                cmpb    $0x0D0, %cl
                je      true
                cmpb    $0x10, %cl
                je      true

not0:           decl    %eax
                cmpb    $0x0FF, (%eax)
                jne     not1
                movb    1(%eax), %cl
                cmpb    $0x54, %cl                 # не бывает d8 [esp]
                je      not1
                andb    $0x0F8, %cl
                cmpb    $0x50, %cl
                je      true

                cmpb    $0x14, 1(%eax)
                jne     not1
                movb    2(%eax), %cl
                andb    $7, %cl
                cmpb    $5, %cl                    # не бывает [ebp + scale * reg2]
                jne     true

not1:           decl    %eax
                cmpb    $0x0FF, (%eax)
                jne     not2
                movb    1(%eax),%cl
                andb    $0x0F8, %cl
                cmpb    $0x050, %cl
                je      true

not2:           decl    %eax
                cmpb    $0x0E8, (%eax)
                je      true

                decl    %eax
                cmpb    $0x0FF, (%eax)
                jne     not3
                movb    -5(%eax), %cl
                cmpb    $0x094, %cl                 # не бывает d32 [esp]
                je      not3
                andb    $0x0B8, %cl
                cmpb    $0x090, %cl
                je      true

not3:           decl    %eax
                cmpb    $0x0FF, (%eax)
                jne     false
                cmpb    $0x94, 1(%eax)
                je      true

false:          xorl    %eax, %eax
true:           ret


#===============================================================================
# Clear exception flags, empty FP stack & set control word.
# Set FPP control word:
#     - all exceptions, except precision loss and denormalized operand, enabled.
#     - presision control set to 11 -- 64 bit extended presision
#     - rounding control set to 00 -- round to nearest or even

                .global  _X2C_InitFPP
_X2C_InitFPP :
                 cmpl    $0, FPP_Init
                 jne     _X2C_InitFPP_L
                 fninit
                 pushl    $0x0332
                 fldcw   (%esp)
                 pop     %eax
                 incl    FPP_Init
 _X2C_InitFPP_L:  ret




# Grabbed from gcc's output of the folowing:
#  long double _X2C_EXPRR(long double base,long double ex)
#  {
#     return pow(base,ex);
#  }

                .global _X2C_EXPRR
_X2C_EXPRR:
        pushl %ebp
        movl %esp,%ebp
        pushl 28(%ebp)
        pushl 24(%ebp)
        pushl 20(%ebp)
        pushl 16(%ebp)
        pushl 12(%ebp)
        pushl 8(%ebp)
        call _pow
        addl $24,%esp
        movl %eax,%eax
        pushl %eax
        fildl (%esp)
        addl $4,%esp
        jmp .L1
        .align 4
.L1:
        movl %ebp,%esp
        popl %ebp
        ret
