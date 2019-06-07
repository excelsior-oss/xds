#ifdef K_INC
#define DLMDEF(x,d) D_##x,
#define OPDEF(x,d,p,a) D_##x,
#else
#undef DLMDEF
#undef OPDEF
#define DLMDEF(x,d) {d,D_TYPE_DLM,D_##x,0},
#define OPDEF(x,d,p,a) {d,D_TYPE_OP,D_##x,p,a},
#endif

DLMDEF(COMMA,",")
DLMDEF(LPAR,"(")
DLMDEF(RPAR,")")

OPDEF(LE,"<=",10,OP_B)
OPDEF(LSHIFT,"<<",11,OP_B)
OPDEF(RSHIFT,">>",11,OP_B)
OPDEF(GE,">=",10,OP_B)
OPDEF(EQ,"==",9,OP_B)
OPDEF(NE,"!=",9,OP_B)
OPDEF(AND,"&&",8,OP_B)
OPDEF(OR,"||",8,OP_B)


OPDEF(NOT,"!",14,OP_U)
OPDEF(BNOT,"~",14,OP_U)
OPDEF(MULT,"*",13,OP_B)
OPDEF(DIV,"/",13,OP_B)
OPDEF(REM,"%",13,OP_B)
OPDEF(PLUS,"+",12,OP_UB)
OPDEF(MINUS,"-",12,OP_UB)
OPDEF(LT,"<",10,OP_B)
OPDEF(GT,">",10,OP_B)
OPDEF(BAND,"&",8,OP_B)
OPDEF(BXOR,"^",8,OP_B)
OPDEF(BOR,"|",8,OP_B)

DLMDEF(ASSIGN,"=")

