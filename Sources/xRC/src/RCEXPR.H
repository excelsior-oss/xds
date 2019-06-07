#define CalcExpr(x) CalcExprVal((x),0,0)
#define CalcExprEx(x,y) CalcExprVal((x),(y),0)
unsigned long CalcExprVal(int dontget,int pmode,unsigned long ival);
