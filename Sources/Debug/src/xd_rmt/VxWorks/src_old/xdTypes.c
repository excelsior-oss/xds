#include "xdTypes.h"

DWORD DWChEnd(DWORD dw){
  BYTE * p, b;

  p = (BYTE*)&dw; b = *p;
  *p = *(p+3);
  *(p+3) = b;
  b = *(p+1);
  *(p+1) = *(p+2);
  *(p+2) = b;
  return dw;
};

WORD WChEnd(WORD w){
  BYTE * p, b;

  p = (BYTE*)&w; b = *p;
  *p = *(p+1);
  *(p+1) = b;
  return w;
};

