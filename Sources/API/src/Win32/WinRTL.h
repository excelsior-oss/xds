#ifndef WinRTL_H_
#define WinRTL_H_

#define PCREATESTRUCT  LPCREATESTRUCT
#define PCREATESTRUCTA LPCREATESTRUCTA
#define PCREATESTRUCTW LPCREATESTRUCTW

#define MyInstance() (GetModuleHandle(NULL))

#define GetPSTR(x)    ((PSTR)x)
#define GetPWSTR(x)   ((PWSTR)x)

#define CharUpper1A(c) ((CHAR)CharUpperA((LPTSTR)c))
#define CharUpper1W(w) ((WCHAR)CharUpperW((LPTSTR)w))
#define CharLower1A(c) ((CHAR)CharLowerA((LPTSTR)c))
#define CharLower1W(w) ((WCHAR)CharLowerW((LPTSTR)w))

#define GetWindowPtrA(hWnd, Index) ((VOID*)GetWindowLongA(hWnd, Index))
#define GetWindowPtrW(hWnd, Index) ((VOID*)GetWindowLongW(hWnd, Index))

#define SetWindowPtrA(hWnd, Index, NewLong) ((VOID*)SetWindowLongA(hWnd, Index, NewLong))
#define SetWindowPtrW(hWnd, Index, NewLong) ((VOID*)SetWindowLongW(hWnd, Index, NewLong))

#define timergt(tvp, uvp)  (timercmp(tvp, uvp, >))
#define timerlt(tvp, uvp)  (timercmp(tvp, uvp, <))
#define timerequ(tvp, uvp) (timercmp(tvp, uvp, ==))

#endif /* WinRTL_H_ */