IMPLEMENTATION MODULE CB_Manag;

IMPORT sys := SYSTEM;
IMPORT win := Windows;

PROCEDURE get (VAR s: ARRAY OF CHAR): BOOLEAN;  
VAR
  h  : win.HANDLE;
  ptr: win.PVOID;
  len: INTEGER;
BEGIN
  IF NOT win.OpenClipboard (NIL) THEN
    -- Clipboard is busy
    RETURN FALSE; 
  END;
  IF NOT win.IsClipboardFormatAvailable (win.CF_TEXT) THEN
    -- No text in clipboard
    win.CloseClipboard;
    RETURN FALSE;
  END;
  h := win.GetClipboardData (win.CF_TEXT);
  IF h = NIL THEN
    -- No text in clipboard
    win.CloseClipboard;
    RETURN FALSE;
  END;
  len := win.GlobalSize (win.HGLOBAL(h));
  IF VAL(INTEGER, HIGH(s)) < len THEN
    -- Data too large
    win.CloseClipboard ();
    RETURN FALSE;
  END;
  ptr := win.GlobalLock (win.HGLOBAL(h));
  sys.MOVE (ptr, sys.ADR(s), len);
  s[len] := 0C;
  win.GlobalUnlock (win.HGLOBAL(h));
  win.CloseClipboard ();
  RETURN TRUE;
END get;


PROCEDURE put (s-: ARRAY OF CHAR): BOOLEAN;
VAR
  len: INTEGER;
  p: win.PVOID;
  h: win.HGLOBAL;
BEGIN
  len := LENGTH (s);
  IF NOT win.OpenClipboard (NIL) THEN
    -- Clipboard is busy
    RETURN FALSE;
  END;
  h := win.GlobalAlloc (win.GMEM_MOVEABLE+win.GMEM_DDESHARE, len+1);
  IF h = NIL THEN
    -- Can't allocate memory
    win.CloseClipboard ();
    RETURN FALSE;
  END;
  p := win.GlobalLock (h);
  sys.MOVE (sys.ADR(s), p, len+1);
  win.GlobalUnlock (h);
  win.EmptyClipboard ();
  win.SetClipboardData (win.CF_TEXT, win.HANDLE(h)); -- use CF_OEMTEXT here if text is OEM
  win.CloseClipboard ();
  RETURN TRUE;
END put;


BEGIN
END CB_Manag.