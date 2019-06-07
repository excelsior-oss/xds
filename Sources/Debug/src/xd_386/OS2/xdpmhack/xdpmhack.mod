<* M2EXTENSIONS+ *>
MODULE XDPMHack;

IMPORT OS2;

--IMPORT io := InOut;

IMPORT arg := ProgEnv;
IMPORT fmt := FormStr;

CONST
  BEEP_WARN_FREQ = 1000; (* frequency of warning beep *)
  BEEP_WARN_DUR  = 200; (* duration of warning beep *)

VAR
   qmsg: OS2.QMSG;          (* message structure *)
   hab : OS2.HAB;
   hmq : OS2.HMQ;
   Started, Stopped: OS2.HEV;

   old: OS2.PFNWP;

TYPE
  CHARSET = SET OF CHAR;

VAR
  name, PID: ARRAY [0..255] OF CHAR;
  hwnd, i : CARDINAL;
  swp: OS2.SWP;
BEGIN
   IF arg.ArgNumber() # 2 THEN
     OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
     HALT (1);
   END;
   arg.GetArg(0, PID);

   hab := OS2.WinInitialize(0);

   IF OS2.NULLHANDLE = hab THEN
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      HALT (2);
   END;

   fmt.print(name, '\\SEM32\\xd_%s_debuggee_stopped.sem', PID);
   IF OS2.DosOpenEventSem(name, Stopped) # 0 THEN
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.WinTerminate(hab);
      HALT (3);
   END;

   fmt.print(name, '\\SEM32\\xd_%s_debuggee_started.sem', PID);
   IF OS2.DosOpenEventSem(name, Started) # 0 THEN
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.WinTerminate(hab);
      HALT (4);
   END;

   hmq := OS2.WinCreateMsgQueue(hab, 0);

   IF OS2.NULLHANDLE = hmq THEN
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.WinTerminate(hab);
      HALT (5);
   END;

  arg.GetArg(1, PID);
  hwnd := 0;
  FOR i := 0 TO 7 DO
    IF PID[i] IN CHARSET{'0'..'9'} THEN
      hwnd := 16*hwnd + (ORD(PID[i])-ORD('0'))
    ELSE
      hwnd := 16*hwnd + (ORD(PID[i])-ORD('A'))+10;
    END;
  END;
   
  LOOP
    OS2.DosWaitEventSem(Stopped, OS2.SEM_INDEFINITE_WAIT);

    IF NOT OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.HWND_THREADCAPTURE)  THEN
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.WinTerminate(hab);
      HALT (6);
    END;

    LOOP
      OS2.WinGetMsg(hab, qmsg, 0, 0, 0);
      OS2.WinQueryWindowPos(OS2.HWND(hwnd), swp);
--      io.WriteHex(qmsg.msg, 0); io.WriteCard(qmsg.msg, 0); io.WriteLn;
      IF (swp.x <= qmsg.ptl.x) AND (qmsg.ptl.x <= swp.x+swp.cx) AND
         (swp.y <= qmsg.ptl.y) AND (qmsg.ptl.y <= swp.y+swp.cy) THEN
        OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.NULLHANDLE);
        OS2.WinSendMsg(OS2.HWND(hwnd), qmsg.msg, qmsg.mp1, qmsg.mp2);
        OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.HWND_THREADCAPTURE);
      END;
      IF OS2.DosWaitEventSem(Started, OS2.SEM_IMMEDIATE_RETURN) = 0 THEN EXIT END;
    END;

    IF NOT OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.NULLHANDLE)  THEN
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
      OS2.WinTerminate(hab);
      HALT (7);
    END;
    WHILE OS2.WinPeekMsg(hab, qmsg, 0, 0, 0, OS2.PM_REMOVE) DO
    END;

  END;
END XDPMHack.
