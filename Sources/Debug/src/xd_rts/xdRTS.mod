<* +STORAGE *>

<* NEW dbg_print- *>

IMPLEMENTATION MODULE xdRTS;

IMPORT sys := SYSTEM;

IMPORT xi  := XDInterface;
IMPORT xi4 := XDInterface4;

IMPORT ki  := KrnIndex;
IMPORT kt  := KrnTypes;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT eve := Events;
IMPORT exe := ExeMain;
IMPORT erc := ExeReact;
IMPORT mem := Exe_Mem;

IMPORT lst := List;

IMPORT opt := Options;

<* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>

FROM Printf IMPORT printf;

<* END *>

PROCEDURE Min(a, b: CARDINAL): CARDINAL;
BEGIN
  IF a > b THEN RETURN b ELSE RETURN a END;
END Min;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

VAR
  Handler: HANDLER;

PROCEDURE SetHandler (handler: HANDLER): HANDLER;
VAR
  tmp: HANDLER;
BEGIN
  tmp := Handler;
  Handler := handler;
  RETURN tmp;
END SetHandler;


PROCEDURE CallHandler (type: HandlerType; IP: kt.ADDRESS; msg-: ARRAY OF CHAR; SEQ args: sys.BYTE): BOOLEAN;
BEGIN
  IF Handler # NIL THEN
    RETURN Handler (type, IP, msg, args);
  END;
  RETURN FALSE;
END CallHandler;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

TYPE
  InterfaceInitializedResults = ( ii_Cannot_Read_Interface_Link
                                , ii_Cannot_Read_Interface
                                , ii_Interface_Version_Mismatch
                                , ii_Unknown_Interface
                                , ii_Interface_Initialized
                                , ii_Interface_Not_Initialized
                                );

  iiResultsMessage = ARRAY [0..31] OF CHAR;
  iiResultsMessages = ARRAY InterfaceInitializedResults OF iiResultsMessage;


CONST
  ResultsMessages = iiResultsMessages { "cannot read interface link"
                                      , "cannot read interface"
                                      , "interface version mismatch"
                                      , "unknown interface"
                                      , "interface initialized"
                                      , "interface not initialized"
                                      };


TYPE
  HANDLER_DESC = RECORD
                   Desc     : xi.HandlerDesc;
                   Address  : kt.ADDRESS;
                   BreakInx : CARDINAL;
                 END;

  HANDLERS = POINTER TO ARRAY OF HANDLER_DESC;


  XD_Interface = POINTER TO XD_Interface_Desc;

  XD_Interface_Desc = RECORD
                        iiResult          : InterfaceInitializedResults;
                        ComponentHandle   : CARDINAL;
                        ComponentBreakInx : CARDINAL;
                        MainEntry         : kt.ADDRESS;
                        MainEntryBreakInx : CARDINAL;
                        EERing            : kt.ADDRESS;
                        Handlers          : HANDLERS;
                      END;

CONST
  InitialInterface = XD_Interface_Desc { ii_Interface_Not_Initialized
                                       , dt.Invalid_Component
                                       , ki.Invalid_Index
                                       , kt.ADDRESS (0)
                                       , ki.Invalid_Index
                                       , kt.ADDRESS (0)
                                       , NIL
                                       };





VAR
  XD_Interface_List: lst.List;


PROCEDURE AddInterface (): XD_Interface;
VAR
  interface: XD_Interface;
BEGIN
  NEW (interface);
  lst.Assign (lst.Add (XD_Interface_List), interface);
  interface^ := InitialInterface;
  RETURN interface;
END AddInterface;


PROCEDURE GetInterface (item: lst.List): XD_Interface;
VAR
  interface: XD_Interface;
BEGIN
  interface := lst.Get (item);
  RETURN interface;
END GetInterface;


PROCEDURE DeleteInterface (VAR interface: XD_Interface);
VAR
  tmp: lst.List;
BEGIN
  IF interface = NIL THEN
    RETURN;
  END;
  tmp := lst.Find (XD_Interface_List, interface);
  lst.Delete (XD_Interface_List, tmp);
  lst.Dispose (tmp);
  DISPOSE (interface^.Handlers);
  DISPOSE (interface);
END DeleteInterface;



PROCEDURE FindInterfaceByHandle (Handle: CARDINAL): XD_Interface;
VAR
  tmp: lst.List;
  interface: XD_Interface;
BEGIN
  tmp := XD_Interface_List;
  WHILE tmp # NIL DO
    interface := GetInterface (tmp);
    IF interface^.ComponentHandle = Handle THEN
      RETURN interface;
    END;
    tmp := lst.Next (tmp);
  END;
  RETURN NIL;
END FindInterfaceByHandle;


PROCEDURE FindInterfaceByBreakInx (BreakInx: CARDINAL): XD_Interface;
VAR
  tmp: lst.List;
  interface: XD_Interface;
BEGIN
  tmp := XD_Interface_List;
  WHILE tmp # NIL DO
    interface := GetInterface (tmp);
    WITH interface^ DO
      IF (ComponentBreakInx = BreakInx) OR (MainEntryBreakInx = BreakInx) THEN
        RETURN interface;
      END;
    END;
    tmp := lst.Next (tmp);
  END;
  RETURN NIL;
END FindInterfaceByBreakInx;


PROCEDURE FindHandlerByBreakInx (HandlerBreakInx: CARDINAL; VAR handler_inx: CARDINAL): XD_Interface;
VAR
  tmp: lst.List;
  interface: XD_Interface;
  i: CARDINAL;
BEGIN
  tmp := XD_Interface_List;
  WHILE tmp # NIL DO
    interface := GetInterface (tmp);
    ASSERT (interface # NIL);
    WITH interface^ DO
      IF iiResult = ii_Interface_Initialized THEN
        FOR i := 0 TO HIGH (Handlers^) DO
          IF Handlers^[i].BreakInx = HandlerBreakInx THEN
            handler_inx := i;
            RETURN interface;
          END;
        END;
      END;
    END;
    tmp := lst.Next (tmp);
  END;
  RETURN NIL;
END FindHandlerByBreakInx;



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

PROCEDURE SetInterfaceHandlers (interface: XD_Interface; SoftwareExceptionsEnabled: BOOLEAN);
VAR
  i: CARDINAL;
BEGIN
  ASSERT (interface # NIL);
  WITH interface^ DO
    IF iiResult # ii_Interface_Initialized THEN
      RETURN;
    END;
    FOR i := 0 TO HIGH (Handlers^) DO
      IF Handlers^[i].Address # kt.NIL_ADDRESS THEN
        IF NOT Handlers^[i].Desc.Soft OR SoftwareExceptionsEnabled THEN
          sys.EVAL (exe.SetBreakPoint (Handlers^[i].Address, Handlers^[i].BreakInx));
        END;
      END;
    END;
  END;
END SetInterfaceHandlers;


PROCEDURE RemoveInterfaceHandlers (interface: XD_Interface);
VAR
  i: CARDINAL;
BEGIN
  ASSERT (interface # NIL);
  WITH interface^ DO
    IF iiResult # ii_Interface_Initialized THEN
      RETURN;
    END;
    FOR i := 0 TO HIGH (Handlers^) DO
      IF Handlers^[i].BreakInx # ki.Invalid_Index THEN
        sys.EVAL (exe.RemoveBreakPoint (Handlers^[i].BreakInx));
      END;
    END;
  END;
END RemoveInterfaceHandlers;



PROCEDURE FinalizeInterface (interface: XD_Interface);
BEGIN
  IF interface = NIL THEN
    RETURN;
  END;
  sys.EVAL (exe.RemoveBreakPoint (interface^.ComponentBreakInx));
  sys.EVAL (exe.RemoveBreakPoint (interface^.MainEntryBreakInx));
  RemoveInterfaceHandlers (interface);
  interface^.iiResult := ii_Interface_Not_Initialized;
END FinalizeInterface;



PROCEDURE DestroyInterface (Handle: CARDINAL);
VAR
  interface: XD_Interface;
BEGIN
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf (">>> DestroyInterface (Handle = 0x%$8X)\n", Handle);
 <* END *>
  interface := FindInterfaceByHandle (Handle);
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  IF interface # NIL THEN
    printf ("  Interface is found and destroyed\n");
  END;
 <* END *>
  FinalizeInterface (interface);
  DeleteInterface (interface);
END DestroyInterface;



PROCEDURE CreateInterface (Handle: CARDINAL);
VAR
  ComNo: dt.ComNo;
  entry: kt.ADDRESS;
  interface: XD_Interface;
BEGIN
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf (">>> CreateInterface (Handle = 0x%$8X)\n", Handle);
 <* END *>
  IF NOT tls.FindComponentByHandle (Handle, ComNo) THEN
    RETURN;
  END;
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf ("  Component %d found\n", ComNo);
 <* END *>
  IF NOT tls.FindPublicByNameInCom (ComNo, xi.IntializeProc, entry) THEN
    RETURN;
  END;
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf ("  %s is found at 0x%$8X\n", xi.IntializeProc, entry);
 <* END *>
  interface := FindInterfaceByHandle (Handle);
  IF interface = NIL THEN
    interface := AddInterface ();
    interface^.ComponentHandle := Handle;
  END;
  IF NOT exe.SetBreakPoint (entry, interface^.ComponentBreakInx) THEN
    DestroyInterface (Handle);
  END;
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf ("  Interface is waiting to be initialized\n");
 <* END *>
END CreateInterface;



PROCEDURE InitializeInterface (VAR interface: XD_Interface);
VAR
  pXD_Interface: xi.pXD_Interface;
  Interface: xi.XD_Interface;
  Interface_Java: xi.XD_Interface_Java;
  Interface_v4_Java: xi4.XD_Interface_v4_Java;
  i: CARDINAL;
BEGIN
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf (">>> InitializeInterface (interface = 0x%$8X)\n", interface);
  printf ("  Interface is about to be initialized\n");
 <* END *>
  pXD_Interface := NIL;
  IF NOT mem.Get (mem.GetSP()+SIZE(pXD_Interface), sys.ADR(pXD_Interface), SIZE(pXD_Interface)) THEN
    interface^.iiResult := ii_Cannot_Read_Interface_Link;
    RETURN;
  END;
  IF NOT mem.Get (kt.ADDRESS(pXD_Interface), sys.ADR(Interface), SIZE(Interface)) THEN
    interface^.iiResult := ii_Cannot_Read_Interface;
    RETURN;
  END;
  CASE Interface.Magic OF
  | xi4.XDInterface_v4_Magic_Java:
    IF NOT mem.Get (kt.ADDRESS(pXD_Interface), sys.ADR(Interface_v4_Java), SIZE(Interface_v4_Java)) THEN
      interface^.iiResult := ii_Cannot_Read_Interface;
      RETURN;
    END;
    IF Interface_v4_Java.Version # xi4.XDInterface_v4_Version_Java THEN
      interface^.iiResult := ii_Interface_Version_Mismatch;
      RETURN;
    END;
    interface^.MainEntry := kt.ADDRESS (Interface_v4_Java.MainAdr);
    interface^.EERing := kt.ADDRESS (Interface_v4_Java.EERing);
    IF interface^.Handlers = NIL THEN
      NEW (interface^.Handlers, ORD(MAX(xi4.Handlers_v4_Java))-ORD(MIN(xi4.Handlers_v4_Java))+1);
    END;
    WITH interface^ DO
      FOR i := 0 TO HIGH (Handlers^) DO
        WITH Handlers^[i] DO
          Desc     := xi4.Handlers_v4_List_Java[VAL(xi4.Handlers_v4_Java, i)];
          Address  := kt.ADDRESS (Interface_v4_Java.Handlers[VAL(xi4.Handlers_v4_Java, i)]);
          BreakInx := ki.Invalid_Index;
        END;
      END;
    END;

  | xi.XDInterfaceMagic_Java:
    IF NOT mem.Get (kt.ADDRESS(pXD_Interface), sys.ADR(Interface_Java), SIZE(Interface_Java)) THEN
      interface^.iiResult := ii_Cannot_Read_Interface;
      RETURN;
    END;
    IF Interface_Java.Version # xi.XDInterfaceVersion_Java THEN
      interface^.iiResult := ii_Interface_Version_Mismatch;
      RETURN;
    END;
    interface^.MainEntry := kt.ADDRESS (Interface_Java.MainAdr);
    interface^.EERing := kt.ADDRESS (Interface_Java.EERing);
    IF interface^.Handlers = NIL THEN
      NEW (interface^.Handlers, ORD(MAX(xi.Handlers_Java))-ORD(MIN(xi.Handlers_Java))+1);
    END;
    WITH interface^ DO
      FOR i := 0 TO Min(HIGH(Handlers^), Interface_Java.nHandlers-1) DO
        WITH Handlers^[i] DO
          Desc     := xi.Handlers_List_Java[VAL(xi.Handlers_Java, i)];
          Address  := kt.ADDRESS (Interface_Java.Handlers[VAL(xi.Handlers_Java, i)]);
          BreakInx := ki.Invalid_Index;
        END;
      END;
    END;
  ELSE
    interface^.iiResult := ii_Unknown_Interface;
    RETURN;
  END;
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf ("  Interface is initialized\n");
 <* END *>
  interface^.iiResult := ii_Interface_Initialized;
END InitializeInterface;


PROCEDURE ProcessBreakpointHit (BreakInx: CARDINAL);
VAR
  interface: XD_Interface;
  handler_no: CARDINAL;
BEGIN
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf (">>> ProcessBreakpointHit (BreakInx = %d)\n", BreakInx);
 <* END *>
  interface := FindInterfaceByBreakInx (BreakInx);
  IF interface # NIL THEN
    IF interface^.MainEntryBreakInx = BreakInx THEN
      sys.EVAL (exe.RemoveBreakPoint (interface^.MainEntryBreakInx));
      IF exe.ProgramSkipToMainEntry THEN
        exe.StopExec ();
      END;
    ELSE
      sys.EVAL (exe.RemoveBreakPoint (interface^.MainEntryBreakInx));
      RemoveInterfaceHandlers (interface);
      InitializeInterface (interface);
      IF interface^.iiResult # ii_Interface_Initialized THEN
        IF CallHandler (xi.htTrap, mem.GetIP (), "RTS Debugger Interface cannot be initialized\n\nReason: %s\nRestart at startup or go on executing", ResultsMessages[interface^.iiResult]) THEN
          exe.StopExec ();
        END;
        sys.EVAL (exe.RemoveBreakPoint (interface^.ComponentBreakInx));
        RETURN;
      END;
      SetInterfaceHandlers (interface, opt.ShowSoftwareException);
      IF interface^.MainEntry # kt.NIL_ADDRESS THEN
        sys.EVAL (exe.SetBreakPoint (interface^.MainEntry, interface^.MainEntryBreakInx));
      END;
    END;
    RETURN;
  END;
  interface := FindHandlerByBreakInx (BreakInx, handler_no);
  IF interface # NIL THEN
    -- FIX ME
    -- нужно достатать параметры исключения - как?
    WITH interface^.Handlers^[handler_no] DO
      IF CallHandler (Desc.Type, mem.GetIP (), Desc.Message) THEN
        exe.StopExec ();
      END;
    END;
    RETURN;
  END;
END ProcessBreakpointHit;



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

PROCEDURE CompCreated (data: erc.DATA);
BEGIN
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf (">>> ComponentCreated\n");
 <* END *>
  ASSERT (data = NIL);
  ASSERT (eve.LastEvent.Event = eve.CompCreated);
  CreateInterface (eve.LastEvent.Component.Handle);
END CompCreated;



PROCEDURE CompDestroyed (data: erc.DATA);
BEGIN
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf (">>> ComponentDestroyed\n");
 <* END *>
  ASSERT (data = NIL);
  ASSERT (eve.LastEvent.Event = eve.CompDestroyed);
  DestroyInterface (eve.LastEvent.Handle);
END CompDestroyed;


PROCEDURE BreakpointHit (data: erc.DATA);
BEGIN
 <* IF DEFINED (xd_debug) AND xd_debug AND dbg_print THEN *>
  printf (">>> BreakpointHit\n");
 <* END *>
  ASSERT (data = NIL);
  ASSERT (eve.LastEvent.Event = eve.BreakpointHit);
  ProcessBreakpointHit (eve.LastEvent.BreakpointInd);
END BreakpointHit;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


PROCEDURE XDInterfacePresent (): BOOLEAN;
BEGIN
  RETURN XD_Interface_List # NIL;
END XDInterfacePresent;


PROCEDURE DestroyXDInterfaces;
VAR
  interface: XD_Interface;
BEGIN
  WHILE XD_Interface_List # NIL DO
    interface := GetInterface (XD_Interface_List);
    FinalizeInterface (interface);
    DeleteInterface (interface);
  END;
END DestroyXDInterfaces;


PROCEDURE SetXDInterfacesHandlers (SoftwareExceptionsEnabled: BOOLEAN);
VAR
  tmp: lst.List;
BEGIN
  tmp := XD_Interface_List;
  WHILE tmp # NIL DO
    SetInterfaceHandlers (GetInterface (tmp), SoftwareExceptionsEnabled);
    tmp := lst.Next (tmp);
  END;
END SetXDInterfacesHandlers;


PROCEDURE RemoveXDInterfacesHandlers;
VAR
  tmp: lst.List;
BEGIN
  tmp := XD_Interface_List;
  WHILE tmp # NIL DO
    RemoveInterfaceHandlers (GetInterface (tmp));
    tmp := lst.Next (tmp);
  END;
END RemoveXDInterfacesHandlers;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


BEGIN
  erc.Ini ();
  erc.AddAction (eve.CompCreated,   NIL, CompCreated);
  erc.AddAction (eve.CompDestroyed, NIL, CompDestroyed);
  erc.AddAction (eve.BreakpointHit, NIL, BreakpointHit);
  XD_Interface_List := NIL;
  Handler := NIL;
END xdRTS.