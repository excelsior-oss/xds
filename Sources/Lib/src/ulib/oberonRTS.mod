(* Copyright (c) xTech 1992,94. All Rights were Reserved *)
(* Copyright (c) 2000 Excelsior *)

(* XDS (ANSI C) v2.0 *)
<*+ M2EXTENSIONS *>
<*- CHECKNIL *>
<*- CHECKRANGE *>
<*- CHECKINDEX *>
IMPLEMENTATION MODULE oberonRTS; (* Ned 19-Jul-94 *)

IMPORT  SYSTEM, X2C, RTS:=xmRTS, xrMM, xrBlockManager;

<* IF env_target = 'x86linux' THEN *>
IMPORT rlimit;
<* END *>

TYPE
  Module = RTS.X2C_MD;
  Type   = RTS.X2C_TD;
  Name   = POINTER TO ARRAY [0..127] OF CHAR;
  CMDs   = POINTER TO ARRAY [0..4095] OF Command;
  CNMs   = POINTER TO ARRAY [0..4095] OF SYSTEM.ADDRESS;


PROCEDURE SetHeaplimit (newHeaplimit :CARDINAL);
BEGIN
  IF newHeaplimit > xrBlockManager.heapLimit THEN
    RTS.X2C_maxmem := xrBlockManager.heapLimit;
  ELSE
    RTS.X2C_maxmem := newHeaplimit;
  END;
  xrMM.FloatingHeaplimit := (RTS.X2C_maxmem = 0);
END SetHeaplimit;


PROCEDURE Collect;
BEGIN
  RTS.X2C_COLLECT();
END Collect;

PROCEDURE CompactHeap;
BEGIN
  xrMM.DoDefrag := TRUE;
  RTS.X2C_COLLECT();
  xrMM.DoDefrag := FALSE;
END CompactHeap;


PROCEDURE GetInfo(VAR objects, busymem: CARDINAL);
BEGIN
  objects:=RTS.X2C_objects;
  busymem:=RTS.X2C_busymem;
END GetInfo;


PROCEDURE gcAnchorTrace (on :BOOLEAN; weightThreshold := 0 :CARDINAL);
BEGIN
  xrMM.anchorTracing := on;
  xrMM.anchorWeightThreshold := weightThreshold;
END gcAnchorTrace;


PROCEDURE gcHeapTrace (on :BOOLEAN; heapTracingThreshold := 0 :CARDINAL);
BEGIN
  xrMM.heapTracing := on;
  xrMM.heapTracingThreshold := heapTracingThreshold;
END gcHeapTrace;


PROCEDURE ReduceGCConservatism (obj: SYSTEM.ADDRESS);
BEGIN
  xrMM.stampAsNonConservative (obj);
END ReduceGCConservatism;


PROCEDURE InstallFinalizer(f: Finalizer; obj: SYSTEM.ADDRESS);
BEGIN
  RTS.X2C_DESTRUCTOR(obj,SYSTEM.CAST(RTS.X2C_DPROC,f));
END InstallFinalizer;

PROCEDURE Search(name-: ARRAY OF CHAR): Module;
  VAR m: Module; pname: Name;
BEGIN
  m:=RTS.X2C_MODULES;
  WHILE m#NIL DO
    pname:=SYSTEM.CAST(SYSTEM.ADDRESS,m^.name);
    IF name = pname^ THEN RETURN m END;
    m:=m^.next;
  END;
  RETURN NIL
END Search;

PROCEDURE NameOfModule(m: Module; VAR name: ARRAY OF CHAR);
  VAR pname: Name;
BEGIN
  pname:=SYSTEM.CAST(SYSTEM.ADDRESS,m^.name);
  COPY(pname^,name);
END NameOfModule;

PROCEDURE ThisCommand(m: Module; name-: ARRAY OF CHAR): Command;
  VAR cmds: CMDs; cnms: CNMs; i: CARDINAL; pname: Name;
BEGIN
  cmds:=SYSTEM.CAST(SYSTEM.ADDRESS,m^.cmds);
  cnms:=SYSTEM.CAST(SYSTEM.ADDRESS,m^.cnms);
  i:=0;
  LOOP
    pname:=SYSTEM.CAST(SYSTEM.ADDRESS,cnms^[i]);
    IF pname = NIL THEN RETURN NIL END;
    IF pname^ = name THEN RETURN cmds^[i] END;
    INC(i);
  END;
END ThisCommand;

PROCEDURE ThisType(m: Module; name-: ARRAY OF CHAR): Type;
  VAR pname: Name; type: Type;
BEGIN
  type:=m^.types;
  WHILE type # NIL DO
    pname:=SYSTEM.CAST(SYSTEM.ADDRESS,type^.name);
    IF pname^ = name THEN RETURN type END;
    type:=type^.next;
  END;
  RETURN NIL
END ThisType;

PROCEDURE SizeOf(t: Type): INTEGER;
BEGIN
  RETURN t^.size
END SizeOf;

PROCEDURE BaseOf(t: Type; level: INTEGER): Type;
BEGIN
  RETURN t^.base[level]
END BaseOf;

PROCEDURE LevelOf(t: Type): INTEGER;
BEGIN
  (* in type descriptor, "level" has the INT16 type *)
  RETURN VAL (INTEGER, t^.level);
END LevelOf;

PROCEDURE ModuleOf(t: Type): Module;
BEGIN
  RETURN t^.module
END ModuleOf;

PROCEDURE NameOfType(t: Type; VAR name: ARRAY OF CHAR);
  VAR pname: Name;
BEGIN
  pname:=SYSTEM.CAST(SYSTEM.ADDRESS,t^.name);
  COPY(pname^,name);
END NameOfType;

PROCEDURE TypeOf(obj: SYSTEM.ADDRESS): Type;
BEGIN
  IF obj = NIL THEN RETURN NIL END;
  RETURN X2C.X2C_GET_TD(obj);
END TypeOf;

PROCEDURE NewObj(type: Type): SYSTEM.ADDRESS;
  VAR a: SYSTEM.ADDRESS;
BEGIN
  RTS.X2C_NEW(type,a,type^.size,FALSE);
  RETURN a;
END NewObj;

(* -------------- Iterators ------------------- *)

PROCEDURE IterModules(session: SYSTEM.ADDRESS; iter: NameIterator);
  VAR m: Module; pname: Name;
BEGIN
  m:=RTS.X2C_MODULES;
  WHILE m#NIL DO
    pname:=SYSTEM.CAST(SYSTEM.ADDRESS,m^.name);
    IF NOT iter(session,pname^) THEN RETURN END;
    m:=m^.next;
  END;
END IterModules;

PROCEDURE IterCommands(mod: Module; session: SYSTEM.ADDRESS; iter: NameIterator);
  VAR cnms: CNMs; i: CARDINAL; pname: Name;
BEGIN
  cnms:=SYSTEM.CAST(SYSTEM.ADDRESS,mod^.cnms);
  i:=0;
  LOOP
    pname:=SYSTEM.CAST(SYSTEM.ADDRESS,cnms^[i]);
    IF (pname = NIL) OR NOT iter(session,pname^) THEN RETURN END;
    INC(i);
  END;
END IterCommands;

PROCEDURE IterTypes(mod: Module; session: SYSTEM.ADDRESS; iter: NameIterator);
  VAR pname: Name; type: Type;
BEGIN
  type:=mod^.types;
  WHILE type # NIL DO
    pname:=SYSTEM.CAST(SYSTEM.ADDRESS,type^.name);
    IF NOT iter(session,pname^) THEN RETURN END;
    type:=type^.next;
  END;
END IterTypes;

PROCEDURE SetStackSize(newSize: CARDINAL): BOOLEAN;
<* IF env_target = 'x86linux' THEN *>
VAR lim: rlimit.rlimit;
BEGIN
  IF rlimit.getrlimit(rlimit.RLIMIT_STACK, lim) # 0 THEN
    RETURN FALSE
  END;
  IF newSize = lim.rlim_cur THEN
    RETURN TRUE
  END;
  IF newSize > lim.rlim_max THEN
    RETURN FALSE
  END;
  lim.rlim_cur := newSize;
  RETURN rlimit.setrlimit(rlimit.RLIMIT_STACK, lim) = 0;
<* ELSE *>
BEGIN
  RETURN TRUE;
<* END *>  
END SetStackSize;

BEGIN
  nullModule:=NIL;
  nullType:=NIL;
END oberonRTS.
