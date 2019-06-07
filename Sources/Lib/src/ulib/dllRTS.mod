(* Copyright (C)1997 xTech Ltd *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

IMPLEMENTATION MODULE dllRTS; (* VitVit, Jek *)

IMPORT X2C;

FROM SYSTEM IMPORT ADDRESS, ADR, ADDADR, CAST, GET;

<* IF env_target = "x86os2" THEN *>

IMPORT O := OS2;

PROCEDURE LoadModule ( mname- :ARRAY OF CHAR ) :HMOD;
VAR
  hmod :O.HMODULE;
  errs :ARRAY [0..0] OF CHAR;
BEGIN
  IF ( O.DosLoadModule ( errs, 1, mname, hmod ) = O.NO_ERROR ) THEN
    RETURN CAST(HMOD, hmod)
  ELSE
    RETURN NIL;
  END;
END LoadModule;


PROCEDURE GetProcAdr ( hmod :HMOD; procname- :ARRAY OF CHAR ) :PROC;
VAR
  pfn :O.PFN;
BEGIN
  IF ( O.DosQueryProcAddr ( O.HMODULE(hmod), 0, ADR ( procname ), pfn ) = O.NO_ERROR ) THEN
    RETURN CAST( PROC, pfn );
  ELSE
    RETURN NIL
  END;
END GetProcAdr;

PROCEDURE GetVarAdr ( hmod :HMOD; varname- :ARRAY OF CHAR ) :ADDRESS;
VAR
  pfn :O.PFN;
BEGIN
  IF ( O.DosQueryProcAddr ( O.HMODULE(hmod), 0, ADR ( varname ), pfn ) = O.NO_ERROR ) THEN
    RETURN CAST( ADDRESS, pfn );
  ELSE
    RETURN NIL
  END;
END GetVarAdr;

PROCEDURE FreeModule ( hmod :HMOD; VAR rc :BOOLEAN );
BEGIN
  rc := ( O.DosFreeModule ( O.HMODULE(hmod) ) = O.NO_ERROR );
END FreeModule;


PROCEDURE GetModuleHandle ( mname- :ARRAY OF CHAR ) :HMOD;
VAR
  hmod :O.HMODULE;
BEGIN
  IF ( O.DosQueryModuleHandle ( mname, hmod ) = O.NO_ERROR ) THEN
    RETURN CAST(HMOD, hmod)
  ELSE
    RETURN NIL;
  END;
END GetModuleHandle;


PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

PROCEDURE GetMyHandle() :HMOD;
BEGIN
  RETURN CAST(HMOD, X2C_GetMyHandle() );
END GetMyHandle;


PROCEDURE GetModuleName ( hmod :HMOD; VAR mname :ARRAY OF CHAR; VAR rc :BOOLEAN );
BEGIN
  rc := ( O.DosQueryModuleName ( O.HMODULE(hmod), HIGH(mname)+1, mname ) = O.NO_ERROR );
END GetModuleName;



<* ELSIF env_target = 'x86nt' THEN *> -----------------------------------------

IMPORT W := Windows;

PROCEDURE LoadModule ( mname- :ARRAY OF CHAR ) :HMOD;
BEGIN
  RETURN CAST(HMOD, W.LoadLibrary ( mname ));
END LoadModule;


PROCEDURE GetProcAdr ( hmod :HMOD; procname- :ARRAY OF CHAR ) :PROC;
BEGIN
  RETURN CAST ( PROC, W.GetProcAddress ( W.HINSTANCE(hmod), procname ) );   
END GetProcAdr;


PROCEDURE GetVarAdr ( hmod :HMOD; varname- :ARRAY OF CHAR ) :ADDRESS;
  VAR
    adr :ADDRESS;
BEGIN
  adr := CAST(ADDRESS, W.GetProcAddress( W.HINSTANCE(hmod), varname ));   
  RETURN adr;
END GetVarAdr;



PROCEDURE FreeModule ( hmod :HMOD; VAR rc :BOOLEAN );
BEGIN
  rc := W.FreeLibrary ( W.HINSTANCE(hmod) );
END FreeModule;


PROCEDURE GetModuleHandle ( mname- :ARRAY OF CHAR ) :HMOD;
BEGIN
  RETURN CAST(HMOD, W.GetModuleHandle ( mname ));
END GetModuleHandle;


PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

PROCEDURE GetMyHandle() :HMOD;
BEGIN
  RETURN CAST(HMOD, X2C_GetMyHandle() );
END GetMyHandle;


PROCEDURE GetModuleName ( hmod :HMOD; VAR mname :ARRAY OF CHAR; VAR rc :BOOLEAN );
BEGIN
  rc := ( W.GetModuleFileName ( W.HINSTANCE(hmod), mname, HIGH(mname)+1 ) # 0 );
END GetModuleName;

<* ELSIF env_target = 'x86linux' THEN *> -----------------------------------------

IMPORT dl:=dlfcn, elf, xrnProc;

TYPE PCHAR = POINTER TO CHAR;
     pARRAY_OF_CHAR = POINTER TO ARRAY OF CHAR;


PROCEDURE LoadModule ( mname- :ARRAY OF CHAR ) :HMOD;
BEGIN
  RETURN CAST(HMOD, dl.dlopen(ADR(mname[0]), dl.RTLD_LAZY));
END LoadModule;


PROCEDURE GetProcAdr ( hmod :HMOD; procname- :ARRAY OF CHAR ) :PROC;
VAR
  proc : PROC;
BEGIN
  proc := CAST ( PROC, dl.dlsym ( CAST(dl.HANDLE, hmod), procname ) );
  IF dl.dlerror() # NIL THEN RETURN NIL
  ELSE RETURN proc END;
END GetProcAdr;


PROCEDURE GetVarAdr ( hmod :HMOD; varname- :ARRAY OF CHAR ) :ADDRESS;
VAR
  sym : ADDRESS;
BEGIN
  sym := dl.dlsym (CAST(dl.HANDLE, hmod), varname);
  IF dl.dlerror() # NIL THEN RETURN NIL
  ELSE RETURN sym END;
END GetVarAdr;


PROCEDURE FreeModule ( hmod :HMOD; VAR rc :BOOLEAN );
BEGIN
  rc := (dl.dlclose(CAST(dl.HANDLE, hmod)) = 0);
END FreeModule;


PROCEDURE GetModuleHandle ( mname- :ARRAY OF CHAR ) :HMOD;
BEGIN
  RETURN CAST(HMOD, dl.dlopen ( ADR(mname[0]), dl.RTLD_NOW OR dl.RTLD_GLOBAL ));
END GetModuleHandle;


PROCEDURE ["C"] / X2C_GetMyHandle(): ADDRESS;

PROCEDURE GetMyHandle() :HMOD;
BEGIN
  RETURN CAST(HMOD, X2C_GetMyHandle() );
END GetMyHandle;


PROCEDURE GetModuleName ( hmod :HMOD; VAR mname :ARRAY OF CHAR; VAR rc :BOOLEAN );

  PROCEDURE l(s :PCHAR): CARDINAL;
  VAR i :CARDINAL;
  BEGIN
    i := 0;
    WHILE s^ # 0C DO
      s := ADDADR(s, 1);
      INC(i)
    END;
    RETURN i
  END l;
  
  PROCEDURE st(s :PCHAR);
  VAR i :CARDINAL;
  BEGIN
    i := 0;
    WHILE s^ # 0C DO
      mname[i] := s^;
      s := ADDADR(s, 1);
      INC(i)
    END;
    mname[i] := 0C
  END st;
      
VAR
  info  :dl.Dl_info;
  namep :PCHAR;
BEGIN
  IF (hmod = NIL) OR (hmod = dl.dlopen(NIL, dl.RTLD_LAZY)) THEN
    xrnProc.GetExeName(mname, HIGH(mname)+1);
  ELSE
    namep := CAST(elf.link_map, hmod)^.l_name;
    ASSERT(namep # NIL, 911122);
    IF l(namep) >= HIGH(mname) THEN
      rc := FALSE;
      RETURN;
    END;
    st(namep);
  END;
  rc := TRUE;
END GetModuleName;

<* END *>

END dllRTS.
                        
