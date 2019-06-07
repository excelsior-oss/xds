(* Copyright (c) Excelsior, 1995-2005.  All Rights Reserved *)

<*+ M2EXTENSIONS*>
<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<* IF ~ DEFINED(TESTCOVERAGE) THEN *>  <* NEW TESTCOVERAGE- *>
<* ELSE *>                             <*- TESTCOVERAGE *>
<* END *>

IMPLEMENTATION MODULE xrnX2C;

IMPORT
  SYSTEM,
  xmRTS, X2C;

PROCEDURE init_increase ( gc_auto      :BOOLEAN;
                          gc_threshold :CARDINAL;
                          heap_limit   :CARDINAL;
                          stackbotm    :SYSTEM.ADDRESS) :BOOLEAN;
BEGIN
  IF NOT X2C.X2C_wasNLibInit() THEN
    X2C.X2C_makeNLibInit();
    xmRTS.X2C_fs_init := FALSE;
    xmRTS.X2C_ini_coroutines ( stackbotm );
    IF NOT xmRTS.X2C_GC_INIT(gc_auto,gc_threshold,heap_limit) THEN
      RETURN FALSE;
    END;
    xmRTS.X2C_init_exceptions();
  ELSE
    xmRTS.X2C_GC_INCREASE (gc_auto,gc_threshold,heap_limit);
    IF ~(X2C.X2C_wasXEntry()) THEN
      xmRTS.X2C_reg_stackbotm ( stackbotm );
      (*
         this trick was done to handle the following OS/2 feature:
         an initialization of a run-time DLL is executed on the system
         stack not on the calling process stack
       *)

    END;
  END;
  RETURN TRUE;
END init_increase;

PROCEDURE ["C"] X2C_BEGINDLL ( gc_auto      :BOOLEAN;
                               gc_threshold :CARDINAL;
                               heap_limit   :CARDINAL) :BOOLEAN;
BEGIN
  RETURN init_increase ( gc_auto, gc_threshold,
                         heap_limit, SYSTEM.ADDADR(SYSTEM.ADR(heap_limit),4) );
END X2C_BEGINDLL;


PROCEDURE ["C"] / X2C_assign_acav (ac :INTEGER; av :SYSTEM.ADDRESS );

PROCEDURE ["C"] X2C_BEGIN ( VAR argc :INTEGER; argv :SYSTEM.ADDRESS;
                            gc_auto      :BOOLEAN;
                            gc_threshold :CARDINAL;
                            heap_limit   :CARDINAL );
BEGIN
  X2C_assign_acav(argc,argv);

  xmRTS.X2C_InitFPP();

  IF init_increase ( gc_auto, gc_threshold,
        heap_limit, SYSTEM.ADDADR(SYSTEM.ADR(heap_limit),4) ) THEN END;

  X2C.X2C_makeXEntry();

  xmRTS.X2C_ini_termination();

  ASSERT(X2C.X2C_argc=argc);
  ASSERT(SYSTEM.CAST(SYSTEM.ADDRESS,X2C.X2C_argv)=argv);
END X2C_BEGIN;

END xrnX2C.
