(* Copyright (c) 1997 XDS Ltd, Russia. All Rights Reserved. *)
(* o2/m2 development system: user interface *)
MODULE xmPlatform; (* Hady, 3 Oct 1997 *)

IMPORT env:=xiEnv, COMPILER;

(*
  This module is intended to define
  platform-describing options and equations

  Currently it looks that following equations should be enough:
        TARGET_OS  = UNIX|WINNT|OS2
        TARGET_FS  = FATFS|UNC|UNIX|MACOS|VMS
        TARGET_CPU = X86|PPC|VAX|CC|68K
        TARGET_BIGENDIAN = TRUE|FALSE
        HOST_OS
        HOST_CPU
        HOST_FS
        HOST_BIGENDIAN
*)

CONST STRLEN = 32;

CONST
  ill_value_error = 176;

CONST
  HOST_OS*        = "HOST_OS";
  HOST_CPU*       = "HOST_CPU";
  HOST_FS*        = "HOST_FS";
  HOST_BIGENDIAN* = "HOST_BIGENDIAN";
  TARGET_OS*        = "TARGET_OS";
  TARGET_CPU*       = "TARGET_CPU";
  TARGET_FS*        = "TARGET_FS";
  TARGET_BIGENDIAN* = "TARGET_BIGENDIAN";
  CC = "CC";
  CROSS_OS = "CROSS_OS";
  CROSS_CPU = "CROSS_CPU";

  HOST_FAMILY = "HOST_FAMILY";
  TARGET_FAMILY = "TARGET_FAMILY";

  FSVALUE = "FATFS;UNC;UNIX;MACOS;VMS";
  (* attention! adding a new value to FSVALUE do not forget add new
     naming convention support to the "PFNConv" module of the compiler!*)


VAR
  hostOS-,hostCPU-,hostFS-,hostFamily-,hostExeExt-: ARRAY STRLEN OF CHAR;
  hostBigendian-: BOOLEAN;
  targetOS-,targetCPU-,targetFS-,targetFamily-: ARRAY STRLEN OF CHAR;
  targetBigendian-: BOOLEAN;

PROCEDURE check(nm-,val-,set-: ARRAY OF CHAR);
BEGIN
  IF env.IsValueIn(val,set)<0 THEN
    env.errors.Fault(env.null_pos,ill_value_error,val,nm);
  END;
END check;

PROCEDURE checkHFS(VAR str: env.String);
BEGIN
  IF str#NIL THEN
    env.Capitalize(str); check(HOST_FS,str^,FSVALUE); COPY(str^,hostFS);
  END;
END checkHFS;

PROCEDURE checkTFS(VAR str: env.String);
BEGIN
  IF str#NIL THEN
    env.Capitalize(str); check(TARGET_FS,str^,FSVALUE); COPY(str^,targetFS);
  END;
END checkTFS;

(* following procedures perform synchronization for key equation values with
   the corresponding global variables. All procedures are uniform. The name is
   self-explanatory abbreviation. For instance:
      HOS -- stans for "H OS"
   Hint: somewhere in the future when xiEnv supports the notifier list all
   these procedures may be replaced by single type of notifier.
                                      Hady, Mar 25 1998
*)

PROCEDURE HOS(VAR str: env.String);
BEGIN
  IF str#NIL THEN env.Capitalize(str); COPY(str^,hostOS) END;
END HOS;

PROCEDURE TOS(VAR str: env.String);
BEGIN
  IF str#NIL THEN env.Capitalize(str); COPY(str^,targetOS) END;
END TOS;

PROCEDURE HCPU(VAR str: env.String);
BEGIN
  IF str#NIL THEN env.Capitalize(str); COPY(str^,hostCPU) END;
END HCPU;

PROCEDURE TCPU(VAR str: env.String);
BEGIN
  IF str#NIL THEN env.Capitalize(str); COPY(str^,targetCPU) END;
END TCPU;

PROCEDURE HF(VAR str: env.String);
BEGIN
  IF str#NIL THEN env.Capitalize(str); COPY(str^,hostFamily) END;
END HF;

PROCEDURE TF(VAR str: env.String);
BEGIN
  IF str#NIL THEN env.Capitalize(str); COPY(str^,targetFamily) END;
END TF;

PROCEDURE HBE(val: BOOLEAN); BEGIN hostBigendian:=val END HBE;

PROCEDURE TBE(val: BOOLEAN); BEGIN targetBigendian:=val END TBE;

(* ---- calculation of derrivative settings ---- *)

PROCEDURE getEndian(os-,cpu-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF cpu="X86" THEN RETURN FALSE
  ELSIF cpu="PPC" THEN
    IF os="CHORUS" THEN RETURN FALSE
    ELSE RETURN TRUE
    END;
  END;
  RETURN FALSE;
END getEndian;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE getFS(os-,cpu-: ARRAY OF CHAR; VAR fs: ARRAY OF CHAR);
BEGIN
(* TARGET_FS  = FATFS|UNC|UNIX|MACOS|VMS *)
  IF os="MACOS" THEN COPY("MACOS",fs);
  ELSIF os="VMS" THEN COPY("VMS",fs);
  ELSIF os="DOS" THEN COPY("FATFS",fs);
  ELSIF (os="OS2") OR (os="WINNT") OR (os="WIN95") THEN
    COPY("UNC",fs);
  ELSE
    COPY("UNIX",fs);
  END;
END getFS;
<* POP *>

PROCEDURE getFamily(os-: ARRAY OF CHAR; VAR fn: ARRAY OF CHAR);
BEGIN
  IF (os="WINNT") OR (os="WIN95") THEN
    COPY ("WIN32",fn);
  ELSIF os="OS2" THEN COPY("OS2",fn);
  ELSIF os="DOS" THEN COPY("DOS",fn);
  ELSIF os="VMS" THEN COPY("VMS",fn);
  ELSIF os="MACOS" THEN COPY("MACOS",fn);
  ELSE
    COPY("UNIX",fn);
  END;
END getFamily;

PROCEDURE Derivatives;
  VAR hos,hcpu,tos,tcpu: env.String;
  <* IF TARGET_C THEN *>
      cc: env.String;
  <* END *>
      s: ARRAY 32 OF CHAR;
BEGIN
  env.config.Equation(HOST_OS,hos);
  env.config.Equation(HOST_CPU,hcpu);
  env.config.Equation(TARGET_OS,tos);
  env.config.Equation(TARGET_CPU,tcpu);

  (* ------------------------- *)
  env.config.SetOption(HOST_BIGENDIAN,getEndian(hos^,hcpu^));
  env.config.SetOption(TARGET_BIGENDIAN,getEndian(tos^,tcpu^));

  (* ------------------------- *)
  getFS(hos^,hcpu^,s); env.config.SetEquation(HOST_FS,s);
  getFS(tos^,tcpu^,s); env.config.SetEquation(TARGET_FS,s);

  (* ------------------------- *)
  getFamily(hos^,s); env.config.SetEquation(HOST_FAMILY,s);
  IF (s="WIN32") OR (s="OS2") OR (s="DOS") THEN
    COPY("exe",hostExeExt);
  ELSIF (s="VMS") OR (s="MACOS") OR (s="UNIX") THEN
    COPY("",hostExeExt);
  END;
  getFamily(tos^,s); env.config.SetEquation(TARGET_FAMILY,s);

  (* ------------------------- *)
  <* IF TARGET_C THEN *>
  env.config.Equation(CC,cc);
  IF cc=NIL THEN
    env.config.NewActiveEquation("CC",env.Capitalize);
    IF (s="DOS") OR (s="OS2") THEN
      env.config.SetEquation(CC,"WATCOM");
    ELSIF s="WIN32" THEN
      env.config.SetEquation(CC,"MSVC");
    ELSE
      env.config.SetEquation(CC,"GNU");
    END;
  END;
  <* END *>
END Derivatives;

PROCEDURE Declare*;
BEGIN
  env.config.NewActiveEquation(HOST_OS,HOS);
  env.config.NewActiveEquation(HOST_CPU,HCPU);
  env.config.NewActiveEquation(TARGET_OS,TOS);
  env.config.NewActiveEquation(TARGET_CPU,TCPU);

  <* IF DEFINED(CROSS_OS) THEN *>
    env.config.SetEquation(HOST_OS,COMPILER.EQUATION(CROSS_OS));
  <* ELSE *>
    env.config.SetEquation(HOST_OS,COMPILER.EQUATION(TARGET_OS));
  <* END *>
  env.config.SetEquation(TARGET_OS,COMPILER.EQUATION(TARGET_OS));

  <* IF DEFINED(CROSS_CPU) THEN *>
    env.config.SetEquation(HOST_CPU,COMPILER.EQUATION(CROSS_CPU));
  <* ELSE *>
    env.config.SetEquation(HOST_CPU,COMPILER.EQUATION(TARGET_CPU));
  <* END *>
  env.config.SetEquation(TARGET_CPU,COMPILER.EQUATION(TARGET_CPU));

  env.config.NewActiveEquation(TARGET_FS,checkTFS);
  env.config.NewActiveEquation(HOST_FS,checkHFS);
  env.config.NewActiveEquation(HOST_FAMILY,HF);
  env.config.NewActiveEquation(TARGET_FAMILY,TF);
  env.config.NewActiveOption(HOST_BIGENDIAN,FALSE,HBE);
  env.config.NewActiveOption(TARGET_BIGENDIAN,FALSE,TBE);

  Derivatives;

END Declare;

END xmPlatform.