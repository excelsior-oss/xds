<*+ MAIN *>
MODULE vs;

IMPORT SYSTEM, pcVis, pcO, ccCode, pcNum, pcK, pcConst,
	fio:=RndFile,
	str:=FormStr,
	args:=ProgramArgs,
	TextIO,
	IOConsts,
	IOChan,
	xmISOFM, xmFS,
	xfs:=xiFiles, xmConfig, env:=xiEnv,
	xcMain,
	xmErrors, xmArgs;

PROCEDURE ReadRedirection;
  VAR fn: xfs.String; f: xfs.File;
BEGIN
  xfs.sys.SysLookup('red',fn);
  xfs.text.Open(fn^,FALSE);
  f:=xfs.text.file;
  IF f=NIL THEN
    pcVis.print("%s\n",xfs.text.msg^);
    HALT(1);
  END;
  WITH f: xfs.TextFile DO
    IF xcMain.ReadRedirection(f) THEN END;
    f.Close
  END;
END ReadRedirection;

PROCEDURE ReadConfig;
  VAR fn: xfs.String; f: xfs.File;
BEGIN
  xfs.sys.SysLookup('cfg',fn);
  xfs.text.Open(fn^,FALSE);
  f:=xfs.text.file;
  IF f=NIL THEN
    pcVis.print("%s\n",xfs.text.msg^);
    HALT(1);
  END;
  WITH f: xfs.TextFile DO
    IF xcMain.ReadConfig(f) THEN ASSERT(FALSE) END;
    f.Close
  END;
END ReadConfig;

PROCEDURE cur_pos(VAR ps: pcK.TPOS);
BEGIN
  ps:=env.null_pos;
END cur_pos;

VAR
  inp : xfs.SymFile;
  fnm,nm,ext : xfs.String;

BEGIN
  xmConfig.SetManagers;
  xmFS.SetManagers;
  xmISOFM.SetManagers;
  xmErrors.SetManagers;  (* error manager, info manager *)
  xmArgs.SetManagers;    (* arguments *)
  pcNum.Set;
  pcConst.Set;
  ccCode.Set;

  ReadRedirection;
(*
  ReadConfig;
*)
  env.args.Parse;
  IF env.args.Number()#1 THEN
    pcVis.print("Invalid argument\n");
    HALT(1);
  END;
  env.args.GetArg(0,nm);
  env.config.Equation("SYM",ext);
  IF ext = NIL THEN NEW(ext,4); COPY("sym",ext^) END;
  xfs.sys.Create("",nm^,ext^,fnm);
  xfs.sys.Lookup(fnm^,fnm);
  xfs.sym.Open(fnm^,FALSE,FALSE);
  IF xfs.sym.file=NIL THEN
    pcVis.print("%s\n",xfs.sym.msg^);
    HALT(1);
  END;
  inp:=xfs.sym.file(xfs.SymFile);
  NEW(pcK.mods,100);
  pcK.sys_mods := NIL;
  pcK.code.ini();
  pcO.ini(0,cur_pos,TRUE,TRUE);
  pcVis.vis_sym_file(inp);
  inp.Close;
END vs.
