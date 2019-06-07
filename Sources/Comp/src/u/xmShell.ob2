<*-IOVERFLOW*>
<*-COVERFLOW*>
<*-CHECKRANGE*>
(** Compiler to shell interface *)
MODULE xmShell;

<* IF XDSIDE THEN *>

IMPORT
  xShell,
  SYSTEM,
  env := xiEnv;

TYPE
  Shell = POINTER TO ShellDesc;
  ShellDesc = RECORD (env.ShellDesc)
  END;

PROCEDURE (sh: Shell) Active(): BOOLEAN;
BEGIN
  RETURN TRUE  (* shell_active *)
END Active;

PROCEDURE (sh: Shell) String (s-: ARRAY OF CHAR);
BEGIN
  xShell.String (s);
END String;

PROCEDURE (sh: Shell) Caption (s-: ARRAY OF CHAR);
BEGIN
  xShell.Caption (s);
END Caption;

PROCEDURE (sh: Shell) Error (err_class : CHAR;
                             err_no    : INTEGER;
                             x, y      : LONGINT;
                             fname-, s-: ARRAY OF CHAR);
BEGIN
  xShell.Error (err_class, err_no, x+1, y+1, fname, s);
END Error;

PROCEDURE (sh: Shell) Comment (s-: ARRAY OF CHAR);
BEGIN
  xShell.Comment (s);
END Comment;

PROCEDURE (sh: Shell) StartJob (s-: ARRAY OF CHAR; progress_limit: LONGINT);
BEGIN
  xShell.StartJob (s, progress_limit);
END StartJob;

PROCEDURE (sh: Shell) Progress (comment_progress, progress: LONGINT);
BEGIN
  xShell.Progress (comment_progress, progress);
END Progress;

PROCEDURE (sh: Shell) StartFileList;
BEGIN
  xShell.StartFileList;
END StartFileList;

PROCEDURE (sh: Shell) AppendFile (fullname-: ARRAY OF CHAR);
BEGIN
  xShell.AppendFile (fullname);
END AppendFile;

PROCEDURE (sh: Shell) EndFileList;
BEGIN
  xShell.EndFileList;
END EndFileList;

PROCEDURE Set*;
(** Sets "Shell" manager if xShell.Start() returns TRUE.
  The procedure should be called when "Args" manager is set.
*)
  VAR x: Shell;
BEGIN
  IF xShell.Start() THEN
    NEW (x);
    env.shell := x;
    xShell.TurnSortingOn;
  END;
END Set;

<* ELSE *>

PROCEDURE Set*;
BEGIN
END Set;

<* END *>

END xmShell.
