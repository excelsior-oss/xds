(* Copyright (C) 2000 Excelsior. *)

IMPLEMENTATION MODULE SysErr;

IMPORT SYSTEM;
IMPORT Windows;

BEGIN
  allRight      := Windows.NO_ERROR;
  noMoreFiles   := Windows.ERROR_NO_MORE_FILES;
  writeProtect  := Windows.ERROR_WRITE_PROTECT;
  (* !!! Windows.SetCurrentDirectory return ERROR_FILE_NOT_FOUND insted
  pathNotFound  := Windows.ERROR_PATH_NOT_FOUND; *)
  pathNotFound  := Windows.ERROR_FILE_NOT_FOUND;
  invalidDrive  := Windows.ERROR_INVALID_DRIVE;

  tooManyOpen   := Windows.ERROR_TOO_MANY_OPEN_FILES;
  fileNotFound  := Windows.ERROR_FILE_NOT_FOUND;
  fileExists    := Windows.ERROR_FILE_EXISTS;

  cannotMake    := Windows.ERROR_CANNOT_MAKE;
  alreadyExists := Windows.ERROR_ALREADY_EXISTS;
  dirNotEmpty   := Windows.ERROR_DIR_NOT_EMPTY;

  otherProblem  := MAX(SYSTEM.CARD32);

END SysErr.
