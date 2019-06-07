(* Copyright (C) 2000 Excelsior. *)

IMPLEMENTATION MODULE SysErr;

IMPORT SYSTEM;
IMPORT OS2;

BEGIN
  allRight     := OS2.NO_ERROR;
  noMoreFiles  := OS2.ERROR_NO_MORE_FILES;
  writeProtect := OS2.ERROR_WRITE_PROTECT;
  pathNotFound := OS2.ERROR_PATH_NOT_FOUND;
  invalidDrive := OS2.ERROR_INVALID_DRIVE;

  tooManyOpen  := OS2.ERROR_TOO_MANY_OPEN_FILES;
  fileNotFound := OS2.ERROR_FILE_NOT_FOUND;
  fileExists   := OS2.ERROR_FILE_EXISTS;

  cannotMake   := OS2.ERROR_CANNOT_MAKE;
  accessDenied := OS2.ERROR_ACCESS_DENIED;

  otherProblem := MAX(SYSTEM.CARD32);

END SysErr.
