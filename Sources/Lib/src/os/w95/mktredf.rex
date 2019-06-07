/**/


call Process "xds230.edf"  "xds230t.edf"
call Process "xds230m.edf" "xds230tm.edf"
exit

Process:

  arg in out

  call OpenRead in
  'if exist' out 'del' out
  call OpenWrite out

  do while lines(in) > 0
    line = linein(in)
    parse var line name '@'ordinal
    if ordinal <> '' then do
      name = strip(name)
      parse var name module'_'object
      if (object <> "") & (wordpos(module,"X2C x2c rtl SCODE MEVT MCI MAKE IS IN HRESULT FD CommDlg") = 0)  then
        line = "   "module"__"object" @"ordinal
    end
    call lineout out, line
  end

  call Close in
  call Close out


  return

OpenRead: procedure
  arg file
  rc = stream(file,'c','open read')
  if rc <> 'READY:' then do
    say 'Error opening 'file': 'rc
    exit 1
  end
  return

OpenWrite: procedure
  arg file
  rc = stream(file,'c','open write')
  if rc <> 'READY:' then do
    say 'Error opening 'file': 'rc
    exit 1
  end
  return

Close: procedure
  arg file
  rc = stream(file,'c','close')
  if rc <> 'READY:' then do
    say 'Error closing 'file': 'rc
    exit 1
  end
  return
