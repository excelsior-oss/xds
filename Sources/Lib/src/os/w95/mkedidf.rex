/**/

call Process "xds25"  
call Process "xds25m" 
call Process "xts25"
call Process "xts25m"
exit

Process:

  arg dll

  in  = dll".edf"
  out = dll"e.idf"

  call OpenRead in
  'if exist' out 'del' out
  call OpenWrite out

  first = 1  
  call lineout out, "FROM" dll".DLL IMPORT"
  do while lines(in) > 0
    line = linein(in)
    parse var line name ' @'ordinal
    if ordinal <> '' then do
      name = strip(name)
      parse var name module'_'object
      if (object <> "") & (wordpos(module,"X2C x2c rtl SCODE MEVT MCI MAKE IS IN HRESULT FD CommDlg") = 0)  then
        line = name "AS" module"$$"object 
      else
        line = name
      if first then do
        first = 0
        line = "  "line
      end
      else
        line = " ,"line
      call lineout out, line
    end
  end
  call lineout out, " ;"
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
