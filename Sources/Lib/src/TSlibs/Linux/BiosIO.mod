(* Copyright (c) 1999,2000 Excelsior . All rights reserved. *)

<*+ M2EXTENSIONS*>
<*+ M2ADDTYPES*>

IMPLEMENTATION MODULE BiosIO; 

IMPORT
    SYSTEM,
    io:=Printf,
    StdChans,
    stdio,
    term:=h2d_termios,
    ncurses:=h2d_curses,
    poll:=h2d_poll;

FROM SYSTEM IMPORT INT16, INT32, CARD32, ADDRESS;


VAR
    fd: ARRAY [0..0] OF poll.pollfd;
    termios_save, buf: term.termios;
    stdoutn, stdinn: INT32;

PROCEDURE ["C"] / read(fd: INT32; buff: ADDRESS; count: CARD32): CARD32;


(* Check if key pressed *)
PROCEDURE KeyPressed(): BOOLEAN;
BEGIN
  IF poll.poll(SYSTEM.ADR(fd), 1, 0)>0 THEN
    RETURN TRUE;
  END;
  RETURN FALSE;
END KeyPressed;


PROCEDURE curses_decode(VAR char:CHAR; VAR scan:SHORTCARD; code:CARDINAL)
;
BEGIN
  char := 0C; scan := 0;
  CASE code OF
  | 10      : char := CHR(13); (* Enter   *)
  | 0..9,
    11..255 : char := CHR(code);
  | 258     : scan := 80;      (* Down    *)
  | 259     : scan := 72;      (* Up      *)
  | 260     : scan := 75;      (* Left    *)
  | 261     : scan := 77;      (* Right   *)
  | 262     : scan := 71;      (* Home    *)
  | 263     : char := CHR(8);  (* BkSp    *)
  | 265     : scan := 59;      (* F1      *)
  | 266     : scan := 60;      (* F2      *)
  | 267     : scan := 61;      (* F3      *)
  | 268     : scan := 62;      (* F4      *)
  | 269     : scan := 63;      (* F5      *)
  | 270     : scan := 64;      (* F6      *)
  | 271     : scan := 65;      (* F7      *)
  | 272     : scan := 66;      (* F8      *)
  | 273     : scan := 67;      (* F9      *)
  | 274     : scan := 68;      (* F10     *)
  | 277     : scan := 84;      (* Shft F1 *)
  | 278     : scan := 85;      (* Shft F2 *)
  | 279     : scan := 86;      (* Shft F3 *)
  | 280     : scan := 87;      (* Shft F4 *)
  | 281     : scan := 88;      (* Shft F5 *)
  | 282     : scan := 89;      (* Shft F6 *)
  | 283     : scan := 90;      (* Shft F7 *)
  | 284     : scan := 91;      (* Shft F8 *)
  | 330     : scan := 83;      (* Delete  *)
  | 331,332 : scan := 82;      (* Insert  *)
  | 338     : scan := 81;      (* PgDn    *)
  | 339     : scan := 73;      (* PgUp    *)
  | 360     : scan := 79;      (* End     *)
  ELSE
  END;
END curses_decode;

(* Return keys without echo *)
PROCEDURE RdKey(): CHAR;
BEGIN
  IF LastChar = 0C THEN
    LastChar := CHAR(0FFH);
    RETURN CHR(LastScan);
  END;
  curses_decode(LastChar,LastScan,ncurses.getch());
  RETURN LastChar;
END RdKey;

(* Return keys with echo *)
PROCEDURE RdChar(): CHAR;
VAR
    ch: CHAR;
BEGIN
    ch := RdKey();
    io.fprintf(StdChans.StdOutChan(), '%c', ch);
    RETURN ch;
END RdChar;

(* Returns current 'shift' keys depressed *)
PROCEDURE KBFlags(): KBFlagSet;
BEGIN
    RETURN KBFlagSet({});
END KBFlags;

BEGIN
    stdoutn := stdio.fileno(stdio.stdout);
    stdinn := stdio.fileno(stdio.stdin);
    
    term.tcgetattr(stdoutn, SYSTEM.ADR(termios_save));
    buf := termios_save;
    buf.c_iflag := 0;
    buf.c_oflag := term.OPOST OR term.ONLCR;
    buf.c_cflag := term.CREAD;
    buf.c_lflag := term.TOSTOP OR term.CS8;
    term.tcsetattr(stdoutn, term.TCSANOW, SYSTEM.ADR(buf));

    fd[0].fd := stdinn;
    fd[0].events := poll.POLLIN;
    ncurses.initscr();
    ncurses.keypad(ncurses.stdscr,1);
    ncurses.nodelay(ncurses.stdscr,0);
    ncurses.cbreak();
    ncurses.noecho();
    LastChar := CHAR(0FFH);
FINALLY
    ncurses.echo();
    ncurses.nl();
    ncurses.cbreak();
    term.tcsetattr(stdoutn, term.TCSANOW, SYSTEM.ADR(termios_save));
END BiosIO.

