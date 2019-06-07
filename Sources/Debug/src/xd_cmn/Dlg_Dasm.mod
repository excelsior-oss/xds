IMPLEMENTATION MODULE Dlg_Dasm;

IMPORT kdsm := Krn_Dasm;
IMPORT exe := ExeMain;
IMPORT tls := DI_Tools;
IMPORT xStr;

IMPORT mem  := Exe_Mem;

IMPORT fmt := FormStr;

FROM KrnTypes IMPORT ADDRESS;

IMPORT kt := KrnTypes;


PROCEDURE SetDisasmMode (mode: DISASM_MODE);
BEGIN
  Mode := mode;
END SetDisasmMode;


CONST
  MaxInstr = 512;


TYPE
  SORT  = (_addr, _src);
  ENTRY = RECORD
            CASE sort: SORT OF
            | _addr: addr : ADDRESS;
            | _src : com, m, ln: CARDINAL;
            END;
          END;

  FRAME_INDEX = [0..MaxInstr-1];
  FRAME = ARRAY FRAME_INDEX OF ENTRY;

  FRAME_REC = RECORD
                CurrMode  : DISASM_MODE;
                SegmBegin : ADDRESS;
                SegmEnd   : ADDRESS;
                Access    : kt.ATTRIBS;
                MinPos    : FRAME_INDEX;
                MaxPos    : FRAME_INDEX;
                Frame     : FRAME;
              END;

VAR
  Frame: FRAME_REC;


PROCEDURE SetNewPos (addr: ADDRESS; depth: CARDINAL): BOOLEAN;
VAR
  len: CARDINAL;
  str, str2: ARRAY [0..80] OF CHAR;
  com_no, mod_no, line_no,
  ln: CARDINAL;

BEGIN
  ASSERT(depth <= (HIGH(Frame.Frame)+1) DIV 2);
  MakeInvalid;
  IF NOT mem.GetSegmentInfo(addr, Frame.SegmBegin, Frame.SegmEnd, Frame.Access) THEN RETURN FALSE; END;
  <* IF DEST_XDS THEN *>
  IF NOT (kt.execute IN Frame.Access) THEN RETURN FALSE END;
  <* IF TARGET_x86 THEN *>
  kdsm.Seg16 := NOT (kt.bit_32 IN Frame.Access);
  <* END *>
  <* END *>
  INC(Frame.SegmEnd, Frame.SegmBegin);
  DEC(Frame.SegmEnd);
  WITH Frame DO
    CurrMode := None;
    CASE Mode OF
    | D : MinPos := MaxInstr DIV 2;
          MaxPos := MinPos-1;
          LOOP
            IF NOT exe.DisasmInstr(addr, FALSE, str, str2, len) THEN len := 1; END;
            IF addr+len-1 > SegmEnd THEN EXIT; END;
            INC(MaxPos);
            Frame[MaxPos].sort := _addr;
            Frame[MaxPos].addr := addr;
            IF MaxPos >= MinPos + depth - 1 THEN EXIT; END;
            INC(addr, len);
          END;
          IF MinPos <= MaxPos THEN
            D_frame := MinPos;
            D_curr  := D_frame;
            CurrMode := D;
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END;

    | DS: IF NOT tls.FindModByAddr (addr, com_no, mod_no) THEN RETURN FALSE; END;
          line_no := MAX(CARDINAL);
          MinPos := MaxInstr DIV 2;
          MaxPos := MinPos-1;
          LOOP
           <* IF DEST_K26 THEN *>
            IF NOT tls.FindModInCompByAddr (com_no, addr, mod_no) THEN EXIT; END;
           <* END *>
            IF NOT tls.SourceByAddrInMod (com_no, mod_no, addr, ln) THEN EXIT; END;
            IF line_no # ln THEN
              INC(MaxPos);
              Frame[MaxPos].sort := _src;
              Frame[MaxPos].com := com_no;
              Frame[MaxPos].m := mod_no;
              Frame[MaxPos].ln := ln;
              line_no := ln;
            END;
            IF NOT exe.DisasmInstr (addr, FALSE, str, str2, len) THEN len := 1; END;
            IF addr+len-1 > SegmEnd THEN EXIT; END;
            INC(MaxPos);
            Frame[MaxPos].sort := _addr;
            Frame[MaxPos].addr := addr;
            IF MaxPos >= MinPos + depth THEN EXIT; END;
            INC(addr, len);
          END;
          IF MinPos < MaxPos THEN
            D_frame := MinPos;
            D_curr := D_frame+1;
            CurrMode := DS;
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END;
    END;
  END;
END SetNewPos;


PROCEDURE Dasm (dpos: CARDINAL; width: CARDINAL; VAR str: ARRAY OF CHAR): BOOLEAN;
VAR
  len, len2: CARDINAL;
  line: xStr.txt_ptr;
  str2, info: xStr.String;
BEGIN
  COPY("", str);
  WITH Frame DO
   <* IF NOT DEST_K26 THEN *>
    ASSERT(Mode = CurrMode);
   <* END *>
    IF (dpos >= MinPos) & (dpos <= MaxPos) THEN
      WITH Frame[dpos] DO
        CASE sort OF
        | _src  : line := tls.GetSourceLine(com, m, ln-1);
                  COPY(line^, str);
                  RETURN TRUE;
       <* PUSH *>
       <* WOFF903+ *>
        | _addr : IF NOT exe.DisasmInstr (addr, addr = exe.ExecAddr, str2, info, len) THEN END;
       <* POP *>
                 <* IF TARGET_x86 THEN *>
                  IF kdsm.Seg16 THEN
                    fmt.print(str, '%$4X', addr MOD 10000H);
                  ELSE
                    fmt.print(str, '%$8X', addr);
                  END;
                  IF str2[0] # '*' THEN
                    fmt.append(str, '  ');
                  ELSE
                    fmt.append(str, ' ');
                  END;
                  fmt.append(str, '%s', str2);
                 <* ELSE *>
                  fmt.print(str, '%$8X %s', addr, str2);
                 <* END *>
                 len2 := LENGTH(info);
                 IF len2 > 0 THEN
                   len := LENGTH(str);
                   IF len+len2+2 < width THEN
                     fmt.print(str2, '%s %1.*c%s ', str, width-len-len2-2, CHR(32), info);
                     COPY (str2, str);
                   ELSE
                     fmt.print (str2, '%s %s ', str, info);
                     COPY (str2, str);
                   END;
                 END;
                 RETURN FALSE;
        END;
      END;
    ELSE
    END;
  END;
  RETURN FALSE;
END Dasm;

PROCEDURE GetAddr(dpos: CARDINAL; VAR address: ADDRESS): BOOLEAN;
BEGIN
  WITH Frame DO
   <* IF NOT DEST_K26 THEN *>
    ASSERT(Mode = CurrMode);
   <* END *>
    IF (MinPos <= dpos) & (dpos <= MaxPos) THEN
      WITH Frame[dpos] DO
        CASE sort OF
        | _addr : address := addr;
                  RETURN TRUE;
        | _src  : RETURN FALSE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END GetAddr;

PROCEDURE GetPosByAddr(addr: ADDRESS; len: CARDINAL; VAR pos: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH Frame DO
    IF CurrMode = Mode THEN
      FOR i := MinPos TO MaxPos DO
        IF (Frame[i].sort = _addr) AND (addr = Frame[i].addr) THEN
          IF i > D_frame + len - 1 THEN
            D_frame := i - len + 1;
          ELSIF i < D_frame THEN
            D_frame := i;
          END;
          pos := i;
          RETURN TRUE;
        END;
      END;
    END;
    IF NOT SetNewPos(addr, len) THEN RETURN FALSE; END;
    CASE Mode OF
    | D  : pos := D_frame;
    | DS : pos := D_frame+1;
    END;
    RETURN TRUE;
  END;
END GetPosByAddr;



VAR
  Tmp: FRAME_REC;

CONST
  LOOK_UP = 128;


PROCEDURE ShiftFrame (delta: INTEGER; len: CARDINAL);


  PROCEDURE PrevAddr (addr: kt.ADDRESS; VAR prev_addr: kt.ADDRESS): BOOLEAN;
  VAR
    dif: CARDINAL;
  BEGIN
    IF (Frame.SegmBegin <= addr) AND (addr <= Frame.SegmEnd) THEN
      IF kt.read IN Frame.Access THEN
        prev_addr := Frame.SegmBegin;
        dif := addr-prev_addr;
        IF dif # 0 THEN
          IF dif > LOOK_UP THEN
            prev_addr := addr - LOOK_UP;
            prev_addr := 4 * (prev_addr DIV 4 + 1);
          END;
          RETURN TRUE;
        END;  
      END;
    END;  
    RETURN FALSE;
  END PrevAddr;


  PROCEDURE LookUp (addr: kt.ADDRESS; VAR prev_addr: kt.ADDRESS): BOOLEAN;
  VAR
    Com  : BOOLEAN;
    Mod  : BOOLEAN;
    prev : BOOLEAN;
    ComNo: CARDINAL;
    ModNo: CARDINAL;
  BEGIN
    Com := tls.FindComponentByAddr (addr, ComNo);
    Mod := Com AND tls.FindModInCompByAddr (ComNo, addr, ModNo);
    <* PUSH *>
    <* WOFF304+ *>
    prev := Mod AND tls.PrevBasePoint (ComNo, ModNo, addr, prev_addr);
    <* POP *>
    IF NOT prev THEN
      prev := Com AND tls.GetPrevPublicByAddrInCom (ComNo, addr, prev_addr);
      IF NOT prev AND PrevAddr (addr, prev_addr) THEN
        <* PUSH *>
        <* WOFF900+ *>
        <* WOFF903+ *>
        IF tls.FindModByAddr (prev_addr, ComNo, ModNo) AND tls.PrevBasePoint (ComNo, ModNo, prev_addr, prev_addr) THEN END;
        <* POP *>
        prev := TRUE;
      END;
    END;
    RETURN prev;
  END LookUp;


VAR
  i      : CARDINAL;
  addr   : ADDRESS;
  str, str2: ARRAY [0..80] OF CHAR;
  ln     : CARDINAL;
  new_pos: CARDINAL;
  min    : CARDINAL;
  diff   : CARDINAL;
  neg    : CARDINAL;
  com_no : CARDINAL;
  mod_no : CARDINAL;


  PROCEDURE RemoveUp (bound: CARDINAL);
  VAR
    i: CARDINAL;
  BEGIN
    WITH Frame DO
      IF new_pos >= bound THEN
        ln := new_pos-bound+1;
        IF MinPos < ln THEN
          min := ln;
          MinPos := 0;
        ELSE
          min := MinPos;
          DEC(MinPos, ln);
        END;
        FOR i := min TO MaxPos DO
          Frame[i-ln] := Frame[i];
        END;
        DEC(MaxPos, ln);
        DEC(D_frame, ln);
        DEC(D_curr, ln);
        DEC(new_pos, ln);
      END;
    END;
  END RemoveUp;


  PROCEDURE RemoveDown (bound: CARDINAL);
  VAR
    i: CARDINAL;
  BEGIN
    DEC(D_curr, diff);
    DEC(neg, diff);
    WITH Frame DO
      IF MinPos >= neg + bound  THEN
        new_pos := MinPos - neg;
      ELSE
        new_pos := bound;
        diff := neg + bound - MinPos;
        min := MinPos;
        INC(MinPos, diff);
        INC(D_curr, diff);
        IF MaxPos <= (MaxInstr-1) - diff THEN
          INC(MaxPos, diff);
        ELSE
          MaxPos := (MaxInstr-1);
        END;
        FOR i := MaxPos-MinPos TO 0 BY -1 DO
          Frame[MinPos+i] := Frame[min+i];
        END;
      END;
      D_frame := MinPos;
    END;
  END RemoveDown;


BEGIN
 <* IF DEST_K26 THEN *>
  WITH Frame DO
    IF CurrMode = None THEN
      ASSERT(Mode=D);
      SegmBegin := 0;
      SegmEnd   := 0;
      Access    := kt.ATTRIBS{};
      MinPos    := 0;
      MaxPos    := 0;
      WITH Frame[0] DO
        sort := _addr;
        addr := 0;
      END;
      D_curr  := 0;
      D_frame := 0;
    END;
  END;
 <* END *>
  CASE Mode OF
  | D : -- Дизассемблер
    WITH Frame DO
      ASSERT( (MinPos<=D_frame) AND (D_frame<=MaxPos) );
      ASSERT( D_frame<=D_curr );
      IF delta >= 0 THEN
        new_pos := D_frame + len + VAL(CARDINAL, delta) - 1;
        IF new_pos <= MaxPos THEN
          INC(D_frame, delta);
          INC(D_curr, delta);
          RETURN;
        ELSE
          RemoveUp (MaxInstr-1);
          addr := Frame[MaxPos].addr;
          IF NOT exe.DisasmInstr (addr, FALSE, str, str2, ln) THEN ln := 1; END;
          INC(addr, ln);
          LOOP
            IF NOT exe.DisasmInstr (addr, FALSE, str, str2, ln) THEN ln := 1; END;
            IF addr+ln-1 > SegmEnd THEN EXIT; END;
            INC(MaxPos);
            Frame[MaxPos].addr := addr;
            Frame[MaxPos].sort := _addr;
            IF MaxPos = new_pos THEN EXIT; END;
            INC(addr, ln);
          END;
          ln := new_pos - MaxPos;
          IF VAL(CARDINAL, delta) > ln THEN INC(D_frame, VAL(CARDINAL, delta)-ln); END;
          INC(D_curr, delta);
          IF D_curr > MaxPos THEN D_curr := MaxPos; END;
        END;
      ELSE
        neg := CARDINAL(-delta);
        diff := D_frame - MinPos;
        IF diff >= neg THEN
          DEC(D_frame, neg);
          DEC(D_curr, neg);
        ELSE
          RemoveDown (1);

          LOOP
            addr := Frame[MinPos].addr;
            IF NOT LookUp (addr, addr) THEN EXIT; END;

            Tmp.MinPos := 1;
            Tmp.MaxPos := 0;
            LOOP
              IF NOT exe.DisasmInstr(addr, FALSE, str, str2, ln) THEN ln := 1; END;
              IF Tmp.MaxPos < MaxInstr-1 THEN
                INC(Tmp.MaxPos);
              ELSE
                Tmp.MaxPos := 1;
              END;
              Tmp.Frame[Tmp.MaxPos].addr := addr;
              Tmp.Frame[Tmp.MaxPos].sort := _addr;
              INC(addr, ln);
              IF addr = Frame[MinPos].addr THEN EXIT; END;
              IF addr > Frame[MinPos].addr THEN
                Tmp.MaxPos := 0;
                EXIT;
              END;
            END;
            IF Tmp.MaxPos = 0 THEN EXIT; END;

            LOOP
              DEC(MinPos);
              Frame[MinPos].addr := Tmp.Frame[Tmp.MaxPos].addr;
              Frame[MinPos].sort := _addr;
              DEC(Tmp.MaxPos);
              DEC(D_frame);
              IF D_frame = new_pos THEN EXIT; END;
              IF Tmp.MaxPos = 0 THEN EXIT; END;
            END;

            LOOP
              IF (MinPos = 0) OR (Tmp.MaxPos = 0) THEN EXIT; END;
              DEC(MinPos);
              Frame[MinPos].addr := Tmp.Frame[Tmp.MaxPos].addr;
              Frame[MinPos].sort := _addr;
              DEC(Tmp.MaxPos);
            END;

            IF D_frame = new_pos THEN EXIT; END;
          END;

          IF D_curr >= neg THEN DEC(D_curr, neg); END;
          IF D_curr < D_frame THEN D_curr := D_frame; END;
        END;
      END;
    END;

  | DS : -- Смешанный режим
    WITH Frame DO
      ASSERT( (MinPos<=D_frame) AND (D_frame<=MaxPos) );
      ASSERT( D_frame <= D_curr );
      IF delta >= 0 THEN
        new_pos := D_frame + len + VAL(CARDINAL, delta) - 1;
        IF new_pos < MaxPos THEN
          INC(D_frame, delta);
          INC(D_curr, delta);
          IF Frame[D_curr].sort # _addr THEN
            IF D_curr = D_frame+len-1 THEN INC(D_frame); END;
            INC(D_curr);
          END;
        ELSE
          RemoveUp (MaxInstr-2);
          IF Frame[MaxPos].sort = _src THEN DEC(MaxPos); END;
          addr := Frame[MaxPos].addr;
          ASSERT(tls.FindModByAddr (addr, com_no, mod_no));
          ASSERT(tls.SourceByAddrInMod (com_no, mod_no, addr, i));
          IF NOT exe.DisasmInstr (addr, FALSE, str, str2, ln) THEN ln := 1; END;
          INC(addr, ln);
          LOOP
           <* IF DEST_K26 THEN *>
            IF NOT tls.FindModInCompByAddr (com_no, addr, mod_no) THEN EXIT; END;
           <* END *>
            IF NOT tls.SourceByAddrInMod (com_no, mod_no, addr, ln) THEN EXIT; END;
            IF i # ln THEN
              INC(MaxPos);
              Frame[MaxPos].sort := _src;
              Frame[MaxPos].com := com_no;
              Frame[MaxPos].m := mod_no;
              Frame[MaxPos].ln := ln;
              i := ln;
            END;
            IF NOT exe.DisasmInstr (addr, FALSE, str, str2, ln) THEN ln := 1; END;
            IF addr+ln-1 > SegmEnd THEN EXIT; END;
            INC(MaxPos);
            Frame[MaxPos].sort := _addr;
            Frame[MaxPos].addr := addr;
            IF MaxPos >= new_pos THEN EXIT; END;
            INC(addr, ln);
          END;
          IF MaxPos >= new_pos THEN ln := 0; ELSE ln := new_pos - MaxPos; END;
          IF VAL(CARDINAL, delta) > ln THEN INC(D_frame, VAL(CARDINAL, delta)-ln); END;
          INC(D_curr, delta);
          IF D_curr > MaxPos THEN D_curr := MaxPos; END;
          IF Frame[D_curr].sort # _addr THEN
            IF D_curr = D_frame+len-1 THEN INC(D_frame); END;
            INC(D_curr);
          END;
        END;
      ELSE
        neg := CARDINAL(-delta);
        diff := D_frame - MinPos;
        IF diff > neg THEN
          DEC(D_frame, neg);
          DEC(D_curr, neg);
          IF Frame[D_curr].sort # _addr THEN
            IF D_frame = D_curr THEN DEC(D_frame); END;
            DEC(D_curr);
          END;
        ELSE
          RemoveDown (1+len);

          IF Frame[MinPos].sort = _src THEN
            com_no := Frame[MinPos].com;
            mod_no := Frame[MinPos].m;
          ELSE
            addr := Frame[MinPos].addr;
            ASSERT(tls.FindModByAddr (addr, com_no, mod_no));
            ASSERT(tls.SourceByAddrInMod (com_no, mod_no, addr, ln));
            DEC(MinPos);
            Frame[MinPos].sort := _src;
            Frame[MinPos].com := com_no;
            Frame[MinPos].m := mod_no;
            Frame[MinPos].ln := ln;
          END;

          LOOP
            addr := Frame[MinPos+1].addr;
            IF NOT tls.PrevBasePoint (com_no, mod_no, addr, addr) OR
               (addr < SegmBegin)
            THEN
              IF D_curr >= neg THEN DEC(D_curr, neg); END;
              IF D_curr < D_frame THEN D_curr := D_frame; END;
              EXIT;
            END;
            IF NOT tls.SourceByAddrInMod (com_no, mod_no, addr, ln) THEN
              EXIT;
            END;
            Tmp.MaxPos := 0;
            Tmp.Frame[Tmp.MaxPos].sort := _src;
            Tmp.Frame[Tmp.MaxPos].com := com_no;
            Tmp.Frame[Tmp.MaxPos].m := mod_no;
            Tmp.Frame[Tmp.MaxPos].ln := ln;
            LOOP
              IF NOT exe.DisasmInstr(addr, FALSE, str, str2, ln) THEN ln := 1; END;
              IF addr+ln-1 > SegmEnd THEN EXIT; END;
              IF Tmp.MaxPos < MaxInstr-1 THEN
                INC(Tmp.MaxPos);
              ELSE
                Tmp.MaxPos := 1;
              END;
              Tmp.Frame[Tmp.MaxPos].sort := _addr;
              Tmp.Frame[Tmp.MaxPos].addr := addr;
              INC(addr, ln);
              IF addr >= Frame[MinPos+1].addr THEN EXIT; END;
            END;
            ASSERT(Tmp.MaxPos # 0);

            IF Tmp.Frame[0].ln # Frame[MinPos].ln THEN
              DEC(MinPos);
            END;

            LOOP
              Frame[MinPos] := Tmp.Frame[Tmp.MaxPos];
              IF Tmp.MaxPos = 0 THEN EXIT; END;
              IF D_frame = new_pos THEN EXIT; END;
              DEC(MinPos);
              DEC(D_curr);
              DEC(D_frame);
              DEC(Tmp.MaxPos);
            END;

            IF D_frame = new_pos THEN EXIT; END;
          END;

          IF Frame[D_curr].sort # _addr THEN
            IF D_curr = MinPos THEN
              INC(D_curr);
            ELSE
              IF D_frame = D_curr THEN DEC(D_frame); END;
              DEC(D_curr);
            END;
          END;
        END;
      END;
    END;
  END;
END ShiftFrame;


VAR
  Save_Mode            : DISASM_MODE;
  Save_D_frame_address : ADDRESS;
  Save_D_curr_delta    : CARDINAL;

PROCEDURE SaveCurrentState;
BEGIN
  Save_Mode := Mode;
  CASE Mode OF
  | None: RETURN;
  | D   : Save_D_frame_address := Frame.Frame[D_frame].addr;
  | DS  : IF Frame.Frame[D_frame].sort = _addr THEN
            Save_D_frame_address := Frame.Frame[D_frame].addr;
          ELSE
            Save_D_frame_address := Frame.Frame[D_frame+1].addr;
          END;
  END;
  Save_D_curr_delta := D_curr - D_frame;
END SaveCurrentState;


PROCEDURE RestoreCurrentState;
BEGIN
  Mode := Save_Mode;
  IF Mode # None THEN
    IF SetNewPos(Save_D_frame_address, 1) THEN
      D_curr := D_frame + Save_D_curr_delta;
    ELSE
      MakeInvalid ();
      Mode := None;
    END;
  END;
END RestoreCurrentState;


PROCEDURE MakeInvalid;
BEGIN
  WITH Frame DO
    CurrMode := None;
    SegmBegin := 0;
    SegmEnd := 0;
    Access := kt.ATTRIBS{};
    MinPos := MaxInstr-1;
    MaxPos := 0;
  END;
END MakeInvalid;


BEGIN
  MakeInvalid;
  Mode := None;
END Dlg_Dasm.

