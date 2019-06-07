<* IOVERFLOW- *>
<* COVERFLOW- *>
<* O2ADDKWD+ *>
MODULE AsmX86;
IMPORT pcS,pcK,pcO,pcB;
IMPORT env:=xiEnv;
IMPORT cmd:=CodeDef;
IMPORT at:=opAttrs;
IMPORT ir;

IMPORT LowReal;
IMPORT EX:=EXCEPTIONS;
IMPORT SYSTEM;

CONST
  MNEM_LEN=10;
  TAB_LEN=32;

  EOL = 0A3X;

CONST
  BASE_ERROR_NUMBER=3000;
  
  ER_EXPRSTACKOV=1;
  ER_RPAR=2;
  ER_ILLEXPR=3;
  ER_PTR=4;
  ER_RANGE=5;
  ER_ILLAMODE=6;
  ER_REQNUM=7;
  ER_ILLSCALE=8;  
  ER_ILLARG=9;
  ER_DISIMM=10;
  ER_REQCOMMA=11;
  ER_WRONGOPSIZE=12;
  ER_UNKDSIZE=13;
  ER_MIXDSIZE=14;
  ER_REQGREG=15;
  ER_REQGREG32=16;
  ER_DISCS=17;
  ER_REQDSIZE16=18;
  ER_ONLYCL=19;
  ER_DISR8=20;
  ER_DISD8=21;
  ER_REQLABEL=22;
  ER_REQIMM=23;
  ER_ILLMNEM=24;
  ER_EXTRA=25;
  ER_REDEFLABEL=26;
  ER_UNDEFLABEL=27;
  ER_SYNT=28;
  ER_TOOFAR=29;
  ER_UNDEFID=30;
  ER_EXPID=31;
  ER_ILLQID=32;
  ER_EXPCONST=33;
  ER_ILLCONSTTYPE=34;
  ER_JMPDEST=35;
  ER_ONLYAL=36;
  ER_ONLYDX=37;
  ER_ONLYAX=38;
  ER_FREG=39;
  ER_FREG0=40;
  ER_MMXREG=41;
  ER_REQCOLON=42;
  ER_CSEGONLY=43;
  ER_CONSTDIS=44;
  ER_NONDLABEL=45;
  ER_NONCLABEL=46;
  ER_REQREC=47;
  ER_REQPERIOD=48;
  ER_DBSIZE=49;
  ER_TWOLV=50;
  ER_TWOGLOB=51;
  ER_ILLVARUSE=52;
  ER_RPARWOL=53;
  ER_UNKSIZE=54;

  ER_A16UNIMP=201;
  ER_EXSTACKEMPTY=301;
    
TYPE
  INT*=LONGINT;
  CARD=SYSTEM.CARD32;
  BYTE*=SYSTEM.BYTE;

TYPE 
  MarkObject* = PROCEDURE (v: pcK.OBJECT);
  GetOffset* = PROCEDURE (v: pcK.OBJECT): LONGINT;

CONST  
  DATASIZE=16;
    
TYPE  
  PCOD=POINTER TO COD;
  MNEM=ARRAY MNEM_LEN OF CHAR;
  CMD=POINTER TO CMD_REC;
  CMD_REC=
    RECORD
      next:CMD;
      str:MNEM;
    END;

  INST=POINTER TO INST_REC;
  INST_REC=
    RECORD (CMD_REC)
    END;

  PREFIX=POINTER TO PREFIX_REC;
  PREFIX_REC=
    RECORD (CMD_REC)
      code:INT;
    END;

  LABL=POINTER TO LABL_REC;
  LABL_REC=
    RECORD (CMD_REC)
      location:PCOD;
    END;

  COD=RECORD
        next:PCOD;
        bnum:INT;
        index:INT;
        isdseg:BOOLEAN;
        dofs:LONGINT;
        mpos:pcK.TPOS;
      END;

  PCODDATA=POINTER TO CODDATA;
  CODDATA=RECORD (COD)
            len:INT;
            data:POINTER TO ARRAY OF BYTE;
          END;
  PCODJLABL=POINTER TO CODJLABL;
  CODJLABL=RECORD (COD)
            canfar:BOOLEAN;
            j32:BOOLEAN;
            code:INT;
            label:LABL;
            location:PCOD;
            tpos:pcK.TPOS;
          END;  
  PCODJMP=POINTER TO CODJMP;
  CODJMP=RECORD (COD)
          code:INT;
          j32:BOOLEAN;
          code32:INT;
          label:LABL;
          location:PCOD;
          tpos:pcK.TPOS;
         END;

  DREF=POINTER TO DREF_REC;

  REFS=POINTER TO ARRAY OF DREF;
  DREFI=RECORD
          lv:BOOLEAN;
          lvloc:BOOLEAN;
          type:pcK.STRUCT;
          nref:INT;
          refs:REFS;
        END;

  PCODFX=POINTER TO CODFX;
  CODFX=RECORD (COD)
          drefs:DREFI;
          ofs:LONGINT;
          kind:SHORTINT;
        END;
  PCODFP=POINTER TO CODFP;
  CODFP=RECORD (COD)
          drefs:DREFI;
          ofs:LONGINT;
          b1:INT;
          b2:INT;
          is2b:BOOLEAN;
        END;

  COD_VALUE=POINTER TO COD_VALUE_REC;
  COD_VALUE_REC=
      RECORD (pcK.value_rec)
        prepared:BOOLEAN;
        code:PCOD;
        coded:PCOD;
      END;   

  RM_REC= RECORD
            dwidth,awidth:INT;
            simple:BOOLEAN;
            imm:BOOLEAN;
            reg1,reg2:INT;
            scale:INT;
            ofs:INT;
            drefs:DREFI;
            begpos:pcK.TPOS;
          END;  

  DREF_REC=
    RECORD
    END;

  DVAR=POINTER TO DVAR_REC;
  DVAR_REC=
    RECORD (DREF_REC)
      var:pcK.OBJECT;
    END;

  DLAB=POINTER TO DLAB_REC;
  DLAB_REC=
    RECORD (DREF_REC)
      label:LABL;
      tpos:pcK.TPOS;
    END;
    
VAR
  ExS:EX.ExceptionSource;
VAR
  MarkOb:MarkObject;
  GetOf:GetOffset;
  lasttxtpos:pcK.TPOS;
  Begpos:pcK.TPOS;
  EOLflag:BOOLEAN;
  line:INT;
  Token:pcS.Symbol;
  wsy:pcS.Symbol;
  wasEOL:BOOLEAN;

  RM1,RM2:RM_REC;

  segovr:INT;
  deferovr:BOOLEAN;
  
  code:PCOD;
  coded:PCOD;
  curcode:PCOD;
  curccode:PCOD;
  curdcode:PCOD;
  isdseg:BOOLEAN;
  data_ob:pcK.OBJECT;

  zz_tmp:pcK.VALUE;

TYPE
  SEGOVRCODE=ARRAY 6 OF INT;
CONST
  segovrcode=SEGOVRCODE{26H,2EH,36H,3EH,64H,65H};

PROCEDURE ErrorPos(tpos:pcK.TPOS;no:INTEGER);
BEGIN
  env.errors.Error(tpos,BASE_ERROR_NUMBER+no);
  EX.RAISE(ExS,no,"");
END ErrorPos;

PROCEDURE Error(no:INTEGER);
BEGIN
  ErrorPos(lasttxtpos,no);
END Error;

PROCEDURE ErrorNext(no:INTEGER);
BEGIN
  IF wasEOL THEN
    ErrorPos(lasttxtpos,no);
  ELSE
    ErrorPos(pcS.txtpos,no);
  END;
END ErrorNext;

PROCEDURE GetToken;
VAR
  fnm:env.String;
  col,ln:INT;
BEGIN
  IF wasEOL THEN
    wasEOL:=FALSE;
    Token:=wsy;
    RETURN;
  END;
  lasttxtpos:=pcS.txtpos;
  pcS.get(Token);
  pcS.txtpos.unpack(fnm,ln,col);
  IF ln>line THEN
    line:=ln;
    wasEOL:=TRUE;
    wsy:=Token;
    Token:=EOL;
    EOLflag:=TRUE;
    Begpos:=pcS.txtpos;
    RETURN;
  END;
END GetToken;

PROCEDURE IsOffset(r:DREF):BOOLEAN;
BEGIN
  WITH r:DVAR DO
    RETURN (r.var.mode=pcK.ob_field) OR (r.var.lev>0);
  ELSE
    RETURN FALSE;
  END;
END IsOffset;

PROCEDURE IsFrame(r:DREF):BOOLEAN;
BEGIN
  WITH r:DVAR DO
    RETURN r.var.lev>0;
  ELSE
    RETURN FALSE;
  END;
END IsFrame;

PROCEDURE (VAR drefs:DREFI) Init;
BEGIN
   drefs.lv:=FALSE;
   drefs.lvloc:=FALSE;
   drefs.type:=NIL;
   drefs.nref:=0;
   NEW(drefs.refs,10);
END Init;

PROCEDURE (VAR drefs:DREFI) AddRef(r:DREF;isofs:BOOLEAN;tp:pcK.STRUCT);
VAR
  refs:REFS;
  i:INT;
BEGIN
  IF drefs.nref>=LEN(drefs.refs^) THEN
    NEW(refs,LEN(drefs.refs^)+10);
    FOR i:=0 TO LEN(drefs.refs^)-1 DO
      refs[i]:=drefs.refs[i];
    END;
    drefs.refs:=refs;
  END;
  IF NOT isofs THEN
    IF drefs.lv THEN
      Error(ER_TWOLV);
    END;
    drefs.lv:=TRUE;
    IF IsFrame(r) THEN
      drefs.lvloc:=TRUE;
    END;
    IF drefs.type=NIL THEN
      drefs.type:=tp;
    END;
  END;
  IF (drefs.nref=0) OR IsOffset(r) THEN
    drefs.refs[drefs.nref]:=r;
  ELSE
    IF NOT IsOffset(drefs.refs[0]) THEN
      Error(ER_TWOGLOB);
    END;
    FOR i:=drefs.nref TO 1 BY -1 DO
      drefs.refs[i]:=drefs.refs[i-1];
    END;
    drefs.refs[0]:=r;
  END;
  drefs.nref:=drefs.nref+1;
END AddRef;

PROCEDURE BeginInst;
BEGIN
  deferovr:=FALSE;
  segovr:=0;
END BeginInst;  

PROCEDURE NewDataArr;
VAR
  t:PCODDATA;
BEGIN
  NEW(t);
  NEW(t.data,DATASIZE);
  t.len:=0;
  t.isdseg:=isdseg;
  t.next:=NIL;
  t.mpos:=env.null_pos;
  curcode.next:=t;
  curcode:=t;
END NewDataArr;

PROCEDURE GenByte(b:CARD);
BEGIN
  IF curcode(PCODDATA).len>=LEN(curcode(PCODDATA).data^) THEN
    NewDataArr;
  END;
  curcode(PCODDATA).data[curcode(PCODDATA).len]:=VAL(SYSTEM.CARD8,b);
  curcode(PCODDATA).len:=curcode(PCODDATA).len+1;
END GenByte;

PROCEDURE GenBuf(v:ARRAY OF SYSTEM.BYTE);
VAR
  i:INT;
BEGIN
  FOR i:=0 TO LEN(v)-1 DO
    GenByte(VAL(CARD,v[i]));
  END;
END GenBuf;

PROCEDURE GenWord(d:CARD);
BEGIN
  GenByte(d MOD 256);
  GenByte(d DIV 256);
END GenWord;

PROCEDURE GenDWord(d:CARD);
BEGIN
  GenByte(d MOD 256);
  d:=d DIV 256;
  GenByte(d MOD 256);
  d:=d DIV 256;
  GenByte(d MOD 256);
  d:=d DIV 256;
  GenByte(d MOD 256);
END GenDWord;

PROCEDURE GenImmN(v:INT;width:INT;tpos:pcK.TPOS);
BEGIN
  IF width=32 THEN
    GenDWord(v);
  ELSIF width=16 THEN
    IF NOT ((v>=-32768) AND (v<=65535)) THEN
      ErrorPos(tpos,ER_RANGE);
    END;
    GenWord(v);
  ELSE
    IF NOT ((v>=-128) AND (v<=255)) THEN
      ErrorPos(tpos,ER_RANGE);
    END;
    GenByte(v);
  END;
END GenImmN;

PROCEDURE GenSPref(a,d:INT);
BEGIN
  IF segovr#0 THEN
    GenByte(segovr);
    segovr:=0;
  END;
  IF a=16 THEN
    GenByte(67H);
  END;
  IF d=16 THEN
    GenByte(66H);
  END;
END GenSPref;

PROCEDURE GenOfs(VAR RM:RM_REC);
VAR
  t:PCODFX;
BEGIN
  IF RM.drefs.nref=0 THEN
    GenImmN(RM.ofs,RM.awidth,RM.begpos);
  ELSE
    IF RM.awidth#32 THEN
      ErrorPos(RM.begpos,ER_A16UNIMP);
    END;      
    NEW(t);
    t.isdseg:=isdseg;
    t.next:=NIL;
    curcode.next:=t;
    curcode:=t;
    t.drefs:=RM.drefs;
    t.ofs:=RM.ofs;
    t.kind:=cmd.fx_obj32;
    NewDataArr;
  END;  
END GenOfs;  

PROCEDURE GenImmVO(v:INT;VAR o:DREFI;width:INT;tpos:pcK.TPOS);
VAR
  t:PCODFX;
BEGIN
  IF o.nref=0 THEN
    GenImmN(v,width,tpos);
  ELSE
    IF width#32 THEN
      ErrorPos(tpos,ER_RANGE);
    END;
    NEW(t);
    t.isdseg:=isdseg;
    t.next:=NIL;
    curcode.next:=t;
    curcode:=t;
    t.drefs:=o;             (* !!! we suppose that 'o' won't be used later !!! *)
    t.ofs:=v;
    t.kind:=cmd.fx_obj32;
    NewDataArr;
  END;
END GenImmVO;

PROCEDURE GenImm(v:INT;VAR o:DREFI;width:INT);
BEGIN
  GenImmVO(v,o,width,lasttxtpos);
END GenImm;

PROCEDURE GenImmRM(VAR RM:RM_REC;width:INT);
BEGIN
  GenImmVO(RM.ofs,RM.drefs,width,RM.begpos);
END GenImmRM;

PROCEDURE GenRM(VAR RM:RM_REC;digit:INT);
VAR
  b:INT;
  b1,b2:INT;
  is2b,defer:BOOLEAN;
  dispwidth:INT;
  t:PCODFP;
BEGIN
  dispwidth:=0;
  defer:=(RM.drefs.nref>0) AND IsOffset(RM.drefs.refs[0]);
  b1:=0;
  b2:=0;
  is2b:=FALSE;
  IF RM.simple THEN
    GenByte(0C0H+RM.reg1+digit*8);
  ELSE
    IF RM.awidth=32 THEN
      IF (RM.scale#1) OR (RM.reg2#-1) OR (RM.reg1=4) THEN
        -- SIB
        IF RM.reg2=-1 THEN
          IF RM.scale#1 THEN
            GenByte(4+digit*8);
            GenByte((RM.scale+2) DIV 3*64+RM.reg1*8+5);
            dispwidth:=32;
            defer:=FALSE;
          ELSE
            IF (RM.ofs=0) AND (RM.drefs.nref=0) THEN
              b:=4;
            ELSIF (RM.ofs<=127) AND (RM.ofs>=-128) AND (RM.drefs.nref=0) THEN
              b:=44H;
              dispwidth:=8;
            ELSE
              b:=84H;
              dispwidth:=32;
            END;
            b1:=b+digit*8;
--            b2:=24H;
            is2b:=TRUE;
            IF NOT defer THEN
              GenByte(b1);
              GenByte(24H);
            END;
          END;
        ELSE
          IF (RM.scale=1) AND (RM.reg2=5) THEN
            RM.reg2:=RM.reg1;
            RM.reg1:=5;
          ELSIF RM.reg1=4 THEN
            RM.reg1:=RM.reg2;
            RM.reg2:=4;
          END;
          IF (RM.ofs=0) AND (RM.reg2#5) AND (RM.drefs.nref=0) THEN
            b:=4;
          ELSIF (RM.ofs<=127) AND (RM.ofs>=-128) AND (RM.drefs.nref=0) THEN
            b:=44H;
            dispwidth:=8;
          ELSE
            b:=84H;
            dispwidth:=32;
          END;
          b1:=b+digit*8;
          IF RM.scale=8 THEN
            b:=0C0H;
          ELSE
            b:=RM.scale DIV 2*64;
          END;
          b2:=b+RM.reg1*8+RM.reg2;
          is2b:=TRUE;
          IF NOT defer THEN
            GenByte(b1);
            GenByte(b2);
          END;
        END;
      ELSE
        -- ModR/M
        IF RM.reg1=-1 THEN
          b:=5;
          dispwidth:=32;
          defer:=FALSE;
        ELSIF (RM.ofs=0) AND (RM.reg1#5) AND (RM.drefs.nref=0) THEN
          b:=RM.reg1;
        ELSIF (RM.ofs<=127) AND (RM.ofs>=-128) AND (RM.drefs.nref=0) THEN
          b:=40H+RM.reg1;
          dispwidth:=8;
        ELSE
          b:=80H+RM.reg1;
          dispwidth:=32;
        END;
        b1:=b+digit*8;
        IF NOT defer THEN
          GenByte(b1);
        END;
      END;
    ELSE
      ErrorPos(RM.begpos,ER_A16UNIMP);
    END;
  END;
  IF dispwidth=8 THEN
    GenByte(RM.ofs);
  ELSIF dispwidth=32 THEN
    IF defer THEN
      NEW(t);
      t.isdseg:=isdseg;
      t.next:=NIL;
      curcode.next:=t;
      curcode:=t;
      t.drefs:=RM.drefs;
      t.ofs:=RM.ofs;
      t.b1:=b1;
      t.b2:=b2;
      t.is2b:=is2b;
      NewDataArr;
    ELSE
      GenOfs(RM);
    END;
  END;
END GenRM;  

PROCEDURE EndInst;
BEGIN
END EndInst;  

VAR
  ITable:ARRAY TAB_LEN OF CMD;
  i:INT;
  
PROCEDURE Index(n-:ARRAY OF CHAR):INT;
VAR
  i:INT;
  r:INT;
BEGIN
  r:=0;
  i:=0;
  WHILE n[i]#0C DO
    r:=r*8;
    r:=(r+(r DIV 65536)) MOD 65536;
    r:=r+ORD(n[i]);
    i:=i+1;
  END;
  RETURN r MOD MNEM_LEN;
END Index;

PROCEDURE Set(n-:ARRAY OF CHAR;c:CMD);
VAR
  i:INT;
BEGIN
  COPY(n,c.str);
  i:=Index(n);
  c.next:=ITable[i];
  ITable[i]:=c;
END Set;

PROCEDURE Find(n-:ARRAY OF CHAR):CMD;
VAR
  c:CMD;
BEGIN
  c:=ITable[Index(n)];
  WHILE (c#NIL) DO
    IF c.str=n THEN
      RETURN c;
    END;
    c:=c.next;
  END;
  RETURN NIL;
END Find;  

PROCEDURE FindU(n:ARRAY OF CHAR):CMD;
VAR
  c:CMD;
  i:INT;
BEGIN
  c:=Find(n);
  IF c#NIL THEN
    RETURN c;
  END;
  i:=0;
  WHILE n[i]#0C DO
    n[i]:=CAP(n[i]);
    i:=i+1;
  END;
  RETURN Find(n);
END FindU;

PROCEDURE CleanUpSym;
VAR
  p,c:CMD;
  i:INT;
BEGIN
  FOR i:=0 TO TAB_LEN-1 DO
    c:=ITable[i];
    p:=NIL;
    WHILE c#NIL DO
      IF c IS INST THEN
        p:=c;
      ELSE
        IF p=NIL THEN
          ITable[i]:=c.next;
        ELSE
          p.next:=c.next;     (* We never come here (at least in this version) *)
        END;
      END;
      c:=c.next;
    END;
  END;
END CleanUpSym;  
      
PROCEDURE (VAR o:PREFIX_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

TYPE    
  DCODE=POINTER TO DCODE_REC;
  DCODE_REC=
    RECORD (INST_REC)
    END;

  DB=POINTER TO DB_REC;
  DB_REC=
    RECORD (DCODE_REC)
      size:INT;
    END;

  CSEG=POINTER TO CSEG_REC;
  CSEG_REC=
    RECORD (DCODE_REC)
    END;

  DSEG=POINTER TO DSEG_REC;
  DSEG_REC=
    RECORD (DCODE_REC)
    END;

  A2BYTE=ARRAY 2 OF INT;

  INST0=POINTER TO INST0_REC;
  INST0_REC=
    RECORD (INST_REC)
      len:INT;
      code:A2BYTE;
    END;

  INST1G=POINTER TO INST1G_REC;
  INST1G_REC=
    RECORD (INST_REC)
      len:INT;
      code:A2BYTE;
      digit:INT;
      can32:BOOLEAN;
    END;

  INST1NEG=POINTER TO INST1NEG_REC;
  INST1NEG_REC=
    RECORD (INST1G_REC)
    END;
      
  INST1INC=POINTER TO INST1INC_REC;
  INST1INC_REC=
    RECORD (INST1G_REC)
      fast:INT;  
    END;

  INST2ADD=POINTER TO INST2ADD_REC;
  INST2ADD_REC=
    RECORD (INST_REC)
      gbase,ibase,digit:INT;
    END;
    
  INST2MOV=POINTER TO INST2MOV_REC;
  INST2MOV_REC=
    RECORD (INST_REC)
    END;

  INST1PUSH=POINTER TO INST1PUSH_REC;
  INST1PUSH_REC=
    RECORD (INST_REC)
    END;

  INST1POP=POINTER TO INST1POP_REC;
  INST1POP_REC=
    RECORD (INST_REC)
    END;

  INST2SHIFT=POINTER TO INST2SHIFT_REC;
  INST2SHIFT_REC=
    RECORD (INST_REC)
      digit:INT;
    END;  

  INST2LEA=POINTER TO INST2LEA_REC;
  INST2LEA_REC=
    RECORD (INST_REC)
    END;
      
  INST2TEST=POINTER TO INST2TEST_REC;
  INST2TEST_REC=
    RECORD (INST_REC)
    END;

  INST2XCHG=POINTER TO INST2XCHG_REC;
  INST2XCHG_REC=
    RECORD (INST_REC)
    END;

  INST1Jcc=POINTER TO INST1Jcc_REC;
  INST1Jcc_REC=
    RECORD (INST_REC)
      near:INT;
      canfar:BOOLEAN;
    END;

  INST1JECXZ=POINTER TO INST1JECXZ_REC;
  INST1JECXZ_REC=
    RECORD (INST1Jcc_REC)
    END;

  INST1JCXZ=POINTER TO INST1JCXZ_REC;
  INST1JCXZ_REC=
    RECORD (INST1JECXZ_REC)
    END;

  INST2CMOVcc=POINTER TO INST2CMOVcc_REC;
  INST2CMOVcc_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST2BT=POINTER TO INST2BT_REC;
  INST2BT_REC=
    RECORD (INST_REC)
      code:INT;
      digit:INT;
    END;

  INST1BSWAP=POINTER TO INST1BSWAP_REC;
  INST1BSWAP_REC=
    RECORD (INST_REC)
    END;

  INST1RET=POINTER TO INST1RET_REC;
  INST1RET_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INSTxIMUL=POINTER TO INSTxIMUL_REC;
  INSTxIMUL_REC=
    RECORD (INST_REC)
    END;

  INST1JMP=POINTER TO INST1JMP_REC;
  INST1JMP_REC=
    RECORD (INST_REC)
      shortcode:INT;
      relcode:INT;
      digit:INT;
      fpcode:INT;
    END;

  INST1LGDT=POINTER TO INST1LGDT_REC;
  INST1LGDT_REC=
    RECORD (INST_REC)
      digit:INT;
    END;

  INST1LLDT=POINTER TO INST1LLDT_REC;
  INST1LLDT_REC=
    RECORD (INST_REC)
      can32:BOOLEAN;
      code:INT;
      digit:INT;
    END;

  INST1INT=POINTER TO INST1INT_REC;
  INST1INT_REC=
    RECORD (INST_REC)
    END;

  INST2LSL=POINTER TO INST2LSL_REC;
  INST2LSL_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST2MOVSX=POINTER TO INST2MOVSX_REC;
  INST2MOVSX_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST3SHLD=POINTER TO INST3SHLD_REC;
  INST3SHLD_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST2ENTER=POINTER TO INST2ENTER_REC;
  INST2ENTER_REC=
    RECORD (INST_REC)
    END;

  INST2XADD=POINTER TO INST2XADD_REC;
  INST2XADD_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST2ARPL=POINTER TO INST2ARPL_REC;
  INST2ARPL_REC=
    RECORD (INST_REC)
    END;

  INST2BOUND=POINTER TO INST2BOUND_REC;
  INST2BOUND_REC=
    RECORD (INST_REC)
    END;

  INST1CMPXCHG8B=POINTER TO INST1CMPXCHG8B_REC;
  INST1CMPXCHG8B_REC=
    RECORD (INST_REC)
    END;

  INST1INVLPG=POINTER TO INST1INVLPG_REC;
  INST1INVLPG_REC=
    RECORD (INST_REC)
    END;

  INST2LDS=POINTER TO INST2LDS_REC;
  INST2LDS_REC=
    RECORD (INST_REC)
      is0f:BOOLEAN;
      code:INT;
    END;

  INST2IN=POINTER TO INST2IN_REC;
  INST2IN_REC=
    RECORD (INST_REC)
    END;

  INST2OUT=POINTER TO INST2OUT_REC;
  INST2OUT_REC=
    RECORD (INST_REC)
    END;

  INST2F=POINTER TO INST2F_REC;
  INST2F_REC=
    RECORD (INST_REC)
      code,coder:INT;
      digit:INT;
    END;

  INST2FP=POINTER TO INST2FP_REC;
  INST2FP_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST1FI=POINTER TO INST1FI_REC;
  INST1FI_REC=
    RECORD (INST_REC)
      digit:INT;
    END;

  INST1FR=POINTER TO INST1FR_REC;
  INST1FR_REC=
    RECORD (INST_REC)
      code1,code2:INT;
      req:BOOLEAN;
    END;

  INST1FLD=POINTER TO INST1FLD_REC;
  INST1FLD_REC=
    RECORD (INST_REC)
      rcode1,rcode2:INT;
      digit,digit80:INT;
      m80:BOOLEAN;
    END;

  INST1FILD=POINTER TO INST1FILD_REC;
  INST1FILD_REC=
    RECORD (INST_REC)
      digit,digit64:INT;
      m64:BOOLEAN;
    END;

  INST1FCOM=POINTER TO INST1FCOM_REC;
  INST1FCOM_REC=
    RECORD (INST_REC)
      code:INT;
      digit:INT;
    END;

  INST2FCOMI=POINTER TO INST2FCOMI_REC;
  INST2FCOMI_REC=
    RECORD (INST_REC)
      code1,code2:INT;
    END;

  INST1FM=POINTER TO INST1FM_REC;
  INST1FM_REC=
    RECORD (INST_REC)
      width:INT;
      code:INT;
      digit:INT;
      p9b:BOOLEAN;
    END;

  INST0F3=POINTER TO INST0F3_REC;
  INST0F3_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST1FSTSW=POINTER TO INST1FSTSW_REC;
  INST1FSTSW_REC=
    RECORD (INST_REC)
      p9b:BOOLEAN;
    END;

  INST2XRM=POINTER TO INST2XRM_REC;
  INST2XRM_REC=
    RECORD (INST_REC)
      code:INT;
    END;

  INST2XRMI=POINTER TO INST2XRMI_REC;
  INST2XRMI_REC=
    RECORD (INST_REC)
      code1,code2,digit:INT;
    END;

  INST2MOVD=POINTER TO INST2MOVD_REC;
  INST2MOVD_REC=
    RECORD (INST_REC)
    END;

  INST2MOVQ=POINTER TO INST2MOVQ_REC;
  INST2MOVQ_REC=
    RECORD (INST_REC)
    END;

PROCEDURE (VAR o:INST_REC) P;
BEGIN
  ASSERT(FALSE);
END P;
    
PROCEDURE (VAR o:DB_REC) Ini(size:INT);
BEGIN
  o.size:=size;
END Ini;

PROCEDURE (VAR o:DSEG_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:CSEG_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST0_REC) Ini(len:INT;b1,b2:INT);
BEGIN
  o.len:=len;
  o.code[0]:=b1;
  o.code[1]:=b2;
END Ini;

PROCEDURE (VAR o:INST1NEG_REC) Ini(len:INT;b1,b2:INT;digit:INT;c32:BOOLEAN);
BEGIN
  o.len:=len;
  o.code[0]:=b1;
  o.code[1]:=b2;
  o.digit:=digit;
  o.can32:=c32;
END Ini;

PROCEDURE (VAR o:INST1INC_REC) Ini(len:INT;b1,b2:INT;digit:INT;c32:BOOLEAN;fast:INT);
BEGIN
  o.len:=len;
  o.code[0]:=b1;
  o.code[1]:=b2;
  o.digit:=digit;
  o.can32:=c32;
  o.fast:=fast;
END Ini;  
  
PROCEDURE (VAR o:INST2ADD_REC) Ini(b1,b2,digit:INT);
BEGIN
  o.gbase:=b1;
  o.ibase:=b2;
  o.digit:=digit;
END Ini;  

PROCEDURE (VAR o:INST2MOV_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST1POP_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST1PUSH_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2SHIFT_REC) Ini(digit:INT);
BEGIN
  o.digit:=digit;
END Ini;

PROCEDURE (VAR o:INST2LEA_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2TEST_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2XCHG_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST1Jcc_REC) Ini(near:INT);
BEGIN
  o.near:=near;
  o.canfar:=TRUE;
END Ini;

PROCEDURE (VAR o:INST1JECXZ_REC) Ini(near:INT);
BEGIN
  o.near:=near;
  o.canfar:=FALSE;
END Ini;

PROCEDURE (VAR o:INST2CMOVcc_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST2BT_REC) Ini(code,digit:INT);
BEGIN
  o.code:=code;
  o.digit:=digit;
END Ini;

PROCEDURE (VAR o:INST1BSWAP_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST1RET_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INSTxIMUL_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST1JMP_REC) Ini(sh,rel,dig,fp:INT);
BEGIN
  o.shortcode:=sh;
  o.relcode:=rel;
  o.digit:=dig;
  o.fpcode:=fp;
END Ini;

PROCEDURE (VAR o:INST1LGDT_REC) Ini(digit:INT);
BEGIN
  o.digit:=digit;
END Ini;

PROCEDURE (VAR o:INST1LLDT_REC) Ini(code,digit:INT;can32:BOOLEAN);
BEGIN
  o.digit:=digit;
  o.code:=code;
  o.can32:=can32;
END Ini;

PROCEDURE (VAR o:INST1INT_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2LSL_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST2MOVSX_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST3SHLD_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST2ENTER_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2XADD_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST2ARPL_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2BOUND_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST1CMPXCHG8B_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST1INVLPG_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2LDS_REC) Ini(is0f:BOOLEAN;code:INT);
BEGIN
  o.is0f:=is0f;
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST2IN_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2OUT_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2F_REC) Ini(code,coder,digit:INT);
BEGIN
  o.code:=code;
  o.coder:=coder;
  o.digit:=digit;
END Ini;

PROCEDURE (VAR o:INST2FP_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST1FI_REC) Ini(digit:INT);
BEGIN
  o.digit:=digit;
END Ini;

PROCEDURE (VAR o:INST1FR_REC) Ini(code1,code2:INT;req:BOOLEAN);
BEGIN
  o.code1:=code1;
  o.code2:=code2;
  o.req:=req;
END Ini;

PROCEDURE (VAR o:INST1FLD_REC) Ini(rcode1,rcode2:INT;digit,digit80:INT;m80:BOOLEAN);
BEGIN
  o.rcode1:=rcode1;
  o.rcode2:=rcode2;
  o.digit:=digit;
  o.digit80:=digit80;
  o.m80:=m80;
END Ini;

PROCEDURE (VAR o:INST1FILD_REC) Ini(digit,digit64:INT;m64:BOOLEAN);
BEGIN
  o.digit:=digit;
  o.digit64:=digit64;
  o.m64:=m64;
END Ini;

PROCEDURE (VAR o:INST1FCOM_REC) Ini(code,digit:INT);
BEGIN
  o.code:=code;
  o.digit:=digit;
END Ini;

PROCEDURE (VAR o:INST2FCOMI_REC) Ini(code1,code2:INT);
BEGIN
  o.code1:=code1;
  o.code2:=code2;
END Ini;

PROCEDURE (VAR o:INST1FM_REC) Ini(width,code,digit:INT;p9b:BOOLEAN);
BEGIN
  o.width:=width;
  o.code:=code;
  o.digit:=digit;
  o.p9b:=p9b;
END Ini;

PROCEDURE (VAR o:INST0F3_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST1FSTSW_REC) Ini(p9b:BOOLEAN);
BEGIN
  o.p9b:=p9b;
END Ini;

PROCEDURE (VAR o:INST2XRM_REC) Ini(code:INT);
BEGIN
  o.code:=code;
END Ini;

PROCEDURE (VAR o:INST2XRMI_REC) Ini(code1,code2,digit:INT);
BEGIN
  o.code1:=code1;
  o.code2:=code2;
  o.digit:=digit;
END Ini;

PROCEDURE (VAR o:INST2MOVD_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST2MOVQ_REC) Ini;
BEGIN
END Ini;

PROCEDURE (VAR o:INST0_REC) P;
VAR
  i:INT;
BEGIN
  FOR i:=0 TO o.len-1 DO
    GenByte(o.code[i]);
  END;
END P;

PROCEDURE ParseReg(VAR dwidth:INT;VAR reg:INT):BOOLEAN;
VAR
  i:INT;
  wasE:BOOLEAN;
  width:INT;
BEGIN
  IF Token#pcS.ident THEN
    RETURN FALSE;
  END;
  wasE:=CAP(pcS.name[0])='E';
  i:=ORD(wasE);
  IF pcS.name[i+2]#0C THEN
    RETURN FALSE;
  END;
  width:=16;
  CASE CAP(pcS.name[i]) OF
  | 'A':
        CASE CAP(pcS.name[i+1]) OF
        | 'X':reg:=0;
        | 'L':width:=8;reg:=0;
        | 'H':width:=8;reg:=4;
        ELSE
          RETURN FALSE;
        END;
  | 'C':
        CASE CAP(pcS.name[i+1]) OF
        | 'X':reg:=1;
        | 'L':width:=8;reg:=1;
        | 'H':width:=8;reg:=5;
        ELSE
          RETURN FALSE;
        END;
  | 'D':
        CASE CAP(pcS.name[i+1]) OF
        | 'X':reg:=2;
        | 'I':reg:=7;
        | 'L':width:=8;reg:=2;
        | 'H':width:=8;reg:=6;
        ELSE
          RETURN FALSE;
        END;
  | 'B':
        CASE CAP(pcS.name[i+1]) OF
        | 'X':reg:=3;
        | 'P':reg:=5;
        | 'L':width:=8;reg:=3;
        | 'H':width:=8;reg:=7;
        ELSE
          RETURN FALSE;
        END;
  | 'S':
        CASE CAP(pcS.name[i+1]) OF
        | 'P':reg:=4;
        | 'I':reg:=6;
        ELSE
          RETURN FALSE;
        END;
  ELSE
    RETURN FALSE;
  END;
  IF wasE THEN
    IF width=16 THEN
      width:=32;
    ELSE
      RETURN FALSE;
    END;
  END;
  dwidth:=width;
  GetToken;
  RETURN TRUE;
END ParseReg;

PROCEDURE ParseDReg(VAR reg:INT):BOOLEAN;
BEGIN
  IF Token#pcS.ident THEN
    RETURN FALSE;
  END;
  IF (CAP(pcS.name[0])='D') AND (CAP(pcS.name[1])='R') AND (pcS.name[3]=0C) THEN
    reg:=ORD(pcS.name[2])-ORD('0');
    IF (reg>=0) AND (reg<8) THEN
      GetToken;
      RETURN TRUE;
    END;
  END;
 RETURN FALSE;
END ParseDReg;  

PROCEDURE ParseCReg(VAR reg:INT):BOOLEAN;
BEGIN
  IF Token#pcS.ident THEN
    RETURN FALSE;
  END;
  IF (CAP(pcS.name[0])='C') AND (CAP(pcS.name[1])='R') AND (pcS.name[3]=0C) THEN
    reg:=ORD(pcS.name[2])-ORD('0');
    IF (reg=0) OR ((reg>1) AND (reg<5)) THEN
      GetToken;
      RETURN TRUE;
    END;
  END;
 RETURN FALSE;
END ParseCReg;  

PROCEDURE ParseSReg(VAR reg:INT):BOOLEAN;
BEGIN
  IF Token#pcS.ident THEN
    RETURN FALSE;
  END;
  IF (CAP(pcS.name[1])='S') AND (pcS.name[2]=0C) THEN
    CASE CAP(pcS.name[0]) OF
    | 'E': reg:=0;
    | 'C': reg:=1;
    | 'S': reg:=2;
    | 'D': reg:=3;
    | 'F': reg:=4;
    | 'G': reg:=5;
    ELSE
      RETURN FALSE;
    END;
    GetToken;
    RETURN TRUE;
  END;
  RETURN FALSE;
END ParseSReg;  
    
PROCEDURE ParseSRegO(VAR reg:INT):BOOLEAN;
VAR
  sreg:INT;
BEGIN
  IF NOT ParseSReg(sreg) THEN
    RETURN FALSE;
  END;
  IF Token=pcS.colon THEN
    GetToken;
    segovr:=segovrcode[sreg];
    deferovr:=TRUE;
    RETURN FALSE;
  END;
  reg:=sreg;
  RETURN TRUE;
END ParseSRegO;

PROCEDURE ParseMReg(VAR reg:INT):BOOLEAN;
BEGIN
  IF Token#pcS.ident THEN
    RETURN FALSE;
  END;
  IF (CAP(pcS.name[0])='M') AND (CAP(pcS.name[1])='M') AND (pcS.name[3]=0C) THEN
    reg:=ORD(pcS.name[2])-ORD('0');
    IF (reg>=0) AND (reg<8) THEN
      GetToken;
      RETURN TRUE;
    END;
  END;
 RETURN FALSE;
END ParseMReg;

PROCEDURE ParseNum(VAR num:INT):BOOLEAN;
BEGIN
  IF Token=pcS.val_integer THEN
    num:=SYSTEM.VAL(INT,pcS.val.get_cardinal());
    GetToken;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ParseNum;

PROCEDURE ParseFReg(VAR reg:INT):BOOLEAN;
BEGIN
  IF Token#pcS.ident THEN
    RETURN FALSE;
  END;
  IF (pcS.name#"ST") AND (pcS.name#"st") THEN
    RETURN FALSE;
  END;
  GetToken;
  IF Token#pcS.lpar THEN
    reg:=0;
    RETURN TRUE;
  END;
  GetToken;
  IF NOT ParseNum(reg) THEN
    ErrorNext(ER_REQNUM);
  END;
  IF (reg<0) OR (reg>7) THEN
    Error(ER_RANGE);
  END;
  IF Token#pcS.rpar THEN
    ErrorNext(ER_RPAR);
  END;
  GetToken;
  RETURN TRUE;
END ParseFReg;

PROCEDURE ParseComma;
BEGIN
  IF Token#pcS.comma THEN
    ErrorNext(ER_REQCOMMA);
  END;
  GetToken;
END ParseComma;

PROCEDURE ParseQI(VAR o:pcK.OBJECT):BOOLEAN;
VAR
  t:pcK.OBJECT;
BEGIN
  IF Token#pcS.ident THEN
    RETURN FALSE;
  END;
  IF NOT pcO.try_vis(pcO.cur_scope,pcS.name,o) THEN
    RETURN FALSE;
  END;
  GetToken;
  LOOP
    t:=o;
    IF Token#pcS.period THEN
      EXIT;
    ELSIF t.mode=pcK.ob_module THEN
      GetToken;
      IF Token#pcS.ident THEN
        ErrorNext(ER_EXPID);
      ELSE
        IF NOT pcO.try_qua(t.type,pcS.name,o) THEN
          ErrorNext(ER_UNDEFID);
        END;
        GetToken;
      END;
    ELSE
      EXIT;
    END;
  END;
  IF o.mode=pcK.ob_module THEN
    Error(ER_ILLQID);
  END;
  MarkOb(o);
  RETURN TRUE;
END ParseQI;  

PROCEDURE ParseString(VAR val:pcK.VALUE;o:pcK.OBJECT):BOOLEAN;
BEGIN
  IF o#NIL THEN
    IF o.mode#pcK.ob_cons THEN
      RETURN FALSE;
    END;
    IF o.type.mode#pcK.ty_SS THEN
      RETURN FALSE;
    END;
    val:=pcK.const.eval_const(o.val);
  ELSIF Token=pcS.val_string THEN
    val:=pcS.val;
    GetToken;
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END ParseString;

PROCEDURE ParseFloat(VAR num:LONGREAL;VAR long:BOOLEAN;o:pcK.OBJECT):BOOLEAN;
VAR
  t:LONGREAL;
  v:pcK.VALUE;
BEGIN
  IF o#NIL THEN
    IF o.mode#pcK.ob_cons THEN
      RETURN FALSE;
    END;
    IF NOT (o.type.mode IN pcK.REALs) THEN
      RETURN FALSE;
    END;
    v:=pcK.const.eval_const(o.val);
    num:=v.get_real();
  ELSIF (Token=pcS.val_real) OR (Token=pcS.val_long_real) THEN
    num:=pcS.val.get_real();
    GetToken;
  ELSE
    RETURN FALSE;
  END;
  t:=ABS(num);
  long:=(t>0.0) AND (t<LowReal.small) OR (t>LowReal.large);
  RETURN TRUE;
END ParseFloat;
  
PROCEDURE CheckConst(o:pcK.OBJECT;VAR num:INT):BOOLEAN;
VAR
  v:pcK.VALUE;
BEGIN
  IF o.mode#pcK.ob_cons THEN
    RETURN FALSE;
  END;
  IF NOT o.type.is_ordinal() THEN
    Error(ER_ILLCONSTTYPE);
  END;
  v:=pcK.const.eval_const(o.val);
  v.cast_ordinal(pcO.longcard);
  num:=v.get_cardinal();
  RETURN TRUE;
END CheckConst;  

PROCEDURE CheckVar(o:pcK.OBJECT):BOOLEAN;
BEGIN
  RETURN o.mode IN pcK.VARs;
END CheckVar;  

PROCEDURE GetField(t:pcK.STRUCT;VAR rv:DVAR);
BEGIN
  IF Token#pcS.ident THEN
    ErrorNext(ER_EXPID);
  END;
  NEW(rv);
  IF NOT pcO.try_qua(t,pcS.name,rv.var) THEN
    ErrorNext(ER_UNDEFID);
  END;
  GetToken;
END GetField;


PROCEDURE ParseExpr(addterm:BOOLEAN;VAR num:INT;VAR ri:DREFI;segp:BOOLEAN;tm:BOOLEAN;oconst:pcK.OBJECT):BOOLEAN;
CONST
  STSIZE=1000;
  OP_LPAR=0;
  OP_U=1;
  OP_B=2;
TYPE
  OREC=RECORD
         sym:pcS.Symbol;
         optype:INT;
         pri:INT;
       END;
  VREC=RECORD
         v:INT;
         r:BOOLEAN;
       END;
VAR
  first:BOOLEAN;
  unrec:BOOLEAN;
  tofin:BOOLEAN;
  nm:INT;
  vstack:ARRAY STSIZE OF VREC;
  ostack:ARRAY STSIZE OF OREC;
  ostackpos,vstackpos:INT;
  V1,V2,VR:INT;
  R1,R2,RR:BOOLEAN;
  OP:pcS.Symbol;
  IsB:BOOLEAN;
  reqid:BOOLEAN;
  doofs:BOOLEAN;
  dosize:BOOLEAN;
  wasovr:BOOLEAN;
  sreg:INT;
  pri:INT;
  O:CMD;
  L:LABL;
  o:pcK.OBJECT;
  rv:DVAR;
  rl:DLAB;
  tocont:BOOLEAN;
  type:pcK.STRUCT;

  PROCEDURE PushV(num:INT;r:BOOLEAN);
  BEGIN
    IF vstackpos>=STSIZE THEN
      Error(ER_EXPRSTACKOV);
    END;
    vstack[vstackpos].v:=num;
    vstack[vstackpos].r:=r;
    vstackpos:=vstackpos+1;
  END PushV;

  PROCEDURE PushO(num:INT;pri:INT);
  BEGIN
    IF ostackpos>=STSIZE THEN
      Error(ER_EXPRSTACKOV);
    END;
    ostack[ostackpos].optype:=num;
    ostack[ostackpos].sym:=Token;    
    ostack[ostackpos].pri:=pri;
    ostackpos:=ostackpos+1;
  END PushO;

  PROCEDURE PopV(VAR num:INT;VAR r:BOOLEAN);
  BEGIN
    vstackpos:=vstackpos-1;
    num:=vstack[vstackpos].v;
    r:=vstack[vstackpos].r;
  END PopV;
  
  PROCEDURE PopO(VAR sym:pcS.Symbol);
  BEGIN
    ostackpos:=ostackpos-1;
    sym:=ostack[ostackpos].sym;
  END PopO;
(*
  PROCEDURE TopO(VAR sym:pcS.Symbol);
  BEGIN
    sym:=ostack[ostackpos-1].sym;
  END TopO;
*)
  PROCEDURE DoOp(VAR VR:INT;VAR RR:BOOLEAN);
  BEGIN
    RR:=R1 OR R2;
    CASE OP OF
    | pcS.plus: VR:=V2+V1;
    | pcS.minus:IF R1 THEN
                  Error(ER_ILLVARUSE);
                END;
                VR:=V2-V1;
    | pcS.times:IF R1 OR R2 THEN
                  Error(ER_ILLVARUSE);
                END;
                VR:=V2*V1;
    | pcS.slash:IF R1 OR R2 THEN
                  Error(ER_ILLVARUSE);
                END;
                VR:=V2 DIV V1;
    END;
  END DoOp;
  
  PROCEDURE Calc(eof:BOOLEAN;pri:INT);
  BEGIN
    WHILE ostackpos#0 DO
      IF ostack[ostackpos-1].optype=OP_LPAR THEN
        IF eof THEN
          ErrorNext(ER_RPAR);
        END;
        IF Token=pcS.rpar THEN
          PopO(OP);
          RETURN;
        END;
      END;
      IF eof OR (Token=pcS.rpar) OR
--       (ostack[ostackpos-1].optype=OP_U) OR
         (ostack[ostackpos-1].optype=OP_B) AND (ostack[ostackpos-1].pri>=pri)
      THEN
        IF vstackpos=0 THEN
          Error(ER_EXSTACKEMPTY);
        END;
        PopV(V1,R1);
        IsB:=ostack[ostackpos-1].optype=OP_B;
        IF IsB THEN
          IF vstackpos=0 THEN
            Error(ER_EXSTACKEMPTY);
          END;
          PopV(V2,R2);
        ELSE
          V2:=0;
          R2:=FALSE;
        END;
        PopO(OP);
        DoOp(VR,RR);
        PushV(VR,RR);
      ELSE
        RETURN;
      END;
    END;
    IF NOT eof AND (Token=pcS.rpar) THEN
      ErrorNext(ER_RPARWOL);
    END;
  END Calc;
  
BEGIN
  ostackpos:=0;
  vstackpos:=0;
  first:=TRUE;
  unrec:=FALSE;
  tofin:=FALSE;
  o := NIL;

  IF tm AND NOT deferovr AND (oconst=NIL) THEN
    LOOP
      IF Token=pcS.ident THEN
        IF (pcS.name="DWORD") OR (pcS.name="dword") THEN
          ri.type:=pcO.longcard;
        ELSIF (pcS.name="WORD") OR (pcS.name="word") THEN
          ri.type:=pcO.cardinal;
        ELSIF (pcS.name="BYTE") OR (pcS.name="byte") THEN
          ri.type:=pcO.shortcard;
(*        ELSIF (pcS.name="FWORD") OR (pcS.name="fword") THEN
          dw:=48;*)
        ELSIF (pcS.name="QWORD") OR (pcS.name="qword") THEN
          ri.type:=pcO.longreal;
        ELSIF (pcS.name="TBYTE") OR (pcS.name="tbyte") THEN
          ri.type:=pcO.ld_real;
        ELSE
          EXIT;
        END;
        first:=FALSE;
        GetToken;
        IF (Token#pcS.ident) OR ((pcS.name#"PTR") AND (pcS.name#"ptr")) THEN
          ErrorNext(ER_PTR);
        END;
        GetToken;
      END;
      EXIT;
    END;
  END;

  LOOP
    LOOP
      tocont:=FALSE;
      wasovr:=deferovr;
      IF deferovr THEN
        first:=FALSE;
        unrec:=TRUE;
        deferovr:=FALSE;
      END;
      IF (oconst=NIL) AND segp AND NOT wasovr AND ParseSReg(sreg) THEN
        IF Token#pcS.colon THEN
          ErrorNext(ER_REQCOLON);
        END;
        GetToken;
        first:=FALSE;
        wasovr:=TRUE;
        segovr:=segovrcode[sreg];
      END;
      IF (oconst=NIL) AND ParseNum(nm) THEN
        first:=FALSE;
        unrec:=TRUE;
        PushV(nm,FALSE);
        EXIT;
      ELSE
        reqid:=FALSE;
        doofs:=FALSE;
        dosize:=FALSE;
        IF (oconst=NIL) AND NOT wasovr AND (Token=pcS.ident) THEN
          IF  (pcS.name="OFFSET") OR (pcS.name="offset") THEN
            reqid:=TRUE;
            doofs:=TRUE;
            GetToken;
          ELSIF (pcS.name="SIZE") OR (pcS.name="size") THEN
            reqid:=TRUE;
            dosize:=TRUE;
            GetToken;
          END;
        END;
        IF (oconst#NIL) OR ParseQI(o) THEN
          IF oconst#NIL THEN
            o:=oconst;
            oconst:=NIL;
          END;
          IF dosize THEN
            VR:=pcK.code.get_size(pcK.su_bytes,o.type);
            IF VR<=0 THEN
              Error(ER_UNKSIZE);
            END;
            first:=FALSE;
            unrec:=TRUE;
            PushV(VR,FALSE);
            EXIT;
          ELSIF (tm AND first OR doofs) AND (o.mode=pcK.ob_type) THEN
            first:=FALSE;
            IF doofs THEN
              IF Token=pcS.period THEN
                type:=o.type;
                WHILE Token=pcS.period DO
                  IF type.mode#pcK.ty_record THEN
                    Error(ER_REQREC);
                  END;
                  GetToken;
                  GetField(type,rv);
                  type:=rv.var.type;
                  ri.AddRef(rv,TRUE,NIL);
                END;
              ELSE
                ErrorNext(ER_REQPERIOD);
              END;
              unrec:=TRUE;
              PushV(0,TRUE);
              EXIT;
            ELSE
              ri.type:=o.type;
              IF (Token#pcS.ident) OR ((pcS.name#"PTR") AND (pcS.name#"ptr")) THEN
                ErrorNext(ER_PTR);
              END;
              GetToken;
              tocont:=TRUE;
            END;
          ELSIF CheckConst(o,nm) THEN
            first:=FALSE;
            unrec:=TRUE;
            IF reqid THEN
              Error(ER_CONSTDIS);
            END;
            PushV(nm,FALSE);
            EXIT;
          ELSE
            IF NOT CheckVar(o) THEN
              Error(ER_UNDEFID);
            END;
            first:=FALSE;
            unrec:=TRUE;
            type:=o.type;
            WHILE Token=pcS.period DO
              IF type.mode#pcK.ty_record THEN
                ErrorNext(ER_REQREC);
              END;
              GetToken;
              GetField(type,rv);
              type:=rv.var.type;
              ri.AddRef(rv,TRUE,NIL);
            END;
            NEW(rv);
            rv.var:=o;
            ri.AddRef(rv,doofs,type);
            PushV(0,TRUE);
            EXIT;
          END;
        ELSIF Token=pcS.ident THEN
          O:=Find(pcS.name);
          IF O=NIL THEN
            O:=FindU(pcS.name);
            IF (O#NIL) AND NOT (O IS LABL) THEN
              ErrorNext(ER_REQLABEL);
            END;
            NEW(L);
            Set(pcS.name,L);
            L.location:=NIL;
          ELSE
            WITH O:LABL DO
              L:=O;
            ELSE
              ErrorNext(ER_REQLABEL);
            END;
          END;
          GetToken;
          first:=FALSE;
          unrec:=TRUE;
          NEW(rl);
          rl.label:=L;
          rl.tpos:=lasttxtpos;
          ri.AddRef(rl,doofs,NIL);
          PushV(0,TRUE);
          EXIT;
        ELSIF reqid THEN
          ErrorNext(ER_EXPID);
        END;
      END;
      IF NOT tocont THEN
        IF Token=pcS.lpar THEN
          PushO(OP_LPAR,1);
        ELSIF Token=pcS.minus THEN
          PushO(OP_U,8);
        ELSIF NOT unrec THEN
          RETURN FALSE;
        ELSE
          Error(ER_ILLEXPR);
        END;
        first:=FALSE;
        unrec:=TRUE;
        GetToken;
      END;
    END;
    LOOP
      IF Token=pcS.rpar THEN
        Calc(FALSE,1);
        GetToken;
      ELSIF (Token=pcS.plus) OR (Token=pcS.minus) OR (Token=pcS.times) OR (Token=pcS.slash) THEN
        pri:=4+ORD((Token=pcS.times) OR (Token=pcS.slash));
        Calc(FALSE,pri);
        IF addterm AND (vstackpos=1) AND ((Token=pcS.plus) OR (Token=pcS.minus)) THEN
          tofin:=TRUE;
          EXIT;
        END;
        PushO(OP_B,pri);
        GetToken;
        EXIT;
      ELSE
        tofin:=TRUE;
        EXIT;
      END;
    END;
    IF tofin THEN
      EXIT;
    END;
  END;
  Calc(TRUE,0);
  num:=vstack[0].v;
  RETURN TRUE;
END ParseExpr;          

PROCEDURE ParseNumExprEx(addterm:BOOLEAN;VAR num:INT;VAR o:DREFI;ob:pcK.OBJECT):BOOLEAN;
VAR
  f:BOOLEAN;
BEGIN
  o.Init;
  f:=ParseExpr(addterm,num,o,FALSE,FALSE,ob);
  IF o.lv THEN
    ErrorNext(ER_EXPCONST);
  END;
  RETURN f;
END ParseNumExprEx;

PROCEDURE ParseNumExpr(addterm:BOOLEAN;VAR num:INT;VAR o:DREFI):BOOLEAN;
BEGIN
  RETURN ParseNumExprEx(addterm,num,o,NIL);
END ParseNumExpr;

PROCEDURE ExchangeRMs;
VAR
  RM:RM_REC;
BEGIN
  RM:=RM1;
  RM1:=RM2;
  RM2:=RM;
END ExchangeRMs;  

PROCEDURE InitRM(VAR RM:RM_REC);
BEGIN
  RM.awidth:=0;
  RM.dwidth:=0;
  RM.reg1:=-1;
  RM.reg2:=-1;
  RM.scale:=1;
  RM.ofs:=0;
  RM.simple:=FALSE;
  RM.imm:=FALSE;
  RM.drefs.Init;
  RM.begpos:=pcS.txtpos;
END InitRM;

PROCEDURE ParseGOpszao(VAR RM:RM_REC;canx:BOOLEAN;mmx:BOOLEAN;preo:pcK.OBJECT);
VAR
  num:INT;
  reg:INT;
  nreg:INT;
  waslbr:BOOLEAN;
  width:INT;
  scale:INT;
  wasOp:INT;
  wasn:BOOLEAN;
  wassegovr:BOOLEAN;
  wasnref:INT;
  rv:DVAR;
BEGIN
  InitRM(RM);
  IF NOT deferovr AND (preo=NIL) THEN
    IF mmx THEN
      IF ParseMReg(RM.reg1) THEN
        RM.dwidth:=64;
        RM.awidth:=32;
        RM.simple:=TRUE;
        RETURN;
      ELSIF ParseReg(RM.dwidth,RM.reg1) THEN
        ErrorNext(ER_MMXREG);
      END;
    ELSE
      IF ParseReg(RM.dwidth,RM.reg1) THEN
        RM.awidth:=32;
        RM.simple:=TRUE;
        RETURN;
      END;
    END;
  END;
  wassegovr:=segovr#0;
  wasn:=ParseExpr(FALSE,RM.ofs,RM.drefs,TRUE,TRUE,preo);
  
  nreg:=0;
  waslbr:=TRUE;
  IF (Token#pcS.lbr) AND (wassegovr OR (segovr=0)) AND NOT RM.drefs.lv THEN
    IF wasn AND (RM.drefs.type=NIL) THEN
      RM.imm:=TRUE;
      RETURN;
    END;
    ErrorPos(RM.begpos,ER_ILLARG);
  END;
  IF NOT canx THEN
    ErrorPos(RM.begpos,ER_ILLARG);
  END;
  IF Token=pcS.lbr THEN
    GetToken;
    LOOP
      wasnref:=RM.drefs.nref;
      IF Token=pcS.plus THEN
        wasOp:=1;
        GetToken;
      ELSIF Token=pcS.minus THEN
        wasOp:=-1;
        GetToken;
      ELSE
        wasOp:=0;
      END;
      
      IF ParseReg(width,reg) THEN
        IF wasOp=-1 THEN
          Error(ER_ILLAMODE);
        ELSIF (wasOp=1) AND waslbr THEN
          Error(ER_ILLAMODE);
        END;
        
        IF nreg=2 THEN
          Error(ER_ILLAMODE);
        END;
        
        IF width=8 THEN
          Error(ER_ILLAMODE);
        END;
        IF RM.awidth#width THEN
          IF RM.awidth=0 THEN
            RM.awidth:=width;
          ELSE
            Error(ER_ILLAMODE);
          END;
        END;
        IF nreg=0 THEN
          RM.reg1:=reg;
        ELSE
          IF (reg=4) AND (RM.reg1=4) THEN
            Error(ER_ILLAMODE);
          END;
          RM.reg2:=reg;
        END;
        IF Token=pcS.times THEN
          GetToken;
          IF NOT ParseNum(scale) THEN
            ErrorNext(ER_REQNUM);
          END;
          IF (scale#1) AND (scale#2) AND (scale#4) AND (scale#8) THEN
            Error(ER_ILLSCALE);
          END;
          IF scale#1 THEN
            IF RM.scale#1 THEN
              Error(ER_ILLAMODE);
            END;
            IF reg=4 THEN
              Error(ER_ILLAMODE);
            END;
            RM.scale:=scale;
            IF nreg#0 THEN
              RM.reg2:=RM.reg1;
              RM.reg1:=reg;
            END;
          END;
        END;
        nreg:=nreg+1;
      ELSIF ParseExpr(TRUE,num,RM.drefs,FALSE,FALSE,NIL) THEN
        IF (wasOp=-1) AND (RM.drefs.nref>wasnref) THEN
          Error(ER_ILLVARUSE);
        END;
        IF wasOp=-1 THEN
          RM.ofs:=RM.ofs-num;
        ELSE
          RM.ofs:=RM.ofs+num;
        END;
      ELSE
        ErrorNext(ER_SYNT);
      END;
      
      waslbr:=FALSE;
      IF Token=pcS.rbr THEN
        GetToken;
        IF Token=pcS.lbr THEN
          waslbr:=TRUE;
          GetToken;
        ELSE
          EXIT;
        END;
      END;
    END;
  ELSIF NOT wasn THEN
    ErrorPos(RM.begpos,ER_ILLARG);
  END;
  IF RM.drefs.lvloc THEN
--  RM.awidth:=32;
    IF nreg=0 THEN
      RM.reg1:=5;
    ELSIF nreg=1 THEN
      RM.reg2:=5;
    ELSE
      Error(ER_ILLAMODE);
    END;
--    nreg:=nreg+1;	-- avoiding W900: redundant code eliminated
  END;
  WHILE Token=pcS.period DO
    IF (RM.drefs.type=NIL) OR (RM.drefs.type.mode#pcK.ty_record) THEN
      ErrorNext(ER_REQREC);
    END;
    GetToken;
    GetField(RM.drefs.type,rv);
    RM.drefs.type:=rv.var.type;
    RM.drefs.AddRef(rv,TRUE,NIL);
  END;
  IF RM.awidth=0 THEN
    RM.awidth:=32;
  END;
  IF RM.drefs.type#NIL THEN
    RM.dwidth:=pcK.code.get_size(pcK.su_bits,RM.drefs.type);
  END;
END ParseGOpszao;

PROCEDURE ParseGOpsza(VAR RM:RM_REC;canx:BOOLEAN;mmx:BOOLEAN);
BEGIN
  ParseGOpszao(RM,canx,mmx,NIL);
END ParseGOpsza;

PROCEDURE ParseGOpsz(VAR RM:RM_REC;canx:BOOLEAN);
BEGIN
  ParseGOpsza(RM,canx,FALSE);
END ParseGOpsz;

PROCEDURE ParseGOp(VAR RM:RM_REC;canx:BOOLEAN);
BEGIN
  ParseGOpsz(RM,canx);
  IF (RM.dwidth#0) AND (RM.dwidth#8) AND (RM.dwidth#16) AND (RM.dwidth#32) THEN
      ErrorPos(RM.begpos,ER_WRONGOPSIZE);
  END;
END ParseGOp;

PROCEDURE ParseGMMX(VAR RM:RM_REC;canx:BOOLEAN);
BEGIN
  ParseGOpsza(RM,canx,TRUE);
  IF RM.dwidth=0 THEN
    RM.dwidth:=64;
  END;
  IF RM.dwidth#64 THEN
      ErrorPos(RM.begpos,ER_WRONGOPSIZE);
  END;
END ParseGMMX;

PROCEDURE ParseMMX(VAR RM:RM_REC);
BEGIN
  ParseGMMX(RM,TRUE);
  IF RM.imm THEN
    Error(ER_DISIMM);
  END;
END ParseMMX;

PROCEDURE ParseRM(VAR RM:RM_REC);
BEGIN
  ParseGOp(RM,TRUE);
  IF RM.imm THEN
    Error(ER_DISIMM);
  END;
END ParseRM;  

PROCEDURE ParseM(VAR RM:RM_REC);
BEGIN
  ParseRM(RM);
  IF RM.simple THEN
    Error(ER_ILLARG);
  END;
END ParseM;

PROCEDURE ParseRMsz(VAR RM:RM_REC);
BEGIN
  ParseGOpsz(RM,TRUE);
  IF RM.imm THEN
    Error(ER_DISIMM);
  END;
END ParseRMsz;

PROCEDURE ParseMsz(VAR RM:RM_REC);
BEGIN
  ParseRMsz(RM);
  IF RM.simple THEN
    Error(ER_ILLARG);
  END;
END ParseMsz;

PROCEDURE (VAR o:DB_REC) P;
VAR
  num:INT;
  io:DREFI;
  len:LONGINT;
  i:INT;
  lr:LONGREAL;
  long:BOOLEAN;
  val:pcK.VALUE;
  ob:pcK.OBJECT;
BEGIN
  LOOP
    IF NOT ParseQI(ob) THEN
      ob:=NIL;
    END;
    IF ParseString(val,ob) THEN
      IF o.size#1 THEN
        ErrorNext(ER_DBSIZE);
      END;
      zz_tmp.unary(pcK.su_length,val);
      len:=zz_tmp.get_integer();
      FOR i:=0 TO len-1 DO
        zz_tmp.index_get(i,val);
        GenByte(zz_tmp.get_integer());
      END;
    ELSIF ParseFloat(lr,long,ob) THEN
      IF (o.size<4) OR long AND (o.size<8) THEN
        ErrorNext(ER_DBSIZE);
      END;
      CASE o.size OF
      | 4: GenBuf(VAL(REAL,lr));
      | 8: GenBuf(lr);
      |10: GenBuf(VAL(LONGLONGREAL,lr));
      END;
    ELSE
      IF (ob=NIL) AND (Token=pcS.val_char) THEN
        num:=pcS.val.get_integer();
        io.Init;
      ELSIF NOT ParseNumExprEx(FALSE,num,io,ob) THEN
        Error(ER_REQNUM);
      END;
      IF o.size>4 THEN
        Error(ER_DBSIZE);
      END;
      GenImm(num,io,o.size*8);
    END;
    IF Token#pcS.comma THEN
      RETURN;
    END;
    GetToken;
  END;
END P;

PROCEDURE (VAR o:CSEG_REC) P;
BEGIN
  IF NOT isdseg THEN
    RETURN;
  END;
  curdcode:=curcode;
  curcode:=curccode;
  isdseg:=FALSE;
END P;

PROCEDURE (VAR o:DSEG_REC) P;
VAR
  cc:PCODDATA;
BEGIN
  IF isdseg THEN
    RETURN;
  END;
  curccode:=curcode;
  curcode:=curdcode;
  isdseg:=TRUE;
  IF curcode=NIL THEN
    NEW(cc);
    cc.len:=0;
    NEW(cc.data,DATASIZE);
    cc.isdseg:=isdseg;
    curcode:=cc;
    coded:=cc;
  END;
END P;

PROCEDURE Gen1g(VAR o:INST1G_REC);
VAR
  i:INT;
BEGIN
  IF NOT o.can32 THEN
    IF RM1.dwidth=0 THEN
      RM1.dwidth:=8;
    ELSIF RM1.dwidth#8 THEN
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
  END;
  IF RM1.dwidth=0 THEN
    ErrorPos(RM1.begpos,ER_UNKDSIZE);
  END;
  GenSPref(RM1.awidth,RM1.dwidth);
  i:=0;
  IF o.len>1 THEN
    GenByte(o.code[0]);
    i:=1;
  END;
  GenByte(ORD(o.code[i])+ORD(RM1.dwidth#8));
  GenRM(RM1,o.digit);
END Gen1g;    

PROCEDURE (VAR o:INST1NEG_REC) P;
BEGIN
  ParseRM(RM1);
  Gen1g(o);
END P;

PROCEDURE (VAR o:INST1INC_REC) P;
BEGIN
  ParseRM(RM1);
  IF (RM1.simple) AND (RM1.dwidth>8) THEN
    GenSPref(RM1.awidth,RM1.dwidth);
    GenByte(o.fast+RM1.reg1);
  ELSE
    Gen1g(o);
  END;
END P;  
  
PROCEDURE (VAR o:INST2ADD_REC) P;
VAR
  b:INT;
BEGIN
  ParseRM(RM1);
  ParseComma;
  ParseGOp(RM2,RM1.simple);
  IF RM2.imm THEN
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    IF RM1.simple AND (RM1.reg1=0) THEN
      IF RM1.dwidth=8 THEN
        GenByte(o.gbase+4);
      ELSE
        GenByte(o.gbase+5);
      END;
      GenImmRM(RM2,RM1.dwidth);
    ELSE
      IF RM1.dwidth=8 THEN
        GenByte(o.ibase);
        GenRM(RM1,o.digit);
        GenImmRM(RM2,8);
      ELSIF (RM2.drefs.nref=0) AND (RM2.ofs>=-128) AND (RM2.ofs<=127) THEN
        GenByte(o.ibase+3);
        GenRM(RM1,o.digit);
        GenImmRM(RM2,8);
      ELSE
        GenByte(o.ibase+1);
        GenRM(RM1,o.digit);
        GenImmRM(RM2,RM1.dwidth);
      END;
    END;
  ELSE
    IF RM1.simple THEN
      IF RM1.dwidth=8 THEN
        b:=o.gbase+2;
      ELSE
        b:=o.gbase+3;
      END;
      ExchangeRMs;
    ELSE
      IF RM2.dwidth=8 THEN
        b:=o.gbase;
      ELSE
        b:=o.gbase+1;
      END;
    END;      

    IF RM1.dwidth=0 THEN
      RM1.dwidth:=RM2.dwidth;
    ELSIF RM1.dwidth#RM2.dwidth THEN
      ErrorPos(RM2.begpos,ER_MIXDSIZE);
    END;

    GenSPref(RM1.awidth,RM1.dwidth);
    GenByte(b);
    GenRM(RM1,RM2.reg1);
  END;
END P;  

PROCEDURE (VAR o:INST2MOV_REC) P;
VAR
  reg,xreg:INT;
  dwidth:INT;
BEGIN
  IF ParseCReg(xreg) THEN
    ParseComma;
    IF NOT ParseReg(dwidth,reg) THEN
      ErrorNext(ER_REQGREG);
    END;
    IF dwidth#32 THEN
      Error(ER_REQGREG32);
    END;
    GenByte(0FH);
    GenByte(22H);
    GenByte(0C0H+reg+xreg*8);
    RETURN;
  END;
  IF ParseDReg(xreg) THEN
    ParseComma;
    IF NOT ParseReg(dwidth,reg) THEN
      ErrorNext(ER_REQGREG);
    END;
    IF dwidth#32 THEN
      Error(ER_REQGREG32);
    END;
    GenByte(0FH);
    GenByte(23H);
    GenByte(0C0H+reg+xreg*8);
    RETURN;
  END;
  IF ParseSRegO(xreg) THEN
    IF xreg=1 THEN
      Error(ER_DISCS);
    END;
    ParseComma;
    ParseRM(RM1);
    IF RM1.dwidth=0 THEN
      RM1.dwidth:=16;
  -- in PPro (may be in pent too) mov sreg,rm32 is allowed
(*    ELSIF RM1.dwidth#16 THEN
      Error(ER_REQDSIZE16);
*)
    END;
    GenSPref(RM1.awidth,32 (*RM1.dwidth*));
       -- must be mov es,ax w/o prefix 066h
    GenByte(8EH);
    GenRM(RM1,xreg);
    RETURN;
  END;
  -- because asm grammar here isn't LR but LR1
  -- and there are no ways to fix it
  -- the previous GenSPrefO eat up 'sreg:' but
  -- parseExpr doesn't work after it with defeovr==TRUE
  -- this patch allow you to eneter something like
  -- mov es: dword ptr[ebx],10
  deferovr:=FALSE;
  ParseRM(RM1);
  ParseComma;
  
  IF ParseSRegO(xreg) THEN
    IF RM1.dwidth=0 THEN
      RM1.dwidth:=16;
  -- in PPro (may be in pent to) mov rm32,sreg is allowed
(*    ELSIF RM1.dwidth#16 THEN
      ErrorPos(RM1.begpos,ER_REQDSIZE16); *)
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    GenByte(8CH);
    GenRM(RM1,xreg);
    RETURN;
  END;

  IF NOT deferovr AND RM1.simple THEN
    IF ParseDReg(xreg) THEN
      IF RM1.dwidth#32 THEN
        ErrorPos(RM1.begpos,ER_REQGREG32);
      END;
      GenByte(0FH);
      GenByte(21H);
      GenRM(RM1,xreg);
      RETURN;
    END;
    IF ParseCReg(xreg) THEN
      IF RM1.dwidth#32 THEN
        ErrorPos(RM1.begpos,ER_REQGREG32);
      END;
      GenByte(0FH);
      GenByte(20H);
      GenRM(RM1,xreg);
      RETURN;
    END;
  END;

  ParseGOp(RM2,RM1.simple);
  
  IF RM2.imm THEN
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    IF RM1.simple THEN
      IF RM1.dwidth=8 THEN
        GenByte(0B0H+RM1.reg1);
      ELSE
        GenByte(0B8H+RM1.reg1);
      END;
    ELSE
      GenByte(0C6H+ORD(RM1.dwidth#8));
      GenRM(RM1,0);
    END;
    GenImmRM(RM2,RM1.dwidth);
    RETURN;
  END;

  IF RM1.simple THEN
    IF RM2.dwidth=0 THEN
      RM2.dwidth:=RM1.dwidth;
    ELSIF RM2.dwidth#RM1.dwidth THEN
      ErrorPos(RM2.begpos,ER_MIXDSIZE);
    END;
    GenSPref(RM2.awidth,RM2.dwidth);
    IF (RM1.reg1=0) AND (RM2.reg1=-1) AND (RM2.reg2=-1) THEN
      GenByte(0A0H+ORD(RM1.dwidth#8));
      GenOfs(RM2);
    ELSE
      GenByte(8AH+ORD(RM2.dwidth#8));
      GenRM(RM2,RM1.reg1);
    END;
  ELSE
    IF RM1.dwidth=0 THEN
      RM1.dwidth:=RM2.dwidth;
    ELSIF RM1.dwidth#RM2.dwidth THEN
      ErrorPos(RM2.begpos,ER_MIXDSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    IF (RM2.reg1=0) AND (RM1.reg1=-1) AND (RM1.reg2=-1) THEN
      GenByte(0A2H+ORD(RM2.dwidth#8));
      GenOfs(RM1);
    ELSE
      GenByte(88H+ORD(RM1.dwidth#8));
      GenRM(RM1,RM2.reg1);
    END;
  END;
END P;

PROCEDURE (VAR o:INST1POP_REC) P;
VAR
  xreg:INT;
BEGIN
  IF ParseSRegO(xreg) THEN
    IF xreg=1 THEN
      Error(ER_DISCS);
    END;
    CASE xreg OF
    | 0: GenByte(7);
    | 2: GenByte(17H);
    | 3: GenByte(1FH);
    | 4: GenByte(0FH);GenByte(0A1H);
    | 5: GenByte(0FH);GenByte(0A9H);
    END;
  ELSE
    ParseRM(RM1);
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    ELSIF RM1.dwidth=8 THEN
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    IF RM1.simple THEN
      GenByte(58H+RM1.reg1);
    ELSE
      GenByte(8FH);
      GenRM(RM1,0);
    END;
  END;
END P;

PROCEDURE (VAR o:INST1PUSH_REC) P;
VAR
  xreg:INT;
BEGIN
  IF ParseSRegO(xreg) THEN
    CASE xreg OF
    | 0: GenByte(6);
    | 1: GenByte(0EH);
    | 2: GenByte(16H);
    | 3: GenByte(1EH);
    | 4: GenByte(0FH);GenByte(0A0H);
    | 5: GenByte(0FH);GenByte(0A8H);
    END;
  ELSE
    ParseGOp(RM1,TRUE);
    IF RM1.imm THEN
      IF (RM1.drefs.nref=0) AND (RM1.ofs>=-128) AND (RM1.ofs<=127) THEN
        GenByte(6AH);
        GenImmRM(RM1,8);
      ELSE
        GenByte(68H);
        GenImmRM(RM1,32);
      END;
    ELSE  
      IF RM1.dwidth=0 THEN
        ErrorPos(RM1.begpos,ER_UNKDSIZE);
      ELSIF RM1.dwidth=8 THEN
        ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
      END;
      GenSPref(RM1.awidth,RM1.dwidth);
      IF RM1.simple THEN
        GenByte(50H+RM1.reg1);
      ELSE
        GenByte(0FFH);
        GenRM(RM1,6);
      END;
    END;
  END;
END P;

PROCEDURE (VAR o:INST2SHIFT_REC) P;
VAR
  num,dwidth,reg:INT;
  io:DREFI;
BEGIN
  ParseRM(RM1);
  IF RM1.dwidth=0 THEN
    ErrorPos(RM1.begpos,ER_UNKDSIZE);
  END;
  GenSPref(RM1.awidth,RM1.dwidth);
  ParseComma;
  IF ParseReg(dwidth,reg) THEN
    IF (dwidth#8) OR (reg#1) THEN
      Error(ER_ONLYCL);
    END;
    GenByte(0D2H+ORD(RM1.dwidth#8));
    GenRM(RM1,o.digit);
  ELSIF ParseNumExpr(FALSE,num,io) THEN
    IF (io.nref=0) AND (num=1) THEN
      GenByte(0D0H+ORD(RM1.dwidth#8));
      GenRM(RM1,o.digit);
    ELSE
      GenByte(0C0H+ORD(RM1.dwidth#8));
      GenRM(RM1,o.digit);
      GenImm(num,io,8);
    END;
  ELSE
    ErrorNext(ER_ILLARG);
  END;
END P;  

PROCEDURE (VAR o:INST2LEA_REC) P;
VAR
  r:INT;
  width:INT;
BEGIN
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width=8 THEN
    Error(ER_DISR8);
  END;
  ParseComma;
  ParseMsz(RM1);
  GenSPref(RM1.awidth,width);
  GenByte(8DH);
  GenRM(RM1,r);
END P;  

PROCEDURE (VAR o:INST2TEST_REC) P;
VAR
  r:INT;
  dwidth:INT;
  num:INT;
  io:DREFI;
BEGIN
  ParseRM(RM1);
  ParseComma;

  IF ParseReg(dwidth,r) THEN
    IF RM1.dwidth=0 THEN
      RM1.dwidth:=dwidth;
    ELSIF RM1.dwidth#dwidth THEN
      Error(ER_MIXDSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    IF dwidth=8 THEN
      GenByte(84H);
    ELSE
      GenByte(85H);
    END;
    GenRM(RM1,r);
  ELSIF ParseNumExpr(FALSE,num,io) THEN
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    IF RM1.simple AND (RM1.reg1=0) THEN
      IF RM1.dwidth=8 THEN
        GenByte(0A8H);
      ELSE
        GenByte(0A9H);
      END;
      GenImm(num,io,RM1.dwidth);
    ELSE
      IF RM1.dwidth=8 THEN
        GenByte(0F6H);
        GenRM(RM1,0);
        GenImm(num,io,8);
      ELSE
        GenByte(0F7H);
        GenRM(RM1,0);
        GenImm(num,io,RM1.dwidth);
      END;
    END;
  ELSE
    ErrorNext(ER_REQGREG);
  END;
END P;  

PROCEDURE (VAR o:INST2XCHG_REC) P;
VAR
  r:INT;
  dwidth:INT;
  tpos:pcK.TPOS;
BEGIN
  ParseRM(RM1);
  ParseComma;
  tpos:=pcS.txtpos;
  IF RM1.simple THEN
    r:=RM1.reg1;
    dwidth:=RM1.dwidth;
    ParseRM(RM1);
  ELSE
    IF NOT ParseReg(dwidth,r) THEN
      ErrorNext(ER_REQGREG);
    END;
  END;
  IF (RM1.simple) AND (RM1.reg1=0) THEN
    RM1.reg1:=r;
    r:=0;
  END;
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=dwidth;
  ELSIF RM1.dwidth#dwidth THEN
    ErrorPos(tpos,ER_MIXDSIZE);
  END;
  GenSPref(RM1.awidth,RM1.dwidth);
  IF RM1.simple AND (dwidth#8) AND (r=0) THEN
    GenByte(90H+RM1.reg1);
  ELSE
    GenByte(86H+ORD(dwidth#8));
    GenRM(RM1,r);
  END;
END P;  
      
PROCEDURE (VAR o:INST1Jcc_REC) P;
VAR
  O:CMD;
  L:LABL;
  t:PCODJLABL;
BEGIN
  IF Token#pcS.ident THEN
    ErrorNext(ER_REQLABEL);
  END;
  O:=Find(pcS.name);
  IF O=NIL THEN
    O:=FindU(pcS.name);
    IF (O#NIL) AND NOT (O IS LABL) THEN
      ErrorNext(ER_REQLABEL);
    END;
    NEW(L);
    Set(pcS.name,L);
    L.location:=NIL;
  ELSE
    WITH O:LABL DO
      L:=O;
    ELSE
      ErrorNext(ER_REQLABEL);
    END;
  END;
  GetToken;

  NEW(t);
  t.isdseg:=isdseg;
  t.next:=NIL;
  curcode.next:=t;
  curcode:=t;
  t.code:=o.near;
  t.canfar:=o.canfar;
  t.j32:=FALSE;
  t.label:=L;
  t.location:=NIL;
  t.tpos:=lasttxtpos;
  NewDataArr;   
END P;  

PROCEDURE (VAR o:INST1JCXZ_REC) P;
BEGIN
  GenByte(67H);
  o.P^;
END P;  

PROCEDURE (VAR o:INST2CMOVcc_REC) P;
VAR
  r:INT;
  width:INT;
BEGIN
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width=8 THEN
    Error(ER_DISR8);
  END;
  ParseComma;
  ParseRM(RM1);
  IF (RM1.dwidth#0) AND (RM1.dwidth#width) THEN
    ErrorPos(RM1.begpos,ER_MIXDSIZE);
  END;
  GenSPref(RM1.awidth,width);
  GenByte(0FH);
  GenByte(o.code);
  GenRM(RM1,r);
END P;  

PROCEDURE (VAR o:INST2BT_REC) P;
VAR
  num:INT;
  r:INT;
  width:INT;
  io:DREFI;
BEGIN
  ParseRM(RM1);
  IF RM1.dwidth=8 THEN
    Error(ER_DISD8);
  END;
  ParseComma;
  IF ParseReg(width,r) THEN
    IF width=8 THEN
      Error(ER_DISR8);
    END;
    IF (RM1.dwidth#0) AND (RM1.dwidth#width) THEN
      Error(ER_MIXDSIZE);
    END;
    GenSPref(RM1.awidth,width);
    GenByte(0FH);
    GenByte(o.code);
    GenRM(RM1,r);
  ELSIF ParseNumExpr(FALSE,num,io) THEN
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    GenByte(0FH);
    GenByte(0BAH);
    GenRM(RM1,o.digit);
    GenImm(num,io,8);
  ELSE
    ErrorNext(ER_ILLARG);
  END;
END P;  

PROCEDURE (VAR o:INST1BSWAP_REC) P;
VAR
  width:INT;
  r:INT;
BEGIN
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width#32 THEN
    Error(ER_REQGREG32);
  END;
  GenByte(0FH);
  GenByte(0C8H+r);
END P;

PROCEDURE (VAR o:INST1RET_REC) P;
VAR
  num:INT;
  io:DREFI;
BEGIN
  IF ParseNumExpr(FALSE,num,io) THEN
    GenByte(o.code-1);
    GenImm(num,io,16);
  ELSE
    GenByte(o.code);
  END;
END P;  

PROCEDURE (VAR o:INSTxIMUL_REC) P;
VAR
  num:INT;
  io:DREFI;
BEGIN
  ParseRM(RM1);
  IF Token#pcS.comma THEN
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,RM1.dwidth);
    GenByte(0F7H-ORD(RM1.dwidth=8));
    GenRM(RM1,5);
    RETURN;
  END;
  GetToken;
  IF NOT RM1.simple THEN
    ErrorPos(RM1.begpos,ER_REQGREG);
  END;
  IF RM1.dwidth=8 THEN
    ErrorPos(RM1.begpos,ER_DISD8);
  END;
  ParseGOp(RM2,TRUE);
  IF Token=pcS.comma THEN
    GetToken;
    IF RM2.imm THEN
      ErrorPos(RM2.begpos,ER_DISIMM);
    END;
    IF NOT ParseNumExpr(FALSE,num,io) THEN
      Error(ER_REQIMM);
    END;
    IF (RM2.dwidth#0) AND (RM1.dwidth#RM2.dwidth) THEN
      ErrorPos(RM2.begpos,ER_MIXDSIZE);
    END;
  ELSE
    IF RM2.imm THEN
      num:=RM2.ofs;
      io:=RM2.drefs;
      RM2:=RM1;
    ELSE
      IF (RM2.dwidth#0) AND (RM1.dwidth#RM2.dwidth) THEN
        ErrorPos(RM2.begpos,ER_MIXDSIZE);
      END;
      GenSPref(RM2.awidth,RM1.dwidth);
      GenByte(0FH);
      GenByte(0AFH);
      GenRM(RM2,RM1.reg1);
      RETURN;
    END;
  END;
  GenSPref(RM2.awidth,RM1.dwidth);
  IF (io.nref=0) AND (num>=-128) AND (num<=127) THEN
    GenByte(6BH);
    GenRM(RM2,RM1.reg1);
    GenByte(num);
  ELSE
    GenByte(69H);
    GenRM(RM2,RM1.reg1);
    GenImm(num,io,RM1.dwidth);
  END;
END P;      
    
PROCEDURE (VAR o:INST1JMP_REC) P;
VAR
  ob:pcK.OBJECT;
  t:PCODFX;
  tt:PCODJMP;
  dob:DVAR;
BEGIN

  IF NOT ParseQI(ob) THEN
    ob:=NIL;
  END;
  IF (ob#NIL) AND (ob.mode IN pcK.PROCs) THEN
    GenByte(o.relcode);
    NEW(t);
    t.isdseg:=isdseg;
    t.next:=NIL;
    curcode.next:=t;
    curcode:=t;
    NEW(dob);
    dob.var:=ob;
    t.drefs.Init;
    t.drefs.AddRef(dob,FALSE,NIL);
    t.ofs:=0;
    t.kind:=cmd.fx_relcall;
    NewDataArr;
    RETURN;
  END;
  ParseGOpszao(RM1,TRUE,FALSE,ob);
  IF RM1.imm THEN
    IF (RM1.drefs.nref=1) AND (RM1.drefs.refs[0] IS DLAB) AND (RM1.ofs=0) THEN
      NEW(tt);
      tt.isdseg:=isdseg;
      tt.next:=NIL;
      curcode.next:=tt;
      curcode:=tt;
      tt.label:=RM1.drefs.refs[0](DLAB).label;
      tt.location:=NIL;
      tt.tpos:=lasttxtpos;
      IF o.shortcode#0 THEN
        tt.code:=o.shortcode;
        tt.j32:=FALSE;
        tt.code32:=o.relcode;
      ELSE
        tt.code:=o.relcode;
        tt.j32:=TRUE;
      END;
      NewDataArr;
      RETURN;
    ELSE
      ErrorPos(RM1.begpos,ER_REQLABEL);
    END;
  END;
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=32;
  ELSIF RM1.dwidth#32 THEN
    Error(ER_WRONGOPSIZE);
  END;
  GenSPref(RM1.awidth,32);
  GenByte(0FFH);
  GenRM(RM1,o.digit);
END P;

PROCEDURE (VAR o:INST1LGDT_REC) P;
BEGIN
  ParseRMsz(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=48;
  ELSIF RM1.dwidth#48 THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  GenSPref(RM1.awidth,32);
  GenByte(0FH);
  GenByte(1);
  GenRM(RM1,o.digit);
END P;

PROCEDURE (VAR o:INST1LLDT_REC) P;
BEGIN
  ParseRM(RM1);
  IF RM1.dwidth=8 THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  IF RM1.dwidth=0 THEN
    IF o.can32 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    ELSE
      RM1.dwidth:=16;
    END;
  ELSIF (RM1.dwidth=32) AND NOT o.can32 THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  IF o.can32 THEN
    GenSPref(RM1.awidth,48 - RM1.dwidth);
        -- because wasm && tasm generate no opsize prefix
        -- for 32-bit operands in 32-bit code in such cases
        -- for example, smsw ax - no opsiz prefix
  ELSE
    GenSPref(RM1.awidth,32);
  END;
  GenByte(0FH);
  GenByte(o.code);
  GenRM(RM1,o.digit);
END P;

PROCEDURE (VAR o:INST1INT_REC) P;
VAR
  num:INT;
  io:DREFI;
BEGIN
  IF NOT ParseNumExpr(FALSE,num,io) THEN
    Error(ER_REQNUM);
  END;
  GenByte(0CDH);
  GenImm(num,io,8);
END P;

PROCEDURE (VAR o:INST2LSL_REC) P;
VAR
  r:INT;
  width:INT;
BEGIN
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width=8 THEN
    Error(ER_DISR8);
  END;
  ParseComma;
  ParseRM(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=width;
  ELSIF RM1.dwidth#width THEN
    ErrorPos(RM1.begpos,ER_MIXDSIZE);
  END;
  GenSPref(RM1.awidth,RM1.dwidth);
  GenByte(0FH);
  GenByte(o.code);
  GenRM(RM1,r);
END P;

PROCEDURE (VAR o:INST2MOVSX_REC) P;
VAR
  r:INT;
  width:INT;
BEGIN
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width=8 THEN
    Error(ER_DISR8);
  END;
  ParseComma;
  ParseRM(RM1);
  IF width=32 THEN
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    ELSIF RM1.dwidth=32 THEN
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
  ELSE
    IF RM1.dwidth=0 THEN
      RM1.dwidth:=8;
    ELSIF RM1.dwidth#8 THEN
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
  END;
  GenSPref(RM1.awidth,width);
  GenByte(0FH);
  GenByte(o.code-ORD(RM1.dwidth=8));
  GenRM(RM1,r);
END P;

PROCEDURE (VAR o:INST3SHLD_REC) P;
VAR
  r,r1:INT;
  width,width1:INT;
  num:INT;
  io:DREFI;
BEGIN
  ParseRM(RM1);
  IF RM1.dwidth=8 THEN
    ErrorPos(RM1.begpos,ER_DISD8);
  END;
  ParseComma;
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width=8 THEN
    Error(ER_DISR8);
  END;
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=width;
  ELSIF RM1.dwidth#width THEN
    Error(ER_MIXDSIZE);
  END;
  ParseComma;
  GenSPref(RM1.awidth,RM1.dwidth);
  GenByte(0FH);
  IF ParseReg(width1,r1) THEN
    IF (width1#8) OR (r1#1) THEN
      Error(ER_ONLYCL);
    END;
    GenByte(o.code+1);
    GenRM(RM1,r);
  ELSE
    GenByte(o.code);
    GenRM(RM1,r);
    IF NOT ParseNumExpr(FALSE,num,io) THEN
      Error(ER_REQNUM);
    END;
    GenImm(num,io,8);
  END;
END P;

PROCEDURE (VAR o:INST2ENTER_REC) P;
VAR
  num1,num2:INT;
  io1,io2:DREFI;
BEGIN
  IF NOT ParseNumExpr(FALSE,num1,io1) THEN
    Error(ER_REQNUM);
  END;
  ParseComma;
  IF NOT ParseNumExpr(FALSE,num2,io2) THEN
    Error(ER_REQNUM);
  END;
  GenByte(0C8H);
  GenImm(num1,io1,16);
  GenImm(num2,io2,8);
END P;

PROCEDURE (VAR o:INST2XADD_REC) P;
VAR
  r:INT;
  width:INT;
BEGIN
  ParseRM(RM1);
  ParseComma;
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=width;
  ELSIF RM1.dwidth#width THEN
    Error(ER_MIXDSIZE);
  END;
  GenSPref(RM1.awidth,RM1.dwidth);
  GenByte(0FH);
  GenByte(o.code-ORD(width=8));
  GenRM(RM1,r);
END P;

PROCEDURE (VAR o:INST2ARPL_REC) P;
VAR
  r:INT;
  width:INT;
BEGIN
  ParseRM(RM1);
  IF (RM1.dwidth#0) AND (RM1.dwidth#16) THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  ParseComma;
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=width;
  END;
  IF width#16 THEN
    Error(ER_WRONGOPSIZE);
  END;
  GenSPref(RM1.awidth,32);
  GenByte(63H);
  GenRM(RM1,r);
END P;

PROCEDURE (VAR o:INST2BOUND_REC) P;
VAR
  r:INT;
  width:INT;
BEGIN
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width=8 THEN
    Error(ER_DISR8);
  END;
  ParseComma;
  ParseM(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=width;
  ELSIF RM1.dwidth#width THEN
    ErrorPos(RM1.begpos,ER_MIXDSIZE);
  END;
  GenSPref(RM1.awidth,RM1.dwidth);
  GenByte(62H);
  GenRM(RM1,r);
END P;

PROCEDURE (VAR o:INST1CMPXCHG8B_REC) P;
BEGIN
  ParseRMsz(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=64;
  ELSIF RM1.dwidth#64 THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  GenSPref(RM1.awidth,32);
  GenByte(0FH);
  GenByte(0C7H);
  GenRM(RM1,1);
END P;

PROCEDURE (VAR o:INST1INVLPG_REC) P;
BEGIN
  ParseM(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=32;
  ELSIF RM1.dwidth#32 THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  GenSPref(RM1.awidth,32);
  GenByte(0FH);
  GenByte(1);
  GenRM(RM1,7);
END P;

PROCEDURE (VAR o:INST2LDS_REC) P;
VAR
  width:INT;
  r:INT;
BEGIN
  IF NOT ParseReg(width,r) THEN
    ErrorNext(ER_REQGREG);
  END;
  IF width=8 THEN
    Error(ER_DISR8);
  END;
  ParseComma;
  ParseM(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=width;
  ELSIF RM1.dwidth#width THEN
    ErrorPos(RM1.begpos,ER_MIXDSIZE);
  END;
  GenSPref(RM1.awidth,RM1.dwidth);
  IF o.is0f THEN
    GenByte(0FH);
  END;
  GenByte(o.code);
  GenRM(RM1,r);
END P;

PROCEDURE (VAR o:INST2IN_REC) P;
VAR
  width:INT;
  r:INT;
  tpos:pcK.TPOS;
BEGIN
  tpos:=pcS.txtpos;
  IF NOT ParseReg(width,r) OR (r#0) THEN
    ErrorPos(tpos,ER_ONLYAL);
  END;
  ParseComma;
  ParseGOpsz(RM1,TRUE);
  IF RM1.simple THEN
    IF (RM1.reg1#2) OR (RM1.dwidth#16) THEN
      ErrorPos(RM1.begpos,ER_ONLYDX);
    END;
    GenSPref(32,width);
    GenByte(0ECH+ORD(width#8));
  ELSE
    IF (RM1.reg1#-1) OR (RM1.reg2#-1) OR (RM1.drefs.nref#0) THEN
      ErrorPos(RM1.begpos,ER_ILLARG);
    END;
    GenSPref(32,width);
    GenByte(0E4H+ORD(width#8));
    GenImmRM(RM1,8);
  END;
END P;

PROCEDURE (VAR o:INST2OUT_REC) P;
VAR
  width:INT;
  r:INT;
  tpos:pcK.TPOS;
BEGIN
  ParseGOpsz(RM1,TRUE);
  IF RM1.simple THEN
    IF (RM1.reg1#2) OR (RM1.dwidth#16) THEN
      ErrorPos(RM1.begpos,ER_ONLYDX);
    END;
  ELSE
    IF (RM1.reg1#-1) OR (RM1.reg2#-1) OR (RM1.drefs.nref#0) THEN
      ErrorPos(RM1.begpos,ER_ILLARG);
    END;
  END;
  ParseComma;
  tpos:=pcS.txtpos;
  IF NOT ParseReg(width,r) OR (r#0) THEN
    ErrorPos(tpos,ER_ONLYAL);
  END;
  GenSPref(32,width);
  IF RM1.simple THEN
    GenByte(0EEH+ORD(width#8));
  ELSE
    GenByte(0E6H+ORD(width#8));
    GenImmRM(RM1,8);
  END;
END P;

PROCEDURE (VAR o:INST2F_REC) P;
VAR
  freg1,freg2:INT;
  tpos:pcK.TPOS;
BEGIN
  IF ParseFReg(freg1) THEN
    ParseComma;
    tpos:=pcS.txtpos;
    IF NOT ParseFReg(freg2) THEN
      ErrorNext(ER_FREG);
    END;
    IF freg1=0 THEN
      GenByte(0D8H);
      GenByte(o.code+freg2);
    ELSE
      IF freg2#0 THEN
        ErrorPos(tpos,ER_FREG0);
      END;
      GenByte(0DCH);
      GenByte(o.coder+freg1);
    END;
  ELSE
    ParseMsz(RM1);
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,32);
    IF RM1.dwidth=64 THEN
      GenByte(0DCH);
    ELSIF RM1.dwidth=32 THEN
      GenByte(0D8H);
    ELSE
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
    GenRM(RM1,o.digit);
  END;
END P;

PROCEDURE (VAR o:INST2FP_REC) P;
VAR
  freg1,freg2:INT;
  tpos:pcK.TPOS;
BEGIN
  IF ParseFReg(freg1) THEN
    ParseComma;
    tpos:=pcS.txtpos;
    IF NOT ParseFReg(freg2) OR (freg2#0) THEN
      ErrorPos(tpos,ER_FREG0);
    END;
  ELSE
    freg1:=1;
  END;
  GenByte(0DEH);
  GenByte(o.code+freg1);
END P;

PROCEDURE (VAR o:INST1FI_REC) P;
BEGIN
  ParseM(RM1);
  IF RM1.dwidth=0 THEN
    ErrorPos(RM1.begpos,ER_UNKDSIZE);
  END;
  GenSPref(RM1.awidth,32);
  IF RM1.dwidth=32 THEN
    GenByte(0DAH);
  ELSIF RM1.dwidth=16 THEN
    GenByte(0DEH);
  ELSE
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  GenRM(RM1,o.digit);
END P;

PROCEDURE (VAR o:INST1FR_REC) P;
VAR
  freg:INT;
BEGIN
  IF NOT ParseFReg(freg) THEN
    IF o.req THEN
      ErrorNext(ER_FREG);
    END;
    freg:=1;
  END;
  GenByte(o.code1);
  GenByte(o.code2+freg);
END P;

PROCEDURE (VAR o:INST1FLD_REC) P;
VAR
  freg:INT;
BEGIN
  IF ParseFReg(freg) THEN
    GenByte(o.rcode1);
    GenByte(o.rcode2+freg);
  ELSE
    ParseMsz(RM1);
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,32);
    IF RM1.dwidth=32 THEN
      GenByte(0D9H);
      GenRM(RM1,o.digit);
    ELSIF RM1.dwidth=64 THEN
      GenByte(0DDH);
      GenRM(RM1,o.digit);
    ELSIF o.m80 AND (RM1.dwidth=80) THEN
      GenByte(0DBH);
      GenRM(RM1,o.digit80);
    ELSE
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
  END;
END P;

PROCEDURE (VAR o:INST1FILD_REC) P;
BEGIN
  ParseMsz(RM1);
  IF RM1.dwidth=0 THEN
    ErrorPos(RM1.begpos,ER_UNKDSIZE);
  END;
  GenSPref(RM1.awidth,32);
  IF RM1.dwidth=16 THEN
    GenByte(0DFH);
    GenRM(RM1,o.digit);
  ELSIF RM1.dwidth=32 THEN
    GenByte(0DBH);
    GenRM(RM1,o.digit);
  ELSIF o.m64 AND (RM1.dwidth=64) THEN
    GenByte(0DFH);
    GenRM(RM1,o.digit64);
  ELSE
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
END P;

PROCEDURE (VAR o:INST1FCOM_REC) P;
VAR
  freg:INT;
BEGIN
  IF ParseFReg(freg) THEN
    GenByte(0D8H);
    GenByte(o.code+freg);
  ELSIF Token=EOL THEN
    GenByte(0D8H);
    GenByte(o.code+1);
  ELSE
    ParseMsz(RM1);
    IF RM1.dwidth=0 THEN
      ErrorPos(RM1.begpos,ER_UNKDSIZE);
    END;
    GenSPref(RM1.awidth,32);
    IF RM1.dwidth=32 THEN
      GenByte(0D8H);
    ELSIF RM1.dwidth=64 THEN
      GenByte(0DCH);
    ELSE
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
    GenRM(RM1,o.digit);
  END;
END P;

PROCEDURE (VAR o:INST2FCOMI_REC) P;
VAR
  freg:INT;
  tpos:pcK.TPOS;
BEGIN
  tpos:=pcS.txtpos;
  IF NOT ParseFReg(freg) OR (freg#0) THEN
    ErrorPos(tpos,ER_FREG0);
  END;
  ParseComma;
  IF NOT ParseFReg(freg) THEN
    ErrorNext(ER_FREG);
  END;
  GenByte(o.code1);
  GenByte(o.code2+freg);
END P;

PROCEDURE (VAR o:INST1FM_REC) P;
BEGIN
  ParseMsz(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=o.width;
  END;
  IF (o.width#0) AND (RM1.dwidth#o.width) THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  IF o.p9b THEN
    GenByte(9BH);
  END;
  GenSPref(RM1.awidth,32);
  GenByte(o.code);
  GenRM(RM1,o.digit);
END P;

PROCEDURE (VAR o:INST0F3_REC) P;
BEGIN
  GenByte(9BH);
  GenByte(0DBH);
  GenByte(o.code);
END P;

PROCEDURE (VAR o:INST1FSTSW_REC) P;
BEGIN
  ParseRM(RM1);
  IF RM1.dwidth=0 THEN
    RM1.dwidth:=16;
  END;
  IF RM1.dwidth#16 THEN
    ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
  END;
  IF o.p9b THEN
    GenByte(9BH);
  END;
  IF RM1.simple THEN
    IF RM1.reg1#0 THEN
      ErrorPos(RM1.begpos,ER_ONLYAX);
    END;
    GenByte(0DFH);
    GenByte(0E0H);
  ELSE
    GenSPref(RM1.awidth,32);
    GenByte(0DDH);
    GenRM(RM1,7);
  END;
END P;

PROCEDURE (VAR o:INST2XRM_REC) P;
VAR
  mmx:INT;
BEGIN
  IF NOT ParseMReg(mmx) THEN
    ErrorNext(ER_MMXREG);
  END;
  ParseComma;
  ParseMMX(RM1);
  GenSPref(RM1.awidth,32);
  GenByte(0FH);
  GenByte(o.code);
  GenRM(RM1,mmx);
END P;

PROCEDURE (VAR o:INST2XRMI_REC) P;
VAR
  mmx:INT;
BEGIN
  IF NOT ParseMReg(mmx) THEN
    ErrorNext(ER_MMXREG);
  END;
  ParseComma;
  ParseGMMX(RM1,TRUE);
  GenSPref(RM1.awidth,32);
  GenByte(0FH);
  IF RM1.imm THEN
    GenByte(o.code2);
    GenByte(0C0H+mmx+o.digit*8);
    GenImmRM(RM1,8);
  ELSE
    GenByte(o.code1);
    GenRM(RM1,mmx);
  END;
END P;

PROCEDURE (VAR o:INST2MOVQ_REC) P;
VAR
  mmx:INT;
BEGIN
  ParseMMX(RM1);
  ParseComma;
  IF RM1.simple THEN
    ParseMMX(RM2);
    GenSPref(RM2.awidth,32);
    GenByte(0FH);
    GenByte(6FH);
    GenRM(RM2,RM1.reg1);
  ELSE
    IF NOT ParseMReg(mmx) THEN
      ErrorNext(ER_MMXREG);
    END;
    GenSPref(RM1.awidth,32);
    GenByte(0FH);
    GenByte(7FH);
    GenRM(RM1,mmx);
  END;
END P;

PROCEDURE (VAR o:INST2MOVD_REC) P;
VAR
  mmx:INT;
BEGIN
  IF ParseMReg(mmx) THEN
    ParseComma;
    ParseRM(RM1);
    IF (RM1.dwidth#0) AND (RM1.dwidth#32) THEN
      ErrorPos(RM1.begpos,ER_WRONGOPSIZE);
    END;
    GenSPref(RM1.awidth,32);
    GenByte(0FH);
    GenByte(6EH);
    GenRM(RM1,mmx);
  ELSE
    ParseRM(RM1);
    ParseComma;
    IF NOT ParseMReg(mmx) THEN
      ErrorNext(ER_MMXREG);
    END;
    GenSPref(RM1.awidth,32);
    GenByte(0FH);
    GenByte(7EH);
    GenRM(RM1,mmx);
  END;
END P;

PROCEDURE Asm*(mark_object:MarkObject):pcK.VALUE;
VAR
  fnm:env.String;
  col:LONGINT;
  O:CMD;
  V:COD_VALUE;
  L:LABL;
  cc:PCODDATA;
  pcd:PCOD;
  r:DREF;
  ireq:BOOLEAN;
  i:INT;
BEGIN
  zz_tmp:=pcK.value.new(env.null_pos,pcK.ZZ_type);
  MarkOb:=mark_object;
  NEW(V);
  V.prepared:=FALSE;
  V.expr:=NIL;
  V.pos:=pcS.txtpos;
  V.code:=NIL;
  V.coded:=NIL;
  pcS.txtpos.unpack(fnm,line,col);

  NEW(cc);
  cc.isdseg:=isdseg;
  curcode:=cc;
  code:=cc;
  curdcode:=NIL;
  coded:=NIL;
  NEW(cc.data,DATASIZE);
  cc.len:=0;
  isdseg:=FALSE;
  ireq:=FALSE;
  EOLflag:=FALSE;
  LOOP
    GetToken;
    IF Token=pcS.end THEN
      EXIT;
    END;
    IF (Token=pcS.ident) OR (pcS.first_keyword<=Token) AND (Token<=pcS.last_keyword) THEN
      O:=FindU(pcS.name);
      IF O=NIL THEN
        IF ireq THEN
          ErrorNext(ER_ILLMNEM);
        END;
        NEW(L);
        Set(pcS.name,L);
        GetToken;
        IF Token#pcS.colon THEN
          Error(ER_ILLMNEM);
        END;
        NewDataArr;
        ireq:=TRUE;
        L.location:=curcode;
      ELSE
        WITH O:INST DO
          IF isdseg AND NOT (O IS DCODE) THEN
            ErrorPos(pcS.txtpos,ER_CSEGONLY);
          END;
          BeginInst;
          IF EOLflag AND NOT isdseg THEN
            IF curcode(PCODDATA).len#0 THEN
              NewDataArr;
            END;
            curcode.mpos:=Begpos;
            EOLflag:=FALSE;
          END;
          GetToken;
          O.P;
          EndInst;
          IF Token#EOL THEN
            ErrorNext(ER_EXTRA);
          END;
          ireq:=FALSE;
        | O:PREFIX DO
          IF EOLflag AND NOT isdseg THEN
            IF curcode(PCODDATA).len#0 THEN
              NewDataArr;
            END;
            curcode.mpos:=Begpos;
            EOLflag:=FALSE;
          END;
          GenByte(O.code);
          ireq:=TRUE;
        | O:LABL DO
          IF ireq THEN
            ErrorNext(ER_ILLMNEM);
          END;
          GetToken;
          IF Token#pcS.colon THEN
            Error(ER_ILLMNEM);
          END;
          IF O.location#NIL THEN
            Error(ER_REDEFLABEL);
          END;
          ireq:=TRUE;
          NewDataArr;
          O.location:=curcode;
        END;
      END;
    ELSIF Token#EOL THEN
      ErrorPos(pcS.txtpos,ER_SYNT);
    END;
  END;
  pcd:=code;
  WHILE pcd#NIL DO
    WITH pcd:PCODJLABL DO
      IF pcd.label.location=NIL THEN
        ErrorPos(pcd.tpos,ER_UNDEFLABEL);
      ELSE
        IF pcd.label.location.isdseg THEN
          ErrorPos(pcd.tpos,ER_NONCLABEL);
        END;
        pcd.location:=pcd.label.location;
        pcd.label:=NIL;
      END;
    | pcd:PCODJMP DO
      IF pcd.label.location=NIL THEN
        ErrorPos(pcd.tpos,ER_UNDEFLABEL);
      ELSE
        IF pcd.label.location.isdseg THEN
          ErrorPos(pcd.tpos,ER_NONCLABEL);
        END;
        pcd.location:=pcd.label.location;
        pcd.label:=NIL;
      END;
    | pcd:PCODFX DO
      FOR i:=0 TO pcd.drefs.nref-1 DO
        r:=pcd.drefs.refs[i];
        WITH r:DLAB DO
          IF r.label.location=NIL THEN
            ErrorPos(r.tpos,ER_UNDEFLABEL);
          END;
          IF NOT r.label.location.isdseg THEN
            ErrorPos(r.tpos,ER_NONDLABEL);
          END;
        ELSE
        END;
      END;
    ELSE
    END;
    pcd:=pcd.next;
  END;
  V.code:=code;
  V.coded:=coded;
--  CleanUpSym;
  RETURN V;
EXCEPT
  IF EX.IsCurrentSource(ExS) THEN
    WHILE Token#pcS.end DO
      GetToken;
    END;
    RETURN V;  
  END;
END Asm;

PROCEDURE CalcDist(s:PCOD;d:PCOD):INT;
VAR
  c,l:PCOD;
  dist:INT;
BEGIN
  IF d.index<=s.index THEN
    c:=d;
    l:=s;
  ELSE
    c:=s.next;
    l:=d;
  END;
  dist:=0;
  REPEAT
    dist:=dist+c.bnum;
    c:=c.next;
  UNTIL c=l;
  IF d.index<=s.index THEN
    dist:=dist+c.bnum;
    dist:=-dist;
  END;
  RETURN dist;
END CalcDist;

PROCEDURE PrepareJcc(V:COD_VALUE);
VAR
  changed:BOOLEAN;
  c:PCOD;
  d:INT;
BEGIN
  REPEAT
    changed:=FALSE;
    c:=V.code;
    WHILE c#NIL DO
      WITH c:PCODJLABL DO
        IF NOT c.j32 THEN
          d:=CalcDist(c,c.location);
          IF (d<-128) OR (d>127) THEN
            IF NOT c.canfar THEN
              ErrorPos(c.tpos,ER_TOOFAR);
            END;
            c.j32:=TRUE;
            c.bnum:=6;
            changed:=TRUE;
          END;
        END;
      | c:PCODJMP DO
        IF NOT c.j32 THEN
          d:=CalcDist(c,c.location);
          IF (d<-128) OR (d>127) THEN
            c.j32:=TRUE;
            c.bnum:=5;
            c.code:=c.code32;
            changed:=TRUE;
          END;
        END;
      ELSE
      END;
      c:=c.next;
    END;
  UNTIL NOT changed;
END PrepareJcc;

PROCEDURE CalcOffset(VAR drefs:DREFI):INT;
VAR
  i:INT;
  ofs:INT;
BEGIN
  ofs:=0;
  FOR i:=0 TO drefs.nref-1 DO
    IF IsOffset(drefs.refs[i]) THEN
      ofs:=GetOf(drefs.refs[i](DVAR).var)+ofs;
    END;
  END;
  RETURN ofs;
END CalcOffset;

PROCEDURE Prepare(V:COD_VALUE);
VAR
  c:PCOD;
  index:INT;
  dofs:LONGINT;
BEGIN
  c:=V.code;
  index:=0;
  WHILE c#NIL DO
    WITH c:PCODJLABL DO
      c.bnum:=2;
    | c:PCODJMP DO
      IF c.j32 THEN
        c.bnum:=5;
      ELSE
        c.bnum:=2;
      END;
    | c:PCODDATA DO
      c.bnum:=c.len;
    | c:PCODFX DO
      c.bnum:=4;
    | c:PCODFP DO
      c.ofs:=c.ofs+CalcOffset(c.drefs);
      IF (c.ofs>=-128) AND (c.ofs<=127) THEN
        c.bnum:=2+ORD(c.is2b);
      ELSE
        c.bnum:=5+ORD(c.is2b);
      END;
    END;
    c.index:=index;
    index:=index+1;
    c:=c.next;
  END;
  c:=V.coded;
  dofs:=0;
  WHILE c#NIL DO
    c.dofs:=dofs;
    WITH c:PCODDATA DO
      c.bnum:=c.len;
    | c:PCODFX DO
      c.bnum:=4;
    END;
    dofs:=dofs+c.bnum;
    c:=c.next;
  END;
  PrepareJcc(V);
  V.prepared:=TRUE;
END Prepare;

PROCEDURE GenSeg(c:PCOD);
VAR
  i:INT;
  d:INT;
  r:DREF;
BEGIN
  WHILE c#NIL DO
    WITH c:PCODDATA DO
      IF NOT c.mpos.IsNull() THEN
        cmd.mark_pos(c.mpos);
      END;
      FOR i:=0 TO c.len-1 DO
        cmd.GenByte(c.data[i]);
      END;
    | c:PCODFX DO
      c.ofs:=c.ofs+CalcOffset(c.drefs);
      IF IsOffset(c.drefs.refs[0]) THEN
        cmd.GenLWord(c.ofs);
      ELSE
        r:=c.drefs.refs[0];
        WITH r:DVAR DO
          cmd.gen_fixup(r.var,c.ofs,c.kind);
        | r:DLAB DO
          cmd.gen_fixup(data_ob,c.ofs+r.label.location.dofs,cmd.fx_obj32);
        END;
      END;
    | c:PCODFP DO
      cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,c.b1-ORD(c.bnum<=3)*40H));
      IF c.is2b THEN
        cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,c.b2));
      END;
      IF c.bnum>3 THEN
        cmd.GenLWord(c.ofs);
      ELSE
        cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,c.ofs));
      END;
    | c:PCODJLABL DO
      d:=CalcDist(c,c.location);
      IF c.j32 THEN
        cmd.GenByte(0FH);
        cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,c.code+10H));
        cmd.GenLWord(d);
      ELSE
        cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,c.code));
        cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,d));
      END;
    | c:PCODJMP DO
      cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,c.code));
      d:=CalcDist(c,c.location);
      IF c.j32 THEN
        cmd.GenLWord(d);
      ELSE
        cmd.GenByte(SYSTEM.VAL(SYSTEM.CARD8,d));
      END;
    END;
    c:=c.next;
  END;
END GenSeg;

PROCEDURE Gen*(V:pcK.VALUE;get_offs:GetOffset):cmd.CODE_SEGM;
VAR
  old,sg:cmd.CODE_SEGM;
BEGIN
  GetOf:=get_offs;
  cmd.get_segm(old);
  WITH V:COD_VALUE DO
    IF NOT V.prepared THEN
      Prepare(V);
    END;
    IF V.coded#NIL THEN
      data_ob:=at.new_work_object(NIL,NIL,at.curr_mod.type,pcK.ob_cons,FALSE);
      cmd.new_segm(sg);
      cmd.set_segm(sg);
      GenSeg(V.coded);
      cmd.set_ready(data_ob,sg);
    END;
    cmd.new_segm(sg);
    cmd.set_segm(sg);
    GenSeg(V.code);
  END;
  cmd.set_segm(old);
  RETURN sg;
EXCEPT
  IF EX.IsCurrentSource(ExS) THEN
    RETURN sg;
  END;
END Gen;

VAR
  prefix:PREFIX;
  db:DB;
  cseg:CSEG;
  dseg:DSEG;
  inst0:INST0;
  inst1neg:INST1NEG;
  inst1inc:INST1INC;
  inst2add:INST2ADD;
  inst2mov:INST2MOV;
  inst1pop:INST1POP;
  inst1push:INST1PUSH;
  inst2shift:INST2SHIFT;
  inst2lea:INST2LEA;
  inst2test:INST2TEST;
  inst2xchg:INST2XCHG;
  inst1jcc:INST1Jcc;
  inst1jecxz:INST1JECXZ;
  inst1jcxz:INST1JCXZ;
  inst2cmovcc:INST2CMOVcc;
  inst2bt:INST2BT;
  inst1bswap:INST1BSWAP;
  inst1ret:INST1RET;
  instximul:INSTxIMUL;
  inst1jmp:INST1JMP;
  inst1lgdt:INST1LGDT;
  inst1lldt:INST1LLDT;
  inst1int:INST1INT;
  inst2lsl:INST2LSL;
  inst2movsx:INST2MOVSX;
  inst3shld:INST3SHLD;
  inst2enter:INST2ENTER;
  inst2xadd:INST2XADD;
  inst2arpl:INST2ARPL;
  inst2bound:INST2BOUND;
  inst1cmpxchg8b:INST1CMPXCHG8B;
  inst1invlpg:INST1INVLPG;
  inst2lds:INST2LDS;
  inst2in:INST2IN;
  inst2out:INST2OUT;
  inst2f:INST2F;
  inst2fp:INST2FP;
  inst1fi:INST1FI;
  inst1fr:INST1FR;
  inst1fld:INST1FLD;
  inst1fild:INST1FILD;
  inst1fcom:INST1FCOM;
  inst2fcomi:INST2FCOMI;
  inst1fm:INST1FM;
  inst0f3:INST0F3;
  inst1fstsw:INST1FSTSW;
  inst2xrm:INST2XRM;
  inst2xrmi:INST2XRMI;
  inst2movd:INST2MOVD;
  inst2movq:INST2MOVQ;
BEGIN
  FOR i:=0 TO TAB_LEN-1 DO
    ITable[i]:=NIL;
  END;

  NEW(prefix);Set("LOCK",prefix);prefix.Ini(0F0H);
  NEW(prefix);Set("REPNE",prefix);prefix.Ini(0F2H);
  NEW(prefix);Set("REPNZ",prefix);prefix.Ini(0F2H);
  NEW(prefix);Set("REP",prefix);prefix.Ini(0F3H);
  NEW(prefix);Set("REPE",prefix);prefix.Ini(0F3H);
  NEW(prefix);Set("REPZ",prefix);prefix.Ini(0F3H);

  NEW(db);Set("DB",db);db.Ini(1);
  NEW(db);Set("DW",db);db.Ini(2);
  NEW(db);Set("DD",db);db.Ini(4);
--NEW(db);Set("DF",db);db.Ini(6);
  NEW(db);Set("DQ",db);db.Ini(8);
  NEW(db);Set("DT",db);db.Ini(10);

  NEW(cseg);Set("CSEG",cseg);cseg.Ini;
  NEW(dseg);Set("DSEG",dseg);dseg.Ini;

  NEW(inst0);Set("AAA",inst0);inst0.Ini(1,37H,0);
  NEW(inst0);Set("AAD",inst0);inst0.Ini(2,0D5H,0AH);
  NEW(inst0);Set("AAM",inst0);inst0.Ini(2,0D4H,0AH);
  NEW(inst0);Set("AAS",inst0);inst0.Ini(1,03FH,0);
  NEW(inst2add);Set("ADC",inst2add);inst2add.Ini(10H,80H,2);
  NEW(inst2add);Set("ADD",inst2add);inst2add.Ini(0,80H,0);
  NEW(inst2add);Set("AND",inst2add);inst2add.Ini(20H,80H,4);
  NEW(inst2arpl);Set("ARPL",inst2arpl);inst2arpl.Ini;
  NEW(inst2bound);Set("BOUND",inst2bound);inst2bound.Ini;
  NEW(inst2cmovcc);Set("BSF",inst2cmovcc);inst2cmovcc.Ini(0BCH);
  NEW(inst2cmovcc);Set("BSR",inst2cmovcc);inst2cmovcc.Ini(0BDH);
  NEW(inst1bswap);Set("BSWAP",inst1bswap);inst1bswap.Ini;
  NEW(inst2bt);Set("BT",inst2bt);inst2bt.Ini(0A3H,4);
  NEW(inst2bt);Set("BTC",inst2bt);inst2bt.Ini(0BBH,7);
  NEW(inst2bt);Set("BTR",inst2bt);inst2bt.Ini(0B3H,6);
  NEW(inst2bt);Set("BTS",inst2bt);inst2bt.Ini(0ABH,5);
  NEW(inst1jmp);Set("CALL",inst1jmp);inst1jmp.Ini(0,0E8H,2,9AH);
  NEW(inst0);Set("CBW",inst0);inst0.Ini(2,66H,98H);
  NEW(inst0);Set("CWDE",inst0);inst0.Ini(1,98H,0);
  NEW(inst0);Set("CLC",inst0);inst0.Ini(1,0F8H,0);
  NEW(inst0);Set("CLD",inst0);inst0.Ini(1,0FCH,0);
  NEW(inst0);Set("CLI",inst0);inst0.Ini(1,0FAH,0);
  NEW(inst0);Set("CLTS",inst0);inst0.Ini(2,0FH,06);
  NEW(inst0);Set("CMC",inst0);inst0.Ini(1,0F5H,0);
  NEW(inst2cmovcc);Set("CMOVA",inst2cmovcc);inst2cmovcc.Ini(47H);
  NEW(inst2cmovcc);Set("CMOVAE",inst2cmovcc);inst2cmovcc.Ini(43H);
  NEW(inst2cmovcc);Set("CMOVB",inst2cmovcc);inst2cmovcc.Ini(42H);
  NEW(inst2cmovcc);Set("CMOVBE",inst2cmovcc);inst2cmovcc.Ini(46H);
  NEW(inst2cmovcc);Set("CMOVC",inst2cmovcc);inst2cmovcc.Ini(42H);
  NEW(inst2cmovcc);Set("CMOVE",inst2cmovcc);inst2cmovcc.Ini(44H);
  NEW(inst2cmovcc);Set("CMOVG",inst2cmovcc);inst2cmovcc.Ini(4FH);
  NEW(inst2cmovcc);Set("CMOVGE",inst2cmovcc);inst2cmovcc.Ini(4DH);
  NEW(inst2cmovcc);Set("CMOVL",inst2cmovcc);inst2cmovcc.Ini(4CH);
  NEW(inst2cmovcc);Set("CMOVLE",inst2cmovcc);inst2cmovcc.Ini(4EH);
  NEW(inst2cmovcc);Set("CMOVNA",inst2cmovcc);inst2cmovcc.Ini(46H);
  NEW(inst2cmovcc);Set("CMOVNAE",inst2cmovcc);inst2cmovcc.Ini(42H);
  NEW(inst2cmovcc);Set("CMOVNB",inst2cmovcc);inst2cmovcc.Ini(43H);
  NEW(inst2cmovcc);Set("CMOVNBE",inst2cmovcc);inst2cmovcc.Ini(47H);
  NEW(inst2cmovcc);Set("CMOVNC",inst2cmovcc);inst2cmovcc.Ini(43H);
  NEW(inst2cmovcc);Set("CMOVNE",inst2cmovcc);inst2cmovcc.Ini(45H);
  NEW(inst2cmovcc);Set("CMOVNG",inst2cmovcc);inst2cmovcc.Ini(4EH);
  NEW(inst2cmovcc);Set("CMOVNGE",inst2cmovcc);inst2cmovcc.Ini(4CH);
  NEW(inst2cmovcc);Set("CMOVNL",inst2cmovcc);inst2cmovcc.Ini(4DH);
  NEW(inst2cmovcc);Set("CMOVNLE",inst2cmovcc);inst2cmovcc.Ini(4FH);
  NEW(inst2cmovcc);Set("CMOVNO",inst2cmovcc);inst2cmovcc.Ini(41H);
  NEW(inst2cmovcc);Set("CMOVNP",inst2cmovcc);inst2cmovcc.Ini(4BH);
  NEW(inst2cmovcc);Set("CMOVNS",inst2cmovcc);inst2cmovcc.Ini(49H);
  NEW(inst2cmovcc);Set("CMOVNZ",inst2cmovcc);inst2cmovcc.Ini(45H);
  NEW(inst2cmovcc);Set("CMOVO",inst2cmovcc);inst2cmovcc.Ini(40H);
  NEW(inst2cmovcc);Set("CMOVP",inst2cmovcc);inst2cmovcc.Ini(4AH);
  NEW(inst2cmovcc);Set("CMOVPE",inst2cmovcc);inst2cmovcc.Ini(4AH);
  NEW(inst2cmovcc);Set("CMOVPO",inst2cmovcc);inst2cmovcc.Ini(4BH);
  NEW(inst2cmovcc);Set("CMOVS",inst2cmovcc);inst2cmovcc.Ini(48H);
  NEW(inst2cmovcc);Set("CMOVZ",inst2cmovcc);inst2cmovcc.Ini(44H);
  NEW(inst2add);Set("CMP",inst2add);inst2add.Ini(38H,80H,7);
-- CMPS
  NEW(inst0);Set("CMPSB",inst0);inst0.Ini(1,0A6H,0);
  NEW(inst0);Set("CMPSD",inst0);inst0.Ini(1,0A7H,0);
  NEW(inst0);Set("CMPSW",inst0);inst0.Ini(2,66H,0A7H);
  NEW(inst2xadd);Set("CMPXCHG",inst2xadd);inst2xadd.Ini(0B1H);
  NEW(inst1cmpxchg8b);Set("CMPXCHG8B",inst1cmpxchg8b);inst1cmpxchg8b.Ini;
  NEW(inst0);Set("CPUID",inst0);inst0.Ini(2,0FH,0A2H);
  NEW(inst0);Set("CWD",inst0);inst0.Ini(2,66H,99H);
  NEW(inst0);Set("CDQ",inst0);inst0.Ini(1,99H,0);
  NEW(inst0);Set("DAA",inst0);inst0.Ini(1,27H,0);
  NEW(inst0);Set("DAS",inst0);inst0.Ini(1,2FH,0);
  NEW(inst1inc);Set("DEC",inst1inc);inst1inc.Ini(1,0FEH,0,1,TRUE,48H);
  NEW(inst1neg);Set("DIV",inst1neg);inst1neg.Ini(1,0F6H,0,6,TRUE);
  NEW(inst2enter);Set("ENTER",inst2enter);inst2enter.Ini;
------------------------------------------------------------
  NEW(inst0);Set("F2XM1",inst0);inst0.Ini(2,0D9H,0F0H);
  NEW(inst0);Set("FABS",inst0);inst0.Ini(2,0D9H,0E1H);
  NEW(inst2f);Set("FADD",inst2f);inst2f.Ini(0C0H,0C0H,0);
  NEW(inst2fp);Set("FADDP",inst2fp);inst2fp.Ini(0C0H);
  NEW(inst1fi);Set("FIADD",inst1fi);inst1fi.Ini(0);
  NEW(inst1fm);Set("FBLD",inst1fm);inst1fm.Ini(80,0DFH,4,FALSE);
  NEW(inst1fm);Set("FBSTP",inst1fm);inst1fm.Ini(80,0DFH,6,FALSE);
  NEW(inst0);Set("FCHS",inst0);inst0.Ini(2,0D9H,0E0H);
  NEW(inst0f3);Set("FCLEX",inst0f3);inst0f3.Ini(0E2H);
  NEW(inst0);Set("FNCLEX",inst0);inst0.Ini(2,0DBH,0E2H);
  NEW(inst2fcomi);Set("FCMOVB",inst2fcomi);inst2fcomi.Ini(0DAH,0C0H);
  NEW(inst2fcomi);Set("FCMOVE",inst2fcomi);inst2fcomi.Ini(0DAH,0C8H);
  NEW(inst2fcomi);Set("FCMOVBE",inst2fcomi);inst2fcomi.Ini(0DAH,0D0H);
  NEW(inst2fcomi);Set("FCMOVU",inst2fcomi);inst2fcomi.Ini(0DAH,0D8H);
  NEW(inst2fcomi);Set("FCMOVNB",inst2fcomi);inst2fcomi.Ini(0DBH,0C0H);
  NEW(inst2fcomi);Set("FCMOVNE",inst2fcomi);inst2fcomi.Ini(0DBH,0C8H);
  NEW(inst2fcomi);Set("FCMOVNBE",inst2fcomi);inst2fcomi.Ini(0DBH,0D0H);
  NEW(inst2fcomi);Set("FCMOVNU",inst2fcomi);inst2fcomi.Ini(0DBH,0D8H);
  NEW(inst1fcom);Set("FCOM",inst1fcom);inst1fcom.Ini(0D0H,2);
  NEW(inst1fcom);Set("FCOMP",inst1fcom);inst1fcom.Ini(0D8H,3);
  NEW(inst0);Set("FCOMPP",inst0);inst0.Ini(2,0DEH,0D9H);
  NEW(inst2fcomi);Set("FCOMI",inst2fcomi);inst2fcomi.Ini(0DBH,0F0H);
  NEW(inst2fcomi);Set("FCOMIP",inst2fcomi);inst2fcomi.Ini(0DFH,0F0H);
  NEW(inst2fcomi);Set("FUCOMI",inst2fcomi);inst2fcomi.Ini(0DBH,0E8H);
  NEW(inst2fcomi);Set("FUCOMIP",inst2fcomi);inst2fcomi.Ini(0DFH,0E8H);
  NEW(inst0);Set("FCOS",inst0);inst0.Ini(2,0D9H,0FFH);
  NEW(inst0);Set("FDECSTP",inst0);inst0.Ini(2,0D9H,0F6H);
  NEW(inst2f);Set("FDIV",inst2f);inst2f.Ini(0F0H,0F8H,6);
  NEW(inst2fp);Set("FDIVP",inst2fp);inst2fp.Ini(0F8H);
  NEW(inst1fi);Set("FIDIV",inst1fi);inst1fi.Ini(6);
  NEW(inst2f);Set("FDIVR",inst2f);inst2f.Ini(0F8H,0F0H,7);
  NEW(inst2fp);Set("FDIVRP",inst2fp);inst2fp.Ini(0F0H);
  NEW(inst1fi);Set("FIDIVR",inst1fi);inst1fi.Ini(7);
  NEW(inst1fr);Set("FFREE",inst1fr);inst1fr.Ini(0DDH,0C0H,TRUE);
  NEW(inst1fi);Set("FICOM",inst1fi);inst1fi.Ini(2);
  NEW(inst1fi);Set("FICOMP",inst1fi);inst1fi.Ini(3);
  NEW(inst1fild);Set("FILD",inst1fild);inst1fild.Ini(0,5,TRUE);
  NEW(inst0);Set("FINCSTP",inst0);inst0.Ini(2,0D9H,0F7H);
  NEW(inst0f3);Set("FINIT",inst0f3);inst0f3.Ini(0E3H);
  NEW(inst0);Set("FNINIT",inst0);inst0.Ini(2,0DBH,0E3H);
  NEW(inst1fild);Set("FIST",inst1fild);inst1fild.Ini(2,7,FALSE);
  NEW(inst1fild);Set("FISTP",inst1fild);inst1fild.Ini(3,7,TRUE);
  NEW(inst1fld);Set("FLD",inst1fld);inst1fld.Ini(0D9H,0C0H,0,5,TRUE);
  NEW(inst0);Set("FLD1",inst0);inst0.Ini(2,0D9H,0E8H);
  NEW(inst0);Set("FLDL2T",inst0);inst0.Ini(2,0D9H,0E9H);
  NEW(inst0);Set("FLDL2E",inst0);inst0.Ini(2,0D9H,0EAH);
  NEW(inst0);Set("FLDPI",inst0);inst0.Ini(2,0D9H,0EBH);
  NEW(inst0);Set("FLDLG2",inst0);inst0.Ini(2,0D9H,0ECH);
  NEW(inst0);Set("FLDLN2",inst0);inst0.Ini(2,0D9H,0EDH);
  NEW(inst0);Set("FLDZ",inst0);inst0.Ini(2,0D9H,0EEH);
  NEW(inst1fm);Set("FLDCW",inst1fm);inst1fm.Ini(16,0D9H,5,FALSE);
  NEW(inst1fm);Set("FLDENV",inst1fm);inst1fm.Ini(0,0D9H,4,FALSE);
  NEW(inst2f);Set("FMUL",inst2f);inst2f.Ini(0C8H,0C8H,1);
  NEW(inst2fp);Set("FMULP",inst2fp);inst2fp.Ini(0C8H);
  NEW(inst1fi);Set("FIMUL",inst1fi);inst1fi.Ini(1);
  NEW(inst0);Set("FNOP",inst0);inst0.Ini(2,0D9H,0D0H);
  NEW(inst0);Set("FPATAN",inst0);inst0.Ini(2,0D9H,0F3H);
  NEW(inst0);Set("FPREM",inst0);inst0.Ini(2,0D9H,0F8H);
  NEW(inst0);Set("FPREM1",inst0);inst0.Ini(2,0D9H,0F5H);
  NEW(inst0);Set("FPTAN",inst0);inst0.Ini(2,0D9H,0F2H);
  NEW(inst0);Set("FRNDINT",inst0);inst0.Ini(2,0D9H,0FCH);
  NEW(inst1fm);Set("FRSTOR",inst1fm);inst1fm.Ini(0,0DDH,4,FALSE);
  NEW(inst1fm);Set("FSAVE",inst1fm);inst1fm.Ini(0,0DDH,6,TRUE);
  NEW(inst1fm);Set("FNSAVE",inst1fm);inst1fm.Ini(0,0DDH,6,FALSE);
  NEW(inst0);Set("FSCALE",inst0);inst0.Ini(2,0D9H,0FDH);
  NEW(inst0);Set("FSIN",inst0);inst0.Ini(2,0D9H,0FEH);
  NEW(inst0);Set("FSINCOS",inst0);inst0.Ini(2,0D9H,0FBH);
  NEW(inst0);Set("FSQRT",inst0);inst0.Ini(2,0D9H,0FAH);
  NEW(inst1fld);Set("FST",inst1fld);inst1fld.Ini(0DDH,0D0H,2,7,FALSE);
  NEW(inst1fld);Set("FSTP",inst1fld);inst1fld.Ini(0DDH,0D8H,3,7,TRUE);
  NEW(inst1fm);Set("FSTCW",inst1fm);inst1fm.Ini(16,0D9H,7,TRUE);
  NEW(inst1fm);Set("FNSTCW",inst1fm);inst1fm.Ini(16,0D9H,7,FALSE);
  NEW(inst1fm);Set("FSTENV",inst1fm);inst1fm.Ini(16,0D9H,6,TRUE);
  NEW(inst1fm);Set("FNSTENV",inst1fm);inst1fm.Ini(16,0D9H,6,FALSE);
  NEW(inst1fstsw);Set("FSTSW",inst1fstsw);inst1fstsw.Ini(TRUE);
  NEW(inst1fstsw);Set("FNSTSW",inst1fstsw);inst1fstsw.Ini(FALSE);
  NEW(inst2f);Set("FSUB",inst2f);inst2f.Ini(0E0H,0E8H,4);
  NEW(inst2fp);Set("FSUBP",inst2fp);inst2fp.Ini(0E8H);
  NEW(inst1fi);Set("FISUB",inst1fi);inst1fi.Ini(4);
  NEW(inst2f);Set("FSUBR",inst2f);inst2f.Ini(0E8H,0E0H,5);
  NEW(inst2fp);Set("FSUBRP",inst2fp);inst2fp.Ini(0E0H);
  NEW(inst1fi);Set("FISUBR",inst1fi);inst1fi.Ini(5);
  NEW(inst0);Set("FTST",inst0);inst0.Ini(2,0D9H,0E4H);
  NEW(inst1fr);Set("FUCOM",inst1fr);inst1fr.Ini(0DDH,0E0H,FALSE);
  NEW(inst1fr);Set("FUCOMP",inst1fr);inst1fr.Ini(0DDH,0E8H,FALSE);
  NEW(inst0);Set("FUCOMPP",inst0);inst0.Ini(2,0DAH,0E9H);
  NEW(inst0);Set("FWAIT",inst0);inst0.Ini(1,9BH,0);
  NEW(inst0);Set("FXAM",inst0);inst0.Ini(2,0D9H,0E5H);
  NEW(inst1fr);Set("FXCH",inst1fr);inst1fr.Ini(0D9H,0C8H,FALSE);
  NEW(inst0);Set("FXTRACT",inst0);inst0.Ini(2,0D9H,0F4H);
  NEW(inst0);Set("FYL2X",inst0);inst0.Ini(2,0D9H,0F1H);
  NEW(inst0);Set("FYL2XP1",inst0);inst0.Ini(2,0D9H,0F9H);
------------------------------------------------------------------
  NEW(inst0);Set("HLT",inst0);inst0.Ini(1,0F4H,0);
  NEW(inst1neg);Set("IDIV",inst1neg);inst1neg.Ini(1,0F6H,0,7,TRUE);
  NEW(instximul);Set("IMUL",instximul);instximul.Ini;

  NEW(inst2in);Set("IN",inst2in);inst2in.Ini;
  NEW(inst1inc);Set("INC",inst1inc);inst1inc.Ini(1,0FEH,0,0,TRUE,40H);
-- INS
  NEW(inst0);Set("INSB",inst0);inst0.Ini(1,06CH,0);
  NEW(inst0);Set("INSD",inst0);inst0.Ini(1,06DH,0);
  NEW(inst0);Set("INSW",inst0);inst0.Ini(2,66H,06DH);
  NEW(inst0);Set("INT3",inst0);inst0.Ini(1,0CCH,0);
  NEW(inst1int);Set("INT",inst1int);inst1int.Ini();
  NEW(inst0);Set("INTO",inst0);inst0.Ini(1,0CEH,0);
  NEW(inst0);Set("INVD",inst0);inst0.Ini(2,0FH,08H);
  NEW(inst1invlpg);Set("INVLPG",inst1invlpg);inst1invlpg.Ini;
  NEW(inst0);Set("IRET",inst0);inst0.Ini(2,66H,0CFH);
  NEW(inst0);Set("IRETD",inst0);inst0.Ini(1,0CFH,0);
  NEW(inst1jcc);Set("JA",inst1jcc);inst1jcc.Ini(77H);
  NEW(inst1jcc);Set("JAE",inst1jcc);inst1jcc.Ini(73H);
  NEW(inst1jcc);Set("JB",inst1jcc);inst1jcc.Ini(72H);
  NEW(inst1jcc);Set("JBE",inst1jcc);inst1jcc.Ini(76H);
  NEW(inst1jcc);Set("JC",inst1jcc);inst1jcc.Ini(72H);
  NEW(inst1jcxz);Set("JCXZ",inst1jcxz);inst1jcxz.Ini(0E3H);
  NEW(inst1jecxz);Set("JECXZ",inst1jecxz);inst1jecxz.Ini(0E3H);
  NEW(inst1jcc);Set("JE",inst1jcc);inst1jcc.Ini(74H);
  NEW(inst1jcc);Set("JG",inst1jcc);inst1jcc.Ini(7FH);
  NEW(inst1jcc);Set("JGE",inst1jcc);inst1jcc.Ini(7DH);
  NEW(inst1jcc);Set("JL",inst1jcc);inst1jcc.Ini(7CH);
  NEW(inst1jcc);Set("JLE",inst1jcc);inst1jcc.Ini(7EH);
  NEW(inst1jcc);Set("JNA",inst1jcc);inst1jcc.Ini(76H);
  NEW(inst1jcc);Set("JNAE",inst1jcc);inst1jcc.Ini(72H);
  NEW(inst1jcc);Set("JNB",inst1jcc);inst1jcc.Ini(73H);
  NEW(inst1jcc);Set("JNBE",inst1jcc);inst1jcc.Ini(77H);
  NEW(inst1jcc);Set("JNC",inst1jcc);inst1jcc.Ini(73H);
  NEW(inst1jcc);Set("JNE",inst1jcc);inst1jcc.Ini(75H);
  NEW(inst1jcc);Set("JNG",inst1jcc);inst1jcc.Ini(7EH);
  NEW(inst1jcc);Set("JNGE",inst1jcc);inst1jcc.Ini(7CH);
  NEW(inst1jcc);Set("JNL",inst1jcc);inst1jcc.Ini(7DH);
  NEW(inst1jcc);Set("JNLE",inst1jcc);inst1jcc.Ini(7FH);
  NEW(inst1jcc);Set("JNO",inst1jcc);inst1jcc.Ini(71H);
  NEW(inst1jcc);Set("JNP",inst1jcc);inst1jcc.Ini(7BH);
  NEW(inst1jcc);Set("JNS",inst1jcc);inst1jcc.Ini(79H);
  NEW(inst1jcc);Set("JNZ",inst1jcc);inst1jcc.Ini(75H);
  NEW(inst1jcc);Set("JO",inst1jcc);inst1jcc.Ini(70H);
  NEW(inst1jcc);Set("JP",inst1jcc);inst1jcc.Ini(7AH);
  NEW(inst1jcc);Set("JPE",inst1jcc);inst1jcc.Ini(7AH);
  NEW(inst1jcc);Set("JPO",inst1jcc);inst1jcc.Ini(7BH);
  NEW(inst1jcc);Set("JS",inst1jcc);inst1jcc.Ini(78H);
  NEW(inst1jcc);Set("JZ",inst1jcc);inst1jcc.Ini(74H);
  NEW(inst1jmp);Set("JMP",inst1jmp);inst1jmp.Ini(0EBH,0E9H,4,0EAH);
  NEW(inst0);Set("LAHF",inst0);inst0.Ini(1,9FH,0);
  NEW(inst2lsl);Set("LAR",inst2lsl);inst2lsl.Ini(2);
  NEW(inst2lds);Set("LDS",inst2lds);inst2lds.Ini(FALSE,0C5H);
  NEW(inst2lds);Set("LSS",inst2lds);inst2lds.Ini(TRUE,0B2H);
  NEW(inst2lds);Set("LES",inst2lds);inst2lds.Ini(FALSE,0C4H);
  NEW(inst2lds);Set("LFS",inst2lds);inst2lds.Ini(TRUE,0B4H);
  NEW(inst2lds);Set("LGS",inst2lds);inst2lds.Ini(TRUE,0B5H);
  NEW(inst2lea);Set("LEA",inst2lea);inst2lea.Ini;
  NEW(inst0);Set("LEAVE",inst0);inst0.Ini(1,0C9H,0);
  NEW(inst1lgdt);Set("LGDT",inst1lgdt);inst1lgdt.Ini(2);
  NEW(inst1lgdt);Set("LIDT",inst1lgdt);inst1lgdt.Ini(3);
  NEW(inst1lldt);Set("LLDT",inst1lldt);inst1lldt.Ini(0,2,FALSE);
  NEW(inst1lldt);Set("LMSW",inst1lldt);inst1lldt.Ini(1,6,FALSE);
-- LODS
  NEW(inst0);Set("LODSB",inst0);inst0.Ini(1,0ACH,0);
  NEW(inst0);Set("LODSD",inst0);inst0.Ini(1,0ADH,0);
  NEW(inst0);Set("LODSW",inst0);inst0.Ini(2,66H,0ADH);
  NEW(inst1jecxz);Set("LOOP",inst1jecxz);inst1jecxz.Ini(0E2H);
  NEW(inst1jecxz);Set("LOOPE",inst1jecxz);inst1jecxz.Ini(0E1H);
  NEW(inst1jecxz);Set("LOOPZ",inst1jecxz);inst1jecxz.Ini(0E1H);
  NEW(inst1jecxz);Set("LOOPNE",inst1jecxz);inst1jecxz.Ini(0E0H);
  NEW(inst1jecxz);Set("LOOPNZ",inst1jecxz);inst1jecxz.Ini(0E0H);
  NEW(inst2lsl);Set("LSL",inst2lsl);inst2lsl.Ini(3);
  NEW(inst1lldt);Set("LTR",inst1lldt);inst1lldt.Ini(0,3,FALSE);
  NEW(inst2mov);Set("MOV",inst2mov);inst2mov.Ini;
-- MOVS
  NEW(inst0);Set("MOVSB",inst0);inst0.Ini(1,0A4H,0);
  NEW(inst0);Set("MOVSD",inst0);inst0.Ini(1,0A5H,0);
  NEW(inst0);Set("MOVSW",inst0);inst0.Ini(2,66H,0A5H);
  NEW(inst2movsx);Set("MOVSX",inst2movsx);inst2movsx.Ini(0BFH);
  NEW(inst2movsx);Set("MOVZX",inst2movsx);inst2movsx.Ini(0B7H);
  NEW(inst1neg);Set("MUL",inst1neg);inst1neg.Ini(1,0F6H,0,4,TRUE);
  NEW(inst1neg);Set("NEG",inst1neg);inst1neg.Ini(1,0F6H,0,3,TRUE);
  NEW(inst0);Set("NOP",inst0);inst0.Ini(1,090H,0);
  NEW(inst1neg);Set("NOT",inst1neg);inst1neg.Ini(1,0F6H,0,2,TRUE);
  NEW(inst2add);Set("OR",inst2add);inst2add.Ini(08,80H,1);
  NEW(inst2out);Set("OUT",inst2out);inst2out.Ini;
-- OUTS
  NEW(inst0);Set("OUTSB",inst0);inst0.Ini(1,06EH,0);
  NEW(inst0);Set("OUTSD",inst0);inst0.Ini(1,06FH,0);
  NEW(inst0);Set("OUTSW",inst0);inst0.Ini(2,66H,06FH);
  NEW(inst1pop);Set("POP",inst1pop);inst1pop.Ini;
  NEW(inst0);Set("POPA",inst0);inst0.Ini(2,66H,61H);
  NEW(inst0);Set("POPAD",inst0);inst0.Ini(1,61H,0);
  NEW(inst0);Set("POPF",inst0);inst0.Ini(2,66H,9DH);
  NEW(inst0);Set("POPFD",inst0);inst0.Ini(1,9DH,0);
  NEW(inst1push);Set("PUSH",inst1push);inst1push.Ini;
  NEW(inst0);Set("PUSHA",inst0);inst0.Ini(2,66H,60H);
  NEW(inst0);Set("PUSHAD",inst0);inst0.Ini(1,60H,0);
  NEW(inst0);Set("PUSHF",inst0);inst0.Ini(2,66H,9CH);
  NEW(inst0);Set("PUSHFD",inst0);inst0.Ini(1,9CH,0);
  NEW(inst2shift);Set("RCL",inst2shift);inst2shift.Ini(2);
  NEW(inst2shift);Set("RCR",inst2shift);inst2shift.Ini(3);
  NEW(inst2shift);Set("ROL",inst2shift);inst2shift.Ini(0);
  NEW(inst2shift);Set("ROR",inst2shift);inst2shift.Ini(1);
  NEW(inst0);Set("RDMSR",inst0);inst0.Ini(2,0FH,32H);
  NEW(inst0);Set("RDPMC",inst0);inst0.Ini(2,0FH,33H);
  NEW(inst0);Set("RDTSC",inst0);inst0.Ini(2,0FH,31H);
  NEW(inst1ret);Set("RET",inst1ret);inst1ret.Ini(0C3H);
  NEW(inst1ret);Set("RETN",inst1ret);inst1ret.Ini(0C3H);
  NEW(inst1ret);Set("RETF",inst1ret);inst1ret.Ini(0CBH);
  NEW(inst0);Set("RSM",inst0);inst0.Ini(2,0FH,0AAH);
  NEW(inst0);Set("SAHF",inst0);inst0.Ini(1,9EH,0);
  NEW(inst2shift);Set("SAL",inst2shift);inst2shift.Ini(4);
  NEW(inst2shift);Set("SAR",inst2shift);inst2shift.Ini(7);
  NEW(inst2shift);Set("SHL",inst2shift);inst2shift.Ini(4);
  NEW(inst2shift);Set("SHR",inst2shift);inst2shift.Ini(5);
  NEW(inst2add);Set("SBB",inst2add);inst2add.Ini(18H,80H,3);
-- SCAS
  NEW(inst0);Set("SCASB",inst0);inst0.Ini(1,0AEH,0);
  NEW(inst0);Set("SCASD",inst0);inst0.Ini(1,0AFH,0);
  NEW(inst0);Set("SCASW",inst0);inst0.Ini(2,66H,0AFH);
  NEW(inst1neg);Set("SETA",inst1neg);inst1neg.Ini(2,0FH,97H,0,FALSE);
  NEW(inst1neg);Set("SETAE",inst1neg);inst1neg.Ini(2,0FH,93H,0,FALSE);
  NEW(inst1neg);Set("SETB",inst1neg);inst1neg.Ini(2,0FH,92H,0,FALSE);
  NEW(inst1neg);Set("SETBE",inst1neg);inst1neg.Ini(2,0FH,96H,0,FALSE);
  NEW(inst1neg);Set("SETC",inst1neg);inst1neg.Ini(2,0FH,92H,0,FALSE);
  NEW(inst1neg);Set("SETE",inst1neg);inst1neg.Ini(2,0FH,94H,0,FALSE);
  NEW(inst1neg);Set("SETG",inst1neg);inst1neg.Ini(2,0FH,9FH,0,FALSE);
  NEW(inst1neg);Set("SETGE",inst1neg);inst1neg.Ini(2,0FH,9DH,0,FALSE);
  NEW(inst1neg);Set("SETL",inst1neg);inst1neg.Ini(2,0FH,9CH,0,FALSE);
  NEW(inst1neg);Set("SETLE",inst1neg);inst1neg.Ini(2,0FH,9EH,0,FALSE);
  NEW(inst1neg);Set("SETNA",inst1neg);inst1neg.Ini(2,0FH,96H,0,FALSE);
  NEW(inst1neg);Set("SETNAE",inst1neg);inst1neg.Ini(2,0FH,92H,0,FALSE);
  NEW(inst1neg);Set("SETNB",inst1neg);inst1neg.Ini(2,0FH,93H,0,FALSE);
  NEW(inst1neg);Set("SETNBE",inst1neg);inst1neg.Ini(2,0FH,97H,0,FALSE);
  NEW(inst1neg);Set("SETNC",inst1neg);inst1neg.Ini(2,0FH,93H,0,FALSE);
  NEW(inst1neg);Set("SETNE",inst1neg);inst1neg.Ini(2,0FH,95H,0,FALSE);
  NEW(inst1neg);Set("SETNG",inst1neg);inst1neg.Ini(2,0FH,9EH,0,FALSE);
  NEW(inst1neg);Set("SETNGE",inst1neg);inst1neg.Ini(2,0FH,9CH,0,FALSE);
  NEW(inst1neg);Set("SETNL",inst1neg);inst1neg.Ini(2,0FH,9DH,0,FALSE);
  NEW(inst1neg);Set("SETNLE",inst1neg);inst1neg.Ini(2,0FH,9FH,0,FALSE);
  NEW(inst1neg);Set("SETNO",inst1neg);inst1neg.Ini(2,0FH,91H,0,FALSE);
  NEW(inst1neg);Set("SETNP",inst1neg);inst1neg.Ini(2,0FH,9BH,0,FALSE);
  NEW(inst1neg);Set("SETNS",inst1neg);inst1neg.Ini(2,0FH,99H,0,FALSE);
  NEW(inst1neg);Set("SETNZ",inst1neg);inst1neg.Ini(2,0FH,95H,0,FALSE);
  NEW(inst1neg);Set("SETO",inst1neg);inst1neg.Ini(2,0FH,90H,0,FALSE);
  NEW(inst1neg);Set("SETP",inst1neg);inst1neg.Ini(2,0FH,9AH,0,FALSE);
  NEW(inst1neg);Set("SETPE",inst1neg);inst1neg.Ini(2,0FH,9AH,0,FALSE);
  NEW(inst1neg);Set("SETPO",inst1neg);inst1neg.Ini(2,0FH,9BH,0,FALSE);
  NEW(inst1neg);Set("SETS",inst1neg);inst1neg.Ini(2,0FH,98H,0,FALSE);
  NEW(inst1neg);Set("SETZ",inst1neg);inst1neg.Ini(2,0FH,94H,0,FALSE);
  NEW(inst1lgdt);Set("SGDT",inst1lgdt);inst1lgdt.Ini(0);
  NEW(inst1lgdt);Set("SIDT",inst1lgdt);inst1lgdt.Ini(1);
  NEW(inst3shld);Set("SHLD",inst3shld);inst3shld.Ini(0A4H);
  NEW(inst3shld);Set("SHRD",inst3shld);inst3shld.Ini(0ACH);
  NEW(inst1lldt);Set("SLDT",inst1lldt);inst1lldt.Ini(0,0,TRUE);
  NEW(inst1lldt);Set("SMSW",inst1lldt);inst1lldt.Ini(1,4,TRUE);
  NEW(inst0);Set("STC",inst0);inst0.Ini(1,0F9H,0);
  NEW(inst0);Set("STD",inst0);inst0.Ini(1,0FDH,0);
  NEW(inst0);Set("STI",inst0);inst0.Ini(1,0FBH,0);
-- STOS
  NEW(inst0);Set("STOSB",inst0);inst0.Ini(1,0AAH,0);
  NEW(inst0);Set("STOSD",inst0);inst0.Ini(1,0ABH,0);
  NEW(inst0);Set("STOSW",inst0);inst0.Ini(2,66H,0ABH);
  NEW(inst1lldt);Set("STR",inst1lldt);inst1lldt.Ini(0,1,TRUE);
  NEW(inst2add);Set("SUB",inst2add);inst2add.Ini(28H,80H,5);
  NEW(inst2test);Set("TEST",inst2test);inst2test.Ini;
  NEW(inst0);Set("UD2",inst0);inst0.Ini(2,0FH,0BH);
  NEW(inst1lldt);Set("VERR",inst1lldt);inst1lldt.Ini(0,4,FALSE);
  NEW(inst1lldt);Set("VERW",inst1lldt);inst1lldt.Ini(0,5,FALSE);
  NEW(inst0);Set("WAIT",inst0);inst0.Ini(1,9BH,0);
  NEW(inst0);Set("WBINVD",inst0);inst0.Ini(2,0FH,09);
  NEW(inst0);Set("WRMSR",inst0);inst0.Ini(2,0FH,30H);
  NEW(inst2xadd);Set("XADD",inst2xadd);inst2xadd.Ini(0C1H);
  NEW(inst2xchg);Set("XCHG",inst2xchg);inst2xchg.Ini;
  NEW(inst0);Set("XLAT",inst0);inst0.Ini(1,0D7H,0);
  NEW(inst2add);Set("XOR",inst2add);inst2add.Ini(30H,80H,6);
-------------------------------------------------------------
  NEW(inst0);Set("EMMS",inst0);inst0.Ini(2,0FH,77H);
  NEW(inst2movd);Set("MOVD",inst2movd);inst2movd.Ini;
  NEW(inst2movq);Set("MOVQ",inst2movq);inst2movq.Ini;
  NEW(inst2xrm);Set("PACKSSDW",inst2xrm);inst2xrm.Ini(6BH);
  NEW(inst2xrm);Set("PACKSSWB",inst2xrm);inst2xrm.Ini(63H);
  NEW(inst2xrm);Set("PACKUSWB",inst2xrm);inst2xrm.Ini(67H);
  NEW(inst2xrm);Set("PADDB",inst2xrm);inst2xrm.Ini(0FCH);
  NEW(inst2xrm);Set("PADDD",inst2xrm);inst2xrm.Ini(0FEH);
  NEW(inst2xrm);Set("PADDSB",inst2xrm);inst2xrm.Ini(0ECH);
  NEW(inst2xrm);Set("PADDSW",inst2xrm);inst2xrm.Ini(0EDH);
  NEW(inst2xrm);Set("PADDUSB",inst2xrm);inst2xrm.Ini(0DCH);
  NEW(inst2xrm);Set("PADDUSW",inst2xrm);inst2xrm.Ini(0DDH);
  NEW(inst2xrm);Set("PADDW",inst2xrm);inst2xrm.Ini(0FDH);
  NEW(inst2xrm);Set("PAND",inst2xrm);inst2xrm.Ini(0DBH);
  NEW(inst2xrm);Set("PANDN",inst2xrm);inst2xrm.Ini(0DFH);
  NEW(inst2xrm);Set("PCMPEQB",inst2xrm);inst2xrm.Ini(74H);
  NEW(inst2xrm);Set("PCMPEQD",inst2xrm);inst2xrm.Ini(76H);
  NEW(inst2xrm);Set("PCMPEQW",inst2xrm);inst2xrm.Ini(75H);
  NEW(inst2xrm);Set("PCMPGTB",inst2xrm);inst2xrm.Ini(64H);
  NEW(inst2xrm);Set("PCMPGTD",inst2xrm);inst2xrm.Ini(66H);
  NEW(inst2xrm);Set("PCMPGTW",inst2xrm);inst2xrm.Ini(65H);
  NEW(inst2xrm);Set("PMADDWD",inst2xrm);inst2xrm.Ini(0F5H);
  NEW(inst2xrm);Set("PMULHW",inst2xrm);inst2xrm.Ini(0E5H);
  NEW(inst2xrm);Set("PMULLW",inst2xrm);inst2xrm.Ini(0D5H);
  NEW(inst2xrm);Set("POR",inst2xrm);inst2xrm.Ini(0EBH);
  NEW(inst2xrmi);Set("PSLLW",inst2xrmi);inst2xrmi.Ini(0F1H,71H,6);
  NEW(inst2xrmi);Set("PSLLD",inst2xrmi);inst2xrmi.Ini(0F2H,72H,6);
  NEW(inst2xrmi);Set("PSLLQ",inst2xrmi);inst2xrmi.Ini(0F3H,73H,6);
  NEW(inst2xrmi);Set("PSRAW",inst2xrmi);inst2xrmi.Ini(0E1H,71H,4);
  NEW(inst2xrmi);Set("PSRAD",inst2xrmi);inst2xrmi.Ini(0E2H,72H,4);
  NEW(inst2xrmi);Set("PSRLW",inst2xrmi);inst2xrmi.Ini(0D1H,71H,2);
  NEW(inst2xrmi);Set("PSRLD",inst2xrmi);inst2xrmi.Ini(0D2H,72H,2);
  NEW(inst2xrmi);Set("PSRLQ",inst2xrmi);inst2xrmi.Ini(0D3H,73H,2);
  NEW(inst2xrm);Set("PSUBB",inst2xrm);inst2xrm.Ini(0F8H);
  NEW(inst2xrm);Set("PSUBD",inst2xrm);inst2xrm.Ini(0FAH);
  NEW(inst2xrm);Set("PSUBSB",inst2xrm);inst2xrm.Ini(0E8H);
  NEW(inst2xrm);Set("PSUBSW",inst2xrm);inst2xrm.Ini(0E9H);
  NEW(inst2xrm);Set("PSUBUSB",inst2xrm);inst2xrm.Ini(0D8H);
  NEW(inst2xrm);Set("PSUBUSW",inst2xrm);inst2xrm.Ini(0D9H);
  NEW(inst2xrm);Set("PSUBW",inst2xrm);inst2xrm.Ini(0F9H);
  NEW(inst2xrm);Set("PUNPCKHBW",inst2xrm);inst2xrm.Ini(068H);
  NEW(inst2xrm);Set("PUNPCKHDQ",inst2xrm);inst2xrm.Ini(06AH);
  NEW(inst2xrm);Set("PUNPCKHWD",inst2xrm);inst2xrm.Ini(069H);
  NEW(inst2xrm);Set("PUNPCKLBW",inst2xrm);inst2xrm.Ini(060H);
  NEW(inst2xrm);Set("PUNPCKLDQ",inst2xrm);inst2xrm.Ini(062H);
  NEW(inst2xrm);Set("PUNPCKLWD",inst2xrm);inst2xrm.Ini(061H);
  NEW(inst2xrm);Set("PXOR",inst2xrm);inst2xrm.Ini(0EFH);

  EX.AllocateSource(ExS);
  
END AsmX86.  
