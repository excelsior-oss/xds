<* NEW NOREGVARS+ *>
MODULE hgr;
IMPORT hgl;
IMPORT dllRTS;
IMPORT SYSTEM;
IMPORT Printf;
IMPORT WholeStr;

CONST
    DefVertexType   *= 0;
    DefFragmentType *= 1;
    DefEdgeType     *= 2;

TYPE
    H2D_PtrSChar = hgl.H2D_PtrSChar;
    TNormalizeGraph = PROCEDURE [2]( (* = 1 *) red: SYSTEM.int );
    TSetVisCode_Ex = PROCEDURE  [2]( t: SYSTEM.int; str: H2D_PtrSChar;
                       is_x: SYSTEM.int ): SYSTEM.int;
    TSetLabelName = PROCEDURE [2]( t: SYSTEM.int; ln: SYSTEM.int; str: H2D_PtrSChar );
    TSetLabVal    = PROCEDURE  [2]( obj: SYSTEM.int; n: SYSTEM.int; ln: SYSTEM.int;
                      str: H2D_PtrSChar ): SYSTEM.int;
    TAddLabel_Ex  = PROCEDURE [2]( t: SYSTEM.int; d: SYSTEM.int ): SYSTEM.int;
    TAddType      = PROCEDURE [2]( object: SYSTEM.int; name: H2D_PtrSChar ): SYSTEM.int;
    TGetVertex    = PROCEDURE [2]( vertex: SYSTEM.int ): hgl.PVertex;
    TGetEdge    = PROCEDURE [2]( edge: SYSTEM.int ): hgl.PEdge;
    TBeginWork    = PROCEDURE [2]( SEQ arg0: SYSTEM.BYTE );
    TEndWork    = PROCEDURE [2]( SEQ arg0: SYSTEM.BYTE );
    TSetFrTitle   = PROCEDURE [2]( fr: SYSTEM.int; str: H2D_PtrSChar );
    TAddVertex    = PROCEDURE [2]( t: SYSTEM.int; fr: SYSTEM.int; x: SYSTEM.int; y: SYSTEM.int );
    TAddEdge      = PROCEDURE [2]( t: SYSTEM.int; from: SYSTEM.int; to: SYSTEM.int );
    TSaveGraph    = PROCEDURE [2]( filename: H2D_PtrSChar ): SYSTEM.int;
    TCreateNewGraph = PROCEDURE [2]( t: SYSTEM.int );
    TPushOutEdge = PROCEDURE [2];
    TPopOutEdge  = PROCEDURE [2];
    TNextOutEdge = PROCEDURE [2] (VAR e: SYSTEM.int): BOOLEAN;
    TGetOutEdges = PROCEDURE [2] ( e: SYSTEM.int);
    TDeleteEdge  = PROCEDURE [2] ( e: SYSTEM.int);
    TSetTypeShape = PROCEDURE [2]  ( typenum, newShape: SYSTEM.int );
    TSetTypeLV_PAR = PROCEDURE [2] ( typenum, newLV_PAR: SYSTEM.int );
    TSetTypeB_STYLE = PROCEDURE [2] ( typenum, newB_STYLE: SYSTEM.int );

VAR
    irNodeType*, labelNode*, labelExt*,
    woodEdgeType*, woodEdgeLabel*,
    hostEdgeType*, hostLabelType*,
    pcNodeType*, pcNodeLabelType*,
    pcStructType*, pcStructLabelType*,
    pcObjectType*, pcObjectLabelType*,
    pcValueType*, pcValueLabelType*
                : SYSTEM.int;
    hmod : dllRTS.HMOD;

    NormalizeGraph *: TNormalizeGraph;
    SetVisCode_Ex*:TSetVisCode_Ex;
    SetLabelName *: TSetLabelName ;
    SetLabVal    *: TSetLabVal    ;
    AddLabel_Ex  *: TAddLabel_Ex  ;
    AddType      *: TAddType      ;
    GetVertex    *: TGetVertex    ;
    GetEdge      *: TGetEdge      ;
    EndWork      *: TEndWork      ;
    BeginWork    *: TBeginWork    ;
    SetFrTitle   *: TSetFrTitle   ;
    AddVertex    *: TAddVertex    ;
    AddEdge      *: TAddEdge      ;
    SaveGraph    *: TSaveGraph    ;
    CreateNewGraph *: TCreateNewGraph;
    PushOutEdge *: TPushOutEdge;
    PopOutEdge  *: TPopOutEdge ;
    NextOutEdge *: TNextOutEdge;
    GetOutEdges *: TGetOutEdges;
    DeleteEdge *: TDeleteEdge;
    SetTypeShape *: TSetTypeShape;
    SetTypeLV_PAR*: TSetTypeLV_PAR;
    SetTypeB_STYLE*: TSetTypeB_STYLE;

VAR
    STEP_Y :INTEGER;
    STEP_X :INTEGER;

TYPE Vertex = LONGINT;

PROCEDURE CountLPTs*(Nedges: LONGINT);
VAR i: LONGINT;
    e: hgl.PEdge;
    lpt: INTEGER;
    from, to: hgl.PVertex;
BEGIN
    FOR i := 0 TO Nedges-1 DO;
        e := GetEdge(i);
        from := GetVertex(e.from);
        to  :=  GetVertex(e.to);
        IF    (from.x >= to.x) AND (from.y >= to.y) THEN
            lpt := 2;
        ELSIF (from.x < to.x) AND (from.y >= to.y) THEN
            lpt := 1;
        ELSIF (from.x >= to.x) AND (from.y < to.y) THEN
            lpt := 3;
        ELSE
            lpt := 0;
        END;
        e.lpt := lpt;
    END;
END CountLPTs;



PROCEDURE RearrangeGraph*(Nnodes: LONGINT; _STEP_X, _STEP_Y: INTEGER);
VAR
    widthBoxes, heightBoxes, ranges : POINTER TO ARRAY OF LONGINT;
    processed :POINTER TO ARRAY OF BOOLEAN;
    PROCEDURE GetBox(node: Vertex):LONGINT;
    VAR
        child: Vertex;
        v: hgl.PVertex;
        nedge: SYSTEM.int;
        edge : hgl.PEdge;
--        str: ARRAY 7 OF CHAR;
        width : LONGINT;
    BEGIN
        IF processed[node] THEN
            RETURN 0; END;
        processed[node] := TRUE;
        GetOutEdges(node);
        IF NextOutEdge(nedge) THEN
            widthBoxes [node] := 0;
            heightBoxes[node] := 0;
            REPEAT
                edge := GetEdge(nedge);
                child := edge.to;
                IF ranges[child] = ranges[node]+1 THEN
                    PushOutEdge;
                    width := GetBox(child);
                    PopOutEdge;
                    v := GetVertex(child);
                    INC (widthBoxes [node], width);
                    IF   heightBoxes[node] < heightBoxes[child] THEN
                        heightBoxes[node] := heightBoxes[child];
                    END;
                END;
            UNTIL ~NextOutEdge(nedge);

            v := GetVertex(node);
            INC (heightBoxes[node], v.h*2 + STEP_Y);
            IF  widthBoxes [node] < v.w*2 + STEP_X THEN
                widthBoxes [node] :=v.w*2 + STEP_X;
            END;
        ELSE
            v := GetVertex(node);
            widthBoxes [node] := v.w*2 + STEP_X;
            heightBoxes[node] := v.h*2;
        END;
--        WholeStr.IntToStr (widthBoxes [node], str);
--        SetLabVal (hgl.ObjVertex, node, labelExt, str);
        RETURN widthBoxes [node];
    END GetBox;

    PROCEDURE PlaceBox(node: Vertex; x, y: LONGINT): LONGINT;
    VAR
        child: Vertex;
        v: hgl.PVertex;
        nedge : SYSTEM.int;
        edge : hgl.PEdge;
    BEGIN
        IF processed[node] THEN
            RETURN 0; END;
        processed[node] := TRUE;
        v := GetVertex(node);
        v.x := x + widthBoxes [node] DIV 2;
        v.y := y + v.h;
        INC(y, v.h*2 + STEP_Y);
        GetOutEdges(node);
        WHILE NextOutEdge(nedge) DO
            edge := GetEdge(nedge);
            child := edge.to;
            IF ranges[child] = ranges[node]+1 THEN
                PushOutEdge;
                INC (x, PlaceBox(child, x, y));
                PopOutEdge;
            END;
        END;
        RETURN widthBoxes [node];
    END PlaceBox;

    VAR
         changed:BOOLEAN;

    PROCEDURE GetRanges(node: Vertex; ancestors: ARRAY OF BOOLEAN);
    VAR
        nedge : SYSTEM.int;
        edge : hgl.PEdge;
        child: LONGINT;
        ancestorsWithMe: POINTER TO ARRAY OF BOOLEAN;
    i: LONGINT;
    BEGIN
(*
        IF processed[node] THEN
            RETURN; END;
        processed[node] := TRUE;
*)
        NEW (ancestorsWithMe,Nnodes);
        FOR i:= 0 TO Nnodes-1 DO
            ancestorsWithMe[i]  := ancestors[i];
        END;
        ancestorsWithMe[node] := TRUE;
        GetOutEdges(node);
        WHILE NextOutEdge(nedge) DO
            edge := GetEdge(nedge);
            child := edge.to;
            IF ~ancestors[child] & (ranges[child] < ranges[node] + 1) THEN
               changed := TRUE;
               ranges[child]:= ranges[node] + 1;
               PushOutEdge;
               GetRanges(child, ancestorsWithMe^);
               PopOutEdge;
            END;
        END;

    END GetRanges;
VAR
    i: LONGINT;
    emptyancestors: POINTER TO ARRAY OF BOOLEAN;
BEGIN
    IF Nnodes = 0 THEN
      RETURN
    END;
    STEP_X := _STEP_X;
    STEP_Y := _STEP_Y;
    NEW (widthBoxes, Nnodes);
    NEW (heightBoxes,Nnodes);
    NEW (ranges, Nnodes);
    FOR i := 0 TO Nnodes-1 DO
        ranges[i] := 0;
    END;

    NEW (processed,Nnodes);
    NEW (emptyancestors,Nnodes);
    FOR i:= 0 TO Nnodes-1 DO
        emptyancestors[i]  := FALSE;
    END;

    REPEAT
       FOR i:= 0 TO Nnodes-1 DO
           processed[i]  := FALSE;
       END;
       changed := FALSE;
       GetRanges(0, emptyancestors^);
    UNTIL ~changed;

    FOR i:= 0 TO Nnodes-1 DO
        processed[i]  := FALSE;
    END;
    IF GetBox(0) = 0 THEN END;;
    FOR i:= 0 TO Nnodes-1 DO
        processed[i]  := FALSE;
    END;
    IF PlaceBox(0, 0, 0)=0 THEN END;
END RearrangeGraph;

(*PROCEDURE RearrangeGraph2*;
TYPE
    OrderType = RECORD
        num: LONGINT;
        nodes: POINTER TO ARRAY OF ;
    END;
VAR
    n,m: ir.Node;
    t: ir.TSNode;
    order : POINTER TO ARRAY OF OrderType;
    ranges: POINTER TO ARRAY OF ir.INT;
    maxRange: ir.INT;
    i: SHORTINT;
    v : hgl.PVertex;
--    v.h := STEP_Y DIV 3;
--    v.w := STEP_X DIV 3;
BEGIN
    v := GetVertex(0);
    v.x := STEP_X;
    NEW (order,  ir.Nnodes);
    NEW (ranges, ir.Nnodes);
    FOR m := 0 TO ir.Nnodes-1 DO
        ranges[m] := 0;
        order[m].num := 0;
        NEW (order[m].nodes, ir.Nnodes );
    END;

    FOR t := ir.StartOrder+1 TO VAL(ir.TSNode,LEN(ir.Order^)-1) DO
        n := ir.Order[t];
        v := GetVertex(n);
        FOR i:=0 TO ir.Nodes^[n].NIn-1 DO
            IF (n # ir.Nodes^[n].In^[i]) & (ranges[n] < ranges[ir.Nodes^[n].In^[i]] + 1) THEN
               ranges[n]:= ranges[ir.Nodes^[n].In^[i]] + 1
            END;
        END;
    END;
    maxRange := 0;
    FOR t := ir.StartOrder TO VAL(ir.TSNode,LEN(ir.Order^)-1) DO
        n := ir.Order[t];
        IF maxRange < ranges[n] THEN
           maxRange:= ranges[n];
        END;

        v := GetVertex(n);
        v.y := STEP_Y * ranges[n];
        v.x := STEP_X * order[ranges[n]].num;
        order[ranges[n]].nodes[order[ranges[n]].num] := n;
        INC (order[ranges[n]].num);
    END;
END RearrangeGraph2;


*)

VAR wasInitialized: BOOLEAN;
PROCEDURE Init*;
BEGIN
    IF wasInitialized THEN RETURN END;
    wasInitialized := TRUE;
    hmod := dllRTS.LoadModule("hgl.dll");
    IF hmod = dllRTS.InvalidHandle THEN
        Printf.printf("hgl.dll not found\n");
        HALT(0);
    END;
        --dllRTS.GetProcAdr(hmod, "");
    SetVisCode_Ex:= SYSTEM.VAL(TSetVisCode_Ex, dllRTS.GetProcAdr(hmod, "SetVisCode_Ex"));
    SetLabelName := SYSTEM.VAL(TSetLabelName,  dllRTS.GetProcAdr(hmod, "SetLabelName"   ));
    SetLabVal    := SYSTEM.VAL(TSetLabVal,  dllRTS.GetProcAdr(hmod, "SetLabVal"        ));
    AddLabel_Ex  := SYSTEM.VAL(TAddLabel_Ex,  dllRTS.GetProcAdr(hmod, "AddLabel_Ex"        ));
    AddType      := SYSTEM.VAL(TAddType,  dllRTS.GetProcAdr(hmod, "AddType"             ) );
    GetVertex    := SYSTEM.VAL(TGetVertex,  dllRTS.GetProcAdr(hmod, "GetVertex"              ));
    GetEdge    := SYSTEM.VAL(TGetEdge,  dllRTS.GetProcAdr(hmod, "GetEdge"              ));
    EndWork      := SYSTEM.VAL(TEndWork,  dllRTS.GetProcAdr(hmod, "EndWork"                  ));
    BeginWork    := SYSTEM.VAL(TBeginWork,  dllRTS.GetProcAdr(hmod, "BeginWork"                  ));
    SetFrTitle   := SYSTEM.VAL(TSetFrTitle,  dllRTS.GetProcAdr(hmod, "SetFrTitle"                   ));
    AddVertex    := SYSTEM.VAL(TAddVertex,  dllRTS.GetProcAdr(hmod, "AddVertex"                      ));
    AddEdge      := SYSTEM.VAL(TAddEdge,  dllRTS.GetProcAdr(hmod, "AddEdge"                          ));
    SaveGraph    := SYSTEM.VAL(TSaveGraph,  dllRTS.GetProcAdr(hmod, "SaveGraph"                          ));
    CreateNewGraph:=SYSTEM.VAL(TCreateNewGraph, dllRTS.GetProcAdr(hmod, "CreateNewGraph"                       ));

    NormalizeGraph:= SYSTEM.VAL(TNormalizeGraph, dllRTS.GetProcAdr(hmod, "NormalizeGraph"));
    PushOutEdge:= SYSTEM.VAL(TPushOutEdge, dllRTS.GetProcAdr(hmod, "PushOutEdge"));
    PopOutEdge:= SYSTEM.VAL(TPopOutEdge, dllRTS.GetProcAdr(hmod, "PopOutEdge"));
    NextOutEdge:= SYSTEM.VAL(TNextOutEdge, dllRTS.GetProcAdr(hmod, "NextOutEdge"));
    GetOutEdges:= SYSTEM.VAL(TGetOutEdges, dllRTS.GetProcAdr(hmod, "GetOutEdges"));
    DeleteEdge:= SYSTEM.VAL(TDeleteEdge, dllRTS.GetProcAdr(hmod, "DeleteEdge"));
    SetTypeShape := SYSTEM.VAL( TSetTypeShape , dllRTS.GetProcAdr(hmod, "SetTypeShape"));
    SetTypeLV_PAR := SYSTEM.VAL( TSetTypeLV_PAR, dllRTS.GetProcAdr(hmod, "SetTypeLV_PAR"));
    SetTypeB_STYLE := SYSTEM.VAL( TSetTypeB_STYLE, dllRTS.GetProcAdr(hmod, "SetTypeB_STYLE"));
    BeginWork ();

    irNodeType := AddType (0, 'ir.Node');
    labelNode := AddLabel_Ex (irNodeType, 4);
    SetLabelName (irNodeType, labelNode, 'Node');
    labelExt := AddLabel_Ex (irNodeType, 4);
    SetLabelName (irNodeType, labelExt, 'Ext');
    SetVisCode_Ex (irNodeType, "\\{Node}", 0);
    SetVisCode_Ex (irNodeType, "\\{Ext}", 1);

    pcNodeType := AddType (0, 'pc.NODE');
--    SetTypeShape (pcNodeType, 4);
    pcNodeLabelType := AddLabel_Ex (pcNodeType, 4);
    SetLabelName (pcNodeType, pcNodeLabelType, 'Node');
    SetVisCode_Ex (pcNodeType, "\\{Node}", 0);

    pcStructType := AddType (0, 'pc.STRUCT');
    SetTypeShape (pcStructType, 2);
    pcStructLabelType := AddLabel_Ex (pcStructType, 4);
    SetLabelName (pcStructType, pcStructLabelType, 'Node');
    SetVisCode_Ex (pcStructType, "\\{Node}", 0);

    pcObjectType := AddType (0, 'pc.OBJECT');
    SetTypeShape (pcObjectType, 1);
    pcObjectLabelType := AddLabel_Ex (pcObjectType, 4);
    SetLabelName (pcObjectType, pcObjectLabelType, 'Node');
    SetVisCode_Ex (pcObjectType, "\\{Node}", 0);

    pcValueType := AddType (0, 'pc.VALUE');
    SetTypeShape (pcValueType, 3);
    pcValueLabelType := AddLabel_Ex (pcValueType, 4);
    SetLabelName (pcValueType, pcValueLabelType, 'Node');
    SetVisCode_Ex (pcValueType, "\\{Node}", 0);

    woodEdgeType  := AddType (2, 'woodEdge');
    SetTypeLV_PAR (woodEdgeType, 6CH );
    woodEdgeLabel := AddLabel_Ex (woodEdgeType, 4);
    SetLabelName (woodEdgeType, woodEdgeLabel, 'Edge');
    SetVisCode_Ex (woodEdgeType, "\\{Edge}", 0);

    hostEdgeType  := AddType (2, 'hostEdge');
    SetTypeB_STYLE (hostEdgeType, 3 );
    hostLabelType := AddLabel_Ex (hostEdgeType, 4);
    SetLabelName (hostEdgeType, hostLabelType, 'Host');
    SetVisCode_Ex (hostEdgeType, "\\{Host}", 0);


END Init;

BEGIN
    wasInitialized := FALSE;
END hgr.
