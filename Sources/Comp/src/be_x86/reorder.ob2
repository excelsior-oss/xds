MODULE reorder;

IMPORT
       ocir,
       OcGraph,
       ss    := ScheStr,
       r486  := Rrd486,
       r  := RrdUV,
       at := opAttrs;

VAR   DoReOrder0* : PROCEDURE (sg : ocir.Segment);

PROCEDURE DoReOrder*(sg : ocir.Segment);
BEGIN
    ASSERT (sg <> NIL);
    REPEAT
        DoReOrder0(sg);
        sg := sg^.next;
    UNTIL sg = NIL;
END DoReOrder;

PROCEDURE InitReordering*();
BEGIN
   ocir.InitInOut;
   OcGraph.InitOcGraph;
   CASE at.CPU OF
   | at.i386, at.iGeneric, at.iPentium, at.iPentiumPro :
       DoReOrder0 := r.DoReOrder;
   | at.i486 :
       DoReOrder0 := r486.DoReOrder;
   END;
END InitReordering;

CONST DoRemove* = ss.DoRemove;

END reorder.
