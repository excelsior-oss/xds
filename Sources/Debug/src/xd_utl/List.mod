<* STORAGE+ *>

IMPLEMENTATION MODULE List;


TYPE
  List = POINTER TO ListDesc;

  ListDesc = RECORD
               next: List;
               info: Info;
             END;

------------------------------------------------------------------------------

PROCEDURE New (): List;
VAR
  list: List;
BEGIN
  NEW (list);
  list^.next := NIL;
  list^.info := NIL;
  RETURN list;
END New;


PROCEDURE Dispose (VAR list: List);
BEGIN
  DISPOSE (list);
END Dispose;


PROCEDURE Add (VAR list: List): List;
VAR
  tmp: List;
BEGIN
  tmp := New ();
  tmp^.next := list;
  list := tmp;
  RETURN list;
END Add;


PROCEDURE Delete (VAR head: List; list: List);
VAR
  tmp: List;
BEGIN
  ASSERT (head # NIL);
  ASSERT (list # NIL);
  IF head = list THEN
    head := list^.next;
  ELSE
    tmp := head;
    WHILE (tmp # NIL) AND (tmp^.next # list) DO
      tmp := tmp^.next;
    END;
    IF tmp # NIL THEN
      tmp^.next := list^.next;
    END;
  END;
END Delete;


PROCEDURE Next (list: List): List;
BEGIN
  ASSERT (list # NIL);
  RETURN list^.next;
END Next;


PROCEDURE Iterate (list: List; operator: Operator): List;
BEGIN
  ASSERT (operator # NIL);
  WHILE (list # NIL) AND operator (list) DO
    list := list^.next;
  END;
  RETURN list;
END Iterate;

------------------------------------------------------------------------------

PROCEDURE Assign (list: List; info: Info);
BEGIN
  ASSERT (list # NIL);
  list^.info := info;
END Assign;


PROCEDURE Clear (list: List);
BEGIN
  ASSERT (list # NIL);
  list^.info := NIL;
END Clear;


PROCEDURE Get (list: List): Info;
BEGIN
  ASSERT (list # NIL);
  RETURN list^.info;
END Get;


PROCEDURE Find (list: List; info: Info): List;
BEGIN
  WHILE (list # NIL) AND (list^.info # info) DO
    list := list^.next;
  END;
  RETURN list;
END Find;

------------------------------------------------------------------------------


END List.
