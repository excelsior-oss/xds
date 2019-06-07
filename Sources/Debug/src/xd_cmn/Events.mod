<* storage+ *>

(* Обработка очереди событий *)

IMPLEMENTATION MODULE Events;


<* IF DEFINED (xd_debug) & xd_debug THEN *>

IMPORT opt := Options;
IMPORT xs  := xStr;

FROM Printf IMPORT printf;

<* IF DEST_XDS THEN *>
IMPORT Threads;
<* END *>

<* END *>


CONST
  INITIAL_EVENTS_QUANTITY = 32;


TYPE
  QUEUE = POINTER TO ARRAY OF EVENT;

VAR
  Queue: QUEUE;         (* Очередь событий                         *)
  Head, Tail: CARDINAL; (* Номера начального и последнего элемента *)


PROCEDURE QuantityEvents (): CARDINAL;
BEGIN
  IF Head <= Tail THEN
    RETURN Tail - Head;
  ELSE
    RETURN Tail + HIGH(Queue^)+1 - Head;
  END;    
END QuantityEvents;


(* Очеpедь пуста? Если пуста - TRUE, непуста - FALSE *)
PROCEDURE QueueIsEmpty () : BOOLEAN;
BEGIN
  RETURN Head = Tail;
END QueueIsEmpty;


<* PUSH *>
<* -CHECKINDEX *>

PROCEDURE ReallocQueue();
VAR
  size: CARDINAL;
  i   : CARDINAL;
  tmp : QUEUE;
BEGIN
  size := HIGH(Queue^)+1;
  NEW (tmp, 2*size);
  FOR i := 0 TO size-1 DO
    tmp^[i] := Queue^[i];
  END;
  DISPOSE (Queue);
  Queue := tmp;
END ReallocQueue;


(* Добавить событие в очеpедь, при переполнении очереди вернет FALSE *)
PROCEDURE AddEvent (event: EVENT): BOOLEAN;
VAR
  max, tail, i, offs: CARDINAL;
BEGIN
  Queue^[Tail] := event;
  max := HIGH(Queue^);
  tail := Tail;
  IF tail = max THEN
    Tail := 0;
  ELSE
    INC(Tail);
  END;
  IF Head = Tail THEN
    ReallocQueue();
    IF Tail = 0 THEN
      Tail := tail+1;
    ELSE
      offs := HIGH(Queue^)-max;
      FOR i := max TO Head BY -1 DO
        Queue^[i+offs] := Queue^[i];
      END;
      INC (Head, offs);
    END;
  END;
  RETURN NOT QueueIsEmpty();
END AddEvent;


(* Получить событие, он помещается в LastEvent и удаляется очереди *)
PROCEDURE GetEvent;
BEGIN
  ASSERT (NOT QueueIsEmpty());
  LastEvent := Queue^[Head];
  IF Head = HIGH(Queue^) THEN
    Head := 0;
  ELSE
    INC(Head);
  END;
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug (opt.Another) THEN
    PrintEvent (LastEvent);
  END;
 <* END *>
END GetEvent;

<* POP *>

(* Возвращеющая TRUE, если в очереди есть событие *)
(* указаного типа, иначе вернет FALSE             *)
PROCEDURE QueryEvent (event_type: EVENT_TYPE): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  i := Head;
  LOOP
    IF i = Tail THEN
      RETURN FALSE;
    END;
    IF Queue^[i].Event = event_type THEN
      RETURN TRUE;
    END;
    IF i = HIGH(Queue^) THEN
      i := 0;
    ELSE
      INC(i);
    END;
  END;
END QueryEvent;

(* Очищает текущее содержимое очереди событий *)
PROCEDURE ClearQueue;
BEGIN
  Head := 0;
  Tail := 0;
  LastEvent.Event := InternalError;
END ClearQueue;



<* IF DEFINED (xd_debug) & xd_debug THEN *>

PROCEDURE PrintEvent (event: EVENT);
VAR
  i: CARDINAL;
  buf: xs.String;
BEGIN
  printf('===> EVENT\n');
  WITH event DO
    printf('IP=%$8X;\nevent=', IP);
    CASE Event OF
    | SingleStep:
      printf('SingleStep');

    | InternalError:
      printf('InternalError: ErrorNo=%u, ErrorContext=%u', ErrorNo, ErrorContext);

    | Exception:
      printf('Exception: Exception_ID=');      
      CASE Exception_ID OF
      | OutOfMemory     : printf('OutOfMemory');
      | WriteProtected  : printf('WriteProtected');
      | ProgramException: printf('ProgramException');
      | UserException   : printf('UserException');
      END;
      printf(', %$8X, %$8X, %$8X', CARDINAL(XCPT_INFO_1), CARDINAL(XCPT_INFO_2), CARDINAL(XCPT_INFO_3));

    | Call:
      printf('Call, CallAddr=%$8X', CallAddr);

    | Return:
      printf('Return, ReturnAddr=%$8X', ReturnAddr);

    | BreakpointHit:
      printf('BreakpointHit, BreakpointInd=%u', BreakpointInd);

    | MemoryAccess:
      printf('MemoryAccess, MemAccess_Ind=%u\n', MemAccess_Ind);

    | CompCreated:
      printf('CompCreated, ');
      WITH Component DO
        printf('Name=%s, Handle=%X\n', full_name, Handle);
        printf('DebugInfoTag=%s\n', DebugInfoTag);
        printf('N_Objects=%u\n', N_Objects);
        IF N_Objects # 0 THEN
          FOR i := 0 TO N_Objects-1 DO
            WITH Objects^[i] DO
              <* IF TARGET_x86 THEN *>
              printf('RelocationBase=%$8X, ', RelocationBase);
              <* END *>
              printf ('Begin=%$8X, End=%$8X\n', Begin, End);
            END;
          END;
        END;
      END;

    | CompDestroyed:
      printf('CompDestroyed, Handle=%X', Handle);

<* IF DEST_K26 THEN *>

      CASE MemAccess_Type OF
      | Nothing  : printf('Nothing');
      | Read     : printf('Read');
      | Write    : printf('Write');
      | ReadWrite: printf('ReadWrite');
      END;
      printf(' MemAccess_Addr=%$8X, MemAccess_Len=%u', MemAccess_Addr, MemAccess_Len); 
      
    | RegisterAccess:
      printf('RegisterAccess, RegAccess_Ind=%u\n', RegAccess_Ind);
      CASE RegAccess_Type OF
      | Nothing  : printf('Nothing');
      | Read     : printf('Read');
      | Write    : printf('Write');
      | ReadWrite: printf('ReadWrite');
      END;
      printf(' RegAccess_RegNo=%u, RegAccess_Len=%u', RegAccess_RegNo, RegAccess_Len); 

    | DeviceAccess:
      printf('DeviceAccess, ');
      CASE DevAccess_Type OF
      | Nothing  : printf('Nothing');
      | Read     : printf('Read');
      | Write    : printf('Write');
      | ReadWrite: printf('ReadWrite');
      END;
      printf('DevAccess_DevNo=%u, DevAccess_RegNo=%u, DevAccess_Len=%u',
              DevAccess_DevNo,    DevAccess_RegNo,    DevAccess_Len);
      
<* ELSIF DEST_XDS THEN *>

    | ThreadCreated:
     <* IF TARGET_OS = "WINNT" THEN *>
      Threads.GetThreadDescription (Thread, buf);
      printf('ThreadCreated: %s', buf);
     <* ELSE *>
      printf('ThreadCreated');
     <* END *>
    
    | ThreadDestroyed:
      printf('ThreadDestroyed: inx = %d', Thread);
     
<* END *>
      
    END;
    printf('\n');
  END;  
  printf('<=== EVENT\n');
END PrintEvent;

<* END *>


BEGIN
  Head := 0;
  Tail := Head;
  NEW (Queue, INITIAL_EVENTS_QUANTITY);
  ASSERT (Queue # NIL);
END Events.
