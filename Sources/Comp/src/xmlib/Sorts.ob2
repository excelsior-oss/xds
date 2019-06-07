(* AVY: Процедуры для сортировки и поиска информации. *)

MODULE Sorts;

TYPE
  COMPARE * = PROCEDURE (i: LONGINT; j: LONGINT): BOOLEAN; (* Процедура сравнения *)
  SHAKE   * = PROCEDURE (i: LONGINT; j: LONGINT);          (* Процедура обмена    *)


(* Сортировка Шелла                                 *)
(* Сортирует N-элементное множество                 *)
(* Сортировка неустойчивая, меняет вторичный ключ *)
PROCEDURE ShellSort *(N: LONGINT; Compare: COMPARE; Shake: SHAKE);
VAR
  i, j, gap: LONGINT; (* gap - интервал между сравниваемыми элементами     *)
BEGIN
  (* Цикл управления интервалом *)
  gap := N DIV 2;
  WHILE gap > 0 DO
    (* Цикл сравнения каждой пары *)
    i := gap;
    WHILE i < VAL(LONGINT,N) DO
      (* Цикл перестановки неупорядоченной пары *)
      j := i - gap;
      WHILE (j >= 0) AND Compare (j, j+gap) DO
        Shake (j, j+gap);
        j := j-gap;
      END;
      INC(i);
    END;
    gap := gap DIV 2;
  END;
END ShellSort;


(* Быстрая сортировка                             *)
(* Сортирует N-элементное множество               *)
(* Сортировка неустойчивая, меняет вторичный ключ *)
PROCEDURE qSort *(N: LONGINT; Less: COMPARE; Swap: SHAKE);

  PROCEDURE Sort (l, r: LONGINT);
  VAR
    i, j: LONGINT;
  BEGIN
    WHILE r > l DO
      i := l+1;
      j := r;
      WHILE i <= j DO
        WHILE (i <= j) AND NOT Less(l,i) DO
          INC(i);
        END;
        WHILE (i <= j) AND Less(l,j) DO
          DEC(j);
        END;
        IF i <= j THEN
          IF i # j THEN
            Swap (i, j);
          END;
          INC(i);
          DEC(j)
        END;
      END;
      IF j # l THEN
        Swap(j,l)
      END;
      IF j+j > r+l THEN
        Sort (j+1, r);
        r := j-1;
      ELSE
        Sort (l, j-1);
        l := j+1;
      END;
    END;
  END Sort;

BEGIN
  IF N > 0 THEN
    Sort (0, N-1);
  END;
END qSort;


TYPE
  (* Процедура сравнения двух элементов *)
  (* -1: текущий "меньше" чем искомый   *)
  (*  0: элементы "равны"               *)
  (* +1: текущий "больше" чем искомый   *)
  BINARY_COMPARE * = PROCEDURE (i: LONGINT): LONGINT;

(* Бинарный поиск в N-элементном упорядоченном множестве *)
(* Возвращает номер элемента, если он найден, иначе 0    *)
PROCEDURE BinaryFind *(N: LONGINT; Compare: BINARY_COMPARE; VAR i: LONGINT): BOOLEAN;
VAR
  j, k: LONGINT;
BEGIN
  j := 0;
  k := N;
  WHILE j < k DO
    i := (j+k) DIV 2;
    CASE Compare(i) OF
    | -1 : j := i+1;
    |  0 : RETURN TRUE;
    | +1 : k := i;
    ELSE
      ASSERT(FALSE);
    END;
  END;
  RETURN FALSE;
END BinaryFind;


END Sorts.