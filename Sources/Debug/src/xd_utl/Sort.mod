IMPLEMENTATION MODULE Sort;



(* Сортировка Шелла  *)
PROCEDURE Shell (N: CARDINAL; Compare: COMPARE; Shake: SHAKE);
VAR
  i, j, gap: INTEGER;  (* gap - интервал между сравниваемыми элементами     *)
BEGIN
  (* Цикл управления интервалом *)
  gap := N DIV 2;
  WHILE gap > 0 DO
    (* Цикл сравнения каждой пары *)
    i := gap;
    WHILE i < VAL(INTEGER,N) DO
      (* Цикл перестановки неупорядоченной пары *)
      j := i - gap;
      WHILE (j >= 0) AND Compare(j,j+gap) DO
        Shake(j,j+gap);
        j := j-gap;
      END;
      INC(i);
    END;
    gap := gap DIV 2;
  END;
END Shell;

PROCEDURE qsort(N : CARDINAL;Compare: COMPARE; swap: SHAKE);
VAR
  right : INTEGER;
PROCEDURE sort( left, right: INTEGER);
VAR i, last : INTEGER;
BEGIN
  IF (left >= right) THEN
    RETURN;
  END;
  swap(left, (left + right) DIV 2);
  last := left;
  FOR i:= left + 1 TO right DO
    IF  Compare(left,i) THEN
      INC(last);
      swap(last,i);
    END;
  END;
  swap(left, last);
  sort(left, last -1);
  sort(last + 1, right);
 END sort;
BEGIN
 right:= VAL(INTEGER, N) - 1;
 sort(0,right);
END qsort;

(* Бинарный поиск в N-элементном упорядоченном множестве *)
(* Возвращает номер элемента, если он найден, иначе 0    *)
PROCEDURE BinaryFind (N: CARDINAL; Compare: BINARY_COMPARE;
                      VAR i: CARDINAL): BOOLEAN;
VAR
  j, k: CARDINAL;
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

END Sort.
