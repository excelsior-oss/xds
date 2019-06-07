/****************************************************************/
/* <oemit.c> 30 августа 94  последнее изменение:   11 марта 95  */
/*    Процедуры сборки выходного текста конвертором BurBeG      */
/*    по внутреннему представлению входной грамматики           */
/****************************************************************/
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "oburg.h"              /* Константы, собственные типы  */
                                /* и спецификации всех процедур */
                                /*      Входная грамматика:     */
int ntnumber = 0;               /* общее число нетерминалов     */
Nonterm start=NULL;             /* начальный нетерминал         */
Term terms=NULL;                /* список терминалов            */
Nonterm nts=NULL;               /* список нетерминалов          */
Rule rules=NULL;                /* список правил                */
int nrules=0;                   /* общее число правил           */
int costvl=1;                   /* общее число стоимостей       */
int *auxtype;                   /* классы функций стоимостей    */
                                /*     Внештекстовые опции:     */
char *pronmac[STSIZE];          /* вектор макросов              */

/****************************************************************/
/*  Сборка основного текста селектора на языке Оберон-2 или СИ: */
/****************************************************************/
void emitnames(Nonterm nts,   /* список нетерминалов      */
                     int ntnumber)  /* число нетерминалов       */
{
  Nonterm p;                        /* очередной нетерминал     */
    print ("<* IF ~nodebug THEN *>\nTYPE ArrayOfString = ARRAY NT OF ARRAY 20 OF CHAR;\n");
    print ("CONST\n    NTName* = ArrayOfString {\n      'XXX'");
    for (p = nts; p != NULL; p = p->link)
    {
      print (",\n      '%S'", p );
    }
    print("\n    };\n<* END *>\n\n");
}

void emitdmodule(
                 char *b,      /* имя файла и модуля            */
                 int l        /* длина этого имени             */
                )
{
  print ("-- source grammar = %s\n<*+WOFF*>\n", b);
  print("MODULE BurgNT;\nIMPORT ir, SYSTEM;\n",b);
//  b[l]='d';
  emitdefsd(nts, ntnumber);    /* сборка определений нетерм.   */
  emitstruct(nts, ntnumber);  /* сборка вых.структуры state   */
  emitnames(nts, ntnumber);
  print ("END BurgNT.\n", b);
}

/****************************************************************/
void emitnmodule(
                 char *b,      /* имя файла и модуля            */
                 int l        /* длина этого имени             */
                )
{
  print ("-- source grammar = %s\n<*+WOFF*>\n", b);
  print("MODULE BurgTables;\n\nIMPORT ir, RD := RDefs, BurgNT, SYSTEM;\n",b);
//  b[l]='d';

  emitnts(rules, nrules);     /* сборка списка шабл. нетерм.  */
  emitkids(rules, nrules);    /* сборка процедуры burm_kids   */
  emitterms(terms);           /* сборка структур терминалов   */

  print ("END BurgTables.\n", b);
}

/****************************************************************/

int emitimodule()
{
Nonterm p;                      /* очередной нетерминал         */
    if (start)                  /* отметка всех нетерминалов,   */
      ckreach(start);           /* достижимых из начального     */
    for (p = nts; p; p = p->link)
      if (!p->reached)
        error("недостижимый нетерминал `%s'\n", p->name);
//    emitheader();               /* сборка ALLOC и burm_assert   */
//    emitdefs(nts, ntnumber);    /* сборка определений нетерм.   */
//    emitstruct(nts, ntnumber);  /* сборка вых.структуры state   */
 //d    emitterms(terms);           /* сборка структур терминалов   */
    if (Iflag) emitstring(rules); /*сборка масс. стоим. и правил*/
//    emitrule0(nts);             /* сборка определений сп.правил */
 //d    emitnts(rules, nrules);     /* сборка списка шабл. нетерм.  */
//    emitrule(nts);              /* сборка сп.правил + burm_rule */
//  emitclosure (nts, b, j-2, bf1);
//    emitstate(terms, start, ntnumber,b, bf2); }
    return errcnt;
}

/****************************************************************/
/* emitaction - сборка заголовка процедуры action               */
/****************************************************************/
void emitactions()
{
  print("PROCEDURE %Pactions* (c: BurgNT.Rule; n: RD.DAGNODE);\n"
        "BEGIN\n"
        "  CASE c OF\n");
}

/****************************************************************/
/* emitactrule - сборка фрагмента процедуры action из           */
/*               внешнего текста с параметрами                  */
/****************************************************************/
void emitactrule(int x,         /* внешний номер правила        */
                Tree p)         /* шаблон правила               */
{
char *stbp[DIMSTM];             /* стек указателей тел макросов */
register m;                     /* индекс стека макросов        */
//register i;                     /* счетчики литер               */
int n=0;                        /* номер макроопределения       */
char *t;                        /* адрес копии фрагмента        */
char names[PVSIZE][PLSIZE];     /* таблица имен меток           */
int fspace=0;                   /* признак пропуска пробела     */
  t = bp;
  while (* t == ' ' || * t == '\r' || * t == '\n') t ++;
  if (* t == '}') {
    bp = ++ t;
        return;
  }
//  print("%1(* : %T *)\n", p );
  
  print("    |BurgNT.Rule{ %d }:\n%1 ", x);
  emitactpar(p,"n",names);      /* инициация таблицы параметров */
  m=0;
  while(*bp!='}')               /* их внешними именами          */
  {
    switch(*bp)
    {
      case '\n':                /* новая строка вх. текста      */
nl:     *bp=0; get(); --bp;     /* чтение новой строки          */
/*%1*/
        print("\n"); fspace=1; continue;
      case '@': t=bp;           /* ссылка на метку @[INT]       */
        for(n=0, ++bp; isdigit(*bp); ++bp) n=n*10+(*bp-'0');
        if(n>=PVSIZE)
        {
          error("Некорректный номер параметра: %s\n",t); goto cl;
        }
        print("%s",&names[n][0]); /* внешнее имя параметра      */
        fspace=0; continue;
      case '%':
        t = bp;
        print(" %P");
        for(++bp; isalpha(*bp); ++bp)
          print("%c", *bp);
//        print("_NT");
        fspace=0; continue;

      case '$': t=bp;           /* макроподстановка $[INT]      */
        for(n=0, ++bp; isdigit(*bp); ++bp) n=n*10+(*bp-'0');
        if(n>=STSIZE)
        {
          error("Некорректный номер макроса: %s\n",t); goto cl;
        }
        if(m<DIMSTM)
        {
          stbp[m++]=bp; bp=pronmac[n];
        }
        else print("%s",pronmac[n]); /* тело макроопределения   */
        fspace=0; continue;
      case 0:
        if(m>0)
        {
          bp=stbp[--m]; continue;
        }
        else goto nl;
      case ' ': case '\t': ++bp;
//        if(!fspace)
        print(" ");
        continue;
      default:                  /* простое копирование литеры   */
      cl: print("%c",*bp++);
          fspace=0; continue;
    }
  }
  print("\n"); ++bp;
}

/****************************************************************/
/* emitactpar - сборка процедурных фрагментов - имен            */
/* отмеченных узлов шаблона как параметров                      */
/* на соответствующих узлах покрываемого дерева                 */
/****************************************************************/
static void emitactpar(Tree t,  /* корень дерева шаблона        */
                       char *v, /* выходное имя узла об.дерева  */
                       char names[PVSIZE][PLSIZE])
                                /* таблица имен меток           */
{
//register i;                     /* счетчик литер                */
  if(t->par>=0)                 /* узел шаблона отмечен         */
  {
    if(strlen(v)>=PLSIZE)
    {
      error("Длина имени параметра %d недопустимо велика",t->par);
      return;
    }
    if(t->par<PVSIZE) sprintf(&names[t->par][0],"%s\0",v);
    else error("Номер параметра %d недопустимо велик",t->par);
  }
  if(t->left) {
    char bs[BFSIZE];
    sprintf(bs,"%s.l",v);
    emitactpar(t->left,bs,names);
  }
  if(t->right) {
    char bs[BFSIZE];
    sprintf(bs,"%s.r",v);
    emitactpar(t->right,bs,names);
  }
}

/****************************************************************/
/* reach - отметка всех нетеминалов в дереве t как достижимых   */
/****************************************************************/
static void reach(Tree t)
{
  Nonterm p = t->op;
  if (p->kind == NONTERM)
   if (!p->reached) ckreach(p);
  if (t->left)  reach(t->left);
  if (t->right) reach(t->right);
}
/****************************************************************/
/* ckreach - отметка всех нетерминалов, достижимых из p         */
/****************************************************************/
static void ckreach(Nonterm p)
{
  Rule r;                       /* Очередное правило            */
  p->reached = 1;
  for (r = p->rules; r; r = r->decode) reach(r->pattern);
}

/****************************************************************/
/* emitaux -  сборка текста дополнительных стоимостей правила   */
/*            в составе блока case функции state                */
/****************************************************************/
static void emitaux(Rule r,     /* правило                      */
                    char *b)    /* имя файла _d                 */
{
register i,j;                   /* счетчик                      */
int m;                          /* число аргументов ф.стоимости */
  for(i=0; i<costvl-1; ++i)
  {
    print("%3c%d := ",i+1);
    switch(auxtype[i])
    {
      case 0:
        if(r->pattern->left)  emitcost1(r->pattern->left, "l",i,b);
        if(r->pattern->right) emitcost1(r->pattern->right,"r",i,b);
        break;
      case 1:
        m=0;
        if(r->pattern->left)  emitcost2(r->pattern->left, "l",i+1,&m,b);
        if(r->pattern->right) emitcost2(r->pattern->right,"r",i+1,&m,b);
        if(m>0) print(",0");
        for(j=0; j<m; ++j) print(")"); if(m>0) print("+");
        break;
    }
    print("%d;\n", r->auxc[i]);
  }
}

static void print_tabs (int n)
{
        while (n) {
                print ("\t");
                n --;
        }
}

/****************************************************************/
/* emitcase - сборка одного case в составе функции state для    */
/*            одного входного терминала, управляющего разбором  */
/* и обеспечивающего вычисление разметки минимальных покрытий   */
/* деревьев, корнем которых является данный терминал, для       */
/* всех нетерминалов, корнем шаблона которых является данный    */
/* терминальный символ                                          */
/****************************************************************/
static void emitcase(   Term p,         /* входной терминал     */
                        int ntnumber,   /* число нетерминалов   */
                        char *b,        /* имя ядра селектора   */
                        int l)          /* длина имени файла    */
{
  Rule r;                               /* текущее правило      */
  int i;
  int ntabs;
  char names[PVSIZE][PLSIZE];           /* имена параметров     */

  print ("\n"
         "PROCEDURE newstate_%S* (n: RD.DAGNODE); (* %d *)\n"
         "VAR     c0:   LONGINT;\n"
         "        p:    ir.TriadePtr;\n"
         "        l, r: RD.DAGNODE;\n",
         p, p -> esn
        );
  if (costvl > 1) {
    print ("VAR c1");
    for (i = 1; i < costvl - 1; ++ i)
      print (",c%d", i + 1);
    print (": INTEGER;\n");
  }

  print ("BEGIN\n"
         "%1n.cost := MaxCost;\n"
         "%1l := n.l;\n"
         "%1r := n.r;\n"
         "%1p := n.tr;\n"
    );

  switch (p->arity) {
    case -1:
    case 0:
    case 1:
    case 2:  break;
    default: assert(0);
  }

  for (r = p -> rules; r != NULL; r = r -> next) {  /*для всех правил с этим*/
                                                    /*терминалом в качестве */
                                                    /*корня дерева шаблона  */
    print("\n(* %R  %d *)\n", r, r->ern );
    ntabs = 1;
    switch (p -> arity) {
      case -1:
      case  0:
        if (r->cond || r -> dyncost)
          emitactpar (r -> pattern, "n", names);        /*связыв.параметров*/
        if (r -> cond)
          emitcond (r -> cond, ntabs ++, names);        /*дополнит. условие*/
        if (costvl > 1)
          emitaux (r, b);                       /* дополнительные стоимости */
        print_tabs (ntabs);
        print ("c0 :=");
        break;                      /* здесь нет стоимости ветвей*/
      case 1:
        if (r -> pattern -> nterms > 1) {
          print_tabs (ntabs);
          print ("IF ");
          emittest (r -> pattern -> left, ntabs * 8 + 3, 0, "l", " ");
          print_tabs (ntabs ++);
          print ("THEN\n");
        }
        if (r -> cond || r -> dyncost)
          emitactpar (r -> pattern, "n", names);      /*связыв.параметров*/
        if (r -> cond)
          emitcond (r -> cond, ntabs ++, names);      /*дополнит. условие*/
        if (costvl > 1)
          emitaux (r, b);                     /* дополнительные стоимости */
        print_tabs (ntabs);
        print ("c0 := ");
        emitcost (r -> pattern -> left, "l", b);
        break;
      case 2:
        if (r -> pattern -> nterms > 1) {
          int was_text;
          print_tabs (ntabs);
          print ("IF ");
          was_text = emittest (r -> pattern -> left, ntabs * 8 + 3, 0, "l",
                               r -> pattern -> right -> nterms ? "&": " ");
          emittest (r -> pattern -> right, ntabs * 8 + 3, was_text, "r", " ");
          print_tabs (ntabs ++);
          print ("THEN\n");
        }
        if (r -> cond || r -> dyncost)
          emitactpar (r -> pattern, "n", names);        /*связыв.параметров*/
        if (r -> cond)
          emitcond (r -> cond, ntabs ++, names);        /*дополнит. условие*/
        if (costvl > 1)
          emitaux (r, b);                       /* дополнительные стоимости */
        print_tabs (ntabs);
        print("c0 := ");
        emitcost (r -> pattern -> left,  "l", b);
        emitcost (r -> pattern -> right, "r", b);
        break;
      default:
        assert (0);
    }
    /* Cобственная основная стоимость правила и учет числа      */
    /* использований данного результата:                        */
    print ("%d;\n", r -> cost);
    emitrecord (ntabs == 1 ? "\t" : ntabs == 2 ? "\t\t" : "\t\t\t",
                r, b, l, 1, names);  /* сборка текста для выб. прав.*/
    if (r -> cond != NULL) {
      print_tabs (-- ntabs);
      print ("END;\n");
    }
    if (r -> pattern -> nterms > 1) {
      print_tabs (-- ntabs);
      print ("END;\n");
    }
  }
  print ("END newstate_%S;\n", p );
}


/****************************************************************/
/* emitclosure - сборка процедур closureX, обеспечивающих выбор */
/* цепных правил для всех нетерминалов X входной грамматики     */
/****************************************************************/
void emitclosure(Nonterm nts,  /* список нетерминалов           */
                 char *b,      /* имя файла и модуля            */
                 int l,        /* длина этого имени             */
                 char *import) /* строка для списка импорта */
{
Nonterm p;                              /* текущий нетерминал   */
register int i;                         /* счетчик              */
char names[PVSIZE][PLSIZE];             /* имена параметров     */

//  print ("<*+WOFF*>\n");
//  print("MODULE %s;\n\n",b);
//  b[l-1]='d';
//  print("IMPORT D := %s, %s;\n",b, import);
//  b[l-1]='c';
  for (p = nts; p!=NULL; p = p->link)   /* прототипы процедур   */
    if (p->chain!=NULL)                 /* у данного нетерминала*/
                                        /* есть цепные правила  */
      {
        print(
    "PROCEDURE ^ %Pclosure_%S (n: RD.DAGNODE; ic0: LONGINT", p);
        for(i=0; i<costvl-1; ++i) print("; ic%d: INTEGER",i+1);
        print(");\n");
      }
  print("\n");
  for (p = nts; p!=NULL; p = p->link)   /* собственно процедуры */
    if (p->chain !=NULL)
    {
      Rule r;                           /* очередное правило    */

      print("PROCEDURE %Pclosure_%S* (n: RD.DAGNODE; ic0: LONGINT", p);
      for(i=0; i<costvl-1; ++i) print("; ic%d: INTEGER",i+1);
      print(");\n"
        "VAR c0: LONGINT;\n");
      if(costvl>1)
        {
          print("VAR c1");
          for(i=1; i<costvl-1; ++i) print(", c%d", i+1);
          print(": INTEGER;\n");
        }
      print("BEGIN\n");
      for (r =p->chain;r!=NULL;r=r->chain)/* все цепные правила */
      {                                   /* данного нетерминала*/
        if(r->cond!=NULL || r->dyncost!=NULL)
              emitactpar(r->pattern,"n",names);  /*связыв.параметров*/
        if(r->cond!=NULL)
                emitcond (r->cond, 0, names);         /*дополнит. условие*/
        if(r->cost!=NULL) /* цепное правило ненулевой стоимости */
          print("%1c0 := ic0 + %d;\n",r->cost);
        else            /* цепное правило нулевой стоимости     */
          print("%1c0 := ic0;\n");
        for(i=0; i<costvl-1; ++i)
        {
          switch(auxtype[i])
          {
            case 0: case 1:
          if(r->auxc[i]!=NULL)
                 /*правило ненулев. доп.стоимости  */
                print("%1c%d := ic%d + %d;\n",i+1,i+1,r->auxc[i]);
              else           /*правило нулевой стоимости       */
                print("%1c%d := ic%d;\n",i+1,i+1);
              break;
            case 2:
              print("%1c%d :=%d;\n",i+1,r->auxc[i]);
              break;
          }
        }
        emitrecord("\t",r,"D.",l,0,names);
        if(r->cond!=NULL)
          print ("%1END;\n");
      }
      print ("END %Pclosure_%S;\n\n", p);
    }
//  print ("END %s.\n", b);
}

/****************************************************************/
/* emitcond - сборка текста дополнительного условия             */
/****************************************************************/
static void emitcond(char *s,        /* текст внешнего условия  */
                     int ntabs,
                     char names[PVSIZE][PLSIZE]) /* имена парам.*/
{
  print_tabs (ntabs);
  print ("IF ");
  if (emitxtxt (s, ntabs * 8 + 3, names)) {
    print ("\n");
    print_tabs (ntabs);
    print ("THEN\n");
  }
  else
    print (" THEN\n");
}

/****************************************************************/
/* emitcost - сборка процедурного фрагмента для вычисления      */
/*            основной стоимости правила с шаблоном t           */
/****************************************************************/
static void emitcost (Tree t,           /* дерево шаблона       */
                      char * v,         /* текстовое имя корня  */
                      char * b)         /* имя файла _d         */
{
  Nonterm p = t -> op;                  /* текущий узел дерева  */

  if (p -> kind == TERM) {
    if (t -> left)
      emitcost (t -> left,  stringf ("%s.l", v), b);
    if (t -> right)
      emitcost (t -> right, stringf ("%s.r", v), b);
  }
  else
    print ("VAL (LONGINT, %s.cost [%P%S]) + ", v, p);
}

/****************************************************************/
/* emitcost1 - сборка процедурного фрагмента для вычисления     */
/*             дополнительной стоимости правила с шаблоном t    */
/****************************************************************/
static void emitcost1(  Tree t,         /* дерево шаблона       */
                        char *v,        /* текстовое имя корня  */
                        int n,          /* номер доп.стоимости  */
                        char *b)        /* имя файла _d         */
{
  Nonterm p = t->op;                    /* текущий узел дерева  */
  if (p->kind == TERM)                  /* это терминал         */
  {
    if (t->left)
      emitcost1(t->left, stringf(Cflag?"%s->l":"%s.l",v),n,b);
    if (t->right)
      emitcost1(t->right,stringf(Cflag?"%s->g":"%s.r",v),n,b);
  }
  else                                  /* нетерминал-потомок   */
  {
    if(Cflag)
      print("\n%3%s->auxc[%P%S][%d] + ", v, p, n);
    else
      print("\n%3%s.auxc[%s%P%S][%d] + ", v, b, p, n);
  }
}

/****************************************************************/
/* emitcost2 - сборка процедурного фрагмента для вычисления     */
/* специальной дополнительной стоимости правила с шаблоном t    */
/****************************************************************/
static void emitcost2(  Tree t,         /* дерево шаблона       */
                        char *v,        /* текстовое имя корня  */
                        int n,          /* номер доп.стоимости  */
                        int *m,         /* чмсло аргументов     */
                        char *b)        /* имя файла _d         */
{
  Nonterm p = t->op;                    /* текущий узел дерева  */
  if (p->kind == TERM)                  /* это терминал         */
  {
    if (t -> left)
      emitcost2 (t->left,  stringf ("%s.l", v), n, m, b);
    if (t -> right)
      emitcost2 (t->right, stringf ("%s.r", v), n, m, b);
  }
  else                                  /* нетерминал-потомок   */
  {
    if(* m > 0)
      print (",\n");
    print ("\n%3%s%Pfcost%d (%s, %s%P%S", b, n, v, b, p);
    ++ (* m);
  }
}

/****************************************************************/
/* emitdefsd - сборка выходных определений нетерминалов          */
/****************************************************************/
void emitdefsd(Nonterm nts,   /* список нетерминалов      */
                     int ntnumber)  /* число нетерминалов       */
{
  Nonterm p;                        /* очередной нетерминал     */
    print ("TYPE NT *= (\n     NTnowhere,\n");
    for (p = nts; p->link != NULL; p = p->link)
    {
      print ("     %P%S,\n", p );
//kevin      print ("CONST %P%S *= %d;\n", p, p -> number);
    }
    print("     %P%S\n     );\n", p);
//    print("CONST %Pmax *= %P%S;\n\n", p);
}

/****************************************************************/
/* emitdefsi - сборка выходных определений нетерминалов          */
/****************************************************************/
void emitdefsi(Nonterm nts,   /* список нетерминалов      */
                     int ntnumber)  /* число нетерминалов       */
{
  Nonterm p;                        /* очередной нетерминал     */
    print ("\nTYPE NT = BurgNT.NT;\nCONST NTnowhere = BurgNT.NTnowhere;\n");
    for (p = nts; p != NULL; p = p->link)
    {
      print ("CONST %P%S = BurgNT.%P%S;\n", p, p);
    }
    print("\n\n");
}

/****************************************************************/
/* emitheader - сборка определений ALLOC и burm_assert          */
/****************************************************************/
//static void emitheader(void)
//{
//}

/****************************************************************/
/* computekids - вычисление всех путей к "детям"-нетерминалам   */
/****************************************************************/
static char *computekids(Tree t,        /* узел дерева          */
                         char *v,       /* имя для "ребенка"    */
                         char *bp,      /* буферная строка      */
                         int *ip)       /* текущее число детей- */
{                                       /* нетерминалов шаблона */
  Term p = t->op;                       /* текущий узел дерева  */
  if (p->kind == NONTERM)    /* нетерминал, обрывающий поиск    */
  {                          /* по данной ветви дерева          */
    sprintf(bp, "\t\tkids[%d] := %s;\n", (*ip)++, v);
    bp += strlen(bp);
  }
  else          /* терминал: рекурсивный обход продолжается     */
    if (p->arity > 0)
    {
      bp = computekids(t->left,
           stringf("%s.l", v), bp, ip);
      if (p->arity == 2)
        bp = computekids(t->right,
           stringf("%s.r",v), bp, ip);
    }
  return bp;    /* возвращается свободный хвост буфера          */
}

/****************************************************************/
/* emitkids - сборка процедуры burm_kids                        */
/****************************************************************/
static void emitkids(   Rule rules,     /* список правил        */
                        int nrules)     /* число правил         */
{
int f=0;                /*Признак самого первого case при выводе*/
/****************************************************************/
/* Вычисление попарно несовпадающих текстовых фрагментов вида   */
/* kids[i]=v;... перечисляющих детей-нетерминалов в составе     */
/* шаблонов всех правил входной Д-грамматики:                   */
int i,ix;              /* счетчик правил и граница диапазонов   */
Rule r,                /* очередное правило                     */
    *rc;       /* правила, сопоставленные фрагментам:           */
char **str;    /*тексты процедурных фрагментов вида kids[i]=v;..*/
  int maxNKids = 0;

  rc = alloc((nrules + 1)*sizeof *rc);
  for(i=0; i<=nrules; ++i) rc[i]=NULL;
  str=(char **)alloc((nrules + 1)*sizeof *str);
  for(i=0; i<=nrules; ++i) str[i]=NULL;

  for (r = rules; r!=NULL; r = r->link)
  {
    int d = 0;      /*число детей(нетерминалов) шаблона правила */
    register j;     /*вспомогательный счетчик                   */
    char buf[1024], /*буфер для накопления  текста kids[i]=v;...*/
         *bp = buf; /*указатель на свободное место в этом буфере*/

    /* Вычисление текста фрагмента kids[i]=v;...данного правила:*/
       *computekids(r->pattern, "n", bp, &d) = 0;
    if( maxNKids < d ) 
      maxNKids = d;
    /* Сравнение полученного фрагмента с уже существующими:     */
    for (j = 0; str[j]!=0 && strcmp(str[j], buf)!=0; j++);
    if (str[j] == NULL)         /* Такого фрагмента еще не было */
      str[j]=strcpy(alloc(strlen(buf)+1),buf);  /*Запоминаем его*/
    r->kids = rc[j];    /* Сопоставление правилу его фрагмента  */
    rc[j] = r;          /* Сопоставление фрагменту его правила  */
  }
/****************************************************************/
/* Cборка ранее накопленных текстовых фрагментов:               */
  print ("PROCEDURE %Pkids* (n: RD.DAGNODE; eruleno: BurgNT.Rule;\n"
                           "%2VAR kids: ARRAY OF RD.DAGNODE);\n"
         "BEGIN\n"
         "    CASE eruleno OF\n");

  for (i = 0; (r = rc [i]) != NULL; i ++) {    /* По списку фрагментов */
    ix = 0;
    for (f = 0; r != NULL; r = r -> kids) {    /* Для всех правил,инцид.фрагм.*/
      ix = 1;
      if (f)
        print (",\n%1 BurgNT.Rule{ %d }%1(* %R *)", r -> ern, r);
      else {
        print    ("%1|BurgNT.Rule{ %d }%1(* %R *) ", r -> ern, r);
        f = 1;
      }
    }
    if (ix)
      print (":\n%s\n", str [i]);
  }
  print("    END;\n");
  print("END %Pkids;\n");
  print("CONST MAXNKIDS *= %d;\n\n", maxNKids);
}

/****************************************************************/
/* closure - заполнение полей стоимостей и правил результатами  */
/* цепных правил с нетерминалом p в качестве правой части       */
/****************************************************************/
static void closure(int cost[], Rule rule[], Nonterm p, int c)
{
  Rule r;                               /* очередное правило    */
  for (r = p->chain; r; r = r->chain)
    if (c + r->cost < cost[r->lhs->number])
    {
      cost[r->lhs->number] = c + r->cost;
      rule[r->lhs->number] = r;
      closure(cost, rule, r->lhs, c + r->cost);
    }
}

/****************************************************************/
/* computents - вывод в строку bp имен всех нетерминалов из     */
/*              шаблона правила, заданного деревом t            */
/****************************************************************/
static char *computents(Tree t,         /* дерево шаблона       */
                        char *bp,       /* буфер для вывода     */
                        int *k)         /* число нетерминалов в */
{                                       /* шаблоне              */
  if (t)
  {
    Nonterm p = t->op;                  /* текущий узел дерева  */
    if (p->kind == NONTERM)
    {
      sprintf(bp, "BurgNT.%s%s, ", prefix, p->name);
      bp += strlen(bp); ++*k;
    }
    else bp = computents(t->right, computents(t->left,  bp, k), k);
  }
  return bp;
}

/****************************************************************/
/* emitnts - сборка определений массива burm_nts попарно        */
/* несовпадающих списков нетерминалов из шаблонов правил,       */
/* массивов burm_nts_i этих списков со стат.инициацией для Си   */
/* версии и сборкой процедуры burm_nts_init для Оберон-2 версии */
/****************************************************************/
static void emitnts(Rule rules, /* список правил                */
                    int nrules) /* число правил                 */
{
Rule r;                         /* текущее правило              */
int i, j,
   k,  /* число нетерминалов, встречающихся в шаблоне правила   */
   l,  /* суммарная длина массива nts_n (для версии O-2)        */
   m,  /* число отличающихся между собой списков нетерминалов   */
   *nts = alloc(nrules*sizeof *nts), /* номера списков правил   */
   *lts= alloc(nrules*sizeof *nts);        /* позиции списков   */
char **str = alloc(nrules*sizeof *str);    /* тексты списков    */
  for (m = l = k = i = 0, r = rules; r!=NULL;r = r->link,k = 0)
  {
    char buf[1024]; /* буфер для текущего текста списка         */
    *computents(r->pattern,buf,&k) = 0; /* формирование текста  */
    /*     Сравнение с уже имеющимися списками нетерминалов:    */
    for (j = 0; j<m && strcmp(str[j], buf); j++);
    if (j==m)           /* Список ранее не встречался           */
    {
      /* Запоминание нового списка имен под номером j:          */
      str[j] = strcpy(alloc(strlen(buf) + 1), buf);
      ++m; lts[j]=l++; l+=k;
    }
    nts[i++] = j;       /* сопоставление правилу его шаблона    */
  }
  print("TYPE Index *= INTEGER;\n");
  print("TYPE NTnts_nType = ARRAY OF BurgNT.NT;\n");
  print("CONST %Pnts_n*= NTnts_nType {\n");
  for(j=0; j<m; j++) print("%1%sBurgNT.NTnowhere%s (*%d*)\n",
                     str[j], j<m-1?" ,":" ", j);
  print("};\n");

  print("CONST nRules = BurgNT.Rule{ %d };\n", nrules);
  print("TYPE RuleRange   = BurgNT.Rule[BurgNT.Rule{0}..nRules];\n");
  print("     %PntsType = ARRAY RuleRange OF Index;\n");
  print("CONST %Pnts*= %PntsType {\n");
  for (i = j = 0, r = rules; r!=NULL; r = r->link, ++i, ++j)
  {
    for ( ; j < r->ern; j++)  print("%1Index{ 0 }, (*%d*)\n", j);
    print("%1Index{ %d }%s (*%d->%d*) \n",
    lts[nts[i]], r->link? "," : " ", j, nts[i]);
  }
  print("};\n");

  print("<* IF ~nodebug THEN*>\n");
  print("TYPE RuleNamesType = ARRAY RuleRange OF ARRAY 64 OF CHAR;\n");
  print("CONST RuleNames *= RuleNamesType {\n    'zerorule'");
  for (r = rules; r!=NULL; r = r->link)
  {
    print(",\n    '%R'", r);
  }
  print("\n    };\n");
  print("<* END *>\n");
}

/****************************************************************/
/* emitrecord - сборка процедурного фрагмента, обеспечивающего  */
/* вычисление стоимостного условия выбора правила r для узла "p"*/
/* и необходимых отметок в векторе вариантных атрибутов узла "p"*/
/* и действий при реализации выбора r->"p" в процессе разметки  */
/* (в составе фрагмента case функции state и в составе функций  */
/* выбора цепных правил closure для разных нетерминалов).       */
/****************************************************************/
static void emitrecord( char *pre,      /* текстовый отступ     */
                        Rule r,         /* правило для выбора   */
                        char *b,        /* имя файла closure    */
                        int l,          /* длина имени файла    */
                        int f,          /* признак case-вызова  */
                        char names[PVSIZE][PLSIZE]) /* имена парам.*/
{
  register int i;                       /* счетчик              */

  if(Tflag)
  {
      print("%s%Ptrace(%s%Pnp, %d, c0,", b,b,r->ern);
      for(i=0; i<costvl-1; ++i)   /* дополнительные стоимости   */
        print("c%d,\n",i+1);
      print("n.cost[%P%S]", r->lhs);
      for(i=0; i<costvl-1; ++i)   /* дополнительные стоимости   */
        print(", n.auxc[%s%P%S][%d]\n", b, r->lhs, i);
      print(");\n", r->lhs);
  }

  /**************************************************************/
  /*     условие выбора правила r для узла "p":                 */
  print("%sIF (c0 < n.cost[%P%S])", pre, r -> lhs);
  for(i = 0; i < costvl - 1; ++ i)      /* дополнительные стоимости     */
    print(
      " OR\n%s((c%d=n.cost[%P%S]) &\n"
      "%s ((c%d<p.auxc[%s%P%S][%d])",
      pre,i,r->lhs, pre,i+1,b, r->lhs,i);
  for (i = 0; i < costvl - 1; ++ i)
    print ("))");
  print(" THEN\n");

  /**************************************************************/
  /* текст действий, предпринимаемых в случае успешного выбора: */
  /* 1) основная стоимость выбора правила r для узла "p":       */
  print("%s%1n.cost[%P%S] := VAL (INTEGER, c0);\n",
         pre, r->lhs);

  /* 2) отметка номера правила, дающего min покрытие для узла   */
  /* "p" и результирующего нетерминала правила r:               */
  print("%s%1n.rule[%P%S] := BurgNT.Rule { %d };\n", pre, r->lhs, r->packed);

  for(i=0; i<costvl-1; ++i)     /* 3) дополнительные стоимости  */
                                /*    данного выбора:           */
     print("%s%1n.auxc[%s%P%S][%d] := c%d;\n", pre, b, r->lhs, i, i+1);

  /* 4) отладочное сообщение об успехе выбора                   */
  if(Tflag)
      print("%s%1%s%PPLUS(\"%S\");\n",pre,b,r->lhs);
  if(r->dyncost)
  {
      print("\n"); emitxtxt(r->dyncost, 0, names); print("\n");
  }

  /* 5) вызов процедуры обработки цепных правил для             */
  /*    результирующего нетерминала данного правила r:          */
  if (r->lhs->chain)    /* список цепных правил для него непуст:*/
  {
    if(f)               /* фрагмент в теле модуля _s            */
    {
      print("%s%1%Pclosure_%S(n, c0", pre, r->lhs);
    }
    else                /* фрагмент в теле модуля _c            */
      print("%s%1%Pclosure_%S(n, c0", pre, r->lhs);
    for(i=0; i<costvl-1; ++i)   /* дополнительные стоимости     */
      print(",c%d",i+1);
    print(");\n");
  }

  /* 6) отладочное сообщение об неуспехе выбора                 */
  if(Tflag)
      print("%sELSE%1%s%PMINUS(\"%S\");\n%sEND;\n",
            pre,b,r->lhs,pre);
  else
      print("%sEND;\n", pre);
}

/****************************************************************/
/*emitrule0 - сборка списков правил для нетерминалов (правил)   */
/****************************************************************/
//static void emitrule0(Nonterm nts)      /* список нетерминалов  */
//{
//  Nonterm p;                            /* текущий нетерминал   */
//
//  /*    Сборка списка правил, инцидентных нетерминалам          */
//  for (p = nts; p!=NULL; p = p->link)
//  {
//    Rule r;                             /* текущее правило      */
//
//    print("CONST %Pdecode_%S*= ARRAY OF INTEGER {\n%10", p);
//    for (r = p->rules; r!=NULL; r = r->decode)
//      print(",\n%1%d", r->ern);
//    print("\n};\n\n");
//  }
//}

/****************************************************************/
/* emitrule - сборка процедуры burm_rule, возвращающей по номеру*/
/*       нетерминала и адресу узла дерева покрытия номер правила*/
/****************************************************************/
//static void emitrule(Nonterm nts)       /* список нетерминалов  */
//{
//  Nonterm p;                            /* текущий нетерминал   */
//
//  print("PROCEDURE %Prule*(state: RD.DAGNODE; goalnt: NT):INTEGER; \n"
//        "BEGIN\n"
///*        "  ASSERT ((goalnt >= 1) & (goalnt <= %d));\n"*/
///*      "  IF state=NIL THEN RETURN 0 END;\n" */
//        "  CASE goalnt OF\n", ntnumber);
//
//  for (p = nts; p!=NULL; p = p->link)
//      print("    | %P%S:\n"
////kevin      "        RETURN %Pdecode_%S [state.rule.%P%S]\n", p, p, p);
//      "        RETURN state.rule.%P%S\n", p, p);
//  print("  END;\n"
//        "  RETURN 0\n"
//        "END %Prule;\n"
//        "\n");
//} */

/****************************************************************/
/* emitstate - сборка процедуры newstate, создающей новый узел  */
/* покрывающего дерева и вычисляющей его атрибуты (векторы      */
/* стоимостей и выбора минимальных A-покрытий) в зависимости от */
/* нетерминала на входе и значений соответствующих атрибутов    */
/* сыновей данного узла в покрывающем дереве                    */
/****************************************************************/
void emitstate(  Term terms,            /* список терминалов    */
                        Nonterm start,  /* начальный нетерминал */
                        int ntnumber,   /* число нетерминалов   */
                        char *b,        /* имя для файла _s     */
                        char *import)   /* список импорта       */
{
  int i, l, N;                          /* вспомог. счетчик     */
  Term p;                               /* очередной терминал   */

//  print ("<*+WOFF*>\n\n"
//         "MODULE %s;\n\n", b);
  for (l = 0; b [l] != 0; l ++)
    ;
//  b [l - 1] = 'c'; print ("IMPORT C := %s, ", b);
//  b [l - 1] = 'd'; print ("D := %s, %s, SYSTEM;\n", b, import);
//  b [l - 1] = 's';
  print ("\n"
     "TYPE newstate_proc *= PROCEDURE (n: RD.DAGNODE);\n"
         "\n"
         "CONST MaxCost = BurgNT.CostArray {\n"
         "%10,\n"
    );
  for (i = 1; i <= ntnumber; i ++) {
    print ("%1MAX (INTEGER)");
    print (i == ntnumber ? "\n" : ",\n");
  }
  print ("};\n");

/****************************************************************/
  N = 0;
  for (p = terms; p!=NULL; p = p->link)
  {
    if (p -> esn > N)
        N = p -> esn;
    emitcase (p, ntnumber,"D.",l);      /* сборка одного case   */
  }
/****************************************************************/

#if 1
  print("TYPE NewstatesType = ARRAY BurgNT.OpRange OF newstate_proc;\n");
  print("CONST newstates *= NewstatesType {\n");
  for (i = 0; i <= N; i ++) {
    print (i ? "%1," : "%1 ");
    for (p = terms; p; p = p -> link)
      if (p -> esn == i) {
        print ("newstate_%S\n", p);
        goto next;
      }
    print ("NIL\n");
next:;
  }
  print ("};\n"
         "\n");
//     "END %s.\n", b);
#else
  print("\nVAR newstates* : ARRAY %d OF newstate_proc;\n\nBEGIN\n", N + 1);
  for (p = terms; p; p = p -> link)
    print("%1newstates [%d] := newstate%d;\n", p -> esn, p -> esn);
  print ("END %s.\n", b);
#endif
}

/****************************************************************/
/* emitstring - сборка массивов правил и стоимостей             */
/****************************************************************/
static void emitstring(Rule rules)      /* список правил        */
{
  Rule r;
  int k;

  print("CONST %Pcost= {\n");
  for (k = 0, r = rules; r!=NULL; r = r->link)
  {
    for ( ; k < r->ern; k++)            /* заполнение разрывов  */
      print("%10,%1(* %d *)\n", k);
    print("%1%d%s%1(* %d = %R *)\n", r->cost, r->link ? ",":" ", k++, r);
  }
  print("};\n\n");
}

/****************************************************************/
/* emitstruct - сборка определения структуры state              */
/*              (узел динамического дерева покрытия)            */
/****************************************************************/
void emitstruct( Nonterm nts,    /* список нетерминалов  */
                        int ntnumber)   /* число  нетерминалов  */
{
  Term p;
  print("TYPE\n%1CostArray   *= ARRAY NT OF INTEGER;\n");
  print("%1NTSet       *= PACKEDSET OF NT;\n");
  print("%1Rule        *= INTEGER;\n");
  print("%1RuleArray   *= ARRAY NT OF Rule;\n");
  for (p = terms; p->link != NULL; p = p->link) {}
  print("%1OpRange     *= ir.Operation[ir.o_invalid..ir.%S];\n\n",p);


//        "%1%Pstate_p *= POINTER TO %Pstate;\n"
//        "%1%Pstate   *= RECORD\n"
//        "%3op*:   INTEGER;\n"
//        "%3l*,\n"
//        "%3g*:    %Pstate_p;\n"
//        "%3cost*: cost_array;\n" );
//  if(costvl>1) print("%3auxc*: ARRAY %d OF ARRAY %d OF INTEGER;\n",
//        ntnumber + 1, costvl - 1);
//  print("%3rule*: ARRAY NT OF INTEGER\n%2END;\n\n");

/*
  print("%3rule*: RECORD\n");

  for (;nts!=NULL; nts=nts->link)
  {
      int n=1, m=nts->lhscount;
      while ((m>>=1)>0) n++;
      print("%4%P%S*: INTEGER;\n", nts);
  }
  print("%3END\n%2END;\n\n");
*/
}

/****************************************************************/
/* emitterms - сборка структур данных для терминалов            */
/****************************************************************/
static void emitterms(Term terms)       /* список терминалов    */
{
  Term p;                               /* текущий терминал     */
  int k;

  print("TYPE %Parity_type = ARRAY BurgNT.OpRange OF SHORTINT;\n");
  print("CONST %Parity*= %Parity_type {\n");
  for (k = 0, p = terms; p!=NULL; p = p->link)
  {
    for ( ; k < p->esn; k++)    /* заполнение разрывов нулями   */
      print("%10,%1(* %d *)\n", k);
      print("%1%d%s%1(* %d=%S *)\n",
            p->arity < 0 ? 0 : p->arity, p->link ? ",":" ", k++, p);
  }
  print("};\n\n");
}

/****************************************************************/
/* emittest - сборка процедурного фрагмента для  проверки       */
/*            совпадения с образцом                             */
/****************************************************************/
static int emittest(Tree t,            /* шаблон для сравнения */
                    int nblanks,
                    int print_blanks,
                    char *v,           /* симв.имя узла        */
                    char *suffix)      /* хвост конструкции    */
{
  Term p = t -> op;                     /* текущий узел шаблона */
  int i;

  if (p -> kind != TERM)
    return 0;
  if (print_blanks)
    for (i = 0; i < nblanks; i ++)
      print (" ");
  print("(%s.op = ir.%S (* %d *)) %s\n",
        v, p, p -> esn, t -> nterms > 1 ? "&" : suffix);
  if (t -> left)
     emittest (t -> left, nblanks, 1, stringf ("%s.l", v),
               t -> right && t -> right -> nterms ? "&" : suffix);
  if (t -> right)
     emittest (t -> right, nblanks, 1, stringf ("%s.r", v), suffix);
  return 1;
}

/****************************************************************/
/*emitxtxt - вывод внешнего текста с редактированием местоимений*/
/****************************************************************/
static int emitxtxt (char *s, int nblanks, char names [PVSIZE][PLSIZE])
{
  char *stbp [DIMSTM];          /* стек указателей тел макросов */
  register m;                   /* индекс стека макросов        */
  char * q;
  int i, nlines;

  nlines = m = 0;
  while (* s && (* s == ' ' || * s == '\t' || * s == '\r' || * s == '\n'))
    s ++;
l:
  while (* s) {
    if (* s == '@') {                           /* местоимение @INT  */
      int n = 0;                                /* номер параметра   */
      char * ss = s;
      for (s ++; isdigit (*s); s ++)
        n = n * 10 + (* s - '0');
      if (n >= PVSIZE) {
        error ("Некорректный номер макроса: %s\n", ss);
        goto cl;
      }
      q = & names [n][0];                       /* тело макроопредел.*/
      if (* q == 'n' && * (q + 1) == '.') {
        if (* (q + 2) == 'l')
          q += 2;
        else if (* (q + 2) == 'g') {
          print ("r");
          q += 3;
    }
      }
      else if (* q == 'n' && * (q + 1) == 0 &&
           * s == '^' && * (s + 1) == '.' && * (s + 2) == 'p' &&
           ! isalpha (* (s + 3)) && ! isdigit (* (s + 3))) {
      q ++;
      s += 2;
      }
      print("%s", q);
    }
    if (* s == '$') {                           /* местоимение  $    */
      int n = 0;                                /* номер местоимения */
      char * ss = s;
      for (s ++; isdigit (* s); s ++)
        n = n * 10 + (* s - '0');
      if (n >= STSIZE) {
        error ("Некорректный номер макроса: %s\n", ss);
        goto cl;
      }
      if (m < DIMSTM) {
        stbp [m ++] = s;
        s = pronmac [n];
        if (!s) {
          error ("Неизвестный номер макроса: %d\n", n);
          exit(1);
        }
        goto l;
      }
    }
    if (* s == '%') {                           /* местоимение  %    */
        print(" %P");
        for(++s; isalpha(*s); ++s)
          print("%c", *s);
//        print("_NT");
    }
cl: if (* s == '\r' || * s == '\n') {
      nlines ++;
      print ("\n");
      do {
        s ++;
      } while (* s && (* s == ' ' || * s == '\t' || * s == '\r' || * s == '\n'));
      if (* s || m > 0)
        for (i = 0; i < nblanks; i ++)
          print (" ");
    }
    else
      print ("%c", * s ++);                     /*любая другая литера*/
  }
  if (m > 0) {
    s = stbp [-- m];
    goto l;
  }
  return nlines;
}
