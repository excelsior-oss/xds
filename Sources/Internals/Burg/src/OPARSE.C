/****************************************************************/
/* <oparse.c> 22-августа-94 последнее изменение  20 февраля 94  */
/*   Анализатор исходного описания грамматики на языке BURG+ и  */
/*   генератор ее внутреннего представления в системе BurBeG.   */
/*   В процессе анализа собирает текст процедуры actions.       */
/****************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include "oburg.h"      /* определения типов и констант         */
#include "burgpt.h"     /* основная таблица анализа BURG-текста */
#include "treept.h"     /* табл.анализа описания дерева шаблона */

static int ppercent=0;  /* признак текста после %%              */

static union
{
  int n;                /* значение целого числа                */
  char *string;         /* строка имени идентификатора          */
}
lexval;                 /* обобщенное лексическое значение      */

struct entry            /* дескриптор имени хеш-таблицы         */
{
   union
   {
     char *name;                /* строка входного имени        */
     struct term t;             /* дескриптор терминала         */
     struct nonterm nt;         /* дескриптор нетерминала       */
   } sym;
   struct entry *link;          /* следующий элемент списка     */
                                /* дескр. коллизирующих имен    */
};                              /* хеш-таблица как массив       */
                                /* адресов списков дескрипторов */
static struct entry *table[HASHSIZE];  /* тело хеш-таблицы имен */
extern int lexineno;

int parse()
/****************************************************************/
/*         Головной анализатор исходного текста                 */
/*       Возвращает число обнаруженных в тексте ошибок          */
/****************************************************************/
{
char *plex;             /* позиция текущей лексемы в буфере     */
int sts;                /* текущее состояние разбора            */
int stl;                /* индекс стека разбора                 */
char *name;             /* строка имени идентификатора          */
Tree  p;                /* шаблон правила                       */
char *c;                /* припис. правилу текст доп.условия    */
char *d;                /* припис. правилу текст дин.стоимости  */
int x;                  /* внешний номер правила                */
int cc;                 /* константная стоимость правила        */
int *auxc;              /* вектор дополнительных стоимостей     */
register i,l;           /* счетчик литер                        */
int ximpl;              /* неявный счетчик правил               */
int naux;               /* счетчик дополнительных стоимостей    */
int oper,type;          /* семантическое действие и вх.лексема  */
  sts=0; stl=0; plex=inbuf;
  costvl=1; ximpl=0;
  do
  {
    type=lex();
rl: oper=qikpas(type,&sts,&stl,burgpt);
/*
printf("*** type=%d op=%d %s\n",type,oper,plex);
*/
    switch(oper)
    {
      case 1 : *bp=0; break;            /* новая строка         */
      case 2 : break;                   /* пропуск лексемы      */
      case 3:  return errcnt;           /* конец входного текста*/
      case 4 : error("Cинтаксическая ошибка:\n %s",plex);
               *bp=0; break;
      case 5 : name=lexval.string;      /* имя терминала        */
               break;
      case 6 : term(name, lexval.n);    /* внешний номер        */
               break;                   /* терминала            */
      case 7 : emitdefsi(nts, ntnumber);    /* сборка определений нетерм.   */
               emitactions();      /* сборка заголовка процедуры actions   */
               start=                   /* явно заданный        */
                 nonterm(lexval.string);/* стартовый нетерминал */
               break;
      case 8 : name=lexval.string;      /* имя результирующего  */
               if(start==NULL)          /* нетерминала правила  */
                  start=nonterm(name);  /* стартовый нетерминал,*/
               c=d=NULL; cc=0;          /*   заданный неявно    */
               if(costvl>1)             /* список доп.стоимостей*/
               { auxc = (int *)alloc((sizeof(int)*(costvl-1)));
                 for(i=0; i<costvl-1; ++i) auxc[i]=0;
               }
               else auxc =NULL;
               break;
      case 9 : p=treeparse(0);          /* дерево шаблона:запуск*/
               plex=bp; continue;       /* рекур.поданализатора!*/
      case 10: x=lexval.n; break;       /* внешн.номер правила  */
      case 11: rule(name,p,x,0,c,d,auxc); /*правило стоимости 0 */
               break;
      case 12: cc=lexval.n; break;      /* константная стоимость*/
      case 13: c=xtext(); break;        /* текст условия правила*/
      case 14: emitactrule(x,p); break; /* 1-e сем.дейст.правила*/
      case 15: d=xtext(); break;        /* динамическая стоим-ть*/
      case 16: rule(name,p,x,cc,c,d,auxc); /* правило разобрано */
               break;                   /* полностью            */
      case 17: if(lexval.n>=STSIZE)     /* целый номер макроса  */
               {
                 error("Недопустимый номер макроса %s\n",plex);
                 *bp=0; break;
               }
               x=lexval.n; goto mbody;
      case 18: bp=plex; x=0;            /* 0 макрос (без номера)*/
      mbody:   {                        /* тело макроса         */
               char buf[1024];
                 l=0;
                 while(1)
                 {
                   while(*bp!='\n' && *bp!='/' && *bp!='\\' && l<1023)
                     buf[l++]= *(bp++);
                   if(*bp=='\\')        /* многострочный макрос */
                   {
                     buf[l++]='\n'; *bp=0; get(); continue;
                   }
                   else break;
                 };
                 d=pronmac[x]=alloc(l+1);
                 for(i=0;i<l;++i) d[i]=buf[i]; d[i]=0;
                 *bp=0; break;
               }
      case 19: cc=0; emitactrule(x,p); /* текст обраб.правила   */
               break;                  /* при опущенной стоим.  */
      case 20: include(); break;       /* включение файла       */
      case 21: costvl=lexval.n;        /* длина вект.стоимости  */
               if(costvl>1)
               {
                 auxtype=(int *)alloc((sizeof(int)*(costvl-1)));
               }
               else auxtype=NULL;
               break;
      case 22: x= ++ximpl; goto rl;    /* неявный номер правила */
                                       /*ПОВТОР С ТОЙ ЖЕ ЛЕКСЕМ!*/
      case 23: naux=0; break;          /* начало списка стоимос.*/
      case 24: if(++naux>=costvl)      /* следует доп.стоимость */
               { warn("Лишняя стоимость правила:\n %s",bp); }
               break;
      case 25: if(costvl>1 && naux<costvl)    /* доп.стоимость  */
                 auxc[naux-1]=lexval.n;
               break;
      case 26: rule(name,p,++ximpl,cc,c,d,auxc); /*обрыв правила*/
               break;
      case 27: auxtype[lexval.n-1]=1;  /* спец.функция стоимости*/
               break;
      case 28: auxtype[lexval.n-1]=2;  /* пустая функция стоим. */
               break;
      case 29: emitactrule(-x,p); break; /*2-e сем.дейст.правила*/
      case 30: cc=0; emitactrule(-x,p); /* текст 2-го действия  */
               break;                   /* при опущенной стоим. */
      case 31: addnonterm(lexval.string);      /* имя нетерминала        */
               break;
    }
    plex=bp;            /* переход к выборке следующей лексемы  */
  }
  while(stl>=0);
}
/****************************************************************/
/*          Лексический анализатор конвертора OBURG:            */
/*  Игнорирует как разделители ' ' \f \t                        */
/*  Игнорирует комментарии /... в конце строки                  */
/*  Выводит в выходной текст преамбулу %{...}%  (косвенно в get)*/
/*  Возвращает как целое число код типа очередной лексемы:      */
/*  - Однолитерные лексемы: ( ) { } < > , ; = :                 */
/*  - START или TERM  перед описанием грамматики (после %...)   */
/*  - MACRO перед описанием грамматики (после %define)          */
/*  - СOST  перед описанием грамматики (после %cost)            */
/*  - INCLUDE перед описанием грамматики (после %include)       */
/*  - PPERCENT - начало описания грамматики (после первого %%)  */
/*  - INT для целого числа  (значение в lexval.n)               */
/*  - ID для идентификатора (тело строки в lexval.string)       */
/*  - 1 - для конца строки  (\n)                                */
/*  - 0 - описание исчерпано     (после второго %% или EOF)     */
/*  Все прочие литеры считаются ошибочными и пропускаются с     */
/*  выводом сообщения об ошибке                                 */
/****************************************************************/
static int lex(void)
{
int c;
  while ((c = get()) != EOF)    /* очередная  литера текста     */
  {
    switch (c)
    {
      case ' ': case '\f': case '\t':   continue;
      case '\n':case 0:   case '/':     return 1;
      case '(': case ')': case '{': case '}':
      case '<': case '>': case ',':
      case ';': case '=': case ':':     return c;
    }
    if (c == '%' && *bp == '%')
    {
      bp++; return ppercent++ ? 0 : PPERCENT;
    }
    else
      if (c == '%' && strncmp(bp, "define", 6) == 0
                   && isspace(bp[6]))
      { bp +=6; return MACRO; }
    else
      if (c == '%' && strncmp(bp, "include", 7) == 0
                   && isspace(bp[7]))
      { bp +=7;
        lexineno=1;
        return INCLUDE; }
    else
      if (c == '%' && strncmp(bp, "nonterm", 7) == 0
                   && isspace(bp[7]))
      { bp += 7; return NTERM; }
    else
      if (c == '%' && strncmp(bp, "term", 4) == 0
                   && isspace(bp[4]))
      { bp += 4; return TERM; }
    else
      if (c == '%' && strncmp(bp, "start", 5) == 0
                   && isspace(bp[5]))
      { bp += 5; return START; }
    else
      if (c == '%' && strncmp(bp, "cost", 4) == 0
                   && isspace(bp[4]))
      { bp += 4; return COST; }
    else
      if (c == '%' && strncmp(bp, "special", 7) == 0
                   && isspace(bp[7]))
      { bp += 7; return SPEC; }
    else
      if (c == '%' && strncmp(bp, "empty", 5) == 0
                   && isspace(bp[5]))
      { bp += 5; return EMPTY; }
    else
      if (isdigit(c))
      {
        int n = 0;
        do
        {
          n = 10*n + (c - '0');
          c = get();
        }
        while (isdigit(c));
        if (n > 32767)
          error("целое %d больше чем 32767\n", n);
        bp--;
        lexval.n = n;
        return INT;
      }
    else
      if (isalpha(c))
      {
        char *p = bp - 1;
        while (isalpha(c) || isdigit(c) || c == '_')
          c = get();
        bp--;
        lexval.string = (char *)alloc(bp - p + 1);
        strncpy(lexval.string, p, bp - p);
        lexval.string[bp - p] = 0;
        return ID;
      }
    else
      if (isprint(c))
         error("лексически недопустимая литера `%c'\n", c);
      else
         error("лексически недопустимый (непечатный) символ `\0%o'\n",
               c);
  }
  return 0;
}
/****************************************************************/
/*  процедура шага синтаксического анализа текста               */
/*      возвращает код семантической операции                   */
/****************************************************************/
static int qikpas(swt,sts,stl,pat)
int swt;                        /* переключатель разбора        */
int *sts;                       /* адрес стека состояний разбора*/
int *stl;                       /* индекс стека разбора         */
int *pat;                       /* таблица разбора              */
{
register int j;                 /* счетчик состояний перехода   */
register int i;                 /* число состояний перехода     */
register int l;                 /* индекс стека разбора         */
int *sp;                        /* указатель на таблицу разбора */
int jump;                       /* код действий перехода        */
/****************************************************************/
        l= *stl;
//        printf("swt=%d\tsts=%d\n", swt, sts[l]);
        do
        {
                sp=pat+sts[l]*4;
                while(1)
                {
                        switch(*(sp++))
                        {
                                case 0: sp ++;
                                        goto jmp;
                                case 1: if(swt== *(sp++)) goto jmp;
                                        break;
                                case 2: if(swt>= *(sp++))
                                          if(swt<=*(sp++))
                                            goto jmp;
                                          else break;
                                        sp++;break;
                        }
                        sp += 2;
                }
           jmp: jump= *(sp++);i= 1;//*(sp++);
                if(i>0)
                    for(j=0;j<i;++j)
                        sts[l++]= *(sp++);
                --l;
        }
        while(jump==0 && l>=0);
        *stl=l;
//        printf("\t\t\t\tnewsts=%d\tjump=%d\n", sts[l], jump);
        return(jump);
}

static char *xtext()
/****************************************************************/
/* Выборка из входного файла внешнего текста в форме {...}      */
/* Удаляются лишние пробелы и табуляции                         */
/* Возвращает адрес скопированного в память фрагмента           */
/****************************************************************/
{
char buf[1024];                 /* временный текстовый буфер    */
register i,m;                   /* счетчики литер               */
char *s;                        /* адрес фрагмента во вх.тексте */
char *t;                        /* адрес копии фрагмента        */
int f;                          /* признак подавляемого раздел. */
  f=m=0;
l:for(s=bp; *bp!='}' &&  *bp!='\n' && m<1023; ++bp)
  {
    if(*bp==' ' || *bp=='\t')
    {
      if(f==0) { f=1;  buf[m++]=' '; continue; }
      else continue;
    }
    buf[m++]=*bp; f=0;
  }
  if(m==1024)
  {
    error("Слишком длинный внешний текст:\n %s", s);
    *bp=0; return NULL;
  }
  if(*bp=='\n')
  {
    buf[m++]=*bp; *bp=0;
    if(get()==EOF)
    {
      error("Обрыв внешнего текста:\n %s", s);
      *bp=0; exit(0);
    }
    f=0; goto l;
  }
  if(m>0) t= (char *)alloc(m+1); else t=NULL;
  for(i=0; i<m; ++i) t[i]=buf[i]; t[i]=0;
  ++bp; return t;
}

static Tree treeparse(int p)            /* номер метки-параметра*/
/****************************************************************/
/* Анализатор описания дерева в составе шаблона правила         */
/* Возвращает: адрес дескриптора корня дерева в составе шаблона */
/*             NULL в случае ошибки                             */
/****************************************************************/
{
char *plex;             /* позиция текущей лексемы в буфере     */
int sts;                /* состояние разбора                    */
int stl;                /* индекс стека разбора                 */
char *name;             /* строка имени идентификатора          */
Tree l;                 /* адрес дескриптора левого поддерева   */
Tree r;                 /* адрес дескриптора правого поддерева  */
/*
int type, oper;
*/
  sts=0; stl=0; plex=bp;
  do
  {
/*
type=lex();
oper=qikpas(type,&sts,&stl,treept);
printf("<TREEPARSE> type=%d op=%d %s\n",type,oper,plex);
switch(oper)
*/
    switch(qikpas(lex(),&sts,&stl,treept))
    {
      case 1: *bp=0; break;             /* новая строка         */
      case 2: break;                    /* пропуск лексемы      */
      case 3: name=lexval.string;       /* узел дерева шаблона  */
              break;
      case 4: error("Синтаксическая ошибка описания шаблона: %s",
              plex); break;
      case 5: l=treeparse(-1);          /* левое поддерево      */
                                        /* (рекурсивный спуск)  */
              plex=bp; continue;        /* лексема установлена! */
      case 6: --bp;                             /* ОТКАТКА      */
              return tree(name,NULL,NULL,p);    /* лист         */
      case 7: return tree(name,l,NULL,p);       /* единств. сын */
      case 8: r=treeparse(-1);          /* правое поддерево     */
                                        /* (рекурсивный спуск)  */
              plex=bp; continue;        /* лексема установлена! */
      case 9: return tree(name,l,r,p);  /* полное дерево        */
      case 10:--bp; return NULL;        /* ОТКАТКА после ошибки */
      case 11:p=lexval.n; break;        /* номер параметра      */
    }
    plex=bp;            /* переход к выборке следующей лексемы  */
  }
  while(stl>=0);
}
/****************************************************************/
/* hash - вычисление хеш-функции для текстовой цепочки str      */
/****************************************************************/
static unsigned hash(char *str)
{
   unsigned h = 0;
   while (*str) h = (h<<1) + *str++;
        return h;
}
/****************************************************************/
/* lookup - поиск имени объекта входного текста в хеш-таблице   */
/****************************************************************/
static void *lookup(char *name)
{
  struct entry *p;

  /* Линейный поиск по всему списку дескрипторов, связанных с   */
  /* данной хеш-позицией после первичного хеширования имени     */

  for (p=table[hash(name)%HASHSIZE]; p!=NULL; p = p->link)
    if (strcmp(name, p->sym.name) == 0)
      return &p->sym;     /* Имя найдено, возвращается адрес    */
  return NULL;            /* Поиск оказался безуспешным         */
}
/****************************************************************/
/* install - включение нового имени в хеш-таблицу               */
/****************************************************************/
static void *install(char *name)
{
  struct entry *p = alloc(sizeof *p);  /* создание дескриптора  */
  int i = hash(name)%HASHSIZE;  /* позиция имени в хеш-таблице  */
  p->sym.name = name;           /* как нормированная хеш-функция*/
  p->link = table[i]; /* вытеснение уже существующего списка    */
                      /* имен, инцидентных данной хеш-позиции   */
  table[i] = p;       /* дескр. как новая голова этого списка   */
  return &p->sym;     /* адрес вновь созданного дескриптора     */
}
/****************************************************************/
/* nonterm - поиск нетерминала по имени в хеш-таблице; создание */
/* нового дескриптора имени, если он отсутствует и включение    */
/* этого дескриптора в хвост общего списка нетерминалов         */
/****************************************************************/
static Nonterm nonterm(char *id)/* входное имя нетерминала  */
{
Nonterm p;              /* адрес дескриптора нетерминала*/
  p = lookup(id);           /* адрес дескриптора нетерминала*/
  if ((p!=NULL) && p->kind==NONTERM) /*нетерминал уже существует*/
  {
    return p;                   /* возвращаем адрес дескриптора */
  }
  if ((p!=NULL) && p->kind == TERM)
    error("`%s' - имя ранее определенного терминала\n", id);
  error("`%s' - не определенный нетерминал\n", id);
  return addnonterm(id);
}

static Nonterm addnonterm(char* id)
{
Nonterm p,              /* адрес дескриптора нетерминала*/
    *q;                 /* адрес адреса эл.списка нетер.*/
  q = &nts;                 /* адрес адреса эл.списка нетер.*/
  p = install(id);              /* создание нового дескриптора  */
  p->kind = NONTERM; p->number = ++ntnumber;
  p->lhscount=0; p->reached=0;
  p->rules = NULL; p->chain = NULL; p->link = NULL;
  /*            Поиск хвоста списка нетерминалов :              */
  while (*q!=NULL && (*q)->number < p->number) q = &(*q)->link;
  assert(*q == NULL || (*q)->number != p->number);
  p->link = *q;                 /* включ. дескриптора в качестве*/
  *q = p;                       /* хвоста списка нетерминалов   */
  p->rules=NULL;
  return p;
}
/****************************************************************/
/* term - поиск имени терминала в хеш-таблице; создание нового  */
/* дескриптора имени, если он отсутствует и включение его в     */
/* общий список терминалов в соответствии с внешним номером esn */
/****************************************************************/
static Term term(char *id,  /* имя терминала                */
            int esn)            /* внешний символический номер  */
{
  Term p = lookup(id),          /* адрес дескриптора терминала  */
      *q = &terms;              /* адрес адреса общего списка т.*/
  if (p) error("повторное определение терминала`%s'\n", id);
  else p = install(id);         /* создание нового дескриптора  */
  p->kind = TERM; p->esn = esn; p->arity = -1;
  p->rules = NULL;
  /* Поиск места в общем списке терминалов, упорядоченном по    */
  /* внешним номерам                                            */
  while ((*q!=NULL) && (*q)->esn < esn) q = &(*q)->link;
  if ((*q!=NULL) && (*q)->esn == esn)
    error("дублирование внешнего номера терминала`%s=%d'\n",
    p->name, p->esn);
  p->link = *q;                 /* включение дескриптора в общий*/
  *q = p;                       /* список терминалов            */
  return p;
}
/****************************************************************/
/* tree - создание дескриптора узла дерева для шаблона правила  */
/****************************************************************/
static Tree tree(char *id,      /* входное имя узла             */
                 Tree left,     /* левое поддерево              */
                 Tree right,    /* правое поддерево             */
                 int par)       /* номер приписанного параметра */
{
  Tree t = alloc(sizeof *t);    /* создание дескриптора узла    */
  Term p = lookup(id);          /* поиск имени узла в таблице   */
  int arity = 0;                /* фактическая арность узла     */
  if (left && right) arity = 2;
  else if (left) arity = 1;
  if (p == NULL && arity > 0)
  {
    error("неопределенный терминал`%s'\n", id);
    p = term(id, -1);
  }
  else if (p == NULL && arity == 0) p = (Term)nonterm(id);
       else if (p && p->kind == NONTERM && arity > 0)
            {
              error("`%s'ранее определенный нетерминал`%s'\n",
                      id);
              p = term(id, -1);
            }
  if (p->kind == TERM && p->arity == -1) p->arity = arity;
  if (p->kind == TERM && arity != p->arity)
    error("недопустимая арность для нетерминала `%s'\n", id);
  t->op = p; t->par=par;
  t->nterms = p->kind == TERM;
  if (t->left = left) t->nterms += left->nterms;
  if (t->right = right) t->nterms += right->nterms;
  return t;
}
/****************************************************************/
/* rule - создание и инициализация дескриптора правила          */
/****************************************************************/
static Rule rule(char *id,      /* рез. имя правила(нетерминал) */
                 Tree pattern,  /* дерево шаблона правила       */
                 int ern,       /* внешний номер этого правила  */
                 int cost,      /* константная стоимость        */
                 char *c,       /* текст условия срабатывания   */
                 char *d,       /* текст динамической стоимости */
                 int  *auxc)    /* вектор дополнит. стоимостей  */
{
  Rule r, *q;
  Term p;
  r = alloc(sizeof *r);         /* создание дескриптора правила */
  p = pattern->op;          /* верхушка дерева шаблона      */
  nrules++;                     /* общее число правил грамматики*/
  r->lhs = nonterm(id);         /* адрес дескр.нетермин. правила*/
//kevin  r->packed = ++r->lhs->lhscount;
  r->packed = nrules;

  /* Поиск хвоста списка правил грамматики с тем же             */
  /* результирующим нетерминалом, что и у данного правила       */

  for (q = &r->lhs->rules; *q!=NULL; q = &(*q)->decode);
  *q = r;
  r->pattern = pattern; r->ern = ern; r->cost = cost;
  r->auxc = auxc; r->cond=c; r->dyncost=d;
  r->decode=NULL; r->chain=NULL; r->kids=NULL; r->link=NULL;
  if (p->kind == TERM)
  {
    r->next = p->rules;
    p->rules = r;
  }
  else if (pattern->left == NULL && pattern->right == NULL)
       {
         Nonterm p = pattern->op;
         r->chain = p->chain;
         p->chain = r;
       }
  /*     Поиск хвоста общего списка правил Д-грамматики:        */
  for (q = &rules; *q!= NULL && (*q)->ern<r->ern;q =&(*q)->link);
  if (*q && (*q)->ern == r->ern)
    error("дублированный внешний номер правила `%d'\n", r->ern);
  r->link = *q;
  *q = r;
  print("%1(* %R *)\n\n", r); 
  return r;
}

