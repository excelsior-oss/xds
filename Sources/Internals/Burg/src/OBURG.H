/****************************************************************/
/* <oburg.h> 1 августа 94    последнее изменение  17 февраля 95 */
/*    Определения констант, внутренних типов, глобалов и        */
/*  спецификаций всех собственных процедур конвертора BurBeG    */
/****************************************************************/
#ifndef BURG_INCLUDED
#define BURG_INCLUDED
#define PVSIZE 8                /* станд. размер вектора парам. */
#define PLSIZE 64               /* макс. длина имени параметра  */
#define STSIZE 128              /* размер таблицы макросов      */
#define BFSIZE 128              /* размер текстовых буферов     */
#define HASHSIZE 211            /* размер хеш-таблицы           */
#define DIMSTM 8                /* глубина вложенности макросов */
/****************************************************************/
/*    Внутреннее представление элементов входной грамматики:    */
/****************************************************************/
typedef enum
  { TERM=1, NONTERM } Kind;     /* признак терминал|нетерминал  */
                                /* в дескрипторе символа        */

typedef struct rule *Rule;  /* указатель на дескр.правила   */
typedef struct term *Term;  /* указатель на дескр.терминала */
typedef struct nonterm *Nonterm;/* Указ. на дескр. нетермин.*/
typedef struct tree *Tree;  /* указатель на дескр.дерева    */
                                /* (в составе шаблона правила)  */

struct term                     /* дескриптор терминала         */
{
        char *name;             /* имя терминала                */
        Kind kind;              /* признак "терминальности"=TERM*/
        int esn;                /* внешний номер (код) символа  */
        int arity;              /* арность как оператора        */
        Term link;              /* следующий терм. в порядке esn*/
        Rule rules;             /* список правил, чьи шаблоны   */
                                /* начинаются данным терминалом */
};

struct nonterm                  /* дескриптор нетерминала       */
{
        char *name;             /* имя терминала                */
        Kind kind;              /* пр."нетерминальности"=NONTERM*/
        int number;             /* номер-идентификатор          */
        int lhscount;           /* число правил, чьи шаблоны    */
                                /* начинаются этим нетерминалом */
        int reached;            /* признак достижимости из start*/
        Rule rules;             /* список таких правил          */
        Rule chain;             /* сп.цепных правил, чьи шаблоны*/
                                /* начинаются этим нетерминалом */
        Nonterm link;           /* Следующий нетерминал в       */
                                /* порядке возрастания номеров  */
};

struct tree                     /* дескриптор дерева в составе  */
{                               /* шаблона правила              */
        void *op;               /* корень (дескр.терм./нетерм.) */
        Tree left, right;       /* ветви  корня                 */
        int nterms;             /* число терминалов в дереве    */
        int par;                /* номер приписанного параметра */
};

struct rule                     /* дескриптор правила свертки   */
{
        Nonterm lhs;            /* результирующий нетерминал    */
                                /* ("левая часть" правила)      */
        Tree pattern;           /* шаблон данного правила       */
                                /* ("правая часть" правила)     */
        int ern;                /* Внешний номер правила        */
        int packed;             /* Упакованный внешний номер    */
        int cost;               /* основная стоимость           */
        int *auxc;              /* дополнительные стоимости     */
        Rule link;              /* Следующее правило в порядке  */
                                /* возрастания ern номеров      */
        Rule next;              /* Следующее правило с тем же   */
                                /* корнем шаблона               */
        Rule chain;             /* Следующее цепное правило с   */
                                /* левой частью данного правила */
                                /* в качестве своей правой части*/
        Rule decode;            /* Следующее правило с тем же   */
                                /* результирующим нетерминалом  */
        Rule kids;              /* Следующее правило с тем же   */
                                /* шаблоном нетерминалов-детей  */
        char *cond;             /* внешний текст доп.условия    */
        char *dyncost;          /* внешний текст дин.стоимости  */
};
extern int ntnumber;            /* общее число нетерминалов     */
extern Nonterm start;           /* начальный нетерминал         */
extern Term terms;              /* список терминалов            */
extern Nonterm nts;             /* список нетерминалов          */
extern Rule rules;              /* список правил                */
extern int nrules;              /* общее число правил           */
extern char *pronmac[STSIZE];   /* макроопределения         */
extern int costvl;              /* длина вектора стоимостей     */
extern int *auxtype;            /* типы функций доп.стоимостей  */
                                /*   (определены в oemit.c)     */
/****************************************************************/
/*                  Опции и ввод-вывод                          */
/****************************************************************/
extern int                      /*      Опции конвертора:       */
     Iflag,                     /* 1=сборка отладочной версии   */
     Tflag,                     /* 1=сборка трассирующей версии */
     Cflag;                     /* 1=сборка версии на языке СИ  */
extern char *prefix;            /* общий префикс выходных имен  */
extern int errcnt;              /* число выявленных ошибок      */
extern char inbuf[BFSIZE];      /* буфер входного текста        */
extern char *bp;                /* указатель на начало лексемы  */
                                /*   (определены  в oburg.c)    */
/****************************************************************/
/*                      Лексический анализ                      */
/****************************************************************/
/*   Коды лексических типов (прямо соотносятся с burgdf.q):     */

#define NTERM 267     /* ключевое слово "exrtaNT"             */
#define TERM 257        /* ключевое слово "term"                */
#define START 258       /* ключевое слово "start"               */
#define PPERCENT 259    /* лексема %%                           */
#define ID 260          /* идентификатор                        */
#define INT 261         /* целое число                          */
#define INCLUDE 262     /* оператор включения текта             */
#define MACRO 263       /* переопределение своб.местоимения     */
#define COST 264        /* определение длины вектора стоимости  */
#define SPEC 265        /* определение спец.функции стоимости   */
#define EMPTY 266       /* определение выр.функции стоимости    */
/****************************************************************/
/*     Спецификации собственных процедур проекта BurBeG:        */
/****************************************************************/
/*  Головной модуль oburg.c (организация общего потока данных): */
void main(int argc, char *argv[]);
int  get(void);
void include();
void error(char *fmt, ...);
void warn(char *fmt, ...);
void *alloc(int nbytes);
char *stringf(char *fmt, ...);
void print(char *fmt, ...);
/****************************************************************/
/* oparse.c (разбор входного текста и генерация вн.представл.): */
/****************************************************************/
int parse(void);
int lex(void);                                          /*static*/
int qikpas(int swt,int *sts, int *stl,
           int *pat);                                   /*static*/
char *xtext();                                          /*static*/
Tree treeparse(int p);                                  /*static*/
unsigned hash(char *str);                               /*static*/
void *lookup(char *name);                               /*static*/
void *install(char *name);                              /*static*/
Nonterm nonterm(char *id);                              /*static*/
Nonterm addnonterm(char *id);                           /*static*/
Term term(char *id, int esn);                           /*static*/
Tree tree(char *op, Tree left, Tree right, int par);    /*static*/
Rule rule(char *id, Tree pattern, int ern, int cost,    /*static*/
          char *c, char *d, int *auxc);
/****************************************************************/
/*        Mодуль oemit.c (сборка выходного текста):             */
/****************************************************************/
//int emit();
int emitimodule();
void emitdmodule(char* b, int l);
void emitnmodule(char* b, int l);

void emitactrule();
void emitactions();
void emitstate(Term terms, Nonterm start, int ntnumber,
               char *b, char *i);
void emitclosure(Nonterm nts,char *b, int l, char *i);
void emitcase1(Term p, int ntnumber, char *b, int l, char *bb);

void emitactpar(Tree t,char *v,
                       char names[PVSIZE][PLSIZE]);     /*static*/
void reach(Tree t);                                     /*static*/
void ckreach(Nonterm p);                                /*static*/
void emitaux(Rule r, char *b);                          /*static*/
void emitcase(Term p, int ntnumber, char *b, int l);    /*static*/
void emitcond(char *s, int n, char names[PVSIZE][PLSIZE]); /*static*/
void emitcost(Tree t, char *v, char *b);                /*static*/
void emitcost1(Tree t, char *v, int n, char *b);        /*static*/
void emitcost2(Tree t, char *v, int n, int *m, char *b);/*static*/
void emitdefsd(Nonterm nts, int ntnumber);               /*static*/
void emitdefsi(Nonterm nts, int ntnumber);               /*static*/
void emitfuncs(void);                                   /*static*/
void emitheader(void);                                  /*static*/
char *computekids(Tree t, char *v, char *bp, int *ip);  /*static*/
void emitkids(Rule rules, int nrules);                  /*static*/
void emitlabel(Nonterm start);                          /*static*/
void closure(int cost[], Rule rule[], Nonterm p, int c);/*static*/
void emitleaf(Term p, int ntnumber, char *b);           /*static*/
static char *computents(Tree t, char *bp, int *k);      /*static*/
void emitnts(Rule rules, int nrules);                   /*static*/
void emitrecord(char *pre, Rule r, char *b,int l,int f,
                char names[PVSIZE][PLSIZE]);            /*static*/
void emitrule0(Nonterm nts);                            /*static*/
void emitrule(Nonterm nts);                             /*static*/
void emitstring(Rule rules);                            /*static*/
void emitstruct(Nonterm nts, int ntnumber);             /*static*/
void emitterms(Term terms);                             /*static*/
int  emittest(Tree t, int nb, int pb, char * v, char *suffix); /*static*/
int  emitxtxt(char *s, int nb, char names[PVSIZE][PLSIZE]);    /*static*/

#endif
