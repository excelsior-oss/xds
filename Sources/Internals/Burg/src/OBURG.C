/****************************************************************/
/*  <oburg.c> 1 августа 94   последнее изменение:  3 февраля 94 */
/*           Bottom-Up Rewriting Back-End Generator             */
/*    Конвертор BurBeG (модификация системы iburg) производит   */
/*  сборку селектора для выбора последовательности команд       */
/*  оптимальной стоимости для линейных участков программы в     */
/*  составе генератора кода для произвольной целевой машины     */
/*  методом восходящего редуцирующего покрытия дерева           */
/*  внутреннего представления линейных участков шаблонами       */
/*  правил с динамическим сравнением стоимости всех возможных   */
/*  вариантов (время работы алгоритма линейно по отношению к    */
/*  размеру покрываемого дерева).                               */
/*    Выходом системы является текст на языке Оберон-2 или Си.  */
/*    Входом является текст на языке BURG+, описывающий любую   */
/*  редуцирующую грамматику покрытия деревьев, составленных из  */
/*  терминальных символов промежуточного представления линейных */
/*  участков исходной программы. В отличии от системы iburg     */
/*  возможно включение дополнительных динамических условий      */
/*  применения правил в форме выражений над значениями атрибутов*/
/*  узлов покрываемого дерева (как в системе BEG), и сходным    */
/*  образом вычисляемых динамических стоимостей правил,         */
/*  зависящих от текущих значений атрибутов исходного дерева,   */
/*  но однозначно определенных в момент вычисления оптимального */
/*  покрытия. Выражения условий и динамических стоимостей       */
/*  кодируются на внешнем инструментальном языке реализации     */
/*  целевого компилятора (Обероне-2 или Си). Возможно включение */
/*  в текст описания грамматики фрагментов семантической        */
/*  обработки правил на внешнем языке реализации, оперирующих   */
/*  текущими значениями атрибутов покрываемого дерева.          */
/*                                                              */
/*  (С) Ю.Погудин, xTech Ltd., 1994    Новосибирск, т.35-11-53  */
/****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "oburg.h"              /* Константы, собственные типы  */
                                /* и спецификации всех процедур */
                                /*      Опции конвертора:       */
char *prefix = "NT";          /* Общий префикс выходных имен  */
int  Iflag = 0,                 /* 1=сборка отладочной версии   */
     Tflag = 0,                 /* 1=сборка трассирующей версии */
     Cflag = 0;                 /* 1=сборка версии на языке СИ  */

char * computer = "386";

static FILE *infp = NULL;       /* Файл входного текста         */
static FILE *outfp = NULL;      /* Файл выходного текста        */
static FILE *outfp1 = NULL;     /* Файл выходного текста        */
static FILE *incp = NULL;       /* Файл вх.текста (include)     */

int    lexineno;                /* Номер входной строки         */
static outline=0;               /* Номер выходной строки        */

char inbuf[BFSIZE];             /* Буфер входного текста        */
char *bp;                       /* Указатель на начало лексемы  */
int errcnt=0;                   /* Число выявленных ошибок      */

char bf1 [80], bf2 [80];

/****************************************************************/
/* Монитор системы: анализ командной строки, открытие файлов    */
/*                  вызов фаз конвертирования                   */
/****************************************************************/
void main(int argc, char *argv[])
{
int i, j;                       /* вспомогательные счетчики     */
char b[BFSIZE];                 /* буфер для модификации имен   */
char bb[BFSIZE];                /* буфер для модификации имен   */
char *a;                        /* указ. на вх.строку параметра */
int c;                          /* буферная литера вх.текста    */
/****************************************************************/
/* Извлечение опций и имен файлов из командной строки MS-DOS    */
/****************************************************************/
  if(argc<2) goto empty;
  for (i = 1; i < argc; i++)
    if (strcmp(argv[i], "-I") == 0)      Iflag = 1;
    else if (strcmp(argv[i], "-T") == 0) Tflag = 1;
    else if (strcmp(argv[i], "-C") == 0) Cflag = 1;
    else if (strncmp(argv[i], "-p", 2) == 0 && argv[i][2])
             prefix = &argv[i][2];
    else if (strncmp(argv[i], "-p", 2) == 0 && i + 1 < argc)
             prefix = argv[++i];
    else if (strncmp(argv[i], "-m", 2) == 0 && argv[i][2])
         computer = &argv[i][2];
    else if (strncmp(argv[i], "-m", 2) == 0 && i + 1 < argc)
         computer = argv[++i];
    else if (*argv[i] == '-' && argv[i][1])
            {
    empty:    error("вызов: %s [-T | -I | -С | -p prefix | -m machine]... "
                    "input[.b] [output[.o|.c]]\n",argv[0]);
              exit(1);
            }
    else if (infp == NULL)      /* файл входного текста         */
         {
           if (strcmp(argv[i], "-") == 0) infp = stdin;
           else
           {
             for(a=argv[i],j=0;*a!=0 && *a!='.';++a) b[j++]=*a;
             if(*a==0) b[j++]='.',b[j++]='b',b[j]=0;
             else { for(;*a!=0;++a) b[j++]=*a; b[j]=0; }
             if ((infp = fopen(b, "r")) == NULL)
             {
                error("%s: не открывается для чтения файл `%s'\n",
                        argv[0],b);
                exit(1);
             }
           }
         }
    else if (outfp == NULL)     /* файл выходного текста        */
         {
           if (strcmp(argv[i], "-") == 0)  outfp = stdout;
           else
           {
             for(a=argv[i],j=0; *a!=0 && *a!='.';++a) b[j++]=*a;
             if(*a==0)
             {
               b[j++]='_'; b[j++]='d'; b[j++]='.';
               b[j++]=Cflag?'c':'o'; b[j]=0;
             }
             else
             {
               b[j++]='_'; b[j++]='d';
               for(;*a!=0;++a) b[j++]=*a;
               b[j]=0;
             }
    ow:      if ((outfp = fopen("Burg.o", "w")) == NULL)
             {
               error("%s: не открывается для записи файл `%s'\n",
                       argv[0], b);
               exit(1);
             }
           }
         }
    if (infp == NULL)  exit(1);
    if (outfp == NULL)  /* явное имя выходного файла опущено    */
      if(j>2)
      {
//        b[j-2]='.';  
//        b[j-1]=Cflag?'c':'o';
          b[j]=0; goto ow;
      }
      else outfp=stdout;

    fputs ("<*+WOFF*>\n", outfp);
    print ("-- source grammar = %s\n", b);
    print ("MODULE Burg;\n");
    print ("IMPORT BurgNT;\n");

/****************************************************************/
    lexineno=1;
    terms=NULL; nts=NULL; rules=NULL;
    bp=inbuf;
    *bp=0; get(); --bp; /* пропуск текста преамбулы в вых.файл  */

/****************************************************************/
    parse();          /* анализ входного описания на языке BURG,*/
                      /* генерация внутреннего представления и  */
                      /* сборка частей процедуры actions        */
/****************************************************************/
    printf("Проанализировано строк: %d. Найдено ошибок: %d\n",
            lexineno, errcnt);
    if(errcnt)
    {
el:    printf("Выходной файл не может быть корректно использован!\n");
       exit(1);
    }
    if(Cflag) print("  }\n}\n\n");      /* хвост для actions    */
    else print("  ELSE\n  END;\nEND %Pactions;\n\n");
/****************************************************************/
    emitimodule();        /* Сборка основного текста селектора    */
//    emit();
//    b[j-2]=0;             
    emitclosure (nts, b, j-2, bf1);
//    b[j-2]=0;
    emitstate(terms, start, ntnumber,b, bf2); 
    outfp1=outfp;
//e386d.o
//    b[j-2]='d'; b[j-1]='.'; b[j]=Cflag?'c':'o';
    if ((outfp = fopen("BurgNT.o", "w")) == NULL)
    {
      error("%s: не открывается для записи файл `%s'\n",
      argv[0],b); exit(1);
    }
//    b[j-1]=0;             /*сборка процедур closure в отд.модуле*/
//    strcpy (bf1, "ir, R := r");
//    strcat (bf1, computer);
    emitdmodule(b,j-2);
//    emitdefs(nts, ntnumber);    /* сборка определений нетерм.   */
//    emitstruct(nts, ntnumber);  /* сборка вых.структуры state   */

//    emitclosure (nts, b, j-2, bf1);
    fclose(outfp);
//e386n.o
//    b[j-2]='n'; b[j-1]='.'; b[j]=Cflag?'c':'o';
    if ((outfp = fopen("BurgTables.o", "w")) == NULL)
    {
      error("%s: не открывается для записи файл `%s'\n",
      argv[0],b); exit(1);
    }
//    b[j-1]=0;             /*сборка процедур closure в отд.модуле*/
//    strcpy (bf1, "ir, R := r");
//    strcat (bf1, computer);
    emitnmodule(b,j-2);
    fclose(outfp);

//    b[j-3]='s'; b[j-2]='.'; b[j-1]=Cflag?'c':'o'; b[j]=0;
//    if ((outfp = fopen(b, "w")) == NULL)
//    {
//      error("%s: не открывается для записи файл `%s'\n",
//      argv[0],b); exit(1);
//    }
//    if (start)  /* сборка процедуры state в отдельном модуле    */
//      { b[j-2]=0;
//    strcpy (bf2, "ir, StdIO := opIO, R := r");
//    strcat (bf2, computer);
//    emitstate(terms, start, ntnumber,b, bf2); }
//
//    fclose(outfp);
//
    outfp=outfp1;
/****************************************************************/
    if (!feof(infp))  /* вывод текста пост-скриптума в вых.файл */
      while (fgets(inbuf, sizeof inbuf, infp))
      {
        fputs(inbuf, outfp); ++outline;
      }
    print("END Burg.\n");

    printf("Собрано строк: %d. Найдено ошибок: %d\n",
            outline, errcnt);
    if(errcnt) goto el;
    else printf("Успешная сборка\n");
}
/****************************************************************/
/* чтение очередной литеры с пропуском текста %{...}% в вых.файл*/
/****************************************************************/
int get(void)
{
  if (*bp == 0)                         /* строка исчерпана     */
  {
rl: if (fgets(inbuf, sizeof inbuf, infp) == NULL)
    {
ri:   if(incp==NULL) return EOF;
      infp=incp; incp=NULL; goto rl;
    }
    bp = inbuf;
    lexineno++;
    while (inbuf[0] == '%' && inbuf[1] == '{' && inbuf[2] == '\n')
    {
      for (;;)
      {
        if (fgets(inbuf, sizeof inbuf, infp) == NULL)
        {
          warn("оборванный текст %{...%}\n");
          goto ri;
        }
        lexineno++;
        if (strcmp(inbuf, "%}\n") == 0) break;
        fputs(inbuf, outfp); ++outline;
      }
      if (fgets(inbuf, sizeof inbuf, infp) == NULL) goto ri;
      lexineno++;
    }
  }
  return *bp++;
}
/****************************************************************/
/*  Включение текстового файла в пределах преамбулы             */
/****************************************************************/
void include ()
{
char *bf;
  if(incp)
  {
     error("Вложенные %include недопустимы\n");
     return;
  }
  incp=infp;
  while(*bp==' '|| *bp=='\t') ++bp; bf=bp;
  while(*bp!=' '&& *bp!='\t' && *bp!='\n' && *bp!=0 && *bp!='/')
    ++bp; *bp=0;
  if ((infp = fopen(bf, "r")) == NULL)
  {
     error("не открывается для чтения include-файл `%s'\n",bf);
     exit(1);
  }
  *bp=0;
}
/****************************************************************/
/*  Сообщение конвертора об ошибке                              */
/****************************************************************/
void error(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  if (lexineno > 0)
    fprintf(stderr, "строка %d: ", lexineno);
  vfprintf(stderr, fmt, ap);
  if (fmt[strlen(fmt)-1] != '\n') fprintf(stderr, "\n");
  errcnt++;
}
/****************************************************************/
/*  Предупреждение конвертора об ошибке                         */
/****************************************************************/
void warn(char *fmt, ...)
{
        va_list ap;

        va_start(ap, fmt);
        if (lexineno > 0) fprintf(stderr, "строка %d: ", lexineno);
        fprintf(stderr, "Предупреждение: ");
        vfprintf(stderr, fmt, ap);
}
/****************************************************************/
/* alloc - получение nbytes памяти с авостом в случае неудачи   */
/****************************************************************/
void *alloc(int nbytes)
{
    void *p = malloc(nbytes);
        if (p == NULL)
        {
                error("не хватает памяти!\n");
                exit(1);
        }
        return p;
}
/****************************************************************/
/* stringf - форматный вывод в новую строку необходимой длины   */
/****************************************************************/
char *stringf(char *fmt, ...)
{
va_list ap;
char *s,
     buf[512];          /* буфер для форматного вывода          */
  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);
  va_end(ap);
  return strcpy((char *)alloc(strlen(buf) + 1), buf);
}
/****************************************************************/
/* print - собственный форматированный вывод конвертора         */
/****************************************************************/
void print(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  for ( ; *fmt; fmt++)
  {
    if (*fmt == '\n') ++outline;   /*  подсчет числа вых. строк */
    if (*fmt == '%')
      switch (*++fmt)
      {
        case 'd': fprintf(outfp, "%d", va_arg(ap, int)); break;
        case 's': fputs(va_arg(ap, char *), outfp); break;
        case 'c': fprintf(outfp, "%c", va_arg(ap, int)); break;
        case 'P': fprintf(outfp, "%s", prefix);
                  break;
        case 'T': {
                    Tree t = va_arg(ap, Tree);
                    print("%S", t->op);
                    if (t->left && t->right)
                      print("(%T,%T)", t->left, t->right);
                    else if (t->left)
                           print("(%T)", t->left);
                    break;
                  }
        case 'R': {
                    Rule r = va_arg(ap, Rule);
                    print("%S: %T", r->lhs, r->pattern);
                    break;
                  }
        case 'S': fputs(va_arg(ap, Term)->name, outfp); break;
        case '1': case '2': case '3': case '4': case '5':
                  {
                    int n = *fmt - '0';
                    while (n-- > 0) putc('\t', outfp);
                    break;
                  }
        default:  putc(*fmt, outfp); break;
      }
    else putc(*fmt, outfp);
  }
  va_end(ap);
}
