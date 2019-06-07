
typedef enum
   {
      STB_NONE,
      STB_NUM,
      STB_STR,
      STB_RAW,
      STB_KEY,
   }  stbType;

typedef struct
   {
      stbType type;
      union
      {
         long num;
         const char *text;
      } val;
   } stbVal;

#define STB_FL_PERMANENT   1
#define STB_FL_INSERT      0x10000L

#define StbLookup(x,y) StbLookupEx((x),(y),0)
stbVal *StbLookupEx(const char *str,int flags,int *rflags);
int StbDelete(const char *str);
void StbRead(const char *name);

#ifdef _DEBUG
void StbDump(const char *name);
#endif

