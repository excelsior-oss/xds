
#include "xdefs.h"
#include "xmem.h"

#define DBG_FMT_NO   0
#define DBG_FMT_HLL4 1
#define DBG_FMT_CV   2
#define DBG_FMT_NB99 4
#define DBG_FMT_EDIF 8

#define MAX_DBG_FORMATS 4

extern byte  DebugFormat;
extern const char * FirstHLL4Module;
extern const char * FirstNB99Module;
extern const char * FirstCVModule;
extern const char * FirstEDIFModule;

extern Storage * DebugInfo;

extern void CreateDebugInfo();

class OBJFile;
class Segment;

struct DebugInfoModule {
        OBJFile * file;
        Segment * first_seg, * last_seg;
        const char * name;
        char    * srcname;
        word      no;

        dword     name_offs,     name_len,         /* for CodeView debug info */
                  linenums_offs, linenums_len,
                  align_offs,    align_len;

        struct DebugInfoModule * next;
};

extern word NDebugInfoModules;
extern struct DebugInfoModule * DebugInfoModules;

extern void CheckDebugInfoFormat ();
