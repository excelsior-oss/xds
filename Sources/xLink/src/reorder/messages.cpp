
#include "xdefs.h"
#include "xdebug.h"
#include "messages.h"
#include "xmem.h"

#include <string.h>

int      TotalErrors   = 0;
int      TotalWarnings = 0;

struct msg{
  short   level;
  char  * text;
};

struct msg Messages[MESSAGE_TYPES] = {
  {1, "Unable to open file %s" },
  {1, "Unable to write file %s" },
  {1, "Unable to read file %s" },
  {1, "Insufficient memory" },
  {1, "Empty file %s" },
  {1, "File %s too long" },
  {1, "File %s (%d:%d) - string isn't closed" },
  {1, "File %s (%d:%d) - %s expected"},
  {1, "Component relocation detected, profile is inaccurate"},
  {1, "Component \"%s\" not found (use full path to desired component)"},
  {1, "Unknown directive %s in link info file %s"},
  {1, "PF detected: %d."}
};

void putMsg(int number, dword code, char * text, dword level) {
    if (code != xMESSAGE) {
        printf("%s (%d): ", code == xFATAL   ? "Fatal error" :
                            code == xERROR   ? "Error"       :
                            code == xWARNING ? "Warning"     : "Message", number);
    }
    puts (text);

    if (code == xFATAL) {
        exit (255);
    } else if (code == xERROR) {
        TotalErrors ++;
    } else if (code == xWARNING) {
        TotalWarnings ++;
    } else if (code != xMESSAGE) {
        ASSERT_FALSE();
    }
}


void Message(int code, int number,...) {
  char           buf [1024];
  va_list        vl;

  va_start(vl, number);
  vsprintf(buf, Messages[number].text, vl);
  putMsg(number, code, buf, Messages[number].level);
  va_end(vl);
}
