#include <stdio.h>
#include <windows.h>

main ()
{
        FILE * fp;
        char buf [128];

        fp = fopen ("lnk.exe", "rb");
        fread (buf, 1, 128, fp);
        fclose (fp);
        ((PIMAGE_DOS_HEADER) buf) -> e_lfanew = 0;
        fp = fopen ("stub.exe", "wb");
        fwrite (buf, 1, 128, fp);
        fclose (fp);
        return 0;
}

