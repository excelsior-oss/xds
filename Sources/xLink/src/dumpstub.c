#include <stdio.h>
#include <windows.h>

main ()
{
        FILE * fp;
        char buf [128];
        int i, j;

        fp = fopen ("stub.exe", "rb");
        fread (buf, 1, 128, fp);
        fclose (fp);
        ((PIMAGE_DOS_HEADER) buf) -> e_lfanew = 0;

        for (i = 0; i < 16; i ++) {
                for (j = 0; j < 8; j ++)
                        printf ("0x%02X, ", buf [8 * i + j]);
                putchar ('\n');
        }
}

