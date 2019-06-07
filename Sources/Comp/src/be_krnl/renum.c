#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define TEMPNAME        "tempfile.tmp"

int main (int argc, char * argv [])
{
        FILE * in, * out;
        char buf [1024];
        char * p, * q;
        int len;
        int count = 0;

        if (argc != 2) {
                puts ("Usage: renum file");
                exit (1);
        }
        in = fopen (argv [1], "r");
        if (in == NULL) {
                puts ("Unable to open input file");
                exit (1);
        }
        out = fopen (TEMPNAME, "w");
        if (out == NULL) {
                fclose (in);
                puts ("Unable to create output file");
                exit (1);
        }
        buf [sizeof (buf) - 3] = 0;
        fgets (buf, sizeof (buf) - 3, in);
        while (! feof (in)) {
                len = strlen (buf);
                if (len) {
                        p = & buf [len - 1];
                        if (* p == '\n')
                                p --;
                        while (p != buf && isspace (* p))
                                p --;
                        if (p == buf || ! isdigit (* p))
                                goto next;
                        do {
                                p --;
                        } while (p != buf && isdigit (* p));
                        q = p + 1;
                        while (p != buf && isspace (* p))
                                p --;
                        if (p == buf || * p != '/' ||
                            (p - 1) == buf || p [-1] != '/')
                                goto next;
                        itoa (++ count, q, 10);
                        q += strlen (q);
                        * q ++ = '\n';
                        * q = 0;

                }
next:           if (fputs (buf, out) < 0) {
                        fclose (in);
                        fclose (out);
                        remove (TEMPNAME);
                        puts ("Unable to write output file");
                        exit (1);
                }
                fgets (buf, sizeof (buf) - 3, in);
        }
        fclose (in);
        if (fclose (out)) {
                remove (TEMPNAME);
                puts ("Unable to write output file");
                exit (1);
        }
        remove (argv [1]);
        rename (TEMPNAME, argv [1]);
        return 0;
}
