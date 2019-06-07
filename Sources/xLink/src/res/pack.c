
#include <stdio.h>
#include <malloc.h>
#include "zlib.h"


int main(int argc, char **argv)
{
    int err, i, j;
    FILE *in, *out;
    char *in_data, *out_data;
    unsigned long in_size, out_size;
    unsigned int compr_len[255];
    unsigned int uncompr_len[255];

    if (argc <= 1) {
        printf ("usage: pack <file-to-compress-1> ... <file-to-compress-n>\n");
        printf ("       dump data definitions to the packed_data.c\n");
        return 0;
    }

    out = fopen ("packed_data.c", "w");
    if (out == NULL) {
        printf ("Unable to open packed_data.c\n");
        return 1;
    }

    fprintf (out, "\n");
    fprintf (out, "#include \"packed_data.h\"\n");
    fprintf (out, "\n");
    for (i = 1; i < argc; i++) {
        in = fopen (argv[i], "rb");
        if (in == NULL) {
            printf ("Can't open input file %s\n", argv[i]);
            return 1;
        }
        fseek (in, 0L, SEEK_END);
        in_size = ftell (in);
        fseek (in, 0L, SEEK_SET);
        in_data = malloc (in_size);
        memset (in_data, 0, in_size);
        if (fread (in_data, 1, in_size, in) != in_size) {
            fclose (in);
            printf ("Unable to read input file %s\n", argv[i]);
            return 1;
        }
        fclose (in);

        out_size = compressBound(in_size);
        out_data = malloc (out_size);
        memset (out_data, 0, out_size);

        if ((err = compress(out_data, &out_size, (const Bytef*)in_data, in_size)) != Z_OK) {
            printf ("Compress error: %d\n", err);
            return 1;
        }

        fprintf (out, "unsigned char packed_data_%d[] = {\n", i);
        for (j = 0; j < out_size; j++) {
            if (j % 8 == 0)  fprintf (out, "  ");
            fprintf (out, "0x%02x", (int)(unsigned char)out_data[j]);
            if (j != out_size-1) fprintf (out, ", ");
            if (j % 8 == 7)  fprintf (out, "\n");
        }
        fprintf (out, "};\n\n");
        free (out_data);

        compr_len[i]   = out_size;
        uncompr_len[i] = in_size;
    }

    fprintf (out, "PackedFile PackedData[] = {\n");
    for (i = 1; i < argc; i++) {
        fprintf (out, "  {\"%s\", packed_data_%d, %lu, %lu},\n", argv[i], i, compr_len[i], uncompr_len[i]);
    }
    fprintf (out, "  {0, 0, 0}\n};\n\n");
    fclose (out);

    return 0;
}
