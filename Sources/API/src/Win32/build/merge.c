#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define SUFFIX ".def"

FILE * fi, * fo;

char * skipspaces (char * p)
{
	while (*p == ' ' || *p == '\t') ++p;
	return p;
}

int	testword (char ** p, char * w)
{
	char * q = * p;
	while (*w)	{
		if (*w != *q) return 0;
		++ w;
		++ q;
	}
	if (isalnum(*q)||(*q=='_')) return 0;
	*p = q;
	return 1;
}

void	catfile (char * name)
{
	FILE * fi;
	char line [256];
	char fname [128];
	char * p;
	long count;
	int scolflag;
	int wasdef;
	strcpy (fname, name);
	strcat (fname, SUFFIX);
	fi = fopen (fname, "r");
	if (!fi)	{
		fprintf (stderr, "can't open %s\n", name);
		exit(1);
	}
	printf ("importing %s\n", name);
	scolflag = 0;
	wasdef = 0;
	count = 0;
	while (fgets (line, 256, fi))	{
                ++ count;
		p = skipspaces (line);
		if (testword (&p, "DEFINITION")) {wasdef = 1; continue; }
		if (!wasdef) continue;
		if (testword (&p, "FROM") || testword (&p, "IMPORT") || scolflag)	{
			scolflag = strchr (line, ';') == NULL;
			continue;
		}
		if (testword (&p, "END"))	{
			p = skipspaces (p);
			if (testword (&p, name)) continue;
		}
		fputs (line, fo);
	}
	fclose (fi);
}

main (int argc, char ** argv)
{
	char line [256];
	char *inname, modname[256], *stubname, outname [256];
	char * p, * q;
	int c;
	FILE * sfile;

/*
	if (argc < 3)   {
		fprintf (stderr, "Two parameters required\n");
		exit (1);
	}
*/
	inname = argc > 1 ? argv [1] : "build\\import.mod";
	fi = fopen (inname, "r");
	if (!fi)	{
		fprintf (stderr, "Can't open %s\n", inname);
		exit (1);
	}
	strcpy (modname, argc > 2 ? argv [2] : "Windows");
	strcpy (outname, modname);
	strcat (outname, SUFFIX);
	fo = fopen (outname, "w");
	if (!fo)	{
		fprintf (stderr, "Can't create %s\n", outname);
		exit (1);
	}
	stubname = argc > 3 ? argv [3] : "build\\pragmas";
	sfile = fopen (stubname, "r");
	if (! sfile)	{
		fprintf (stderr, "Can't open %s\n", stubname);
		exit (1);
	}
	while ((c = getc (sfile)) != EOF) putc (c, fo);
	fclose (sfile);
		
	fprintf (fo, "DEFINITION MODULE [\"StdCall\"] %s;\n", modname);
        fprintf (fo, "\nIMPORT SYSTEM;\n");
	while (fgets (line, 256, fi))	{
		p = skipspaces (line);
		if (testword (&p, "IMPORT"))	{
			p = skipspaces (p);
			if (!isalpha (*p)) continue;
			q = p;
			while (isalnum (*p)) ++ p;
			*p = 0;
			catfile (q);
		}
	}
	fprintf (fo, "END %s.\n", modname);
	fclose (fi);
	fclose (fo);
	return 0;
}
