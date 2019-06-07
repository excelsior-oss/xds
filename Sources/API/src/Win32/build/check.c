#include <stdio.h>
#include <string.h>

char buf [4000];
char tok [1000] = {0};
int wasequ = 0;

FILE * mfile, * cfile;

void	take (char * s)
{
	if (!strcmp (s, "PARALLELOGRAM")) return;
	fprintf (mfile, "STextIO.WriteString (\"%s \"); SWholeIO.WriteInt (SIZE (Windows.%s), 0); STextIO.WriteLn;\n", s, s);
	fprintf (cfile, "printf (\"%s  %%ld\\n\",", s);
	if (!strcmp (s, "VALCONTEXT"))
		s = "*valcontext";
	fprintf (cfile, " sizeof (%s));\n", s);
}

void token (char * s, int l)
{
	char c = s[l];
	s[l] = 0;
	if (wasequ && !strcmp (s, "RECORD"))
		take (tok);
	wasequ = s [0] == '=';
	if (!wasequ)
		strcpy (tok, s);
	s[l] = c;
}

int main (int argc, char ** argv)
{
	FILE * f;
	int l, i, s;
	
	if (argc <= 1)	{
		fprintf (stderr, "No file name\n");
		exit (1);
	}
	f = fopen (argv [1], "r");
	if (!f)	{
		fprintf (stderr, "Can't open %s\n", argv [1]);
		exit (1);
	}

	mfile = fopen ("chkm.mod", "w");
	cfile = fopen ("chkc.c", "w");

	fprintf (cfile, "#define INC_OLE1 1\n");
	fprintf (cfile, "#define _WIN32_WINNT 0x400\n");
	fprintf (cfile, "#include <stdio.h>\n#include <windows.h>\n\nint main (void)\n{\n");
	fprintf (cfile, "\nPVALCONTEXT valcontext;\n");

	fprintf (mfile, "MODULE chkm;\n\nIMPORT STextIO, SWholeIO;\nIMPORT Windows;\n\nBEGIN\n");

	while (fgets (buf, sizeof (buf)-1, f))	{
		l = strlen (buf);
		if (buf [l-1] == '\n')	{
			buf [l-1] = 0;
			l--;
		}
		i = 0;
		for (;;)	{
			while (i < l && isspace (buf [i])) ++i;
			if (i >= l) break;
			if (isdigit (buf [i]))	{
				s = i;
				while (isdigit (buf [i])) ++i;
				token (buf+s, i-s);
				continue;
			}
			if (isalpha (buf [i]))	{
				s = i;
				while (isdigit (buf [i]) ||
				       isalpha (buf [i]) ||
				       buf [i] == '_') ++i;
				token (buf+s, i-s);
				continue;
			}
			token (buf+i, 1);
			++i;
		}
	}
	fprintf (cfile, "}\n");
	fprintf (mfile, "END chkm.\n");
	return 0;
}