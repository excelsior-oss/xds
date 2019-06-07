#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char * modules [50];
int nmodules;
char * prefix;

char *	buf = NULL;
int	buflen = 0;
char *	ptr;

FILE * fout;

#define is_alpha(c) (c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_')
#define is_alnum(c) (is_alpha(c) || c >= '0' && c <= '9')

enum	MODE {
	mode_none,
	mode_var,
	mode_const,
	mode_type
}
	cur_mode, last_mode;

char * cur_module;

struct	WORD {
		char * p;
		int len;
	};

#define NWORDS 40
struct WORD words [NWORDS];
int	nwords;

void	prword (char * p, int len)
{
	while (len--) putchar (*p++);
}

void	outword (char * p, int len)
{
	while (len--)	{
		putc (*p, fout);
		++p;
	}
}

void	add_symbol (char * p, int len, enum MODE mode)
{
	int i;
	for (i = nwords-1; i >= 0; i--)	{
		if (len == words [i].len && !strncmp (p, words [i].p, len))
			return;
	}
	if (nwords == NWORDS)	{
		memmove (words, words+1, (NWORDS-1) * sizeof (struct WORD));
		-- nwords;
	}
	words [nwords].p = p;
	words [nwords].len = len;
	++ nwords;
	switch (mode)	{
	case mode_var:
		printf ("VAR ");
		prword (p, len);
		printf (": not supported: ignored\n");
		break;
	case mode_const:
		fputs (mode == last_mode ? "      " : "\nCONST ", fout);
		break;
	case mode_type:
		fputs (mode == last_mode ? "     " : "\nTYPE ", fout);
		break;
	}
	last_mode = mode;
	outword (p, len);
	fputs (" = ", fout);
	fputs (cur_module, fout);
	putc ('.', fout);
	outword (p, len);
	fputs (";\n", fout);
}

void	get_lexem (char ** p, int * len)
{
	int nest;
	
	*p = 0;
	*len = 0;	
again:
	while (*ptr == ' ' || *ptr == '\n' || *ptr == '\t')
		++ptr;
	if (!*ptr) return;
	if (*ptr == '-' && ptr[1]=='-')	{
		while (*ptr != '\n') ++ptr;
		goto again;
	}
	if (*ptr == '(' && ptr[1] == '*')	{
		ptr += 2;
		nest = 1;
		while (nest && *ptr)	{
			if (ptr[0] == '*' && ptr[1]==')')	{
				ptr += 2;
				-- nest;
			} else if (ptr[0] == '(' && ptr[1]=='*')	{
				ptr += 2;
				++ nest;
			} else
				ptr ++;
		}
		goto again;
	}
	if (*ptr == '\'' || *ptr == '"')	{
		*p = ptr++;
		while (*ptr && *ptr != **p && *ptr != '\n')
			++ptr;
		if (*ptr == **p) ++ptr;
		*len = ptr - *p;
		return;
	}
	if (is_alnum (*ptr))	{
		*p = ptr;
		while (is_alnum (*ptr)) ++ptr;
		*len = ptr - *p;
		return;
	}
	*p = ptr++;
	* len = 1;
}

void	lexem (char ** p, int * len)
{
again:
	get_lexem (p, len);
	if (!*len) return;
	if (*len != 1 || **p != '<') return;
	get_lexem (p, len);
	if (!*len) return;
	if (*len != 1 || **p != '*') return;
	for (;;)	{
		get_lexem (p, len);
		if (!*len) return;
		if (*len != 1 || **p != '*') continue;
		get_lexem (p, len);
		if (!*len) return;
		if (*len != 1 || **p != '>') continue;
		goto again;
	}
}

int	equ (char * p, int len, char * s)
{
	if (len != (int) strlen (s)) return 0;
	return !strncmp (p, s, len);
}

void	skip_to_char (char c)
{
	char * p;
	int len;
	for (;;)	{
		lexem (&p, &len);
		if (!len || len==1 && *p == c) break;
	}
}

void	process_const (char * p, int len)
{
	cur_mode = mode_const;
	add_symbol (p, len, mode_const);
	skip_to_char (';');
}

void	process_var (char * p, int len)
{
	cur_mode = mode_var;
	add_symbol (p, len, mode_var);
	skip_to_char (';');
}

void	process_procedure (void)
{
	char * p;
	int len;
	cur_mode = mode_none;
	lexem (&p, &len);
	if (equ (p, len, "["))	{
		skip_to_char (']');
		lexem (&p, &len);
	}
	add_symbol (p, len, mode_const);
	lexem (&p, &len);
	if (equ (p, len, ";"))
		return;
	skip_to_char (')');
	skip_to_char (';');
}

void	process_type (char * p, int len)
{
	int nest;

	cur_mode = mode_type;
	add_symbol (p, len, mode_type);
	lexem (&p, &len);
	if (len != 1 || *p != '=')	{
		printf ("Error: no '=' after TYPE\n");
		exit (1);
	}
	lexem (&p, &len);
	if (!len)	{
		printf ("Error: no type declaration after '='\n");
		exit (1);
	}
	if (!equ (p, len, "RECORD"))	{
		skip_to_char (';');
		return;
	}
	nest = 1;
	while (nest)	{
		lexem (&p, &len);
		if (!len) break;
		if (equ (p, len, "RECORD"))
			++ nest;
		else if (equ (p, len, "CASE"))
			++ nest;
		else if (equ (p, len, "END"))
			-- nest;
	}
	skip_to_char (';');
}

void	scan_file (void)
{
	char * p;
	int len;

	ptr = buf;
	cur_mode = last_mode = mode_none;
	nwords = 0;
	for (;;)	{
		lexem (&p, &len);
		if (!len) break;
		if (equ (p, len, "DEFINITION") ||
		    equ (p, len, "MODULE") ||
		    equ (p, len, "IMPORT") ||
		    equ (p, len, "FROM"))
			skip_to_char (';');
		else if (equ (p, len, "CONST"))	{
			lexem (&p, &len);
			process_const (p, len);
		} else if (equ (p, len, "TYPE"))	{
			lexem (&p, &len);
			process_type (p, len);
		} else if (equ (p, len, "VAR"))	{
			lexem (&p, &len);
			process_var (p, len);
		} else if (equ (p, len, "PROCEDURE"))
			process_procedure ();
		else if (equ (p, len, "END"))	{
			return;
		} else if (is_alpha (p[0]))	{
			switch (cur_mode)	{
			case mode_none:
				printf ("Unknown symbol found:");
				prword (p, len);
				printf ("\n");
				return;
			case mode_var:
				process_var (p, len);
				break;
			case mode_const:
				process_const (p, len);
				break;
			case mode_type:
				process_type (p, len);
				break;
			}
		} else	{
			printf ("Unknown symbol found:");
			prword (p, len);
			printf ("\n");
			return;
		}
	}
}

void	make_import_list (void)
{
	int len;
	char * p, * s;

	nmodules = 0;
	ptr = buf;
	lexem (&p, &len);
	if (!len)	{
		printf ("Empty main module\n");
		exit (1);
	}
	if (!equ (p, len, "MODULE"))	{
		printf ("Module not starting with MODULE\n");
		exit (1);
	}
	if (p != buf)	{
		prefix = malloc (p-buf+1);
		if (!prefix)	{
			printf ("Out of memory\n");
			exit (1);
		}
		memcpy (prefix, buf, p-buf);
		prefix [p-buf] = 0;
	} else
		prefix = "";

	skip_to_char (';');
	for (;;)	{
		lexem (&p, &len);
		if (!len)	{
			printf ("Unexpected termination of main module\n");
			exit (1);
		}
		if (!equ (p, len, "IMPORT"))
			break;

		lexem (&p, &len);
		if (!len)	{
			printf ("Unexpected termination of main module\n");
			exit (1);
		}
		s = malloc (len+1);
		if (!s)	{
			printf ("Out of memory\n");
			exit (1);
		}
		memcpy (s, p, len);
		s[len] = 0;
		modules [nmodules++] = s;
		skip_to_char (';');
	}
}

void	read_file (char * fname)
{
	FILE * f;
	int l, len;
	
	f = fopen (fname, "r");
	if (!f)	{
		printf ("Can't open file %s\n", fname);
		exit (1);
	}
	if (fseek (f, 0, SEEK_END))	{
		printf ("Can't seek file %s\n", fname);
		exit (1);
	}
	len = ftell (f) + 1;
	if (len > buflen)	{
		buf = realloc (buf, len);
		if (!buf)	{
			printf ("Out of memory error\n");
			exit (1);
		}
		buflen = len;
	}
	fseek (f, 0, SEEK_SET);
	l = fread (buf, 1, len-1, f);
	if (l >= len) exit (1);
	buf [l] = 0;
	fclose (f);
}

void	make_file (char * s)
{
	char f [100];
	int i;
	
	sprintf (f, "%s.def", s);
	fout = fopen (f, "w");
	if (!fout)	{
		printf ("Can't create file %s\n", f);
		exit (1);
	}
	fputs (prefix, fout);
	fprintf (fout, "<* M2EXTENSIONS+ *>\nDEFINITION MODULE [\"StdCall\"] %s;\n\n", s);
	for (i = 0; i < nmodules; i++)
		fprintf (fout, "IMPORT %s;\n", modules [i]);
	
	for (i = 0; i < nmodules; i++)	{
		printf ("%s\n", modules [i]);
		sprintf (f, "%s.def", modules [i]);
		read_file (f);
		cur_module = modules [i];
		fprintf (fout, "\n(*\n");
		fprintf (fout, "***************** Definitions from %s *****************\n", cur_module);
		fprintf (fout, "*)\n\n");
		scan_file ();
	}
	fprintf (fout, "\nEND %s.\n", s);
	fclose (fout);
}

int	main (int argc, char ** argv)
{
	char *in  = argc > 1 ? argv[1] : "build\\import.mod";
	char *out = argc > 2 ? argv[2] : "Windows";
/*
	if (argc < 3)	{
		printf ("Usage: trans source-file resulting-module\n"
			"   where source file is main module that imports all required defs;\n"
			"   resulting-module is name of module to build (without .def extension)\n\n");
		return 1;
	}
*/
	read_file (in);
	make_import_list ();
	make_file (out);
	return 0;
}
