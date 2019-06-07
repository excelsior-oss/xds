#include <windows.h>
#include <commctrl.h>
#include <stdio.h>
#include "profapi.h"
#include "mdi.h"
#include "res.h"


#define	DEFEXT "xpt"
char open_filter[] =	"Profiler trace files (*.xpt)\0*.xpt\0"
			"All files (*.*)\0*.*\0";

#define ABSCOLOR RGB (255, 0, 0)
#define RELCOLOR RGB (0, 128, 0)

HINSTANCE	MyInstance;
HWND		FrameWindow;
HFONT		SystemFont;
HFONT		PlainFont;
HFONT		FixedFont;
HFONT		HeaderFont;
#define ProgTitle "XDS Profile viewer"
#define NT35 (LOBYTE (LOWORD (GetVersion())) <= 3)

int	same_window_mode = 0;

HWND	com_wnd;
HWND	mod_wnd;
HWND	proc_wnd;
HWND	calls_wnd;
HWND	line_wnd;
HWND	all_mod_calls_wnd;
HWND	all_com_calls_wnd;
HWND	all_prg_calls_wnd;

char	progname [100];

MDIPLACEMENT component_placement;
MDIPLACEMENT module_placement;
MDIPLACEMENT proc_placement;
MDIPLACEMENT calls_placement;
MDIPLACEMENT text_placement;

BOOL	component_placement_present;
BOOL	mod_placement_present;
BOOL	proc_placement_present;
BOOL	calls_placement_present;
BOOL	text_placement_present;

#define SYSTEMFONT 0
#define PLAINFONT 1
#define FIXEDFONT 2

#define YDELTA 2

#define CONTROL_ID (LOWORD (wparam))
#define NOTIFY_CODE (HIWORD (wparam))
#define CONTROL_HWND (HWND) lparam

#define HIBIT  0x80000000u
#define LOMASK 0x7FFFFFFFu

#define PERCENT(x,s) ((s)==0?0 : ((x) >= (s)) ? 100.0 : 100.0 * (double) (x) / (s))
#define TOINT64(lo,hi)  ((__int64)(lo) + ((__int64)(hi))*(__int64)(HIBIT)*2)
#define TODBL(lo,hi)  ((double)(lo) + (double)(hi)*(double)(HIBIT)*2.0)

void ADD64(unsigned * lo,unsigned * hi,unsigned l,unsigned h)
{
	__int64 t = TOINT64(*lo,*hi);
	t += TOINT64(l,h);
	*lo = (unsigned int)(t & 0xFFFFFFFFu);
	*hi = (unsigned int)(t / 0x100000000u);
}

char IniFile [2000];

/* ------------------------------------------------------ */

typedef struct  {
		unsigned count;
		int com;
		int mod;
		int line;
        }
                CallsData;

typedef struct	{
		char * name;
		HWND wnd;
		int begin;
		int end;
	union	{
	struct	{
		int snaps;
		int lines_ready;
		};
	struct	{
		unsigned pure_time_lo;
		unsigned pure_time_hi;
		unsigned dirty_time_lo;
		unsigned dirty_time_hi;
		unsigned total_calls;
		unsigned unrec_calls;
		int ncall;
		CallsData *calls;
		int * sort;
		};
	};
	}
		ProcData;

typedef struct	{
		char * name;
		char * filename;
		int nproc;
		char * text_status;
		int * sort;
		HWND wnd;
		HWND twnd;
		ProcData * procs;
		int nlines;
		char ** lines;
		int lines_ready;
		int nlinesnaps;
		int * linesnaps;
	union	{
	struct	{
		int snaps;
		int * procind;
		};
	struct	{
		unsigned pure_time_lo;
		unsigned pure_time_hi;
		int ncall;
		int * all_calls_sort;
		HWND ac_wnd;
		};
	};
	}
		ModData;

typedef struct	{
		char * name;
		unsigned addr;
		unsigned len;
		int snaps;
	}
		PubData;

typedef struct	{
		char * name;
		char * shortname;
		BOOL has_modules;
		int nmodules;
		ModData * modules;
		int * sort;
		HWND wnd;
	union	{
	struct	{
		int snaps;
//		int user_snaps;
		int npublics;
		PubData * publics;
		};
	struct	{
		unsigned pure_time_lo;
		unsigned pure_time_hi;
		int ncall;
		int * all_calls_sort;
		HWND ac_wnd;
		};
	};
	}
		ComData;

int xMouseCoord;
int SelectedRow;

unsigned util;
int	ncomponents;
ComData * components;
int *	com_sort;

int prg_ncall;
int *	all_calls_prg_sort;

int	total_snaps;
unsigned total_time_lo;
unsigned total_time_hi;
unsigned exec_time_lo;
unsigned exec_time_hi;

char * name_part (char * name)
{
	char * p;
	for (p = name; *p; p++)	{
		if (*p==':' || *p == '\\') name = p+1;
	}
	return name;
}

void	add_ext (char * s, char * d, char * ext)
{
	char * n;
	if (d != s)
		strcpy (d, s);
	for (n = name_part (d); *n; n++)
		if (*n == '.') return;
	strcat (d, ".");
	strcat (d, ext);
}

void	read_module_text (ModData * mod);

BOOL	read_module_info (ModData * m, int com, int mod)
{
	char * s, * d;
	int j, k;
        PROFDATA * p;

	s = ModuleName (com, mod);
	d = malloc (strlen (s)+1);
	if (!d) return FALSE;
	strcpy (d, s);
	m->name = d;

	s = SourceName (com, mod);
	d = malloc (strlen (s)+1);
	if (!d) return FALSE;
	strcpy (d, s);
	m->filename = d;

	m->nproc = N_Proc (com, mod);

	if(util==UT_TRACE_CALLS_PROFILE){
	        m->pure_time_lo = 0;
	        m->pure_time_hi = 0;
		m->ncall = 0;
		p = malloc (sizeof(PROFDATA));
                if(!p) return FALSE;
		m->nproc++;
	}

	m->procs = malloc (sizeof (ProcData) * m->nproc);
	if (!m->procs) return FALSE;
	memset (m->procs, 0, sizeof (ProcData) * m->nproc);

	m->sort = malloc (sizeof (int) * m->nproc);
	if (!m->sort) return FALSE;

	m->nlinesnaps = 0;

	for (j = 0; j < m->nproc; j++)	{
		if((util==UT_TRACE_CALLS_PROFILE) && (j==m->nproc-1))
			break;
		if(util==UT_TRACE_CALLS_PROFILE)
			m->sort [j] = j-1;
		else
			m->sort [j] = j;

		s = ProcName (com, mod, j);
		d = malloc (strlen (s)+1);
		if (!d) return FALSE;
		strcpy (d, s);
		m->procs [j].name = d;
		if(!ProcBounds (com, mod, j, &m->procs [j].begin, &m->procs [j].end)) {
			if(!m->lines_ready)
				read_module_text(m);
			if (m->nlines >= m->nlinesnaps)
				m->nlinesnaps = m->nlines;
		} else {
			if (m->procs [j].end >= m->nlinesnaps)
				m->nlinesnaps = m->procs [j].end + 1;
		}
		switch	(util)	{
			case UT_TRACE_EXECUTION:
				m->procs [j].snaps = ProcSnapshots (com, mod, j);
				break;
			case UT_TRACE_CALLS_PROFILE:
				if( ProcInfo(com,mod,j,p) )
				{	m->procs [j].pure_time_lo = p->pure_dur_lo;
					m->procs [j].pure_time_hi = p->pure_dur_hi;
					m->procs [j].dirty_time_lo = p->dirty_dur_lo;
					m->procs [j].dirty_time_hi = p->dirty_dur_hi;
					m->procs [j].total_calls   = p->total_entry_count;
					m->procs [j].unrec_calls   = p->norec_entry_count;
					m->procs[m->nproc-1].total_calls += m->procs[j].total_calls;
					m->procs[m->nproc-1].unrec_calls += m->procs[j].unrec_calls;
		                } else
					return FALSE;
				m->procs [j].ncall = (int)N_Call(com,mod,j);
				m->ncall += m->procs[j].ncall;
				m->procs [j].calls  = malloc (sizeof (CallsData) * m->procs[j].ncall);
				if (!m->procs[j].calls) return FALSE;
				memset(m->procs[j].calls, 0, sizeof(CallsData) * m->procs[j].ncall);
				for(k=0; k < m->procs[j].ncall;k++) {
					if(!CallPlace(com,mod,j,k,
							&m->procs[j].calls[k].com,&m->procs[j].calls[k].mod,&m->procs[j].calls[k].line))
						m->procs[j].calls[k].com = -1;
					m->procs[j].calls[k].count = CallCount(com,mod,j,k);
				}

				m->procs [j].sort = malloc (sizeof(int)*m->procs[j].ncall);
				if(!m->procs [j].sort) return FALSE;
				for(k=0; k < m->procs[j].ncall; k++)
					m->procs[j].sort[k]=k;
				ADD64(&m->pure_time_lo,&m->pure_time_hi,m->procs [j].pure_time_lo,m->procs [j].pure_time_hi);
				break;
			}
	}

	m->linesnaps = malloc (m->nlinesnaps * sizeof (int));
	if (!m->linesnaps) return FALSE;
	switch	(util)	{
		case UT_TRACE_EXECUTION:
			m->snaps = ModuleSnapshots (com, mod);
			m->procind = malloc (m->nlinesnaps * sizeof (int));
			if (!m->procind) return FALSE;
			for (k = 0; k < m->nlinesnaps; k++)
				m->linesnaps [k] = m->procind [k] = -1;
			for (j = 0; j < m->nproc; j++)
				for (k = m->procs [j].begin; k <= m->procs [j].end; k++)
					m->procind [k] = j;
			for (j = 0; j < m->nlinesnaps; j++)
				m->linesnaps [j] = LineSnapshots (com, mod, j);
			break;
		case UT_TRACE_CALLS_PROFILE:
			memset(m->linesnaps, 0, sizeof(int) * (m->nlinesnaps+1));
			m->sort[0] = m->nproc-1;
			m->all_calls_sort = malloc (m->ncall * sizeof(int));
			if(!m->all_calls_sort) return FALSE;
			for (j=0;j < m->ncall;j++)
				m->all_calls_sort[j] = j;
                        m->procs[m->nproc-1].name = "All calls";
			m->procs[m->nproc-1].pure_time_lo = 0;
			m->procs[m->nproc-1].pure_time_hi = 0;
			m->procs[m->nproc-1].dirty_time_lo = 0;
			m->procs[m->nproc-1].dirty_time_hi = 0;
			m->procs[m->nproc-1].ncall = 0;
			m->procs[m->nproc-1].calls = 0;
			break;
		}
	return TRUE;
}

BOOL	make_module_summary (int ncom, ModData * m)
{
	int i, j, k;

	m->nproc = 0;
	switch	(util)	{
		case UT_TRACE_EXECUTION:
			m->snaps = 0;
			for (i = 0; i < components [ncom].nmodules-2; i++)	{
				m->nproc += components [ncom].modules [i].nproc;
				m->snaps += components [ncom].modules [i].snaps;
			}
			break;
		case UT_TRACE_CALLS_PROFILE:
			m->pure_time_lo = 0;
			m->pure_time_hi = 0;
			for (i = 0; i < components [ncom].nmodules-2; i++)	{
				m->nproc += components [ncom].modules [i].nproc-1;
				ADD64(&m->pure_time_lo,&m->pure_time_hi,components [ncom].modules [i].pure_time_lo,components [ncom].modules [i].pure_time_hi);
			}
			break;
		}

	m->procs = malloc (m->nproc * sizeof (ProcData));
	if (!m->procs) return FALSE;
	m->sort = malloc (m->nproc * sizeof (int));
	if (!m->sort) return FALSE;
	k = 0;
	switch	(util)	{
		case UT_TRACE_EXECUTION:
			for (i = 0; i < components [ncom].nmodules-2; i++)
				for (j = 0; j < components [ncom].modules [i].nproc; j++)	{
					m->procs [k] = components [ncom].modules [i].procs [j];
					m->sort [k] = k;
					++k;
				}
			break;
		case UT_TRACE_CALLS_PROFILE:
			for (i = 0; i < components [ncom].nmodules-2; i++)
				for (j = 0; j < components [ncom].modules [i].nproc-1; j++)	{
					m->procs [k] = components [ncom].modules [i].procs [j];
					m->sort [k] = k;
					++k;
				}
			break;
		}
	m->name = "All user modules";
	m->filename = "";
	return TRUE;
}

void	make_unknown (int ncom, ModData * m)
{
	m->nproc = 0;
	m->snaps = GetUnknownParts (ncom);
	m->name = "Unknown modules";
	m->filename = "";
}

BOOL	read_public_info (PubData * p, int com, int pub)
{
	p->name = PublicName (com, pub);
	p->snaps = PublicSnapshots (com, pub);
	PublicAttr (com, pub, &p->addr, &p->len);
	return TRUE;
}

void	make_publics_summary (int ncom, PubData * m)
{
	int i;

	m->name = "All known publics";
	m->snaps = 0;
	for (i = 0; i < components [ncom].npublics-2; i++)
		m->snaps += components [ncom].publics [i].snaps;
}

void	make_unknown_publics (int ncom, PubData * m)
{
	m->snaps = GetUnknownParts (ncom);
	m->name = "Unknown";
}

BOOL	read_component (ComData * c, int ncom)
{
	int i;
	char * s;

	s = ComponentName (ncom);
	c->name = malloc (strlen (s)+1);
	if (!c->name) return FALSE;
	strcpy (c->name, s);

	s = name_part (s);
	c->shortname = malloc (strlen (s)+1);
	if (!c->shortname) return FALSE;
	strcpy (c->shortname, s);
	
	if (util==UT_TRACE_EXECUTION)
		c->snaps = ComponentSnapshots (ncom);
	c->nmodules = N_Parts (ncom);
	if (c->nmodules >= 0)	{
		c->has_modules = TRUE;
		c->nmodules+=2;
		c->modules = malloc (sizeof (ModData) * c->nmodules);
		if (! c->modules) return FALSE;
		c->sort = malloc (sizeof (int) * c->nmodules);
		if (! c->sort) return FALSE;
		memset (c->modules, 0, sizeof (ModData) * c->nmodules);

		switch (util)	{
			case UT_TRACE_EXECUTION:
				for (i = 0; i < c->nmodules-2; i++)
					if (! read_module_info (&c->modules [i], ncom, i)) return FALSE;
				if (! make_module_summary (ncom, &c->modules [c->nmodules-2])) return FALSE;
				make_unknown;
		//		c->user_snaps = c->modules [c->nmodules-2].snaps;
				break;
			case UT_TRACE_CALLS_PROFILE:
				c->pure_time_lo = 0;
				c->pure_time_hi = 0;
				c->ncall = 0;
				for (i = 0; i < c->nmodules-2; i++)	{
					if (! read_module_info (&c->modules [i], ncom, i)) return FALSE;
					ADD64(&c->pure_time_lo,&c->pure_time_hi,c->modules [i].pure_time_lo,c->modules [i].pure_time_hi);
					c->ncall += c->modules[i].ncall;
				}
				c->all_calls_sort = malloc( c->ncall * sizeof(int));
				if(!(c->all_calls_sort)) return FALSE;
				for(i=0;i< c->ncall; i++)
					c->all_calls_sort[i]=i;
				c->modules[c->nmodules-1].name = "All calls";
				if (! make_module_summary (ncom, &c->modules [c->nmodules-2])) return FALSE;
				break;
			}
			c->sort [0] = c->nmodules - 2;
			c->sort [1] = c->nmodules - 1;
			for (i = 0; i < c->nmodules-2; i++)
				c->sort [i+2] = i;
	} else	{
		c->has_modules = 0;
		c->nmodules = 0;
		if(util==UT_TRACE_CALLS_PROFILE) 
			return TRUE;
		c->npublics = -c-> nmodules;
		c->npublics += 2;
		c->publics = malloc (sizeof (PubData) * c->npublics);
		if (! c->publics) return FALSE;
		c->sort = malloc (sizeof (int) * c->npublics);
		if (! c->sort) return FALSE;
		memset (c->publics, 0, sizeof (PubData) * c->npublics);

		for (i = 0; i < c->npublics-2; i++)
			if (! read_public_info (&c->publics [i], ncom, i)) return FALSE;
		make_publics_summary (ncom, &c->publics [c->npublics-2]);
		make_unknown_publics (ncom, &c->publics [c->npublics-1]);
//		c->user_snaps = c->publics [c->npublics-2].snaps;
		c->sort [0] = c->npublics - 2;
		c->sort [1] = c->npublics - 1;
		for (i = 0; i < c->npublics-2; i++)
			c->sort [i+2] = i;
	}
	return TRUE;
}

int	read_info (void)
{
	int i,j,k,l;
	__int64 t=0;
	CallsData *call;

        util = Utility();
	switch	(util)	{
		case UT_TRACE_MEMORY:	util = UT_TRACE_EXECUTION;
		case UT_TRACE_EXECUTION:	break;
		case UT_TRACE_CALLS_PROFILE:	total_time_lo = 0; total_time_hi = 0; ncomponents++; break;
		default:	return 2;
	}
	components = malloc (sizeof (ComData) * ncomponents);
	if (! components) return 1;
	com_sort = malloc (sizeof (int) * ncomponents);
	if (!com_sort) return 1;

	memset (components, 0, sizeof (ComData) * ncomponents);
	com_wnd = 0;
	switch	(util)	{
		case UT_TRACE_EXECUTION:	
			for (i = 0; i < ncomponents; i++)
				if (! read_component (&components [i], i))
					return 1;
			for (i = 0; i < ncomponents; i++)
				com_sort [i] = i;
			total_snaps = GetSnapshots ();
			break;
		case UT_TRACE_CALLS_PROFILE:
			for (i = 0; i < ncomponents-1; i++)
				if (! read_component (&components [i], i))
					return 1;
			prg_ncall = 0;
			for (i = 0; i < ncomponents-1; i++)
			{
				ADD64(&total_time_lo,&total_time_hi,components[i].pure_time_lo,components[i].pure_time_hi);
				prg_ncall += components[i].ncall;
			}
			com_sort [0] = ncomponents-1;
			for (i = 0; i < ncomponents-1; i++)
				com_sort [i+1] = i;
			all_calls_prg_sort = malloc( prg_ncall * sizeof(int));
			if(!all_calls_prg_sort)	return 1;
			for (i = 0; i < prg_ncall; i++)
				all_calls_prg_sort[i]=i;
			components[ncomponents-1].shortname = "All calls";
			for (i = 0; i < ncomponents-1; i++)
			 for (j = 0; j < components[i].nmodules-2; j++)
			  for (k = 0; k < components[i].modules[j].nproc-1; k++)
			   for (l = 0; l < components[i].modules[j].procs[k].ncall; l++)
			   {
				call = &(components[i].modules[j].procs[k].calls[l]);
				if((call->com != -1) && (components[call->com].modules[call->mod].nlinesnaps > call->line))
					components[call->com].modules[call->mod].linesnaps[call->line] += call->count;
			   }
			break;
		}
	if(!GetExecutionTime(&t)) t = 0;
	exec_time_lo = (unsigned int)(t & 0xFFFFFFFFu);
	exec_time_hi = (unsigned int)(t / 0x100000000u);
	return 0;
}

void    StripFullPath (char *filename) {
        char * last_slash = strrchr(filename, '\\');
        if (last_slash)
           strcpy(filename, last_slash + 1);
}

void	read_module_text (ModData * mod)
{
	FILE * f;
	char ** lines, **q, *s;
	int i, cnt, len;
	char buf [1024];
	int c;

	if (mod->lines_ready) return;
	f = fopen (mod->filename, "r");
	if (!f)	{
                StripFullPath(mod->filename);
                f = fopen (mod->filename, "r");
                if (!f) {
 			mod->text_status = "Module text not found: %s";
			return;
		}
	}
	mod -> lines_ready = 1;
	lines = 0;
	cnt = 0;
	while (fgets (buf, sizeof (buf), f))	{
		len = strlen (buf);
		if (len && buf [len-1] == '\n')	{
			--len;
			buf [len] = 0;
		} else
			while ((c = fgetc (f)) != EOF && c != '\n');
		s = malloc (len+1);
		if (!s)	{
			for (i = 0; i < cnt; i++)
				free (lines [i]);
			fclose (f);
			mod->text_status = "No memory for reading module text";
			return;
		}
		memmove (s, buf, len+1);

		if (cnt % 100 == 0)	{
			q = realloc (lines, (cnt+100) * sizeof (char*));
			if (! q)	{
				free (s);
				for (i = 0; i < cnt; i++)
					free (lines [i]);
				fclose (f);
				mod->text_status = "No memory for reading module text";
				return;
			}
			memset (q+cnt, 0, 100 * sizeof (char*));
			lines = q;
		}
		lines [cnt++] = s;
	}
	fclose (f);
	mod->nlines = cnt;
	mod->lines  = lines;
	mod->text_status = 0;
}

void	delete_modules (ComData * c)
{
	int i, j;

	if (!c->nmodules) return;
	switch (util)	{
	case UT_TRACE_EXECUTION:
		for (i = 0; i < c->nmodules-2; i++)	{
			for (j = 0; j < c->modules [i].nproc; j++)
				free (c->modules [i].procs [j].name);
			free (c->modules [i].linesnaps);
			free (c->modules [i].procind);
			free (c->modules [i].name);
			free (c->modules [i].filename);
			free (c->modules [i].procs);
			free (c->modules [i].lines);
			free (c->modules [i].sort);
		}
		free (c->modules [c->nmodules-1].procs);
		free (c->modules [c->nmodules-1].sort);
		return;
	case UT_TRACE_CALLS_PROFILE:
		for (i = 0; i < c->nmodules-2; i++)	{
			for (j = 0; j < c->modules [i].nproc-1; j++) {
				free (c->modules [i].procs [j].name);
				free(c->modules[i].procs[j].calls);
				free(c->modules[i].procs[j].sort);
			}
			free (c->modules [i].all_calls_sort);
			free (c->modules [i].name);
			free (c->modules [i].filename);
			free (c->modules [i].procs);
			free (c->modules [i].lines);
			free (c->modules [i].sort);
		}
		return;
	}
}

void	delete_components (void)
{
	int i;

	if (!ncomponents) return;

	switch(util)	{
	case UT_TRACE_EXECUTION:
		for (i = 0; i < ncomponents; i++)	{
			delete_modules (&components [i]);
			free (components [i].name);
			free (components [i].modules);
			free (components [i].sort);
		}
		break;
	case UT_TRACE_CALLS_PROFILE:
		for (i = 0; i < ncomponents-1; i++)	{
			delete_modules (&components [i]);
			free (components [i].name);
			free (components [i].modules);
			free (components [i].sort);
			free (components [i].all_calls_sort);
		}
		free(all_calls_prg_sort);
		break;
	}
	free (components);
	free (com_sort);
}

/* --------------------------------------------------------- */
/* Profiler MDI child */


typedef struct	{
			char * title;
			int init_size;
			BOOL graph;
			BOOL select;
			DWORD flags;
			int fontnum;
		}
			Column;

typedef struct	{
			DWORD data;
			int ncolumns;
			Column * cols;
			int col_size [10];
			HFONT col_font [10];
			HWND header;
			HWND list;
			HWND * pwnd;
			char * (* text_callback) (DWORD data, int row, int absrow, int col);
			void (* graph_callback)  (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnun);
			void (* select_callback) (DWORD data, int row, int absrow);
			void (* sort_callback)   (DWORD data, int col);
			int  nlines;
			int  startline;
			int  * sort;
			MDIPLACEMENT * save_placement;
			BOOL * placement_present;
		}
			WNDDATA;

WNDDATA * create_wnddata;

int	text_width (HFONT font, char * text)
{
	SIZE s;
	HDC hdc = GetDC (NULL);
	HFONT ofont;

	if (font) ofont = SelectObject (hdc, font);
	GetTextExtentPoint (hdc, text, strlen (text), &s);
	if (font) SelectObject (hdc, ofont);
	ReleaseDC (NULL, hdc);
	return s.cx;
}

void	draw_item (WNDDATA * d, DRAWITEMSTRUCT * is)
{
	int i;
	char * s;
	int x, w1, w2, w3;
	_int64 num, absnum, relnum;
	RECT rc, rcitem;
	DWORD opt;
	HBRUSH br, sbr, absbr, relbr, oldbr;
	HPEN relpen, oldpen;
	COLORREF color, scolor, tcolor, tscolor;
	HBITMAP bmp;
	HDC dc;

	if (is->itemAction == ODA_FOCUS)	{
		DrawFocusRect (is->hDC, &is->rcItem);
		return;
	}

	dc = CreateCompatibleDC (is->hDC);
	rcitem.left = rcitem.top = 0;
	rcitem.right  = is->rcItem.right  - is->rcItem.left;
	rcitem.bottom = is->rcItem.bottom - is->rcItem.top;
	bmp = CreateCompatibleBitmap (is->hDC, rcitem.right, rcitem.bottom);
	bmp = SelectObject (dc, bmp);

	color = scolor = GetSysColor (COLOR_WINDOW);
	tcolor = tscolor = GetSysColor (COLOR_WINDOWTEXT);

	if (is -> itemState & ODS_SELECTED)	{
		scolor = GetSysColor (COLOR_HIGHLIGHT);
		tscolor = GetSysColor (COLOR_HIGHLIGHTTEXT);
	}
	br = CreateSolidBrush (color);
	sbr = CreateSolidBrush (scolor);
	absbr = CreateSolidBrush (ABSCOLOR);
	relbr = CreateSolidBrush (RELCOLOR);
	relpen = CreatePen (PS_INSIDEFRAME, 1, RGB (0, 255, 0));
	SetBkMode (dc, OPAQUE);
	FillRect (dc, &rcitem, br);
	x = 0;
	for (i = 0; i < d->ncolumns && x < rcitem.right; x += d->col_size [i++])	{
		rc = rcitem;
		rc.left = x;
		rc.right = x + d->col_size [i];
		if (i == d->ncolumns-1 && rc.right > rcitem.right) rc.right = rcitem.right;
		rc.right --;
		rc.left ++;
		if (d->cols [i].graph)	{
			d->graph_callback (d->data, is->itemID,
					   d->sort ? d->sort [is->itemID] : is->itemID,
					   i, &num, &absnum, &relnum);
			if (num < 0)
				continue;
			rc.top += YDELTA;
			rc.bottom -= YDELTA;
			w1 = (int)(absnum == 0 ? 0 : num >= absnum ? d->col_size [i] : d->col_size [i] * num / absnum);
			w2 = (int)(relnum == 0 ? 0 : num >= relnum ? d->col_size [i] : d->col_size [i] * num / relnum);
			if (w1 > w2) w1 = w2;
			w3 = d->col_size [i] - w2;
			w2 -= w1;
			rc.right = rc.left + w1;
			FillRect (dc, &rc, absbr);
			rc.right += w2;
			rc.left += w1;
			FillRect (dc, &rc, relbr);
			rc.right += w3;
			rc.left += w2;
			oldbr = SelectObject (dc, br);
			oldpen = SelectObject (dc, relpen);
			FrameRect (dc, &rc, relbr);
			SelectObject (dc, oldbr);
			SelectObject (dc, oldpen);
		} else	{
			s = d-> text_callback (d->data, is->itemID,
						d->sort ? d->sort [is->itemID] : is->itemID,
						i);
			if (!s) s = "";
			opt = d->cols [i].flags;
			opt |= DT_SINGLELINE | DT_NOPREFIX | DT_VCENTER;
			if (d->cols [i].select)	{
				FillRect (dc, &rc, sbr);
				SetBkColor   (dc, scolor);
				SetTextColor (dc, tscolor);
			} else	{
				SetBkColor   (dc, color);
				SetTextColor (dc, tcolor);
			}
			SelectObject (dc, d->col_font [i]);
			if (NT35)
				DrawText (dc, s, -1, &rc, opt);
			else
				DrawTextEx (dc, s, -1, &rc, opt, 0);
		}
	}
	BitBlt	 (is->hDC, is->rcItem.left, is->rcItem.top,
		  rcitem.right, rcitem.bottom, dc, 0, 0, SRCCOPY);
	bmp = SelectObject (dc, bmp);
	DeleteObject (bmp);
	DeleteDC (dc);

	if (is -> itemState & ODS_FOCUS)
		DrawFocusRect (is->hDC, &is->rcItem);
	DeleteObject (relbr);
	DeleteObject (absbr);
	DeleteObject (br);
	DeleteObject (sbr);
	DeleteObject (relpen);
}

void	test_resize_last_col (WNDDATA * d)
{
	RECT rc;
	int s, w, i;
	HD_ITEM item;

	GetClientRect (d->header, &rc);
	s = 0;
	for (i = 0; i < d->ncolumns-1; i++)
		s += d->col_size [i];
	w = rc.right - s;
	if (w > 0 && w != d->col_size [d->ncolumns-1])	{
		item.mask = HDI_WIDTH;
		item.cxy = w;
		Header_SetItem (d->header, d->ncolumns-1, &item);
		d->col_size [d->ncolumns-1] = w;
		rc.left = s;
		InvalidateRect (d->header, &rc, 0);
		GetClientRect (d->list, &rc);
		rc.left = s;
		InvalidateRect (d->list, &rc, 0);
	}
}

LRESULT	header_tracking (WNDDATA * d, HD_NOTIFY * n)
{
	int i;
	RECT rc;
	HD_ITEM item;

	i = n->iItem;
	item.mask = HDI_WIDTH;
	item.cxy = n->pitem -> cxy;
	Header_SetItem (d->header, i, &item);

	d->col_size [i] = n->pitem -> cxy;
//	for (j = 0; j < i; j++)
//		x += d->col_size [j];
	GetClientRect (d->list, &rc);
//	rc.right = x;
	InvalidateRect (d->list, &rc, TRUE);
	GetClientRect (d->header, &rc);
//	rc.right = x;
	InvalidateRect (d->header, &rc, TRUE);
	test_resize_last_col (d);
	return 0;
}

LRESULT	header_begin_tracking (WNDDATA * d, HD_NOTIFY * n)
{
	return n->iItem == d-> ncolumns-1;
}

LRESULT	header_click (WNDDATA * d, HD_NOTIFY * n)
{
	if (d -> sort_callback)	{
		d->sort_callback (d->data, n->iItem);
		InvalidateRect (d->list, 0, 0);
	}
	return 0;
}

LRESULT CALLBACK ProfProc (HWND hwnd, UINT msg,
			    WPARAM wparam, LPARAM lparam)
{
	int i, j, x;
	HD_ITEM item;
	RECT rc;
	WINDOWPOS wp;
	HD_LAYOUT lo;
	NMHDR * nm;
	WNDDATA * d = (WNDDATA*) GetWindowLong (hwnd, GWL_USERDATA);

	switch (msg) {
	case WM_CREATE:
		d = create_wnddata;
		*d->pwnd = hwnd;
		SetWindowLong (hwnd, GWL_USERDATA, (LONG) d);
		d -> header = CreateWindow (
			WC_HEADER,
			"",
			WS_CHILD | WS_VISIBLE | (d->sort_callback ? HDS_BUTTONS : 0),
			0, 0, 0, 0,
			hwnd,
			NULL,                   /* menu handle */
			MyInstance,		/* program handle */
			NULL			/* create parms */
		);
		SendMessage (d->header, WM_SETFONT, (WPARAM) HeaderFont, 0);
		for (i = 0; i < d->ncolumns; i++)	{
			x = text_width (HeaderFont, d->cols [i].title) + 10;
			if (d->cols [i].init_size > x) x = d->cols [i].init_size;
			item.cxy = d->col_size [i] = x;
			item.mask = HDI_TEXT | HDI_FORMAT | HDI_WIDTH;
			item.pszText = d->cols [i].title;
			item.fmt =  HDF_STRING |
				((d->cols [i].flags & DT_RIGHT) ? HDF_RIGHT : HDF_LEFT);
			j = Header_InsertItem (d->header, i, &item);
			d->col_font [i] = d->cols [i].fontnum == FIXEDFONT ? FixedFont :
					  d->cols [i].fontnum == PLAINFONT ? PlainFont : SystemFont;
		}
		d -> list = CreateWindow (
			"LISTBOX",
			"",
			WS_CHILD | WS_VISIBLE |WS_HSCROLL | WS_VSCROLL | 
			LBS_OWNERDRAWFIXED | LBS_NOTIFY | LBS_NOINTEGRALHEIGHT,
			0, 0, 0, 0,
			hwnd,
			NULL,                   /* menu handle */
			MyInstance,		/* program handle */
			NULL			/* create parms */
		);
		for (i = 0; i < d->nlines; i++)
			SendMessage (d->list, LB_ADDSTRING, 0, 0);
		SendMessage (d->list, LB_SETCURSEL, d->startline, 0);
		SendMessage (d->list, LB_SETTOPINDEX, d->startline, 0);
		{
			TEXTMETRIC sys, fix, plain;
			int y;
			HDC dc;
			dc = GetDC (0);
			SelectObject (dc, SystemFont);
			GetTextMetrics (dc, &sys);
			SelectObject (dc, FixedFont);
			GetTextMetrics (dc, &fix);
			SelectObject (dc, PlainFont);
			GetTextMetrics (dc, &plain);
			ReleaseDC (0, dc);
			y = SendMessage (d->list, LB_GETITEMHEIGHT, 0, 0);
			++y;
		}
		break;
	case WM_SIZE:
		GetClientRect (hwnd, &rc);
		lo.prc = &rc;
		lo.pwpos = &wp;
		Header_Layout (d->header, &lo);
		MoveWindow (d->header, 0, 0, rc.right, wp.cy, 1);
		MoveWindow (d->list, 0, wp.cy, rc.right, rc.bottom-wp.cy, 1);
		test_resize_last_col (d);
		break;
	case WM_DRAWITEM:
		draw_item (d, (DRAWITEMSTRUCT*) lparam);
		return 1;
	case WM_NOTIFY:
		nm = (NMHDR*) lparam;
		if (nm->hwndFrom == d->header)	{
			switch (nm->code)	{
			case HDN_TRACK:
				return header_tracking (d, (HD_NOTIFY*) nm);
			case HDN_BEGINTRACK:
				return header_begin_tracking (d, (HD_NOTIFY*) nm);
			case HDN_ITEMCLICK:
				return header_click (d, (HD_NOTIFY*) nm);
			}
		}
		break;
	case WM_SETFOCUS:
		if (d) SetFocus (d->list);
		break;
	case WM_COMMAND:
		if (d && CONTROL_HWND == d->list && NOTIFY_CODE == LBN_DBLCLK)	{
			i = SendMessage (d->list, LB_GETCURSEL, 0, 0);
			if (d->select_callback)	{
				x = xMouseCoord;
				for(j=0; j< d->ncolumns ;j++)
					if(x < d->col_size[j])	{
						SelectedRow = j;
						break;
					}else
						x-=d->col_size[j];
				d->select_callback (d->data, i, d->sort ? d->sort[i] : i);
			}
			return 0;
		}
		break;
	case WM_CLOSE:
		if (d->save_placement)	{
			SendMessage (hwnd, WM_MYMDI_GETPLACEMENT, 0, (LPARAM) d->save_placement);
			*d->placement_present = TRUE;
		}
		break;
	case WM_DESTROY:
		*d->pwnd = 0;
		free (d);
		SetWindowLong (hwnd, GWL_USERDATA, 0);
		break;
	}
	return MyDefMDIChildProc (hwnd, msg, wparam, lparam);
}

void	create_mdi_wnd (char * class, char * title, int activate, MDIPLACEMENT * pl)
{
	MDICREATESTRUCT mdi;
	HWND wnd;

	mdi.szClass = class;
	mdi.szTitle = title;
	mdi.hOwner = MyInstance;
	mdi.x = mdi.y = mdi.cx = mdi.cy = CW_USEDEFAULT;
	mdi.style = WS_OVERLAPPEDWINDOW | WS_MDI_NOCLIENTADJUST;
	mdi.lParam = 0;
	wnd = (HWND) SendMessage (FrameWindow, WM_MDIGETACTIVE,	0, 0);
	SendMessage (FrameWindow, WM_MDICREATE,	(WPARAM) pl, (LPARAM) &mdi);
	if (!activate && wnd)
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) wnd, 0);
}

void	change_list (HWND list, int len, int start, int sel)
{
	int i;

	SendMessage (list, WM_SETREDRAW, 0, 0);
	SendMessage (list, LB_RESETCONTENT, 0, 0);
	for (i = 0; i < len; i++)
		SendMessage (list, LB_ADDSTRING, 0, 0);
	SendMessage (list, LB_SETTOPINDEX, start, 0);
	if (sel >= 0)
		SendMessage (list, LB_SETCURSEL, sel, 0);
	SendMessage (list, WM_SETREDRAW, 1, 0);
	InvalidateRect (list, 0, 0);
}

/* ----------------------------------------------------------------------------- */

Column component_columns [] ={
			{"Samples", 70, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Percentage", 90, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 200, 1, 0, 0, 0},
			{"No.", 35, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Name", 100, 0, 1, DT_LEFT | DT_PATH_ELLIPSIS, PLAINFONT}
		};

char *	 com_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];

	if (absrow >= ncomponents || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%d ", components [absrow].snaps);
		return s;
	case 1:	sprintf (s, "%.2f %% ", PERCENT(components [absrow].snaps, total_snaps));
		return s;
	case 3:	sprintf (s, "%d ", absrow+1); return s;
	case 4:	return components [absrow].shortname;
	}
	return "";
}

void	com_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	if (absrow >= ncomponents || absrow < 0)
		*absnum = *relnum = *num = -1;
	else	{
		*num = components [absrow].snaps;
		*absnum = *relnum = total_snaps;
	}
}

void	open_module_table (int com);
void	open_public_table (int com);

void	com_select_callback (DWORD data, int row, int absrow)
{
	if (components [absrow].has_modules)
		open_module_table (absrow);
	else
		open_public_table (absrow);
}

int	com_snaps_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return components [*i2].snaps != components [*i1].snaps ?
		components [*i2].snaps - components [*i1].snaps :
		*i2 - *i1;
}

int	com_name_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return stricmp (components [*i1].shortname, components [*i2].shortname);
}

void	com_sort_callback (DWORD data, int col)
{
	int i;

	for (i = 0; i < ncomponents; i++)
		com_sort [i] = i;
	switch (col)	{
	case 0:
	case 1:
	case 2:
		qsort (com_sort, ncomponents, sizeof (int), com_snaps_compare);
		break;
	case 4:
		qsort (com_sort, ncomponents, sizeof (int), com_name_compare);
		break;
	}
}

void	open_component_table (void)
{
	char s [200];
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->ncolumns = 5;
	create_wnddata->cols = component_columns;
	create_wnddata->text_callback   = com_text_callback;
	create_wnddata->graph_callback  = com_graph_callback;
	create_wnddata->select_callback = com_select_callback;
	create_wnddata->sort_callback   = com_sort_callback;
	create_wnddata->nlines = ncomponents;
	create_wnddata->sort = com_sort;
	create_wnddata->pwnd = &com_wnd;
	create_wnddata->save_placement = &component_placement;
	create_wnddata->placement_present = &component_placement_present;
	strcpy (s, progname);
	strcat (s, " components");
	com_sort_callback(0,0);
	create_mdi_wnd ("ProfClass", s, 1, component_placement_present ? &component_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column module_columns [] ={
			{"Samples", 65, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Total", 65, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"In Component", 65, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 200, 1, 0, 0, 0},
			{"No.", 35, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Module name", 100, 0, 1, DT_LEFT | DT_PATH_ELLIPSIS, PLAINFONT}
		};

char *	 module_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ComData * c = (ComData *) data;

	if (absrow >= c->nmodules || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%d ", c->modules [absrow].snaps);
		return s;
	case 1:	sprintf (s, "%.2f %% ", PERCENT(c->modules [absrow].snaps, total_snaps));
		return s;
	case 2:	sprintf (s, "%.2f %% ", PERCENT(c->modules [absrow].snaps, c->snaps));
		return s;
	case 4:	if (absrow >= c->nmodules-2) return "";
		sprintf (s, "%d ", absrow+1); return s;
	case 5:	return c->modules [absrow].name;
	}
	return "";
}

void	module_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	ComData * c = (ComData *) data;

	if (absrow >= c->nmodules || absrow < 0)
		*absnum = *relnum = *num = -1;
	else {
		*num = c->modules [absrow].snaps;
		*absnum = total_snaps;
		*relnum = c->snaps;
	} 
}

void	open_procedure_table (ComData * c, int mod);

void	module_select_callback (DWORD data, int row, int absrow)
{
	ComData * c = (ComData *) data;

	if (absrow != c->nmodules-1)
		open_procedure_table (c, absrow);
}

ComData * cur_sort_component;

int	mod_snaps_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return cur_sort_component->modules [*i2].snaps - cur_sort_component->modules [*i1].snaps;
}

int	mod_name_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return stricmp (cur_sort_component->modules [*i1].name,
			cur_sort_component->modules [*i2].name);
}

void	module_sort_callback (DWORD data, int col)
{
	int i;
	ComData * c = (ComData *) data;

	c->sort [0] = c->nmodules - 2;
	c->sort [1] = c->nmodules - 1;
	for (i = 0; i < c->nmodules-2; i++)
		c->sort [i+2] = i;
	cur_sort_component = c;
	switch (col)	{
	case 0:
	case 1:
	case 2:
	case 3:
		qsort (c->sort+2, c->nmodules-2, sizeof (int), mod_snaps_compare);
		break;
	case 5:
		qsort (c->sort+2, c->nmodules-2, sizeof (int), mod_name_compare);
		break;
	}
}

void	change_module_table (int com)
{
	char s [2000];
	WNDDATA * d = (WNDDATA*) GetWindowLong (mod_wnd, GWL_USERDATA);
	if (d -> data == (DWORD) &components [com])
		return;

	d->data = (DWORD) &components [com];
	d->sort = components [com].sort;
	d->nlines = components [com].nmodules;
	change_list (d->list, components [com].nmodules, 0, 0);
	strcpy (s, components [com].name);
	strcat (s, " modules");
	SetWindowText (mod_wnd, s);
}

void	open_module_table (int com)
{
	char s [200];

	if (same_window_mode && mod_wnd)	{
		change_module_table (com);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) mod_wnd, 0);
		return;
	}

	if (! same_window_mode && components [com].wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) components [com].wnd, 0);
		return;
	}

	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) &components [com];
	create_wnddata->ncolumns = 6;
	create_wnddata->cols = module_columns;
	create_wnddata->text_callback   = module_text_callback;
	create_wnddata->graph_callback  = module_graph_callback;
	create_wnddata->select_callback = module_select_callback;
	create_wnddata->sort_callback   = module_sort_callback;
	create_wnddata->nlines = components [com].nmodules;
	create_wnddata->sort = components [com].sort;
	create_wnddata->pwnd = same_window_mode ? &mod_wnd : &components [com].wnd;
	create_wnddata->save_placement = &module_placement;
	create_wnddata->placement_present = &mod_placement_present;
	strcpy (s, components [com].name);
	strcat (s, " modules");
	module_sort_callback((DWORD)&components[com],0);
	create_mdi_wnd ("ProfClass", s, 1, mod_placement_present ? &module_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column public_columns [] ={
			{"Samples", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Total", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"In Component", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 200, 1, 0, 0, 0},
			{"No.", 35, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Name", 100, 0, 1, DT_LEFT | DT_PATH_ELLIPSIS, PLAINFONT},
			{"Address.", 50, 0, 0, DT_RIGHT, FIXEDFONT},
			{"Length.", 50, 0, 0, DT_RIGHT, FIXEDFONT}
		};

char *	public_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ComData * c = (ComData *) data;

	if (absrow >= c->npublics || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%d ", c->publics [absrow].snaps);
		return s;
	case 1:	sprintf (s, "%.2f %% ", PERCENT(c->publics [absrow].snaps, total_snaps));
		return s;
	case 2:	sprintf (s, "%.2f %% ", PERCENT(c->publics [absrow].snaps, c->snaps));
		return s;
	case 4:	if (absrow >= c->npublics-2) return "";
		sprintf (s, "%d ", absrow+1); return s;
	case 5:	return c->publics [absrow].name;
	case 6: if (absrow >= c->npublics-2) return "";
		sprintf (s, "%08lX", c->publics [absrow].addr);
		return s;
	case 7: if (absrow >= c->npublics-2) return "";
		sprintf (s, "%08lX", c->publics [absrow].len);
		return s;
	}
	return "";
}

void	public_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	ComData * c = (ComData *) data;

	if (absrow >= c->npublics || absrow < 0)
		*absnum = *relnum = *num = -1;
	else {
		*num = c->publics [absrow].snaps;
		*absnum = total_snaps;
		*relnum = c->snaps;
	} 
}

void	public_select_callback (DWORD data, int row, int absrow)
{
}

int	pub_snaps_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return cur_sort_component->publics [*i2].snaps - cur_sort_component->publics [*i1].snaps;
}

int	pub_name_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return stricmp (cur_sort_component->publics [*i1].name,
			cur_sort_component->publics [*i2].name);
}

int	pub_addr_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return cur_sort_component->publics [*i2].addr - cur_sort_component->publics [*i1].addr;
}

void	public_sort_callback (DWORD data, int col)
{
	int i;
	ComData * c = (ComData *) data;

	c->sort [0] = c->npublics - 2;
	c->sort [1] = c->npublics - 1;
	for (i = 0; i < c->npublics-2; i++)
		c->sort [i+2] = i;
	cur_sort_component = c;
	switch (col)	{
	case 0:
	case 1:
	case 2:
	case 3:
		qsort (c->sort+2, c->npublics-2, sizeof (int), pub_snaps_compare);
		break;
	case 5:
		qsort (c->sort+2, c->npublics-2, sizeof (int), pub_name_compare);
		break;
	case 6:
	case 7:
		qsort (c->sort+2, c->npublics-2, sizeof (int), pub_addr_compare);
		break;
	}
}

void	change_public_table (int com)
{
	char s [2000];
	WNDDATA * d = (WNDDATA*) GetWindowLong (mod_wnd, GWL_USERDATA);
	if (d -> data == (DWORD) &components [com])
		return;

	d->data = (DWORD) &components [com];
	d->sort = components [com].sort;
	d->nlines = components [com].npublics;
	change_list (d->list, components [com].npublics, 0, 0);
	strcpy (s, components [com].name);
	strcat (s, " publics");
	SetWindowText (mod_wnd, s);
}

void	open_public_table (int com)
{
	char s [200];

	if (same_window_mode && mod_wnd)	{
		change_public_table (com);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) mod_wnd, 0);
		return;
	}

	if (! same_window_mode && components [com].wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) components [com].wnd, 0);
		return;
	}

	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) &components [com];
	create_wnddata->ncolumns = 8;
	create_wnddata->cols = public_columns;
	create_wnddata->text_callback   = public_text_callback;
	create_wnddata->graph_callback  = public_graph_callback;
	create_wnddata->select_callback = public_select_callback;
	create_wnddata->sort_callback   = public_sort_callback;
	create_wnddata->nlines = components [com].npublics;
	create_wnddata->sort = components [com].sort;
	create_wnddata->pwnd = same_window_mode ? &mod_wnd : &components [com].wnd;
	create_wnddata->save_placement = &module_placement;
	create_wnddata->placement_present = &mod_placement_present;
	strcpy (s, components [com].name);
	strcat (s, " publics");
	public_sort_callback((DWORD) &components [com],0);
	create_mdi_wnd ("ProfClass", s, 1, mod_placement_present ? &module_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column proc_columns [] ={
			{"Samples", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Total", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"In module", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 200, 1, 0, 0, 0},
			{"No.", 35, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Procedure name", 100, 0, 1, DT_LEFT | DT_END_ELLIPSIS, PLAINFONT}
		};

char * proc_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ModData * m = (ModData *) data;

	if (absrow >= m->nproc || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%d ", m->procs [absrow].snaps); return s;
	case 1:	sprintf (s, "%.2f %% ", PERCENT(m->procs [absrow].snaps, total_snaps)); return s;
	case 2:	sprintf (s, "%.2f %% ", PERCENT(m->procs [absrow].snaps, m->snaps)); return s;
	case 4:	sprintf (s, "%d ", absrow+1); return s;
	case 5:	return m->procs [absrow].name;
	}
	return "";
}

void	proc_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	ModData * m = (ModData *) data;
	if (absrow >= m->nproc || absrow < 0)
		*absnum = *relnum = *num = -1;
	else	{
		*num = m->procs [absrow].snaps;
		*absnum = total_snaps;
		*relnum = m->snaps;
	}
}

void	open_line_table (ModData * mod, int proc);

void	proc_select_callback (DWORD data, int row, int absrow)
{
	open_line_table ((ModData *) data, absrow);
}

ModData * cur_sort_module;

int	proc_snaps_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return	cur_sort_module->procs [*i2].snaps -
		cur_sort_module->procs [*i1].snaps;
}

int	proc_name_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return	stricmp (cur_sort_module->procs [*i1].name,
			 cur_sort_module->procs [*i2].name);
}

void	proc_sort_callback (DWORD data, int col)
{
	int i;

	cur_sort_module = (ModData *) data;
	for (i = 0; i < cur_sort_module->nproc; i++)
		cur_sort_module->sort [i] = i;

	switch (col)	{
	case 0:
	case 1:
	case 2:
	case 3:
		qsort (cur_sort_module->sort, cur_sort_module->nproc, sizeof (int), proc_snaps_compare);
		break;
	case 5:
		qsort (cur_sort_module->sort, cur_sort_module->nproc, sizeof (int), proc_name_compare);
		break;
	}
}

void	change_proc_table (ComData * c, int mod)
{
	char s [2000];
	WNDDATA * d = (WNDDATA*) GetWindowLong (proc_wnd, GWL_USERDATA);
	if (d -> data == (DWORD) &c->modules [mod])
		return;
	d->data = (DWORD) &c->modules [mod];
	d->sort = c->modules [mod].sort;
	d->nlines = c->modules [mod].nproc;
	change_list (d->list, c->modules [mod].nproc, 0, 0);
	strcpy (s, "Module ");
	strcat (s, c->modules [mod].name);
	SetWindowText (proc_wnd, s);
}

void	open_procedure_table (ComData * c, int mod)
{
	char s [2000];

	if (same_window_mode && proc_wnd)	{
		change_proc_table (c, mod);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) proc_wnd, 0);
		return;
	}

	if (! same_window_mode && c->modules [mod].wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) c->modules [mod].wnd, 0);
		return;
	}
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) &c->modules [mod];
	create_wnddata->ncolumns = 6;
	create_wnddata->cols = proc_columns;
	create_wnddata->text_callback = proc_text_callback;
	create_wnddata->graph_callback = proc_graph_callback;
	create_wnddata->select_callback = proc_select_callback;
	create_wnddata->sort_callback    = proc_sort_callback;
	create_wnddata->nlines = c->modules [mod].nproc;
	create_wnddata->sort = c->modules [mod].sort;
	create_wnddata->pwnd = same_window_mode ? &proc_wnd : &c->modules [mod].wnd;
	create_wnddata->save_placement = &proc_placement;
	create_wnddata->placement_present = &proc_placement_present;
	strcpy (s, "Module ");
	strcat (s, c->modules [mod].name);
	proc_sort_callback((DWORD) &c->modules [mod],0);
	create_mdi_wnd ("ProfModClass", s, TRUE,
			(same_window_mode && proc_placement_present) ? &proc_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column line_columns [] ={
			{"Samples", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Total", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"In procedure", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 60, 1, 0, 0, 0},
			{"Line", 40, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Program text", 100, 0, 1, DT_LEFT | DT_EXPANDTABS, FIXEDFONT}
		};

char *	line_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [2000];
	ModData * m = (ModData *) data;

	switch (col)	{
	case 0:	if (absrow >= m->nlinesnaps || m->procind [absrow] < 0 || m->linesnaps [absrow] < 0) return "";
		sprintf (s, "%d ", m->linesnaps [absrow]);
		return s;
	case 1:	if (absrow >= m->nlinesnaps || m->procind [absrow] < 0 || m->linesnaps [absrow] < 0) return "";
		sprintf (s, "%.2f %% ", PERCENT(m->linesnaps [absrow], total_snaps));
		return s;
	case 2:	if (absrow >= m->nlinesnaps || m->procind [absrow] < 0 || m->linesnaps [absrow] < 0) return "";
		sprintf (s, "%.2f %% ", PERCENT(m->linesnaps [absrow], m->procs [m->procind [absrow]].snaps ));
		return s;
	case 4:	sprintf (s, "%d ", absrow);
		return s;
	case 5:	if (row == 0 && m->text_status)	{
			sprintf (s, m->text_status, m->filename);
			return s;
		}
		if ((m->text_status || absrow >= m->nlines) && m->procind [absrow] >= 0)	{
			if (!absrow || m->procind [absrow] != m->procind [absrow-1])
			{
				sprintf (s, "<Procedure start: %s>", m->procs [m->procind [absrow]].name);
				return s;
			}
			if (absrow == m->nlinesnaps-1 || m->procind [absrow] != m->procind [absrow+1])
			{
				sprintf (s, "<Procedure end: %s>", m->procs [m->procind [absrow]].name);
				return s;
			}
		}

		if (absrow < m->nlines)
			return m->lines [absrow];
	}
	return "";
}

void	line_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	ModData * m = (ModData *) data;

	if (absrow >= m->nlinesnaps ||m->procind [absrow] < 0 ||
	    m->linesnaps [absrow] < 0)
		*absnum = *relnum = *num = -1;
	else	{
		*num = m->linesnaps [absrow];
		*absnum = total_snaps;
		*relnum = m->procs [m->procind [absrow]].snaps;
	}
}

void	change_line_table (HWND wnd, ModData * mod, ProcData * pr)
{
	WNDDATA * d = (WNDDATA*) GetWindowLong (wnd, GWL_USERDATA);

	if (d -> data == (DWORD) mod)	{
		SendMessage (d->list, LB_SETTOPINDEX, pr->begin, 0);
		SendMessage (d->list, LB_SETCURSEL, pr->begin, 0);
		return;
	}
	d->data = (DWORD) mod;
	d->sort = 0;
	SendMessage (d->list, LB_RESETCONTENT, 0, 0);
	read_module_text (mod);
	d->nlines = max (mod->nlinesnaps, mod->nlines);
	change_list (d->list, d->nlines, pr->begin, pr->begin);
	SetWindowText (line_wnd, mod->filename);
}

void	open_line_table (ModData * mod, int proc)
{
	HCURSOR cur;
	ProcData * pr;

	cur = SetCursor (LoadCursor (0, IDC_WAIT));
	pr  = &mod->procs [proc];

	if (same_window_mode && line_wnd)	{
		change_line_table (line_wnd, mod, pr);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) line_wnd, 0);
		SetCursor (cur);
		return;
	}

	if (! same_window_mode && mod->twnd)	{
		change_line_table (mod->twnd, mod, pr);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) mod->twnd, 0);
		SetCursor (cur);
		return;
	}
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata)	{
		SetCursor (cur);
		return;
	}
	read_module_text (mod);
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) mod;
	create_wnddata->ncolumns = 6;
	create_wnddata->cols = line_columns;
	create_wnddata->text_callback = line_text_callback;
	create_wnddata->graph_callback = line_graph_callback;
	create_wnddata->sort_callback = 0;
	create_wnddata->nlines = max (mod->nlinesnaps, mod->nlines);
	create_wnddata->startline = pr->begin;
	create_wnddata->sort   = 0;
	create_wnddata->pwnd = same_window_mode ? &line_wnd : &mod->twnd;
	create_wnddata->save_placement = &text_placement;
	create_wnddata->placement_present = &text_placement_present;
	create_mdi_wnd ("ProfTextClass", mod->filename, TRUE,
			(same_window_mode && text_placement_present) ? &text_placement : 0);
	SetCursor (cur);
}

/* ----------------------------------------------------------------------------- */

Column c_component_columns [] ={
			{"Percentage", 90, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 200, 1, 0, 0, 0},
			{"No.", 35, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Name", 100, 0, 1, DT_LEFT | DT_PATH_ELLIPSIS, PLAINFONT}
		};

char *	 c_com_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];

	if (absrow >= ncomponents || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%.2f %% ", PERCENT( TODBL(components [absrow].pure_time_lo, components [absrow].pure_time_hi), 
					TODBL(total_time_lo,total_time_hi) ));
		return s;
	case 2:	if(absrow >=ncomponents-1) return "";
		sprintf (s, "%d ", absrow+1); return s;
	case 3:	return components [absrow].shortname;
	}
	return "";
}

void	c_com_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	if (absrow >= ncomponents || absrow < 0)
		*absnum = *relnum = *num = -1;
	else	{
		*num = TOINT64(components [absrow].pure_time_lo,components [absrow].pure_time_hi);
		*absnum = *relnum = TOINT64(total_time_lo,total_time_hi);
	}
}

void	c_open_module_table (int com);
void	c_open_all_prg_calls_table (void);

void	c_com_select_callback (DWORD data, int row, int absrow)
{
	if (absrow == ncomponents-1)
		c_open_all_prg_calls_table();
	else
		if (components [absrow].has_modules)
			c_open_module_table (absrow);
}

int	c_com_time_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	__int64 t1, t2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	t1 =TOINT64(components[*i1].pure_time_lo,components[*i1].pure_time_hi);
	t2 =TOINT64(components[*i2].pure_time_lo,components[*i2].pure_time_hi);
	return t1==t2 ? (*i2 - *i1) : (t2>t1 ? 1 : -1);
}

void	c_com_sort_callback (DWORD data, int col)
{
	int i;

	com_sort [0] = ncomponents-1;
	for (i = 0; i < ncomponents-1; i++)
		com_sort [i+1] = i;
	switch (col)	{
	case 0:
	case 1:
		qsort (com_sort+1, ncomponents-1, sizeof (int), c_com_time_compare);
		break;
	case 3:
		qsort (com_sort+1, ncomponents-1, sizeof (int), com_name_compare);
		break;
	}
}

void	c_open_component_table (void)
{
	char s [200];
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->ncolumns = 4;
	create_wnddata->cols = c_component_columns;
	create_wnddata->text_callback   = c_com_text_callback;
	create_wnddata->graph_callback  = c_com_graph_callback;
	create_wnddata->select_callback = c_com_select_callback;
	create_wnddata->sort_callback   = c_com_sort_callback;
	create_wnddata->nlines = ncomponents;
	create_wnddata->sort = com_sort;
	create_wnddata->pwnd = &com_wnd;
	create_wnddata->save_placement = &component_placement;
	create_wnddata->placement_present = &component_placement_present;
	sprintf(s, "%s components",progname);
	c_com_sort_callback(0,0);
	create_mdi_wnd ("ProfClass", s, 1, component_placement_present ? &component_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column c_module_columns [] ={
			{"Total time", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"In Component", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 200, 1, 0, 0, 0},
			{"No.", 35, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Module name", 100, 0, 1, DT_LEFT | DT_PATH_ELLIPSIS, PLAINFONT}
		};

char *	 c_module_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ComData * c = (ComData *) data;

	if (absrow >= c->nmodules || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%.2f %% ", PERCENT(TODBL(c->modules [absrow].pure_time_lo,c->modules [absrow].pure_time_hi),TODBL(total_time_lo,total_time_hi)));
		return s;
	case 1:	sprintf (s, "%.2f %% ", PERCENT(TODBL(c->modules [absrow].pure_time_lo,c->modules [absrow].pure_time_hi),TODBL(c->pure_time_lo,c->pure_time_hi)));
		return s;
	case 3:	if (absrow >= c->nmodules-2) return "";
		sprintf (s, "%d ", absrow+1); return s;
	case 4:	return c->modules [absrow].name;
	}
	return "";
}

void	c_module_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	ComData * c = (ComData *) data;

	if (absrow >= c->nmodules || absrow < 0)
		*absnum = *relnum = *num = -1;
	else {
		*num = TOINT64(c->modules [absrow].pure_time_lo,c->modules [absrow].pure_time_hi);
		*absnum = TOINT64(total_time_lo,total_time_hi);
		*relnum = TOINT64(c->pure_time_lo,c->pure_time_hi);
	} 
}

void	c_open_procedure_table (ComData * c, int mod);
void	c_open_all_com_calls_table (ComData * c);

void	c_module_select_callback (DWORD data, int row, int absrow)
{
	ComData * c = (ComData *) data;
	if (absrow >= c->nmodules) return;
	if (absrow == c->nmodules-1)
		c_open_all_com_calls_table(c);
	else
		c_open_procedure_table (c, absrow);
}

ComData * cur_sort_component;

int	c_mod_time_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	__int64 t1, t2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	t1 =TOINT64(cur_sort_component->modules[*i1].pure_time_lo,cur_sort_component->modules[*i1].pure_time_hi);
	t2 =TOINT64(cur_sort_component->modules[*i2].pure_time_lo,cur_sort_component->modules[*i2].pure_time_hi);
	return t1==t2 ? (*i2 - *i1) : (t2>t1 ? 1 : -1);
}

void	c_module_sort_callback (DWORD data, int col)
{
	int i;
	ComData * c = (ComData *) data;

	c->sort [0] = c->nmodules - 2;
	c->sort [1] = c->nmodules - 1;
	for (i = 0; i < c->nmodules-2; i++)
		c->sort [i+2] = i;
	cur_sort_component = c;
	switch (col)	{
	case 0:
	case 1:
	case 2:
		qsort (c->sort+2, c->nmodules-2, sizeof (int), c_mod_time_compare);
		break;
	case 4:
		qsort (c->sort+2, c->nmodules-2, sizeof (int), mod_name_compare);
		break;
	}
}

void	c_open_module_table (int com)
{
	char s [200];

	if (same_window_mode && mod_wnd)	{
		change_module_table (com);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) mod_wnd, 0);
		return;
	}

	if (! same_window_mode && components [com].wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) components [com].wnd, 0);
		return;
	}

	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) &components [com];
	create_wnddata->ncolumns = 5;
	create_wnddata->cols = c_module_columns;
	create_wnddata->text_callback   = c_module_text_callback;
	create_wnddata->graph_callback  = c_module_graph_callback;
	create_wnddata->select_callback = c_module_select_callback;
	create_wnddata->sort_callback   = c_module_sort_callback;
	create_wnddata->nlines = components [com].nmodules;
	create_wnddata->sort = components [com].sort;
	create_wnddata->pwnd = same_window_mode ? &mod_wnd : &components [com].wnd;
	create_wnddata->save_placement = &module_placement;
	create_wnddata->placement_present = &mod_placement_present;
	strcpy (s, components [com].name);
	strcat (s, " modules");
	c_module_sort_callback((DWORD) &components [com], 0);
	create_mdi_wnd ("ProfClass", s, 1, mod_placement_present ? &module_placement : 0);
}


/* ----------------------------------------------------------------------------- */

Column c_proc_columns [] ={
			{"Pure time", 70, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Dirty time", 70, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"", 200, 1, 0, 0, 0},
			{"Total calls", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Non-rec. calls",85, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"No.", 35, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Procedure name", 120, 0, 1, DT_LEFT | DT_PATH_ELLIPSIS, PLAINFONT}
		};

char * c_proc_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ModData * m = (ModData *) data;

	if (absrow >= m->nproc || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%.2f %% ", PERCENT(TODBL(m->procs[absrow].pure_time_lo,m->procs[absrow].pure_time_hi),TODBL(total_time_lo,total_time_hi)));
		return s;
	case 1:	sprintf (s, "%.2f %% ", PERCENT(TODBL(m->procs[absrow].dirty_time_lo,m->procs[absrow].dirty_time_hi),TODBL(exec_time_lo,exec_time_hi)));
		return s;
	case 3:	sprintf (s, "%d ", m->procs[absrow].total_calls); return s;
	case 4:	sprintf (s, "%d ", m->procs[absrow].unrec_calls); return s;
	case 5: if(absrow==m->nproc-1) return "";
		sprintf (s, "%d ", absrow+1); return s;
	case 6:	return m->procs [absrow].name;
	}
	return "";
}

void	c_proc_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	ModData * m = (ModData *) data;
	if (absrow >= m->nproc || absrow < 0)
		*absnum = *relnum = *num = -1;
	else	{
		*num = TOINT64(m->procs[absrow].pure_time_lo,m->procs[absrow].pure_time_hi);
		*absnum = TOINT64(total_time_lo,total_time_hi);
		*relnum = TOINT64(m->pure_time_lo,m->pure_time_hi);
	}
}

void	c_open_calls_table (ModData * mod, int proc);
void	c_open_all_mod_calls_table (ModData * m);

void	c_open_line_table (ModData * mod, int line, char * proc_called);
int Calls_line;
unsigned Calls_count;

void	c_proc_select_callback (DWORD data, int row, int absrow)
{
	ModData * m = (ModData *)data;

	if(absrow == m->nproc-1)
		c_open_all_mod_calls_table(m);
	else {
		if(SelectedRow == 6)
			c_open_line_table(m, m->procs[absrow].begin, NULL);
		else
			c_open_calls_table (m, absrow);
	}
}

int	c_proc_pure_time_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	__int64 t1, t2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	t1 =TOINT64(cur_sort_module->procs[*i1].pure_time_lo,cur_sort_module->procs[*i1].pure_time_hi);
	t2 =TOINT64(cur_sort_module->procs[*i2].pure_time_lo,cur_sort_module->procs[*i2].pure_time_hi);
	return t1==t2 ? (*i2 - *i1) : (t2>t1 ? 1 : -1);
}

int	c_proc_dirty_time_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	__int64 t1, t2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	t1 =TOINT64(cur_sort_module->procs[*i1].dirty_time_lo,cur_sort_module->procs[*i1].dirty_time_hi);
	t2 =TOINT64(cur_sort_module->procs[*i2].dirty_time_lo,cur_sort_module->procs[*i2].dirty_time_hi);
	return t1==t2 ? (*i2 - *i1) : (t2>t1 ? 1 : -1);
}

int	c_proc_total_calls_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return cur_sort_module->procs[*i2].total_calls == cur_sort_module->procs[*i1].total_calls ?
		(*i2 - *i1)  : (cur_sort_module->procs[*i2].total_calls - cur_sort_module->procs[*i1].total_calls);
}

int	c_proc_unrec_calls_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return cur_sort_module->procs[*i2].unrec_calls == cur_sort_module->procs[*i1].unrec_calls ?
		(*i2 - *i1) : (cur_sort_module->procs[*i2].unrec_calls - cur_sort_module->procs[*i1].unrec_calls);
}

void	c_proc_sort_callback (DWORD data, int col)
{
	int i;

	cur_sort_module = (ModData *) data;
	cur_sort_module->sort[0]=cur_sort_module->nproc-1;
	for (i = 0; i < cur_sort_module->nproc-1; i++)
		cur_sort_module->sort [i+1] = i;

	switch (col)	{
	case 0:
	case 2:
		qsort (cur_sort_module->sort+1, cur_sort_module->nproc-1, sizeof (int), c_proc_pure_time_compare);
		break;
	case 1:
		qsort (cur_sort_module->sort+1, cur_sort_module->nproc-1, sizeof (int), c_proc_dirty_time_compare);
		break;
	case 3:
		qsort (cur_sort_module->sort+1, cur_sort_module->nproc-1, sizeof (int), c_proc_total_calls_compare);
		break;
	case 4:
		qsort (cur_sort_module->sort+1, cur_sort_module->nproc-1, sizeof (int), c_proc_unrec_calls_compare);
		break;
	case 6:
		qsort (cur_sort_module->sort+1, cur_sort_module->nproc-1, sizeof (int), proc_name_compare);
		break;
	}
}

void	c_open_procedure_table (ComData * c, int mod)
{
	char s [2000];

	if (same_window_mode && proc_wnd)	{
		change_proc_table (c, mod);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) proc_wnd, 0);
		return;
	}

	if (! same_window_mode && c->modules [mod].wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) c->modules [mod].wnd, 0);
		return;
	}
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) &c->modules [mod];
	create_wnddata->ncolumns = 7;
	create_wnddata->cols = c_proc_columns;
	create_wnddata->text_callback = c_proc_text_callback;
	create_wnddata->graph_callback = c_proc_graph_callback;
	create_wnddata->select_callback = c_proc_select_callback;
	create_wnddata->sort_callback    = c_proc_sort_callback;
	create_wnddata->nlines = c->modules [mod].nproc;
	create_wnddata->sort = c->modules [mod].sort;
	create_wnddata->pwnd = same_window_mode ? &proc_wnd : &c->modules [mod].wnd;
	create_wnddata->save_placement = &proc_placement;
	create_wnddata->placement_present = &proc_placement_present;
	strcpy (s, "Module ");
	strcat (s, c->modules [mod].name);
	c_proc_sort_callback((DWORD) &c->modules [mod],0);
	create_mdi_wnd ("ProfModClass", s, TRUE,
			(same_window_mode && proc_placement_present) ? &proc_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column c_calls_columns [] ={
			{"Calls", 70, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Place", 120, 0, 0, DT_LEFT | DT_END_ELLIPSIS, PLAINFONT}
		};

char * c_calls_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ProcData * p = (ProcData *) data;

	if (absrow >= p->ncall || absrow < 0) return "";
	switch (col)	{
	case 0:	sprintf (s, "%d ", p->calls[absrow].count); return s;
	case 1:	if(p->calls[absrow].com != -1)
			sprintf (s, "%s:%s line %d ",components[p->calls[absrow].com].shortname,
				components[p->calls[absrow].com].modules[p->calls[absrow].mod].name, p->calls[absrow].line);
		else
			sprintf (s, "unresolved call place");
		return s;
	}
	return "";
}

void	c_calls_graph_callback (DWORD data, int row, int absrow, int col, __int64 * num, __int64 * absnum, __int64 * relnum)
{
	*absnum = *relnum = *num = -1;
}

void	c_calls_select_callback (DWORD data, int row, int absrow)
{
	CallsData * c;
	c = &(((ProcData *)data) -> calls[absrow]);
	if(c->com==-1)
		return;
	Calls_count = c->count;
	c_open_line_table (&components[c->com].modules[c->mod],c->line, ((ProcData *)data)->name);
}

ProcData * cur_sort_proc;

int	c_calls_count_compare (const void * p1, const void * p2)
{
	int * i1, * i2;
	i1 = (int*) p1;
	i2 = (int*) p2;
	return cur_sort_proc->calls[*i2].count == cur_sort_proc->calls[*i1].count ?
		(*i2 - *i1) : 
			(cur_sort_proc->calls[*i2].count > cur_sort_proc->calls[*i1].count) ? 1 : -1;
}

void	c_calls_sort_callback (DWORD data, int col)
{
	int i;

	cur_sort_proc = (ProcData *) data;
	for (i = 0; i < cur_sort_proc->ncall; i++)
		cur_sort_proc->sort [i] = i;

	qsort (cur_sort_proc->sort, cur_sort_proc->ncall, sizeof (int), c_calls_count_compare);
}

void	c_change_calls_table (ModData * m, int proc)
{
	char s [2000];
	WNDDATA * d = (WNDDATA*) GetWindowLong (calls_wnd, GWL_USERDATA);
	if (d -> data == (DWORD) & m->procs [proc])
		return;
	d->data = (DWORD) &m->procs [proc];
	d->sort = m->procs [proc].sort;
	d->nlines = m->procs [proc].ncall;
	change_list (d->list, m->procs [proc].ncall, 0, 0);
	strcpy (s, "Procedure ");
	strcat (s, m->procs [proc].name);
	SetWindowText (calls_wnd, s);
}

void	c_open_calls_table (ModData * m, int proc)
{
	char s [2000];

	if (same_window_mode && calls_wnd)	{
		c_change_calls_table (m, proc);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) calls_wnd, 0);
		return;
	}

	if (! same_window_mode && m->procs [proc].wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) m->procs [proc].wnd, 0);
		return;
	}
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) &m->procs [proc];
	create_wnddata->ncolumns = 2;
	create_wnddata->cols = c_calls_columns;
	create_wnddata->text_callback = c_calls_text_callback;
	create_wnddata->graph_callback = 0; //c_calls_graph_callback;
	create_wnddata->select_callback = c_calls_select_callback;
	create_wnddata->sort_callback    = c_calls_sort_callback;
	create_wnddata->nlines = m->procs [proc].ncall;
	create_wnddata->sort = m->procs [proc].sort;
	create_wnddata->pwnd = same_window_mode ? &calls_wnd : &m->procs [proc].wnd;
	create_wnddata->save_placement = &calls_placement;
	create_wnddata->placement_present = &calls_placement_present;
	strcpy (s, "Procedure ");
	strcat (s, m->procs [proc].name);
	c_calls_sort_callback((DWORD) &m->procs [proc],0);
	create_mdi_wnd ("ProfModClass", s, TRUE,
			(same_window_mode && calls_placement_present) ? &calls_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column c_line_columns [] ={
			{"Calls", 60, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Calls/Line", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Line", 40, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Program text", 100, 0, 1, DT_LEFT | DT_EXPANDTABS, FIXEDFONT}
		};

char *	c_line_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [2000];
	ModData * m = (ModData *) data;

	switch (col)	{
	case 0:	if((Calls_line != -1) && (absrow==Calls_line)) {
			sprintf(s, "%d ", Calls_count);
			return s;
		} else return "";
	case 1: if((absrow < 0) || (absrow >= m->nlinesnaps))
			return "";
		else {
			sprintf(s, "%d ", m->linesnaps[absrow]);
			return s;
		}
	case 2:	if(row==0 && m->text_status)
			return "";
		sprintf (s, "%d ", absrow);
		return s;
	case 3:	if (row == 0 && m->text_status)	{
			sprintf (s, m->text_status, m->filename);
			return s;
		}
		if (absrow < m->nlines)
			return m->lines [absrow];
	}
	return "";
}

void	c_change_line_table (HWND wnd, ModData * mod, int line)
{
	WNDDATA * d = (WNDDATA*) GetWindowLong (wnd, GWL_USERDATA);

	if (d -> data == (DWORD) mod)	{
		SendMessage (d->list, LB_SETTOPINDEX, line, 0);
		SendMessage (d->list, LB_SETCURSEL, line, 0);
		return;
	}
	d->data = (DWORD) mod;
	d->sort = 0;
	SendMessage (d->list, LB_RESETCONTENT, 0, 0);
	read_module_text (mod);
	d->nlines = mod->nlines;
	change_list (d->list, d->nlines, line, line);
	SetWindowText (line_wnd, mod->filename);
}

void	c_open_line_table (ModData * mod, int line, char * proc_called)
{
	HCURSOR cur;
	char s[200];

	cur = SetCursor (LoadCursor (0, IDC_WAIT));
	if(proc_called)
		Calls_line = line;
	else
		Calls_line = -1;

	if (same_window_mode && line_wnd)	{
		c_change_line_table (line_wnd, mod, line);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) line_wnd, 0);
		SetCursor (cur);
		return;
	}

	if (! same_window_mode && mod->twnd)	{
		c_change_line_table (mod->twnd, mod, line);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) mod->twnd, 0);
		SetCursor (cur);
		return;
	}
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata)	{
		SetCursor (cur);
		return;
	}
	read_module_text (mod);
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) mod;
	create_wnddata->ncolumns = 4;
	create_wnddata->cols = c_line_columns;
	create_wnddata->text_callback = c_line_text_callback;
	create_wnddata->graph_callback = 0;
	create_wnddata->sort_callback = 0;
	create_wnddata->nlines = mod->nlines+1;
	create_wnddata->startline = line;
	create_wnddata->sort   = 0;
	create_wnddata->pwnd = same_window_mode ? &line_wnd : &mod->twnd;
	create_wnddata->save_placement = &text_placement;
	create_wnddata->placement_present = &text_placement_present;
	if(proc_called)
		sprintf(s,"%s - Call place of procedure %s",mod->filename, proc_called);
	else
		strcpy(s, mod->filename);
	create_mdi_wnd ("ProfTextClass", s, TRUE,
			(same_window_mode && text_placement_present) ? &text_placement : 0);
	SetCursor (cur);
}

/* ----------------------------------------------------------------------------- */

Column c_all_mod_calls_columns [] ={
			{"Procedure", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Calls", 70, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Place", 120, 0, 0, DT_LEFT | DT_END_ELLIPSIS, PLAINFONT}
		};

int procNum;

void	GetCallInModule(ModData * m, int call, ProcData ** p, int * c)
{
	int i;

	for (i=0; i < m->nproc-1; i++)
	{
		if(call+1 > m->procs[i].ncall)
			call -= m->procs[i].ncall;
		else
		{	*p = &(m->procs[i]);
			*c = call;
			procNum = i;
			return;
		}
	}
}

char * c_all_mod_calls_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ModData * m = (ModData *) data;
	ProcData * p;
	int i;

	if (absrow >= m->ncall || absrow < 0) return "";
	GetCallInModule(m, absrow, &p, &i);

	switch (col)	{
	case 0: return p->name;
	case 1:	sprintf (s, "%d ", p->calls[i].count); return s;
	case 2:	if(p->calls[i].com != -1)
			sprintf (s, "%s:%s line %d ",components[p->calls[i].com].shortname,
				components[p->calls[i].com].modules[p->calls[i].mod].name, p->calls[i].line);
		else
			sprintf (s, "unresolved call place");
		return s;
	}
	return "";
}

void	c_all_mod_calls_select_callback (DWORD data, int row, int absrow)
{
	ProcData * p;
	CallsData * c;
	int i;

	GetCallInModule((ModData *)data, absrow, &p, &i);
	if(SelectedRow==0)	{
		c_open_calls_table((ModData *)data, procNum);
		return;
	}
	c = &(p -> calls[i]);
	if(c->com == -1)
		return;
	Calls_count = c->count;
	c_open_line_table (&components[c->com].modules[c->mod],c->line, p->name);
}

ModData * cur_sort_mod;

int	c_all_mod_calls_count_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	int c1, c2;
	GetCallInModule(cur_sort_mod,*((int*)p1),&pr1,&c1);
	GetCallInModule(cur_sort_mod,*((int*)p2),&pr2,&c2);

	return pr1->calls[c1].count == pr2->calls[c2].count ?
	(*((int*)p1) - *((int*)p2)) : (pr2->calls[c2].count > pr1->calls[c1].count) ? 1 : -1;
}

int	c_all_mod_calls_pname_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	int c1, c2;
	GetCallInModule(cur_sort_mod,*((int*)p1),&pr1,&c1);
	GetCallInModule(cur_sort_mod,*((int*)p2),&pr2,&c2);

	return pr1 == pr2 ?
	(c1 - c2) : stricmp(pr1->name,pr2->name);
}

void	c_all_mod_calls_sort_callback (DWORD data, int col)
{
	int i;

	cur_sort_mod = (ModData *) data;
	for (i = 0; i < cur_sort_mod->ncall; i++)
		cur_sort_mod->all_calls_sort [i] = i;

	switch(col)	{
	case 0:
		qsort (cur_sort_mod->all_calls_sort, cur_sort_mod->ncall, sizeof (int), c_all_mod_calls_pname_compare);
		break;
	case 1:
		qsort (cur_sort_mod->all_calls_sort, cur_sort_mod->ncall, sizeof (int), c_all_mod_calls_count_compare);
	}
}

void	c_change_all_mod_calls_table (ModData * m)
{
	char s [2000];
	WNDDATA * d = (WNDDATA*) GetWindowLong (all_mod_calls_wnd, GWL_USERDATA);
	if (d -> data == (DWORD)m)
		return;
	d->data = (DWORD)m;
	d->sort = m->all_calls_sort;
	d->nlines = m->ncall;
	change_list (d->list, m->ncall, 0, 0);
	sprintf(s, "All calls in %s module",m->name);
	SetWindowText (all_mod_calls_wnd, s);
}

void	c_open_all_mod_calls_table (ModData * m)
{
	char s [2000];

	if (same_window_mode && all_mod_calls_wnd)	{
		c_change_all_mod_calls_table (m);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) all_mod_calls_wnd, 0);
		return;
	}

	if (! same_window_mode && m->ac_wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) m->ac_wnd, 0);
		return;
	}
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) m;
	create_wnddata->ncolumns = 3;
	create_wnddata->cols = c_all_mod_calls_columns;
	create_wnddata->text_callback = c_all_mod_calls_text_callback;
	create_wnddata->graph_callback = 0;
	create_wnddata->select_callback = c_all_mod_calls_select_callback;
	create_wnddata->sort_callback    = c_all_mod_calls_sort_callback;
	create_wnddata->nlines = m->ncall;
	create_wnddata->sort = m->all_calls_sort;
	create_wnddata->pwnd = same_window_mode ? &all_mod_calls_wnd : &m->ac_wnd;
	create_wnddata->save_placement = &calls_placement;
	create_wnddata->placement_present = &calls_placement_present;
	sprintf(s, "All calls in %s module",m->name);
	c_all_mod_calls_sort_callback((DWORD)m,1);
	create_mdi_wnd ("ProfModClass", s, TRUE,
			(same_window_mode && calls_placement_present) ? &calls_placement : 0);
}

/* ----------------------------------------------------------------------------- */

Column c_all_com_calls_columns [] ={
			{"Module", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Procedure", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Calls", 70, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Place", 120, 0, 0, DT_LEFT | DT_END_ELLIPSIS, PLAINFONT}
		};

int modNum;

void	GetCallInComponent(ComData * c, int call, ModData **m, ProcData ** p, int * cl)
{
	int i;

	for (i=0; i < c->nmodules-2; i++)
	{
		if(call+1 > c->modules[i].ncall)
			call -= c->modules[i].ncall;
		else
		{	GetCallInModule(&(c->modules[i]),call,p,cl);
			if ( m )	*m = &c->modules[i];
			modNum = i;
			return;
		}
	}
}

char * c_all_com_calls_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ComData * c = (ComData *) data;
	ModData * m;
	ProcData * p;
	int i;

	if (absrow >= c->ncall || absrow < 0) return "";
	GetCallInComponent(c, absrow, &m, &p, &i);

	switch (col)	{
	case 0: return m->name;
	case 1: return p->name;
	case 2:	sprintf (s, "%d ", p->calls[i].count); return s;
	case 3:	if(p->calls[i].com != -1)
			sprintf (s, "%s:%s line %d ",components[p->calls[i].com].shortname,
				components[p->calls[i].com].modules[p->calls[i].mod].name, p->calls[i].line);
		else
			sprintf (s, "unresolved call place");
		return s;
	}
	return "";
}

void	c_all_com_calls_select_callback (DWORD data, int row, int absrow)
{
	ModData * m;
	ProcData * p;
	CallsData * c;
	int i;

	GetCallInComponent((ComData *)data, absrow, &m, &p, &i);
	switch(SelectedRow)	{
	case 0:
		c_open_procedure_table((ComData *)data, modNum);
		return;
	case 1:
		c_open_calls_table(m, procNum);
		return;
	}

	c = &(p -> calls[i]);
	if(c->com == -1)
		return;
	Calls_count = c->count;
	c_open_line_table (&components[c->com].modules[c->mod],c->line, p->name);
}

ComData * cur_sort_com;

int	c_all_com_calls_count_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	int c1, c2;
	GetCallInComponent(cur_sort_com,*((int*)p1),0,&pr1,&c1);
	GetCallInComponent(cur_sort_com,*((int*)p2),0,&pr2,&c2);

	return pr1->calls[c1].count == pr2->calls[c2].count ?
	(*((int*)p1) - *((int*)p2)) : (pr2->calls[c2].count > pr1->calls[c1].count) ? 1 : -1 ;
}

int	c_all_com_calls_pname_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	int c1, c2;
	GetCallInComponent(cur_sort_com,*((int*)p1),0,&pr1,&c1);
	GetCallInComponent(cur_sort_com,*((int*)p2),0,&pr2,&c2);

	return pr1 == pr2 ?
	(c1 - c2) : stricmp(pr1->name,pr2->name);
}

int	c_all_com_calls_mname_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	ModData *m1, *m2;
	int c1, c2;
	GetCallInComponent(cur_sort_com,*((int*)p1),&m1,&pr1,&c1);
	GetCallInComponent(cur_sort_com,*((int*)p2),&m2,&pr2,&c2);

	return m1 == m2 ?
	(*((int*)p1) - *((int*)p2)) : stricmp(m1->name,m2->name);
}

void	c_all_com_calls_sort_callback (DWORD data, int col)
{
	int i;

	cur_sort_com = (ComData *) data;
	for (i = 0; i < cur_sort_com->ncall; i++)
		cur_sort_com->all_calls_sort [i] = i;

	switch(col)	{
	case 0:
		qsort (cur_sort_com->all_calls_sort, cur_sort_com->ncall, sizeof (int), c_all_com_calls_mname_compare);
		break;
	case 1:
		qsort (cur_sort_com->all_calls_sort, cur_sort_com->ncall, sizeof (int), c_all_com_calls_pname_compare);
		break;
	case 2:
		qsort (cur_sort_com->all_calls_sort, cur_sort_com->ncall, sizeof (int), c_all_com_calls_count_compare);
	}
}

void	c_change_all_com_calls_table (ComData * c)
{
	char s [2000];
	WNDDATA * d = (WNDDATA*) GetWindowLong (all_com_calls_wnd, GWL_USERDATA);
	if (d -> data == (DWORD) c)
		return;
	d->data = (DWORD) c;
	d->sort = c->all_calls_sort;
	d->nlines = c->ncall;
	change_list (d->list, c->ncall, 0, 0);
	sprintf(s, "All calls in %s component",c->shortname);
	SetWindowText (all_com_calls_wnd, s);
}

void	c_open_all_com_calls_table (ComData * c)
{
	char s [2000];

	if (same_window_mode && all_com_calls_wnd)	{
		c_change_all_com_calls_table (c);
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) all_com_calls_wnd, 0);
		return;
	}

	if (! same_window_mode && c->ac_wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) c->ac_wnd, 0);
		return;
	}
	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = (DWORD) c;
	create_wnddata->ncolumns = 4;
	create_wnddata->cols = c_all_com_calls_columns;
	create_wnddata->text_callback = c_all_com_calls_text_callback;
	create_wnddata->graph_callback = 0;
	create_wnddata->select_callback = c_all_com_calls_select_callback;
	create_wnddata->sort_callback    = c_all_com_calls_sort_callback;
	create_wnddata->nlines = c->ncall;
	create_wnddata->sort = c->all_calls_sort;
	create_wnddata->pwnd = same_window_mode ? &all_com_calls_wnd : &c->ac_wnd;
	create_wnddata->save_placement = &calls_placement;
	create_wnddata->placement_present = &calls_placement_present;
	sprintf(s, "All calls in %s component",c->shortname);
	c_all_com_calls_sort_callback((DWORD)c,2);
	create_mdi_wnd ("ProfModClass", s, TRUE,
			(same_window_mode && calls_placement_present) ? &calls_placement : 0);
}
/* ----------------------------------------------------------------------------- */


Column c_all_prg_calls_columns [] ={
			{"Component", 85, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Module", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Procedure", 80, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Calls", 70, 0, 0, DT_RIGHT, SYSTEMFONT},
			{"Place", 120, 0, 0, DT_LEFT | DT_END_ELLIPSIS, PLAINFONT}
		};

int comNum;

void	GetCallInPrg(int call,ComData **c, ModData **m, ProcData ** p, int * cl)
{
	int i;

	for (i=0; i < ncomponents-1; i++)
	{
		if(call+1 > components[i].ncall)
			call -= components[i].ncall;
		else
		{	GetCallInComponent(&(components[i]),call,m,p,cl);
			if ( c )	*c = &components[i];
			comNum = i;
			return;
		}
	}
}

char * c_all_prg_calls_text_callback (DWORD data, int row, int absrow, int col)
{
	static char s [100];
	ComData * c;
	ModData * m;
	ProcData * p;
	int i;

	if (absrow >= prg_ncall || absrow < 0) return "";
	GetCallInPrg(absrow, &c, &m, &p, &i);

	switch (col)	{
	case 0: return c->shortname;
	case 1: return m->name;
	case 2: return p->name;
	case 3:	sprintf (s, "%d ", p->calls[i].count); return s;
	case 4:	if(p->calls[i].com != -1)
			sprintf (s, "%s:%s line %d ",components[p->calls[i].com].shortname,
				components[p->calls[i].com].modules[p->calls[i].mod].name, p->calls[i].line);
		else
			sprintf (s, "unresolved call place");
		return s;
	}
	return "";
}

void	c_all_prg_calls_select_callback (DWORD data, int row, int absrow)
{
	ComData * com;
	ModData * m;
	ProcData * p;
	CallsData * c;
	int i;

	GetCallInPrg(absrow,&com,&m, &p, &i);
	switch(SelectedRow)	{
	case 0:
		c_open_module_table(comNum);
		return;
	case 1:
		c_open_procedure_table(com, modNum);
		return;
	case 2:
		c_open_calls_table(m, procNum);
		return;
	}
	c = &(p -> calls[i]);
	if(c->com == -1)
		return;
	Calls_count = c->count;
	c_open_line_table (&components[c->com].modules[c->mod],c->line, p->name);
}

int	c_all_prg_calls_count_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	int c1, c2;
	GetCallInPrg(*((int*)p1),0,0,&pr1,&c1);
	GetCallInPrg(*((int*)p2),0,0,&pr2,&c2);

	return pr1->calls[c1].count == pr2->calls[c2].count ?
	(*((int*)p1) - *((int*)p2)) : (pr2->calls[c2].count > pr1->calls[c1].count) ? 1 : -1;
}

int	c_all_prg_calls_pname_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	int c1, c2;
	GetCallInPrg(*((int*)p1),0,0,&pr1,&c1);
	GetCallInPrg(*((int*)p2),0,0,&pr2,&c2);

	return pr1 == pr2 ?
	(c1 - c2) : stricmp(pr1->name,pr2->name);
}

int	c_all_prg_calls_mname_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	ModData *m1, *m2;
	int c1, c2;
	GetCallInPrg(*((int*)p1),0,&m1,&pr1,&c1);
	GetCallInPrg(*((int*)p2),0,&m2,&pr2,&c2);

	return m1 == m2 ?
	(*((int*)p1) - *((int*)p2)) : stricmp(m1->name,m2->name);
}

int	c_all_prg_calls_cname_compare (const void * p1, const void * p2)
{
	ProcData *pr1, *pr2;
	ComData *c1, *c2;
	int cl1, cl2;
	GetCallInPrg(*((int*)p1),&c1,0,&pr1,&cl1);
	GetCallInPrg(*((int*)p2),&c2,0,&pr2,&cl2);

	return c1 == c2 ?
	(*((int*)p1) - *((int*)p2)) : stricmp(c1->name,c2->name);
}

void	c_all_prg_calls_sort_callback (DWORD data, int col)
{
	int i;

	for (i = 0; i < prg_ncall; i++)
		all_calls_prg_sort [i] = i;

	switch(col)	{
	case 0:
		qsort (all_calls_prg_sort, prg_ncall, sizeof (int), c_all_prg_calls_cname_compare);
		break;
	case 1:
		qsort (all_calls_prg_sort, prg_ncall, sizeof (int), c_all_prg_calls_mname_compare);
		break;
	case 2:
		qsort (all_calls_prg_sort, prg_ncall, sizeof (int), c_all_prg_calls_pname_compare);
		break;
	case 3:
		qsort (all_calls_prg_sort, prg_ncall, sizeof (int), c_all_prg_calls_count_compare);
	}
}

void	c_open_all_prg_calls_table (void)
{
	if (same_window_mode && all_prg_calls_wnd)	{
		SendMessage (FrameWindow, WM_MDIACTIVATE, (WPARAM) all_prg_calls_wnd, 0);
		return;
	}

	create_wnddata = malloc (sizeof (WNDDATA));
	if (!create_wnddata) return;
	memset (create_wnddata, 0, sizeof (WNDDATA));
	create_wnddata->data = 0;
	create_wnddata->ncolumns = 5;
	create_wnddata->cols = c_all_prg_calls_columns;
	create_wnddata->text_callback = c_all_prg_calls_text_callback;
	create_wnddata->graph_callback = 0;
	create_wnddata->select_callback = c_all_prg_calls_select_callback;
	create_wnddata->sort_callback    = c_all_prg_calls_sort_callback;
	create_wnddata->nlines = prg_ncall;
	create_wnddata->sort = all_calls_prg_sort;
	create_wnddata->pwnd = &all_prg_calls_wnd;
	create_wnddata->save_placement = &calls_placement;
	create_wnddata->placement_present = &calls_placement_present;
	c_all_prg_calls_sort_callback(0,3);
	create_mdi_wnd ("ProfModClass", "All calls in whole program", TRUE,
			(same_window_mode && calls_placement_present) ? &calls_placement : 0);
}
/* ----------------------------------------------------------------------------- */

BOOL	closeall_enum (int num, HWND hwnd, BOOL free, BOOL docked, BOOL active, BOOL mdiactive)
{
	SendMessage (hwnd, WM_CLOSE, 0, 0);
	return TRUE;
}

char * read_info_error (int code)
{
	switch (code)	{
	case LDI_OpenErrorProfilerData:   return "file not found";
	case LDI_ReadErrorProfilerData:   return "read error";
	case LDI_ReadDebugInfo:           return "Debugging information read error";
	case LDI_WrongFormatProfilerData: return "Wrong format of trace file";
        case LDI_IsNot_XDS_ProfilerTraceFile: return "Specified file is not XDS profiler trace file";
	default: return "Unknown error";
	}
}

void	try_open_file (char * name)
{
	int n;
	char s [2000];
	char *p, *q;
	HCURSOR cur;

	cur = SetCursor (LoadCursor (NULL, IDC_WAIT));
	ClearDebugInfo ();
	n = LoadDebugInfo (name);
	if (n <= 0)	{
		SetCursor (cur);
		sprintf (s, "Error reading trace file %s: %s", name, read_info_error (n));
		MessageBox (FrameWindow, s, "Error", MB_OK | MB_ICONSTOP);
		return;
	}
	SendMessage (FrameWindow, WM_MYMDI_ENUM, 0, (LPARAM) closeall_enum);
	delete_components ();
	ncomponents = n;
	switch (read_info ())	{
		case 0:	break;
		case 1:
			EnableMenuItem (GetMenu (FrameWindow), IDM_CLOSE, MF_GRAYED);
			SetWindowText (FrameWindow, ProgTitle);
			SetCursor (cur);
			MessageBox (FrameWindow, "Can't read debug info: no memory", "Error", MB_OK | MB_ICONSTOP);
			return;
		case 2:
			EnableMenuItem (GetMenu (FrameWindow), IDM_CLOSE, MF_GRAYED);
			SetWindowText (FrameWindow, ProgTitle);
			SetCursor (cur);
			MessageBox (FrameWindow, "Unknown type of profile utility", "Error", MB_OK | MB_ICONSTOP);
			return;
	}
	q = name;
	for (p = name; *p; p++) if (*p == '\\' || *p == ':') q = p+1;
	p = progname;
	while (*q && *q != '.') *p++ = *q++;
	*p = 0;
	switch (util)	{
		case UT_TRACE_MEMORY:
		case UT_TRACE_EXECUTION:
			open_component_table ();
			break;
		case UT_TRACE_CALLS_PROFILE:
			c_open_component_table ();
	}
	EnableMenuItem (GetSystemMenu (com_wnd, 0), SC_CLOSE, MF_GRAYED | MF_BYCOMMAND);
        sprintf(s,"%s - %s.  Total execution time: %15.0f tacts", ProgTitle,progname, TODBL(exec_time_lo, exec_time_hi));
	SetWindowText (FrameWindow, s);
	EnableMenuItem (GetMenu (FrameWindow), IDM_CLOSE, MF_ENABLED);
	SetCursor (cur);
}

void	close_file (void)
{
	SendMessage (FrameWindow, WM_MYMDI_ENUM, 0, (LPARAM) closeall_enum);
	SetWindowText (FrameWindow, ProgTitle);
	progname [0] = 0;
	delete_components ();
	ncomponents = 0;
	EnableMenuItem (GetMenu (FrameWindow), IDM_CLOSE, MF_GRAYED);
}

int filterpos = 0;

void	open_new_file (void)
{
	OPENFILENAME of;
	BOOL b;
	char file [1000];

	file [0] = 0;
	memset (&of, 0, sizeof (OPENFILENAME));
	of.lStructSize = sizeof (OPENFILENAME);
	of.hwndOwner = FrameWindow;
	of.lpstrFilter = open_filter;
	of.lpstrDefExt = DEFEXT;
	of.nFilterIndex = filterpos;
	of.lpstrFile = file;
	of.nMaxFile = sizeof (file);
	of.lpstrTitle = "Choose profiler trace file name";
	of.Flags = OFN_HIDEREADONLY;
	b = GetOpenFileName (&of);
	if (!b) return;
	filterpos = of.nFilterIndex;
	try_open_file (file);
}

/* --------------------------------------------------------- */

HWND	ActiveChild (void)
{
	return (HWND) SendMessage (FrameWindow, WM_MDIGETACTIVE, 0, 0);
}

BOOL	frame_command (WPARAM wparam, LPARAM lparam)
{
	switch (CONTROL_ID) {
	case IDM_OPEN:
		open_new_file ();
		break;
	case IDM_CLOSE:
		close_file ();
		break;
	case IDM_EXIT:
		DestroyWindow (FrameWindow);
		break;
	case IDM_TILEVERT:
		SendMessage (FrameWindow, WM_MDITILE, MDITILE_VERTICAL, 0);
		break;
	case IDM_TILEHORIZ:
		SendMessage (FrameWindow, WM_MDITILE, MDITILE_HORIZONTAL, 0);
		break;
	case IDM_CASCADE:
		SendMessage (FrameWindow, WM_MDICASCADE, 0, 0);
		break;
	case IDM_ARRANGE:
		SendMessage (FrameWindow, WM_MDIICONARRANGE, 0, 0);
		break;
	case IDM_DOCK:
	case IDM_UNDOCK:
		SendMessage (FrameWindow, WM_MYMDI_DOCK, (WPARAM) ActiveChild (), 0);
		break;
	case IDM_FREE:
		SendMessage (FrameWindow, WM_MYMDI_FREE, (WPARAM) ActiveChild (), 0);
		break;
	default:
		return FALSE;
	}
	return TRUE;
}

void	Init_frame (HWND hwnd)
{
	MDICURSORS	cur;
	HMENU main = GetMenu (hwnd);
	HMENU winmenu = GetSubMenu (main, WINDOW_MENU_POS);	

	SendMessage (hwnd, WM_MYMDI_MENU, (WPARAM) winmenu, IDM_FIRSTCHILD);
	memset (&cur, 0, sizeof (cur));
	cur.hsize_cursor = LoadCursor (MyInstance, MAKEINTRESOURCE (HSIZE_CURSOR));
	cur.vsize_cursor = LoadCursor (MyInstance, MAKEINTRESOURCE (VSIZE_CURSOR));
	cur.moving_cursor = LoadCursor (MyInstance, MAKEINTRESOURCE (HANDCLOSED_CURSOR));
	SendMessage (hwnd, WM_MYMDI_CURSOR, 0, (LPARAM) &cur);
	SendMessage (hwnd, WM_MYMDI_NUMBERING, MDINUMBER_NOFLIP, 0);
	SendMessage (hwnd, WM_MYMDI_TAB, 1, MDITAB_TEXT | MDITAB_NUMBER | MDITAB_ICON);
	SendMessage (hwnd, WM_MYMDI_DOCKSTYLE, 1, 0);
	EnableMenuItem (GetMenu (FrameWindow), IDM_CLOSE, MF_GRAYED);
}

LRESULT CALLBACK FrameProc (HWND hwnd, UINT msg,
			    WPARAM wparam, LPARAM lparam)
{
	switch (msg) {
	case WM_CREATE:
		Init_frame (hwnd);
		break;
	case WM_COMMAND:
		if (frame_command (wparam, lparam)) return 0;
		break;
	case WM_CLOSE:
		SendMessage (FrameWindow, WM_MYMDI_ENUM, 0, (LPARAM) closeall_enum);
		break;
	case WM_DESTROY:
		PostQuitMessage (0);
		break;
	}
	return MyDefMDIProc (hwnd, msg, wparam, lparam);
}

void	write_placement (char * name, MDIPLACEMENT * pl)
{
	char line [3000];

	strcpy (line, pl->type == DOCKED ? "docked" : pl->type == FREE ? "free" : "frame");

	if (pl->type == DOCKED)
		sprintf (line+strlen(line), ",%s,%d",
			pl->dir == DOCK_UP ? "up" :
			pl->dir == DOCK_DOWN ? "down" :
			pl->dir == DOCK_RIGHT ? "right" : "left",
			pl->docksize);
	else
		sprintf (line+strlen(line), ",%s,%d,%d,%d,%d,%d,%d",
			pl->p.showCmd == SW_SHOWMINIMIZED ? "min" :
			pl->p.showCmd == SW_SHOWMAXIMIZED ? "max" : "normal",
			pl->p.rcNormalPosition.left,  pl->p.rcNormalPosition.top,
			pl->p.rcNormalPosition.right, pl->p.rcNormalPosition.bottom,
			pl->p.ptMinPosition.x, pl->p.ptMinPosition.y);

	WritePrivateProfileString ("windows", name, line, IniFile);
}

char * get_word (char **s)
{
	char *p;
	while (**s && isspace (**s)) ++*s;
	p = *s;
	if (!*p) return p;
	while (**s && **s != ',') ++ *s;
	if (!**s) return p;
	**s = 0;
	++ *s;
	return p;
}

BOOL	read_placement (char * name, MDIPLACEMENT * pl)
{
	char line [2000];
	char *s, *type, *dir, *state;

	memset (pl, 0, sizeof (MDIPLACEMENT));
	GetPrivateProfileString ("windows", name, "", line, sizeof (line), IniFile);

	s = line;
	type  = get_word (&s);
	if (!stricmp (type, "docked"))
		pl->type = DOCKED;
	else if (!stricmp (type, "free"))
		pl->type = FREE;
	else if (!stricmp (type, "frame"))
		pl->type = FRAME;
	else return FALSE;
	if (pl->type == DOCKED)	{
		dir = get_word (&s);
		if (! stricmp (dir, "right"))     pl->dir = DOCK_RIGHT;
		else if (! stricmp (dir, "left")) pl->dir = DOCK_LEFT;
		else if (! stricmp (dir, "up"))   pl->dir = DOCK_UP;
		else pl->dir = DOCK_DOWN;
		pl->docksize = atoi (get_word (&s));
	} else	{
		state = get_word (&s);
		if (!stricmp (state, "min")) pl->p.showCmd = SW_SHOWMINIMIZED;
		else if (!stricmp (state, "max")) pl->p.showCmd = SW_SHOWMAXIMIZED;
		else pl->p.showCmd = SW_SHOWNORMAL;
		pl->p.rcNormalPosition.left  = atoi (get_word (&s));
		pl->p.rcNormalPosition.top   = atoi (get_word (&s));
		pl->p.rcNormalPosition.right = atoi (get_word (&s));
		pl->p.rcNormalPosition.bottom= atoi (get_word (&s));
		pl->p.ptMinPosition.x	    = atoi (get_word (&s));
		pl->p.ptMinPosition.y	    = atoi (get_word (&s));
	}
	return TRUE;
}

BOOL	InitApplication (void)
{
	WNDCLASSEX wc;
	int len;

	GetModuleFileName (MyInstance, IniFile, sizeof (IniFile));
	len = strlen (IniFile);
	if (len < 4 || IniFile [len-4] != '.') return FALSE;
	strcpy (IniFile+len-3, "ini");
	
	/* register the frame window class   */

	wc.cbSize = sizeof (WNDCLASSEX);
	wc.style = 0;
	wc.lpfnWndProc = (LPVOID) FrameProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = MyInstance;
	wc.hIcon = LoadIcon (MyInstance, MAKEINTRESOURCE (MODULES_ICON));
	wc.hCursor = LoadCursor (NULL, IDC_ARROW);
	wc.hbrBackground = 0;
	wc.lpszMenuName = "MainMenu";
	wc.lpszClassName = "MainProfClass";
	wc.hIconSm = 0;
	if (!RegisterClassEx (&wc)) return FALSE;

	wc.cbSize = sizeof (WNDCLASSEX);
	wc.style = 0;
	wc.lpfnWndProc = (LPVOID) ProfProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = MyInstance;
	wc.hIcon = LoadIcon (MyInstance, MAKEINTRESOURCE (MODULES_ICON));
	wc.hCursor = LoadCursor (NULL, IDC_ARROW);
	wc.hbrBackground = 0;
	wc.lpszMenuName = 0;
	wc.lpszClassName = "ProfClass";
	wc.hIconSm = 0;
	if (!RegisterClassEx (&wc)) return FALSE;

	wc.hIcon = LoadIcon (MyInstance, MAKEINTRESOURCE (MODULE_ICON));
	wc.lpszClassName = "ProfModClass";
	if (!RegisterClassEx (&wc)) return FALSE;

	wc.hIcon = LoadIcon (MyInstance, MAKEINTRESOURCE (MODULETEXT_ICON));
	wc.lpszClassName = "ProfTextClass";
	if (!RegisterClassEx (&wc)) return FALSE;

	SystemFont = GetStockObject (SYSTEM_FONT);
	FixedFont = GetStockObject (SYSTEM_FIXED_FONT);;
	PlainFont = CreateFont (0, 0, 0, 0, 0, 0, 0, 0,
			ANSI_CHARSET, 0, 0, 0, 0, "MS Sans Serif");
	HeaderFont = PlainFont;
	return TRUE;
}

BOOL	InitMainWindow (int cmdshow)
{
	int x, y, w, h;

	x = y = w = h = CW_USEDEFAULT;

	FrameWindow = CreateWindow (
			"MainProfClass",
			ProgTitle,
			WS_OVERLAPPEDWINDOW|WS_CLIPCHILDREN|WS_CLIPSIBLINGS,
			x, y, w, h,
			NULL,			/* parent window */
			NULL,                   /* menu handle */
			MyInstance,		/* program handle */
			NULL			/* create parms */
		);

	if (!FrameWindow) return FALSE;
	ShowWindow   (FrameWindow, cmdshow);
	UpdateWindow (FrameWindow);
	return TRUE;
}

int PASCAL WinMain (HANDLE hinst, HANDLE prev_inst, LPSTR cmdline, int cmdshow)
{
	char fname [2000];
	MSG msg;

	MyInstance = hinst;

	if (!Profapi_init ())	{
		MessageBox (NULL, "Error initializing profiler library", "XPVIEW Error", MB_OK | MB_ICONSTOP);
		return FALSE;
	}

	if (! InitApplication () || ! InitMainWindow (cmdshow))
		return FALSE;

	mod_placement_present  = read_placement ("module-list", &module_placement);
	proc_placement_present = read_placement ("module", &proc_placement);
	calls_placement_present= read_placement ("calls", &calls_placement);
	text_placement_present = read_placement ("text", &text_placement);
	
	if (cmdline [0])	{
		strncpy (fname, cmdline, sizeof (fname)-5);
		add_ext (fname, fname, DEFEXT);
		try_open_file (fname);
	}

	while (GetMessage (&msg, NULL, 0, 0)) {
		if (TranslateMyMDISysAccel (FrameWindow, &msg))
			continue;
//		if (TranslateAccelerator (FrameWindow, MainAccel, &msg))
//			continue;

		TranslateMessage (&msg);
		if(msg.message==WM_LBUTTONDOWN)
			xMouseCoord = LOWORD(msg.lParam);
		DispatchMessage (&msg);
	}
	if (mod_placement_present) write_placement ("module-list", &module_placement);
	if (proc_placement_present) write_placement ("module", &proc_placement);
	if (calls_placement_present) write_placement ("calls", &calls_placement);
	if (text_placement_present) write_placement ("text", &text_placement);
	return msg.wParam;
}
