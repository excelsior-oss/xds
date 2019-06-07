#include <windows.h>
#include <stdlib.h>
#include "winmake.h"

HANDLE	outhandle = (HANDLE)-1;

int     ConnectShell (void)
{
        return 1;
}

void    DisconnectShell (void)
{
}

void	writebuf (char * s, int len)
{
	DWORD n;
	if (outhandle == (HANDLE)-1)
		outhandle = GetStdHandle (STD_OUTPUT_HANDLE);

	while (len>0)	{
		if (! WriteFile (outhandle, s, len, &n, 0)) return;
		len -= n;
		s += n;
	}
}

char	StringPat  [] = {1, 'S', 0, 0};
char	CaptionPat [] = {1, 'C', 0, 0};
char	CommentPat [] = {1, 'M', 0, 0};
char	ErrorPat   [] = {1, 'E',
                         0, 0, 0, 0,            /* error # */
                         0, 0, 0, 0,            /* y */
			 0, 0, 0, 0,		/* x */
			 0, 0,			/* filename length */
			 0, 0,			/* error text length */
			 0			/* error class */
			};
char	JobPat	   [] = {1, 'J',
			 0, 0, 0, 0,		/* progress limit */
			 0, 0		        /* comment length */
			};
char	ProgressPat[] = {1, 'P',
			 0, 0, 0, 0,		/* comment progress */
			 0, 0, 0, 0		/* progress         */
			};
char	NamePat        [] = {1, 'f', 0, 0};
char	FileListPat    [] = {1, 'F'};
char	FileCommitPat  [] = {1, 'X'};

char	ModePat	       [] = {1, 'm', 0};

/* following two macroes are only valid for x86 platforms;
   should be rewritten for machines with data alignment;
   anyway, word and long should occupy two and four bytes respectibely
   low-endian format
*/

#define PutWord(addr, w) *(WORD*)(addr) = (WORD) (w)
#define PutLong(addr, l) *(LONG*)(addr) = (LONG) (l)

#define NONNULL(s) ((s)? (s) : "")

void	SendString (char * s)
{
	int l = strlen (s = NONNULL (s));
	PutWord  (StringPat+2, l);
	writebuf (StringPat, sizeof (StringPat));
	writebuf (s, l);
}

void	SendCaption (char * s)
{
	int l = strlen (s = NONNULL (s));
	PutWord  (CaptionPat+2, l);
	writebuf (CaptionPat, sizeof (CaptionPat));
	writebuf (s, l);
}

void    SendError (ERRCLASS err_class, long err_no, long x, long y,
		   char * filename, char * body)
{
	int lf, lt;
        PutLong (ErrorPat+2, err_no);
        PutLong (ErrorPat+6, y);
        PutLong (ErrorPat+10, x);
	lf = strlen (filename = NONNULL (filename));
        lt = strlen (body = NONNULL (body));
        PutWord (ErrorPat+14, lf);
        PutWord (ErrorPat+16, lt);
        ErrorPat [18] = err_class;
	writebuf (ErrorPat, sizeof (ErrorPat));
	writebuf (filename, lf);
	writebuf (body, lt);
}

void	SendComment (char * s)
{
	int l = strlen (s = NONNULL (s));
	PutWord  (CommentPat+2, l);
	writebuf (CommentPat, sizeof (CommentPat));
	writebuf (s, l);
}

void	SendStartJob (char * comment, long progress_limit)
{
	int l = strlen (comment = NONNULL (comment));
	PutLong (JobPat+2, progress_limit);
	PutWord (JobPat+6, l);
	writebuf (JobPat, sizeof (JobPat));
	writebuf (comment, l);
}

void	SendProgress (long comment_progress, long progress)
{
	PutLong (ProgressPat+2, comment_progress);
	PutLong (ProgressPat+6, progress);
	writebuf (ProgressPat, sizeof (ProgressPat));
}

void	SendStartFileList (void)
{
	writebuf (FileListPat, sizeof (FileListPat));
}

void	SendFileName (char * name)
{
	int l = strlen (name = NONNULL (name));
	PutWord  (NamePat+2, l);
	writebuf (NamePat, sizeof (NamePat));
	writebuf (name, l);
}

void	CommitFileList (void)
{
	writebuf (FileCommitPat, sizeof (FileCommitPat));
}

void    SetMode (char mode)
{
	ModePat [2] = mode;
	writebuf (ModePat, sizeof (ModePat));
}
