
#include <stdio.h>
#include <string.h>

#include "xdefs.h"

open_namespace

#include "xmem.h"
#include "xos.h"
#include "messages.h"

#include "parser.h"

#define CR     '\r'
#define LF     '\n'
#define SPACE  ' '
#define TAB    '\t'
#define ZERO   '\0'
#define QUOTE  '"'


TextFileParser :: TextFileParser (const char * _filename, int MaxLineLen)
{
    filename = dup (_filename, strlen (_filename));

    file = OS->File ();
    if (!file -> OpenRead (filename, rawdata, datasize)) {
        Message (xFATAL, msgUNABLE_TO_READ_FILE, filename);
    }

    datapos = 0;

    bufsize  = MaxLineLen;
    linebuf  = (char *) xalloc (bufsize+1);
    tokenbuf = (char *) xalloc (bufsize+1);

    line = 0;
    pos  = bufsize; // end of zero line
}


void TextFileParser :: readLine ()
{
    pos = 0;
    memset (linebuf,  0, bufsize+1);
    memset (tokenbuf, 0, bufsize+1);

    if (eof())
        return;

    int i = 0;
    for (;;) {
        ASSERT (i < bufsize);  // out of line buffer
        if (datapos >= datasize) {
            // reached end of file
            rawdata = NULL;
            break;
        }
        char ch = (char) (rawdata [datapos++]);
        if (ch == CR) {
            // end of line, eat next LF symbol (if any)
            if ((datapos < datasize) && (rawdata [datapos] == LF))
                datapos++;
            break;
        }
        if (ch == LF) {
            // end of line
            break;
        }
        // usual character, append to line buffer
        linebuf [i++] = ch;
    }

    line++;
    pos = 0;
}


Bool isSpace (char ch)
{
    return ((ch == SPACE) || (ch == TAB));
}

void TextFileParser :: skipSpaces ()
{
    if (eof())
        return;

    while ((pos < bufsize) && isSpace (linebuf [pos]))
        pos ++;
}


char * TextFileParser :: peekToken ()
{
    memset (tokenbuf, 0, bufsize+1);

    skipSpaces ();

    if (eof ())
        return NULL;

    int i = 0;
    for (;;) {
        ASSERT (pos + i < bufsize); // we should meet zero-terminator

        char ch = linebuf [pos+i];
        if ((ch == ZERO) || isSpace (ch))
            break;

        tokenbuf [i++] = ch;
    }

    if (i == 0)
        return NULL;

    return tokenbuf;
}


char * TextFileParser :: getToken ()
{
    char * token = peekToken ();

    if (token == NULL)
        return NULL;

    int tokenlen = strlen (token);
    ASSERT (tokenlen + pos <= bufsize);

    pos += tokenlen;
    return token;
}


char * TextFileParser :: getQuoted (char *_buf, unsigned int _bufsize)
{
    memset (tokenbuf, 0, bufsize+1);

    skipSpaces ();

    if (eof ())
        return NULL;

    expect ("\"");

    int i = 0;
    for (;;) {
        ASSERT (pos + i < bufsize); // we should meet zero-terminator

        char ch = linebuf [pos+i];
        if (ch == ZERO) {
            Message (xFATAL, msgSTR_NOT_CLOSED, filename, line, pos);
        }
        if (ch == QUOTE) {
            i++;  // skip closing quotation mark
            break;
        }

        tokenbuf [i++] = ch;
    }
    pos += i;

    if (_buf) {
        ASSERT (strlen (tokenbuf) + 1 <= _bufsize);
        strcpy (_buf, tokenbuf);
        return _buf;
    }
    return tokenbuf;
}

char * TextFileParser :: getQuoted ()
{
    return getQuoted (NULL, 0);
}

int TextFileParser :: getDec ()
{
    char * end;
    int num = strtol (linebuf + pos, &end, 10);
    if (end == linebuf + pos) {
        Message (xFATAL, msgFILE__EXPECTED, filename, line, pos, "decimal number");
    }
    int numlen = (end - (linebuf + pos));
    ASSERT (numlen + pos <= bufsize);

    pos += numlen;
    return num;
}

dword TextFileParser :: getHex ()
{
    char * end;
    dword num = strtoul (linebuf + pos, &end, 16);
    if (end == linebuf + pos) {
        Message (xFATAL, msgFILE__EXPECTED, filename, line, pos, "hex number");
    }
    int numlen = (end - (linebuf + pos));
    ASSERT (numlen + pos <= bufsize);

    pos += numlen;
    return num;
}

void TextFileParser :: expect (char *s)
{
    int len = strlen (s);
    if (len + pos < bufsize) {
        if (!strncmp (linebuf + pos, s, len)) {
            pos += len;
            return;
        }
    }
    Message (xFATAL, msgFILE__EXPECTED, filename, line, pos, s);
}

void TextFileParser :: expectEndOfLine ()
{
    if (eof ())
        return;

    skipSpaces ();

    if (linebuf [pos] != ZERO) {
        Message (xFATAL, msgFILE__EXPECTED, filename, line, pos, "end of line");
    }
}

TextFileParser :: ~TextFileParser ()
{
    xfree (linebuf);
    xfree (tokenbuf);
    delete file;
}

close_namespace

