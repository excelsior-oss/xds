
#ifndef PARSER_H
#define PARSER_H

#include "xos.h"

class TextFileParser {
  private:
    OSFile * file;          // input file
    char   * filename;      // name of input file (for error messages)

    byte        * rawdata;  // raw data of input file
    unsigned long datasize; // size of input file
    unsigned long datapos;  // current position in input file

    int    bufsize;         // size of line/token buffers
    char * linebuf;         // buffer for input line
    char * tokenbuf;        // buffer for token

    int    line;            // current line number
    int    pos;             // position in current line/line buffer

  public:
    TextFileParser (const char * filename, int MaxLineLen = 1024);

    inline Bool eof () {
        return (rawdata == NULL);
    }

    void readLine ();
    void skipSpaces ();

    char * peekToken ();
    char * getToken  ();

    char * getQuoted (char *buf, unsigned int bufsize);
    char * getQuoted ();

    int    getDec ();
    dword  getHex ();

    void expect (char *s);
    void expectEndOfLine ();

    ~TextFileParser ();
};

#endif // PARSER_H
