#ifndef __LEX__
#define __LEX__
#define MAXLEN 256

typedef enum {UNKNOWN, END, INT, ID, ADDSUB, MULDIV, ASSIGN,
    LPAREN, RPAREN, BITWISE, FIN} TokenSet;

extern int match (TokenSet token);
extern int lkahead();
extern void advance(void);
extern char* getLexeme(void);

#endif // __LEX__
