%{
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include "7burg.h"
static int yylex(void);
static int lineno;
%}

%union {
    int ival;
    char *sval;
    struct pattern *pval;
}
%token TERM PERCENT START
%token <sval> ID
%token <ival> NUMBER
%token <sval> TEMPLATE
%token <sval> COST
%type <pval> pattern
%type <sval> nonterm
%type <sval> cost

%%
start    : decls PERCENT rules
         | decls
         ;

decls    : /* empty */
         | decls decl
         ;

decl     : TERM idlist '\n'
         | START nonterm '\n'     { if (nonterm($2)->num != 1)
                                        yyerror("redeclaration of the start symbol"); }
         | '\n'
         | error '\n'             { yyerrok; }
         ;

idlist   : /* empty */
         | idlist ID '=' NUMBER   { term($2, $4); }
         ;

rules    : /* empty */
         | rules nonterm ':' pattern TEMPLATE cost '\n'
                                  { rule($2, $4, $5, $6); }
         | rules '\n'
         | rules error '\n'       { yyerrok; }
         ;

nonterm  : ID                     { nonterm($$ = $1); }
         ;

cost     : COST                   { if (*$1 == 0) $$ = "0"; }
         ;

pattern  : ID                                { $$ = pattern($1, NULL, NULL); }
         | ID '(' pattern ')'                { $$ = pattern($1, $3, NULL); }
         | ID '(' pattern ',' pattern ')'    { $$ = pattern($1, $3, $5); }
         ;

%%

static char buf[BUFSIZ];
static char *bp = buf;
static int percent;
static int cost;

static char *readln(void)
{
    lineno++;
    return fgets(buf, BUFSIZ, stdin);
}

static int get(void)
{
    if (*bp == 0) {
        bp = buf;
        *bp = 0;

        if (readln() == NULL)
            return EOF;

        if (*bp == '%' && bp[1] == '{' && (bp[2] == '\n' || bp[2] == '\r')) {
            do {
                if (readln() == NULL)
                    return EOF;
                if (*bp == '%' && bp[1] == '}' && (bp[2] == '\n' || bp[2] == '\r'))
                    break;
                fputs(bp, stdout);
            } while (1);

            if (readln() == NULL)
                return EOF;
        }
    }
    return (unsigned char)*bp++;
}

static int yylex(void)
{
    int c;

    if (cost) {
        char *p;

        bp += strspn(bp, " \t\f");
        p = strchr(bp, '\n');
        if (p == NULL)
            p = bp + strlen(bp);
        while (p > bp && isspace(p[-1]))
            p--;
        yylval.sval = xstrndup(bp, p - bp);
        bp = p;
        cost--;
        return COST;            // always return COST
    }
    while (1) {
        c = get();

        if (c == EOF)
            return 0;
        if (isblank(c))
            continue;

        if (c == '%' && *bp == '%' && (bp[1] == '\n' || bp[1] == '\r')) {
            bp += 1;
            return percent++ ? 0 : PERCENT;
        } else if (c == '%' && !strncmp(bp, "start", 5)) {
            bp += 5;
            return START;
        } else if (c == '%' && !strncmp(bp, "term", 4)) {
            bp += 4;
            return TERM;
        } else if (isdigit(c)) {
            int n = c - '0';
            while (isdigit(*bp)) {
                n = n*10 + *bp - '0';
                bp++;
            }
            yylval.ival = n;
            return NUMBER;
        } else if (isalpha(c) || c == '_') {
            char *src = bp - 1;
            while (isalpha(*bp) || isdigit(*bp) || *bp == '_')
                bp++;
            yylval.sval = xstrndup(src, bp - src);
            return ID;
        } else if (c == '"') {
            char *src = bp;
            while (*bp != '"' && *bp != 0)
                bp++;
            if (*bp != '"')
                fprintf(stderr, "%d: unclosed string literal\n", lineno);
            yylval.sval = xstrndup(src, bp++ - src);
            cost++;             // expect cost
            return TEMPLATE;
        }
        return c;
    }
}

void yyerror(const char *msg, ...)
{
    fprintf(stderr, "%d: %s\n", lineno, msg);
    exit(EXIT_FAILURE);
}
