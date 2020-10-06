%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define STRING_TOKEN        yylval.string = new std::string(yytext, yyleng)
#define KEYWORD_TOKEN(t)    yylval.token = t
%}

%option noyywrap

%%

[ \t\n]                                         ;
"extern"                                        KEYWORD_TOKEN(TEXTERN); return TEXTERN;
"return"                                        KEYWORD_TOKEN(TRETURN); return TRETURN;

[a-zA-Z_][a-zA-Z0-9_]*                          STRING_TOKEN; return TIDENTIFIER;
[0-9]+\.[0-9]*                                  STRING_TOKEN; return TDOUBLE;
[0-9]+                                          STRING_TOKEN; return TINTEGER;

"="                                             KEYWORD_TOKEN(TEQUAL); return TEQUAL;
"=="                                            KEYWORD_TOKEN(TCEQ); return TCEQ;
"!="                                            KEYWORD_TOKEN(TCNE); return TCNE;
"<"                                             KEYWORD_TOKEN(TCLT); return TCLT;
"<="                                            KEYWORD_TOKEN(TCLE); return TCLE;
">"                                             KEYWORD_TOKEN(TCGT); return TCGT;
">="                                            KEYWORD_TOKEN(TCGE); return TCGE;

"("                                             KEYWORD_TOKEN(TLPAREN); return TLPAREN;
")"                                             KEYWORD_TOKEN(TRPAREN); return TRPAREN;
"{"                                             KEYWORD_TOKEN(TLBRACE); return TLBRACE;
"}"                                             KEYWORD_TOKEN(TRBRACE); return TRBRACE;

"."                                             KEYWORD_TOKEN(TDOT); return TDOT;
","                                             KEYWORD_TOKEN(TCOMMA); return TCOMMA;

"+"                                             KEYWORD_TOKEN(TPLUS); return TPLUS;
"-"                                             KEYWORD_TOKEN(TMINUS); return TMINUS;
"*"                                             KEYWORD_TOKEN(TMUL); return TMUL;
"/"                                             KEYWORD_TOKEN(TDIV); return TDIV;

.                                               printf("Unknown token!\n"); yyterminate();

%%