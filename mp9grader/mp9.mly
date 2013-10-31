/* Use the expression datatype defined in expressions.ml: */
%{
    open Mp9common
(* add any extra code here *)

%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> REAL
%token <bool> BOOL
%token <string> STRING IDENT
%token <(int*int)> OPCOM CLCOM
%token NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV CARAT LT GT LEQ GEQ
       EQUALS NEQ PIPE ARROW SEMI DCOLON AT NIL LET LOCAL VAL REC AND END IN
       IF THEN ELSE FUN FN OP MOD RAISE HANDLE WITH NOT ANDALSO ORELSE
       HD TL FST SND
       LBRAC RBRAC LPAREN RPAREN COMMA UNDERSCORE
       UNIT ERROR EOF
%right COMMA SEMI

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Mp9common.dec> main

%%

main:
    expression SEMI                             { Val("it", $1) }
  | dec SEMI                                    { $1 }

dec:
    atomic_dec                                  { $1 }
  | dec atomic_dec                              { Seq($1, $2) }

atomic_dec:
    VAL simp_bind                               { Val (fst $2, snd $2) }

simp_bind:
    IDENT EQUALS expression                     { ($1,$3) }

expression:
    IDENT                                       { VarExp($1) }
  | INT                                         { ConstExp(IntConst($1)) }
  | REAL                                        { ConstExp(RealConst($1)) }
  | BOOL                                        { ConstExp(BoolConst($1)) }
  | STRING                                      { ConstExp(StringConst($1)) }
  | UNIT                                        { ConstExp(UnitConst) }
  | NIL                                         { ConstExp(NilConst) }
  | NEG expression                              { MonOpAppExp(IntNegOp, $2) }
  | NEG                                         { FnExp("x",MonOpAppExp(IntNegOp,VarExp("x"))) }
  | FST expression                              { MonOpAppExp(FstOp, $2) }
  | FST                                         { FnExp("x",MonOpAppExp(FstOp,VarExp("x"))) }
  | SND expression                              { MonOpAppExp(SndOp, $2) }
  | SND                                         { FnExp("x",MonOpAppExp(SndOp,VarExp("x"))) }
  | HD expression                               { MonOpAppExp(HdOp, $2) }
  | HD                                          { FnExp("x",MonOpAppExp(HdOp,VarExp("x"))) }
  | TL expression                               { MonOpAppExp(TlOp, $2) }
  | TL                                          { FnExp("x",MonOpAppExp(TlOp,VarExp("x"))) }
  | LPAREN expression RPAREN                    { $2 }
  | LPAREN expression COMMA commaexp RPAREN     { BinOpAppExp(CommaOp, $2, $4) }
  | LPAREN expression SEMI letexp RPAREN        { LetExp(Val((""),$2), $4) }
  | LET dec IN letexp END                       { LetExp($2, $4) }
  | LBRAC RBRAC                                 { ConstExp(NilConst) }

commaexp:
  expression COMMA commaexp                     { BinOpAppExp(CommaOp, $1, $3) }
 | expression                                   { $1 }

letexp:
   expression                                   { $1 }
 | expression SEMI letexp                       { LetExp(Val((""),$1), $3) }