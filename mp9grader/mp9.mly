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
%left orelse
%left andalso
%left EQUALS LT GT LEQ GEQ NEQ
%right DCOLON
%left TIMES DIV DTIMES DDIV
%left PLUS MINUS DPLUS DMINUS CARAT

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Mp9common.dec> main

%%

main:
    expression SEMI                                 { Val("it", $1) }
  | dec SEMI                                        { $1 }

dec:
    atomic_dec                                      { $1 }
  | dec atomic_dec                                  { Seq($1, $2) }

atomic_dec:
    VAL simp_bind                                   { Val (fst $2, snd $2) }
  | LOCAL dec IN dec END                            { Local($2, $4) }
  | VAL REC IDENT rec_bind                          { Rec($3, fst $4, snd $4) }
  | FUN IDENT rec_bind                              { Rec($2, fst $3, snd $3) }

rec_bind:
    IDENT EQUALS expression                         { ($1, $3) }
  | IDENT rec_bind                                  { ($1, FnExp(fst $2, snd $2)) }

simp_bind:
    IDENT EQUALS expression                         { ($1, $3) }
  | UNDERSCORE EQUALS expression                    { ("", $3) }

expression:
    raise_exp                                       { $1 }

raise_exp:
  | RAISE expression                                { RaiseExp($2) }
  | fn_exp                                          { $1 }

fn_exp:
    FN IDENT ARROW expression                       { FnExp($2, $4) }
  | if_exp                                          { $1 }

if_exp:
    IF expression THEN expression ELSE expression   { IfExp($2, $4, $6) }
  | or_else_exp                                     { $1 }

or_else_exp:
    or_else_exp ORELSE and_also_exp                 { IfExp($1, ConstExp(BoolConst(true)), $3) }
  | and_also_exp                                    { $1 }

and_also_exp:
    and_also_exp ANDALSO rel_op_exp             { IfExp($1, $3, ConstExp(BoolConst(false))) }
  | rel_op_exp                                  { $1 }

rel_op_exp:
    rel_op_exp rel_op cons_exp                  { $2 $1 $3 }
  | cons_exp                                    { $1 }

cons_exp:
    lo_bin_op_exp DCOLON cons_exp               { BinOpAppExp(ConsOp, $1, $3) }
  | lo_bin_op_exp                               { $1 }      

lo_bin_op_exp:
    lo_bin_op_exp lo_bin_op hi_bin_op_exp       { $2 $1 $3 }
  | hi_bin_op_exp                               { $1 }

hi_bin_op_exp:
    hi_bin_op_exp hi_bin_op monop_exp             { $2 $1 $3 }
  | monop_exp                                     { $1 }

hi_bin_op:
    TIMES                                      { fun x y -> BinOpAppExp(IntTimesOp, x, y) }
  | DIV                                        { fun x y -> BinOpAppExp(IntDivOp, x, y) }
  | DTIMES                                     { fun x y -> BinOpAppExp(RealTimesOp, x, y) }
  | DDIV                                       { fun x y -> BinOpAppExp(RealDivOp, x, y) } 

lo_bin_op:
    PLUS                                       { fun x y -> BinOpAppExp(IntPlusOp, x, y) }
  | MINUS                                      { fun x y -> BinOpAppExp(IntMinusOp, x, y) }
  | DPLUS                                      { fun x y -> BinOpAppExp(RealPlusOp, x, y) }
  | DMINUS                                     { fun x y -> BinOpAppExp(RealMinusOp, x, y) }
  | CARAT                                      { fun x y -> BinOpAppExp(ConcatOp, x, y) }

rel_op:
    EQUALS                                      { fun x y -> BinOpAppExp(EqOp, x, y) }
  | LT                                          { fun x y -> BinOpAppExp(GreaterOp, y, x) }
  | GT                                          { fun x y -> BinOpAppExp(GreaterOp, x, y) }
  | LEQ                                         { fun x y -> IfExp(BinOpAppExp(GreaterOp, y, x), ConstExp(BoolConst(true)), BinOpAppExp(EqOp, x, y)) }
  | GEQ                                         { fun x y -> IfExp(BinOpAppExp(GreaterOp, x, y), ConstExp(BoolConst(true)), BinOpAppExp(EqOp, x, y)) }
  | NEQ                                         { fun x y -> IfExp(BinOpAppExp(EqOp, x, y), ConstExp(BoolConst(false)), ConstExp(BoolConst(true))) }

monop_exp:
    mon_op                                      { FnExp("x",MonOpAppExp($1, VarExp("x"))) }
  | mon_op monop_exp                            { MonOpAppExp($1, $2) }
  | app_exp                                     { $1 }

app_exp:
    app_exp op_exp                              { AppExp($1, $2) }
  | op_exp                                      { $1 }

op_exp:
    OP bin_op                                   { FnExp("x", FnExp("y", $2 (VarExp("x")) (VarExp("y")))) }
  | rest_op                                     { $1 }

bin_op:
    hi_bin_op                                   { $1 }
  | lo_bin_op                                   { $1 }
  | rel_op                                      { $1 }

rest_op:
    LPAREN paren_exp RPAREN                     { $2 }
  | LET dec IN paren_exp END                    { LetExp($2, $4) }
  | LBRAC list_exp RBRAC                        { $2 }
  | constant_exp                                { $1 }
  | IDENT                                       { VarExp($1) }

list_exp:
    expression                                     { BinOpAppExp(ConsOp, $1, ConstExp(NilConst)) }
  | expression COMMA list_exp                      { BinOpAppExp(ConsOp, $1, $3) }

paren_exp:
    expression                                     { $1 }
 |  expression COMMA paren_exp                     { BinOpAppExp(CommaOp, $1, $3) }
 |  expression SEMI  paren_exp                     { LetExp(Val("", $1), $3) }

mon_op:
    NEG                                         { IntNegOp }
  | HD                                          { HdOp }
  | TL                                          { TlOp }
  | FST                                         { FstOp }
  | SND                                         { SndOp }

constant_exp:
    INT                                         { ConstExp(IntConst($1)) }
  | REAL                                        { ConstExp(RealConst($1)) }
  | BOOL                                        { ConstExp(BoolConst($1)) }
  | STRING                                      { ConstExp(StringConst($1)) }
  | UNIT                                        { ConstExp(UnitConst) }
  | NIL                                         { ConstExp(NilConst) }
  | LBRAC RBRAC                                 { ConstExp(NilConst) }

