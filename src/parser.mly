/* Syntactic analyzer */

%{ %}

%token PLUS MINUS MUL DIV FLDIV MOD EXP DDOT LPAREN RPAREN SEMICOLON COMMA
%token AEQ LT LE GT GE EQ NEQ
%token NOT SHARP AND OR
%token DO END WHILE REPEAT UNTIL IF THEN ELSE ELSEIF
%token PRINT
%token EOF

%token <Ast.var> VAR
%token <Ast.value> VALUE

%left MINUS PLUS
%left MUL DIV

%nonassoc uminus
// %nonassoc IF
// %nonassoc ELSE

%start chunk

%type <Ast.chunk> chunk
// %type <Ast.unop> unop
// %type <Ast.binop> binop
// %type <Ast.stmt> stmt
// %type <Ast.expr> expr
// %type <Ast.stmt list> stmt_list
// %type <Ast.expr list> expr_list

%%

chunk : l=block EOF { l };

block :
     | l=list(stmt) { l }
     ;

elseif :
     | ELSEIF e=expr THEN b=block { (e, b) }

elseop :
     | ELSE b=block { b }

stmt :
     | SEMICOLON { Ast.Sempty }
     | vl=separated_nonempty_list(COMMA, VAR) AEQ el=separated_nonempty_list(COMMA, expr) { Ast.Sassign (vl, el) }
     | DO b=block END { Ast.Sblock b }
     | WHILE e=expr DO b=block END { Ast.Swhile (e, b) }
     | REPEAT b=block UNTIL e=expr { Ast.Srepeat (b, e) }
     | IF e=expr THEN b=block l=list(elseif) o=option(elseop) END { Ast.Sif (e, b, l, o) }
     | PRINT LPAREN e=expr RPAREN { Ast.Sprint e }          // tmp
     ;

unop :
     | NOT { Ast.Unot }
     | MINUS { Ast.Uminus }
     | SHARP { Ast.Usharp }
     ;

binop :
     | AND { Ast.Band }
     | OR { Ast.Bor }
     | PLUS { Ast.Badd }
     | MINUS { Ast.Bsub }
     | MUL { Ast.Bmul }
     | DIV { Ast.Bdiv }
     | FLDIV { Ast.Bfldiv }
     | MOD { Ast.Bmod }
     | EXP { Ast.Bexp }
     | LT { Ast.Blt }
     | LE { Ast.Ble }
     | GT { Ast.Bgt }
     | GE { Ast.Bge }
     | EQ { Ast.Beq }
     | NEQ { Ast.Bneq }
     | DDOT { Ast.Bddot }
     ;

expr :
     | v=VAR { Ast.Evar v }
     | v=VALUE { Ast.Evalue v }
     | op=unop e=expr %prec uminus { Ast.Eunop (op, e) }
     | e1=expr op=binop e2=expr { Ast.Ebinop (op, e1, e2) }
     | LPAREN e=expr RPAREN { e }
     ;

%%
