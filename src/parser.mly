/* Syntactic analyzer */

%{ %}

%token PLUS MINUS MUL DDOT LPAREN RPAREN SEMICOLON     // DIV COMMA
%token LT LE GT GE EQ NEQ
%token NOT SHARP AND OR
%token DO END WHILE
%token PRINT
%token EOF

%token <Ast.value> VALUE
%token <Ast.ident> IDENT

%left MINUS PLUS
%left MUL      // DIV

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

stmt :
     | SEMICOLON { Ast.Sempty }
     | DO b=block END { Ast.Sblock b }
     | WHILE e=expr DO b=block END { Ast.Swhile (e, b) }
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
     | LT { Ast.Blt }
     | LE { Ast.Ble }
     | GT { Ast.Bgt }
     | GE { Ast.Bge }
     | EQ { Ast.Beq }
     | NEQ { Ast.Bneq }
     | DDOT { Ast.Bddot }
     ;

expr :
     | v=VALUE { Ast.Evalue v }
     | i=IDENT { Ast.Eident i }
     | op=unop e=expr %prec uminus { Ast.Eunop (op, e) }
     | e1=expr op=binop e2=expr { Ast.Ebinop (op, e1, e2) }
     | LPAREN e=expr RPAREN { e }
     ;

// expr_list :
//      | { [] }
//      | e=expr { [e] }
//      | e=expr COMMA l=expr_list { e :: l }
//      | l=separated_list(COMA, expr) { l }
//      | l=list(expr) { l }
//      ;

%%
