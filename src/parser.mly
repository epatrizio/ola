/* Syntactic analyzer */

%{ %}

%token PLUS MINUS MUL LPAREN RPAREN SEMICOLON     // DIV COMMA
%token NOT AND OR
%token DO END WHILE
%token PRINT
%token EOF

%token <Ast.value> VALUE
%token <Ast.ident> IDENT

%left PLUS MINUS
%left MUL      // DIV

// %nonassoc UMINUS

%start script

%type <Ast.script> script
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Ast.stmt list> stmt_list
// %type <Ast.expr list> expr_list

%%

script : l=stmt_list EOF { l };

stmt :
     | DO b=block END { Ast.Sblock b }
     | WHILE e=expr DO b=block END { Ast.Swhile (e, b) }
     | PRINT LPAREN e=expr RPAREN { Ast.Sprint e }
     ;

block :
     | l=stmt_list { l }
     ;

expr :
     | v=VALUE { Ast.Evalue v }
     | i=IDENT { Ast.Eident i }
     | NOT e=expr { Ast.Eunop (Unot, e) }
     | MINUS e=expr { Ast.Eunop (Uminus, e) }
     | e1=expr AND e2=expr { Ast.Ebinop (Band, e1, e2) }
     | e1=expr OR e2=expr { Ast.Ebinop (Bor, e1, e2) }
     | e1=expr PLUS e2=expr { Ast.Ebinop (Badd, e1, e2) }
     | e1=expr MINUS e2=expr { Ast.Ebinop (Bsub, e1, e2) }
     | e1=expr MUL e2=expr { Ast.Ebinop (Bmul, e1, e2) }
     ;

// expr_list :
//      | { [] }
//      | e=expr { [e] }
//      | e=expr COMMA l=expr_list { e :: l }
//      ;

stmt_list :
     | l=list(stmt) { l }
     | l=separated_list(SEMICOLON, stmt) { l }
     ;

%%
