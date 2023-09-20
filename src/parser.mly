/* Syntactic analyzer */

%{ %}

// %token PLUS MINUS MUL DIV
%token LPAREN RPAREN
%token BEGIN END
%token SEMICOLON    // COMMA
%token PRINT
%token EOF

%token <Ast.value> VALUE
%token<string> IDENT

// %left PLUS MINUS
// %left MUL DIV

// %nonassoc UMINUS

%start script

%type <Ast.stmt> script
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Ast.stmt list> stmt_list
// %type <Ast.expr list> expr_list

%%

script : s=stmt EOF { s };

stmt :
     | BEGIN b=block END { Ast.Sblock b }
     | PRINT LPAREN e=expr RPAREN { Ast.Sprint e }
     ;

block :
     | l=stmt_list { Bstmt l }
     ;

expr :
     | v=VALUE { Ast.Evalue v }
     | i=IDENT { Ast.Eident i }
     ;

// expr_list :
//      | { [] }
//      | e=expr { [e] }
//      | e=expr COMMA l=expr_list { e :: l }
//      ;

stmt_list :
     | { [] }
     | s=stmt { [s] }
     | s=stmt SEMICOLON l=stmt_list { s :: l }
     ;

%%
