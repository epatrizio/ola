/* Syntactic analyzer */

%{ %}

%token PLUS MINUS MUL LPAREN RPAREN SEMICOLON     // DIV COMMA
%token DO END
%token PRINT
%token EOF

%token <Ast.value> NIL VALUE
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
     | PRINT LPAREN e=expr RPAREN { Ast.Sprint e }
     ;

block :
     | l=stmt_list { Bstmt l }
     ;

expr :
     | NIL { Ast.Evalue (Vnil ()) }
     | v=VALUE { Ast.Evalue v }
     | i=IDENT { Ast.Eident i }
     | MINUS e=expr { Ast.Eunop (Uminus, e) }
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
     | { [] }
     | s=stmt l=stmt_list { s :: l }
     | s=stmt SEMICOLON l=stmt_list { s :: l }
     ;

%%
