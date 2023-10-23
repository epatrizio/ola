/* Syntactic analyzer */

%{ %}

%token PLUS MINUS MUL DIV FLDIV MOD EXP DOT DDOT TDOT LPAREN RPAREN FUNCTION
%token COLON DCOLON SEMICOLON COMMA
%token AEQ LT LE GT GE EQ NEQ
%token NOT SHARP AND OR LAND LOR LSL LSR TILDE
%token DO END BREAK RETURN WHILE REPEAT UNTIL IF THEN ELSE ELSEIF GOTO FOR IN LOCAL
%token PRINT
%token EOF

%token <Ast.name> NAME
%token <Ast.attrib> ATTRIB
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
%type <Ast.expr'> expr
// %type <Ast.stmt list> stmt_list
// %type <Ast.expr list> expr_list

%%

chunk : l=block EOF { l };

block :
     | l=list(stmt) { l }
     ;

var :
     | n=NAME { Ast.Name n }

attrib :
     | LT a=ATTRIB GT { a }

attname :
     | n=NAME oa=option(attrib) { (n, oa) }

lexpr :
     | e=expr { (($startpos,$endpos), e) }

exprlist :
     | el=separated_nonempty_list(COMMA, lexpr) { el }

exprlistopt :
     | AEQ el=exprlist { el }

elseif :
     | ELSEIF e=lexpr THEN b=block { (e, b) }

elseop :
     | ELSE b=block { b }

cexpr :
     | COMMA e=lexpr { e }

sempty :
     | SEMICOLON { Ast.Sempty }

variadic :
     | TDOT { Ast.Evariadic }

lvariadic :
     | v=variadic { (($startpos,$endpos), v) }

parlistvariadic :
     | COMMA v=lvariadic { v }

parlist :
     | nl=separated_list(COMMA, NAME) vo=option(parlistvariadic) { (nl, vo) }

funcbody : 
     | LPAREN pl=parlist RPAREN b=block END { (pl, b) }

prefixexp :
     | v=var { Ast.PREvar v }
     | LPAREN e=lexpr RPAREN { Ast.PREexp e }
     // | fc=functioncall { fc }

args :
     | LPAREN el=separated_list(COMMA, lexpr) RPAREN { Ast.Aexplist el }

functioncall :
     | pe=prefixexp a=args { Ast.FCpreargs (pe, a) }

stmt :
     | s=sempty { s }
     | vl=separated_nonempty_list(COMMA, var) AEQ el=exprlist { Ast.Sassign (vl, el) }
     | LOCAL nal=separated_nonempty_list(COMMA, attname) elo=option(exprlistopt) { Ast.SassignLocal (nal, elo) }
     | BREAK { Ast.Sbreak }
     | RETURN elo=option(exprlist) so=option(sempty) { Ast.Sreturn (elo, so) }
     | DCOLON n=NAME DCOLON { Ast.Slabel n }
     | GOTO n=NAME { Ast.Sgoto n }
     | DO b=block END { Ast.Sblock b }
     | WHILE e=lexpr DO b=block END { Ast.Swhile (e, b) }
     | REPEAT b=block UNTIL e=lexpr { Ast.Srepeat (b, e) }
     | IF e=lexpr THEN b=block l=list(elseif) o=option(elseop) END { Ast.Sif (e, b, l, o) }
     | FOR n=NAME AEQ e1=lexpr COMMA e2=lexpr oe=option(cexpr) DO b=block END { Ast.Sfor (n, e1, e2, oe, b) }
     | FOR nl=separated_nonempty_list(COMMA, NAME) IN el=separated_nonempty_list(COMMA, lexpr) DO b=block END { Ast.Siterator (nl, el, b) }
     | FUNCTION n=NAME b=funcbody { Sfunction (n, b) }
     | LOCAL FUNCTION n=NAME b=funcbody { SfunctionLocal (n, b) }
     | fc=functioncall { SfunctionCall fc }
     | PRINT LPAREN e=lexpr RPAREN { Ast.Sprint e }          // tmp
     ;

unop :
     | NOT { Ast.Unot }
     | MINUS { Ast.Uminus }
     | SHARP { Ast.Usharp }
     | TILDE { Ast.Ulnot }
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
     | LAND { Ast.Bland }
     | LOR { Ast.Blor }
     | TILDE { Ast.Blxor }
     | LSL { Ast.Blsl }
     | LSR { Ast.Blsr }
     | LT { Ast.Blt }
     | LE { Ast.Ble }
     | GT { Ast.Bgt }
     | GE { Ast.Bge }
     | EQ { Ast.Beq }
     | NEQ { Ast.Bneq }
     | DDOT { Ast.Bddot }
     ;

expr :
     | v=var { Ast.Evar v }
     | v=VALUE { Ast.Evalue v }
     | op=unop e=lexpr %prec uminus { Ast.Eunop (op, e) }
     | e1=lexpr op=binop e2=lexpr { Ast.Ebinop (op, e1, e2) }
     | v=variadic { v }
     | FUNCTION fb=funcbody { Ast.Efunctiondef fb }
     | LPAREN e=expr RPAREN { e }
     // | pe=prefixexp { Ast.Eprefix pe }    // TODO: BUG
     ;

%%
