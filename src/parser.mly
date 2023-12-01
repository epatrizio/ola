%token PLUS MINUS MUL DIV FLDIV MOD EXP DOT DDOT TDOT FUNCTION LPAREN RPAREN LBRACKET RBRACKET LBRACES RBRACES COLON DCOLON SEMICOLON COMMA SQUOTE DQUOTE EQ LT LE GT GE EQEQ NEQ NOT SHARP AND OR LAND LOR LSL LSR TILDE DO END BREAK RETURN WHILE REPEAT UNTIL IF THEN ELSE ELSEIF GOTO FOR IN LOCAL EOF
%token UNARY_OP (* administrative token to distinguish unary minus from subtraction *)
%token <string> NAME ATTRIB
%token <Ast.value> VALUE

%left OR
%left AND
%left LT GT LE GE NEQ EQEQ
%left LOR
%left TILDE
%left LAND
%left LSL LSR
%right DDOT
%left PLUS MINUS
%left MUL DIV FLDIV MOD
%left UNARY_OP (* unary operators *)
%right EXP

%{

open Ast

%}

%start chunk

%type <Ast.block> chunk
%type <Ast.expr'> exp_bis

%%

let chunk :=
  | ~ = block; EOF; <>

let block :=
  | stats = list(stat); retstat = option(retstat); {
    match retstat with
    | None -> stats
    | Some retstat -> stats @ [ retstat ]
}

let stat ==
  | SEMICOLON; { Sempty }
  | ~ = varlist; EQ; ~ = explist; <Sassign>
  | ~ = functioncall; <SfunctionCall>
  | ~ = label; <Slabel>
  | BREAK; { Sbreak }
  | GOTO; ~ = NAME; <Sgoto>
  | DO; ~ = block; END; <Sblock>
  | WHILE; ~ = exp; DO; ~ = block; END; <Swhile>
  | REPEAT; ~ = block; UNTIL; ~ = exp; <Srepeat>
  | IF; ~ = exp; THEN; ~ = block; ~ = list(elseif); ~ = option(preceded(ELSE, block)); END; <Sif>
  | FOR; ~ = NAME; EQ; e1 = exp; COMMA; e2 = exp; ~ = option(preceded(COMMA, exp)); DO; ~ = block; END; <Sfor>
  | FOR; ~ = namelist; IN; ~ = explist; DO; ~ = block; END; <Siterator>
  | FUNCTION; ~ = funcname; ~ = funcbody; {
    (* Sfunction syntactic sugar *)
    Sassign (
      [ VarName (String.concat "." funcname) ],
      [ ($startpos, $endpos), (Efunctiondef funcbody) ]
    )
  }
  | LOCAL; FUNCTION; name = NAME; ~ = funcbody; {
    (* SfunctionLocal syntactic sugar *)
    SassignLocal (
      [ name, None ],
      [ ($startpos, $endpos), (Efunctiondef funcbody) ]
    )
  }
  | LOCAL; ~ = attnamelist; ~ = loption(preceded(EQ, explist)); <SassignLocal>

(* TODO: remove this one *)
let elseif :=
  | ELSEIF; ~ = exp; THEN; ~ = block; <>

let attname :=
  | ~ = NAME; ~ = option(attrib); <>

let attnamelist :=
  | ~ = separated_nonempty_list(COMMA, attname); <>

let attrib :=
  | LT; ~ = ATTRIB; GT; <>

let retstat :=
  | RETURN; ~ = loption(explist); option(SEMICOLON); <Sreturn>

let label :=
  | DCOLON; ~ = NAME; DCOLON; <>

let funcname :=
  | names = separated_nonempty_list(DOT, NAME); last_name = option(preceded(DCOLON, NAME)); {
    match last_name with
    | None -> names
    | Some lname -> names @ [ lname ]
  }

let varlist :=
  | ~ = separated_nonempty_list(COMMA, var); <>

let var :=
  | ~ = NAME; <VarName>
  | ~ = prefixexp; ~ = delimited(LBRACKET, exp, RBRACKET); <VarTableField>
  | ~ = prefixexp; DOT; name = NAME; {
      VarTableField (
        prefixexp, (($startpos, $endpos), Evalue (Vstring name))
      )
  }

let namelist :=
  | ~ = separated_nonempty_list(COMMA, NAME); <>

let explist :=
  | ~ = separated_nonempty_list(COMMA, exp); <>

let exp_bis :=
  | ~ = VALUE; <Evalue>
  | TDOT; { Evariadic }
  | ~ = functiondef; <Efunctiondef>
  | ~ = prefixexp; <Eprefix>
  | ~ = tableconstructor; <Etableconstructor>
  | e1 = exp; ~ = binop; e2 = exp; <Ebinop>
  | ~ = unop; ~ = exp; %prec UNARY_OP <Eunop>

let exp :=
  | ~ = exp_bis; { (($startpos, $endpos), (exp_bis : expr')) : expr }

let prefixexp :=
  | ~ = var; <PEvar>
  | ~ = functioncall; <PEfunctioncall>
  | ~ = delimited(LPAREN, exp, RPAREN); <PEexp>

let functioncall :=
  | ~ = prefixexp; ~ = args; <FCpreargs>
  | ~ = prefixexp; COLON; ~ = NAME; ~ = args; <FCprename>

let args :=
  | ~ = delimited(LPAREN, loption(explist), RPAREN); <Aexpl>
  | ~ = tableconstructor; <Atable>
  | SQUOTE; name = NAME; SQUOTE; {
      Aexpl [ (($startpos, $endpos), Evalue (Vstring name)) ]
    }
  | DQUOTE; name = NAME; DQUOTE; {
      Aexpl [ (($startpos, $endpos), Evalue (Vstring name)) ]
    }
  | LBRACKET; LBRACKET; name = NAME; RBRACKET; RBRACKET; {
      Aexpl [ (($startpos, $endpos), Evalue (Vstring name)) ]
    }
  (* Astr syntactic sugar
  f'string' (or f"string" or f[[string]]) is syntactic sugar for f('string')
  *)

let functiondef :=
  | FUNCTION; ~ = funcbody; <>

let funcbody :=
  | LPAREN; parlist = option(parlist); RPAREN; ~ = block; END; {
    match parlist with
    | None -> PLlist ([], None), block
    | Some parlist -> parlist, block
  }

let variadic :=
  | TDOT; { ($startpos, $endpos), Evariadic }

let parlist :=
  | ~ = namelist; ~ = option(preceded(COMMA, variadic)); <PLlist>
  | ~ = variadic; <PLvariadic>

let tableconstructor :=
  | LBRACES; ~ = loption(fieldlist); RBRACES; <>

let fieldlist :=
  | ~ = separated_nonempty_list(fieldsep, field); option(fieldsep); <>

let field :=
  | LBRACKET; e1 = exp; RBRACKET; EQ; e2 = exp; <Fcol>
  | ~ = NAME; EQ; ~ = exp; <Fname>
  | ~ = exp; <Fexp>

let fieldsep :=
  | COMMA; { }
  | SEMICOLON; { }

let binop :=
  | AND; { Band }
  | OR; { Bor }
  | PLUS; { Badd }
  | MINUS; { Bsub }
  | MUL; { Bmul }
  | DIV; { Bdiv }
  | FLDIV; { Bfldiv }
  | MOD; { Bmod }
  | EXP; { Bexp }
  | LAND; { Bland }
  | LOR; { Blor }
  | TILDE; { Blxor }
  | LSL; { Blsl }
  | LSR; { Blsr }
  | LT; { Blt }
  | LE; { Ble }
  | GT; { Bgt }
  | GE; { Bge }
  | EQEQ; { Beq }
  | NEQ; { Bneq }
  | DDOT; { Bddot }

let unop :=
  | NOT; { Unot }
  | MINUS; { Uminus }
  | SHARP; { Usharp }
  | TILDE; { Ulnot }

%%
