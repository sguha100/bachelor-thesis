  /* File parser.mly */
      %token <int> INT
	%token <string> IDENT
        %token PLUS MINUS TIMES DIV
        %token LPAREN RPAREN
        %token EOL
	%token NUMSTATES
	%token NUMTRANS
	%token NUMCLOCKS
	%token STATE
	%token PROP
	%token INVAR
	%token TRANS
	%token GOTO
	%token RARROW
	%token COLON
	%token SEMI
	%token LBRACE
	%token RBRACE
	%token LE
	%token LT
	%token GE
	%token GT
	%token EQ
	%token TRUE
	%token FALSE
	%token AND
	%token RESET
        %left PLUS MINUS        /* lowest precedence */
      %left TIMES DIV         /* medium precedence */
      %nonassoc UMINUS        /* highest precedence */
      %left AND
      %start main             /* the entry point */
      %type <Types.timedautomaton> main
      %type <Types.clock list> clocklist
      %type <Types.state> state
      %type <Types.state list> statelist
      %type <Types.proposition> proposition
      %type <Types.transition> transition
      %type <Types.transition list> transitionlist
        %%
      main:
	NUMSTATES INT EOL NUMTRANS INT EOL NUMCLOCKS INT EOL clocklist EOL EOL statelist
	{ {Types.numstates=$2;
	   Types.numtrans=$5;
	   Types.numclocks=$8;
	   Types.clocks = Array.of_list($10);
	  Types.states = Array.of_list($13)} }
        ;
      clocklist:
	                                                              { [] }
    | IDENT clocklist                                                 { {Types.clockname=$1}::$2}
	;
      state:
	STATE COLON INT EOL INVAR COLON proposition EOL TRANS COLON EOL transitionlist
	{ {Types.stateindex=$3; Types.invariant=$7; Types.departures=Array.of_list($12)} }
	;
      statelist:
	                                                              { [] }
    | state EOL statelist                                             { $1::$3 }
	;
      proposition:
	TRUE                                                          { Types.True }
    |	FALSE                                                         { Types.False }
    |	IDENT LT INT                                                  { Types.Comparison($1, Types.Lt, $3) }
    |	IDENT LE INT                                                  { Types.Comparison($1, Types.Le, $3) }
    |	IDENT EQ INT                                                  { Types.Comparison($1, Types.Eq, $3) }
    |	IDENT GE INT                                                  { Types.Comparison($1, Types.Ge, $3) }
    |	IDENT GT INT                                                  { Types.Comparison($1, Types.Gt, $3) }
    |   proposition AND proposition
	{match ($1,$3) with
	  (Types.And l1, Types.And l3) -> Types.And (l1@l3)
	| (Types.And l1, _) -> Types.And ($3::l1)
	| (_, Types.And l3) -> Types.And ($1::l3)
	| (_, _) -> Types.And ([$1; $3])}
	;
      transition:
	proposition RARROW RESET LBRACE clocklist RBRACE SEMI GOTO INT EOL
	{ {Types.condition=$1;Types.clockresets=Array.of_list($5); Types.nextstate=$9} }
	;
      transitionlist:
	                                                               {[]}
    | transition transitionlist                                    {$1::$2}
	;
