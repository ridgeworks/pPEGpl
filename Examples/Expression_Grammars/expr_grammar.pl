:- module(expr_grammar,[
	 expr_grammar/1,
	 parse_expr/2,
	 pratt_expr_grammar/1,
	 pratt_parse_expr/2,
	 pratt_to_term/2,
	 % defining new arithmetic functions
	 '++'/2,
	 '--'/2
	]).

:- current_module(pPEG) -> true ; use_module(library(pPEG)).

expr_grammar({|pPEG||

	expr       = (prefixOp " ")* " " term " " (postfixOp " ")* (infixOp " " expr)?

	term       = number / Pexpr
	number     = [0-9]+
	Pexpr      = '(' expr ')'

	prefixOp   = unaryOp
	infixOp    = addOp / mulOp / expOp
	postfixOp  = postOp

	unaryOp    = [-+]

	addOp      = [-+]
	mulOp      = [*/]
	expOp      = '^'

	postOp     = '++' / '--'
	
|}).

parse_expr(Src,Tree) :-
	expr_grammar(EG),
	peg_parse(EG,Src,Tree).

	
pratt_expr_grammar({|pPEG||

	expr       = (prefixOp " ")* " " term " " (postfixOp " ")* (infixOp " " expr)?

	term       = number / Pexpr
	number     = [0-9]+
	Pexpr      = '(' expr ')'

	prefixOp   = unaryOp_6R
	infixOp    = addOp_3L / mulOp_4L / expOp_5R
	postfixOp  = postOp_6L

	unaryOp_6R = [-+]

	addOp_3L   = [-+]
	mulOp_4L   = [*/]
	expOp_5R   = '^'

	postOp_6L  = '++' / '--'

|}).

:- arithmetic_function(user:(++ /1)).
++(N,R) :- arithmetic_expression_value(N,V), R is V+1.

:- arithmetic_function(user:(-- /1)).
--(N,R) :- arithmetic_expression_value(N,V), R is V-1.

% parse a source string using pratt_expr_grammar/1 
pratt_parse_expr(Src,Tree) :-  
	pratt_expr_grammar(EG),
	peg_parse(EG,Src,Tree).

% convert a pratt tree of an expression in pratt_expr_grammar/1 to an arithmetic term
pratt_to_term(number(S), N) :- !,
	number_string(N,S).
pratt_to_term('Pexpr'([Pratt]), Exp) :- !,
	pratt_to_term(Pratt,Exp).
pratt_to_term(Func, Exp) :-
	Func =.. [F,Args],
	pratt_termList(Args,Args1),
	Exp =.. [F|Args1].

pratt_termList([],[]).
pratt_termList([A|Args],[A1|Args1]) :-
	pratt_to_term(A,A1),
	pratt_termList(Args,Args1).
