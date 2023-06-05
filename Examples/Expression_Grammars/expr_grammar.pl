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

	expr       = _ (prefixOp _)* term _ (postfixOp _)* (infixOp expr)? _

	term       = number / Pexpr
	number     = [0-9]+
	Pexpr      = '(' expr ')'

	prefixOp   = notOp / unaryOp 
	infixOp    = disjOp / compOp /addOp / mulOp / expOp 
	postfixOp  = postOp

	notOp      = [~]
	unaryOp    = [-+]
	
	disjOp     = [?:]
	compOp     = [><=]
	addOp      = [-+]
	mulOp      = [*/]
	expOp      = '^'

	postOp     = '++' / '--'
	
	_          = [ \t\n\r]*     # optional whitespace
	
|}).

parse_expr(Src,Tree) :-
	expr_grammar(EG),
	peg_parse(EG,Src,Tree).

	
pratt_expr_grammar({|pPEG||

	expr       = (prefixOp _)* term _ (postfixOp _)* (infixOp _ expr)?

	term       = number / Pexpr
	number     = [0-9]+
	Pexpr      = '(' expr ')'

	prefixOp   = notOp_2R / unaryOp_7R 
	infixOp    = disjOp_1L / compOp_3L / addOp_4L / mulOp_5L / expOp_6R
	postfixOp  = postOp_7L

	notOp_2R   = [~]
	unaryOp_7R = [-+]

	disjOp_1L  = [?:]
	compOp_3L  = [><=]
	addOp_4L   = [-+]
	mulOp_5L   = [*/]
	expOp_6R   = '^'

	postOp_7L  = '++' / '--' # / '-'
	
	_          = [ \t\n\r]*

|}).

% parse a source string using pratt_expr_grammar/1 
pratt_parse_expr(Src,Tree) :-  
	pratt_expr_grammar(EG),
	peg_parse(EG,Src,Tree).

:- arithmetic_function(user:(++ /1)).
++(N,R) :- arithmetic_expression_value(N,V), R is V+1.

:- arithmetic_function(user:(-- /1)).
--(N,R) :- arithmetic_expression_value(N,V), R is V-1.


% convert a pratt tree of an expression in pratt_expr_grammar/1 to an arithmetic term
pratt_to_term(number(S), N) :- !,            % terminal value
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
