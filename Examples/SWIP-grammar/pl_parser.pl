%
% pl_parser.pl == SWI-Prolog module which maps pl_grammar ptrees to Prolog terms
%
/*	The MIT License (MIT)
 *
 *	Copyright (c) 2022 Rick Workman
 *
 *	Permission is hereby granted, free of charge, to any person obtaining a copy
 *	of this software and associated documentation files (the "Software"), to deal
 *	in the Software without restriction, including without limitation the rights
 *	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *	copies of the Software, and to permit persons to whom the Software is
 *	furnished to do so, subject to the following conditions:
 *
 *	The above copyright notice and this permission notice shall be included in all
 *	copies or substantial portions of the Software.
 *
 *	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *	SOFTWARE.
 */

:- module(pl_parser, [
	 string_termList/2       % parse a string specifying a sequence of terms separated by
	]).                      %   full stops to a list of Prolog terms, i.e., input not
	                         %   restricted to clauses.

:- current_module(pPEG) -> true ; use_module(library(pPEG)).
:- current_module(pl_grammar).

:- multifile prolog:message/1.

/* Caveats:
- no dynamic operator definitions (it's a parser, not a compiler/loader)
- if the quasi-quotation syntax is defined, the named QQ parser is invoked to generate the result,
	otherwise a term of the form '$quasi_quotation'(SyntaxName,Content,SyntaxArgs,VariableNames) is produced
- no support for generating dict expressions, including functional notation - left as Prolog term
- small cheat in that read_term_from_atom/3 is to convert "number" strings to Prolog numbers
- parser maintains rather odd syntax restrictions for NaN values ??
*/

% Exports string_termList/2
string_termList(String,Terms) :-
	prolog_grammar(PG),  % from pl_grammar
	peg_parse(PG,String,'Prolog'(Nodes)),
	nodes_terms(Nodes,Terms).

nodes_terms([],[]).
nodes_terms([Node|Nodes],[Term|Terms]) :-
	node_to_term(Node,[],VarList,Term),
	% Comment out the next two lines to preserve variables:
	name_vars_(VarList),                                   % binds named vars to names
	term_singletons(Term,Singles), name_singles_(Singles), % in effect, binds anonymous to '_'
	nodes_terms(Nodes,Terms).

% utility predicates for managing list of variable names
name_vars_([]).
name_vars_([Name = '$VAR'(Name)|VarList]) :-
	name_vars_(VarList).

name_singles_([]).
name_singles_(['$VAR'('_')|Singles]) :-
	name_singles_(Singles).

% merge Var=Name lists
mergeNamedVars([],_Vars).
mergeNamedVars([V|Vs],Vars) :-
	memberchk(V,Vars),
	mergeNamedVars(Vs,Vars).

%% node_to_term(Node,Term) - convert a ptree node to a native prolog term
node_to_term('Pexpr'([Expr]), Vars, NxtVars, Term) :- !,
	node_to_term(Expr, Vars, NxtVars, Term).
node_to_term('Compound'([atom(FS)|Args]), Vars, NxtVars, Term) :- !,
	node_to_term(atom(FS), Vars, Vars, F),  %%atom_string(F,FS),
	(Args = []
	 -> NxtVars = Vars,
	    compound_name_arity(Term,F,0)
	 ;  args_terms(Args,Vars,NxtVars,TArgs),
	    Term =.. [F|TArgs]
	).
node_to_term('List'(Args), Vars, NxtVars, Term) :- !,
	args_terms(Args,Vars,NxtVars,Term).
node_to_term('Curly'([]), Vars, Vars, {}) :- !.
node_to_term('Curly'([Arg]), Vars, NxtVars, Term) :- !,
	node_to_term(Arg,Vars,NxtVars,Exp),
	Term =.. ['{}',Exp].
node_to_term('QQuote'([SynExp,qcontent(Content)]), Vars, NxtVars, Term) :- !,
	node_to_term(SynExp,Vars,NxtVars,Syntax),
	term_string(Syntax,QQsyntax,[variable_names(NxtVars)]),
	atomics_to_string(['{|',QQsyntax,'||',Content,'|}'],QQuote),
	(catch(term_string(Term,QQuote,[variable_names(VarNames)]),
	       error(syntax_error(unknown_quasi_quotation_syntax(_,_)), _),
	       fail
	      )
	 -> mergeNamedVars(VarNames,NxtVars)
	 ;  Syntax =.. [SyntaxName|SyntaxArgs], % no defined handler - generate a QQ definition
	    Term = '$quasi_quotation'(SyntaxName,Content,SyntaxArgs,NxtVars)
	).
node_to_term(atom(Raw), Vars, Vars, Atom) :- !,
	(sub_string(Raw,0,1,_,"'")        % strip outer quotes if present
	 -> sub_string(Raw,1,_,1,Raw1),   % if quoted process escapes
	    string_chars(Raw1,Cs),
	    unescape_(Cs,'\'',ECs),
	    atom_chars(Atom,ECs)	 	
	 ;  split_string(Raw," "," ",LRaw),  % brackets test (only removes space chars, not whitespace in general)
	    atomics_to_string(LRaw,Str),
	    (Str = "[]" -> Atom = [] ;
	     Str = "{}" -> Atom = {} ;
	     atom_string(Atom,Raw)
	    )
	 ).
node_to_term(string(Raw), Vars, Vars, String):- !,
	sub_string(Raw,1,_,1,Raw1),       % strip outer quotes
	string_chars(Raw1,Cs),
	unescape_(Cs,'"',ECs),
	string_chars(String,ECs).
node_to_term(bquote(Raw), Vars, Vars, Codes):- !,
	sub_string(Raw,1,_,1,Raw1),       % strip outer quotes
	string_chars(Raw1,Cs),
	unescape_(Cs,'`',ECs),
	atom_chars(Atom,ECs), atom_codes(Atom,Codes).  % faster than traversing the list again?
node_to_term(rational(String), Vars, Vars, Num) :- !,
	string_number_(String,Num). 
node_to_term(integer(String), Vars, Vars, Num) :- !,
	string_number_(String,Num). 
node_to_term(float(String), Vars, Vars, Num) :- !,
	string_number_(String,Num). 
node_to_term(var("_"), Vars, Vars, _Var) :- !.  % anonymous var
node_to_term(var(String), Vars, NxtVars, Var) :- !,
	atom_string(Name,String),
	(memberchk(Name = Var, Vars)
	 -> NxtVars = Vars                          % repeat Var
	 ;  NxtVars = [Name = Var|Vars]             % new non-anonymous var
	).
node_to_term(arg(Expr), Vars, NxtVars, Term) :- !,  % 'arg' is equivalent to 'expr'
	node_to_term(expr(Expr), Vars, NxtVars, Term).
node_to_term(elem(Expr), Vars, NxtVars, Term) :- !,  % 'elem' is equivalent to 'expr'
	node_to_term(expr(Expr), Vars, NxtVars, Term).
node_to_term(expr(Expr), Vars, NxtVars, Term) :- !,
	% gateway to nasty operator stuff (precedence and associativity)
	flatten_exp_(Expr,T/T,List/[]),
	build_term_(List, Vars, NxtVars, Term),
	!.  % deterministic

node_to_term('$"'(Term), Vars, Vars, Term).     % already converted

% parse a number using builtin number_string/2 (a bit of a cheat since it uses SWIP parser)
string_number_(String,Num) :-
	catch(read_term_from_atom(String,Num,[]),_,fail),  % error ==> fail
	number(Num), !.
string_number_(String,Num) :-
	sub_string(String,0,3,0,"NaN"),             % mimic current SWIP syntax requirements
	sub_string(String,0,_,3,NString),
	number_string(Flt,NString),
	AFlt is abs(Flt),
	1.0 < AFlt, AFlt < 2.0,
	!,
	Num is nan.
string_number_(String,_Num) :-	
	print_message(informational, prolog_parser(bad_number(String))),
	fail.

prolog:message(prolog_parser(bad_number(String))) -->  % DCG
	[ "prolog_parser Error, invalid number value from  ~p" - [String] ].

% list and structure args
args_terms([],Vars,Vars,[]).
args_terms(['Tail'([])],VarsIn,VarsIn,[]) :- !.
args_terms(['Tail'([Node])],VarsIn,VarsOut,Term) :- !,
	node_to_term(Node,VarsIn,VarsOut,Term).
args_terms([Arg|Args],VarsIn,VarsOut,[Term|Terms]) :-
	node_to_term(Arg,VarsIn,NxtVars,Term),
	args_terms(Args,NxtVars,VarsOut,Terms).

% reduce escape sequences
/*  _esc   = '\\' ( [\\abcefnrstv'"`] 
	              / 'x' _hex+ '\\'?
	              / _octal+ '\\'?
	              / 'u' _hex*4
	              / 'U' _hex*8
	              )
*/
unescape_([],_Q,[]).
unescape_(['\\',x,H|Chars],Q,[Esc|MChars]) :-
	hex_value(H,V), !,  % at least 1
	hex_char(-1,Chars,V,Esc,NxtChars),
	unescape_(NxtChars,Q,MChars).
unescape_(['\\',u|Chars],Q,[Esc|MChars]) :-
	hex_char(4,Chars,0,Esc,NxtChars), !,
	unescape_(NxtChars,Q,MChars).
unescape_(['\\','U'|Chars],Q,[Esc|MChars]) :-
	hex_char(8,Chars,0,Esc,NxtChars), !,
	unescape_(NxtChars,Q,MChars).
unescape_(['\\',c|Chars],Q,MChars) :-
	skip_layout_(Chars,NxtChars), !,
	unescape_(NxtChars,Q,MChars).
unescape_(['\\',CEsc|Chars],Q,[Esc|MChars]) :-
	plg_escape_(CEsc,Esc), !,
	unescape_(Chars,Q,MChars).
unescape_(['\\',O|Chars],Q,[Esc|MChars]) :-
	octal_value(O,V), !,  % at least 1
	octal_char(Chars,V,Esc,NxtChars),
	unescape_(NxtChars,Q,MChars).
unescape_([Q,Q|Chars],Q,[Q|MChars]) :- !,  % double quotes
	unescape_(Chars,Q,MChars). 
unescape_([Char|Chars],Q,[Char|MChars]) :-
	Char \= '\\',
	unescape_(Chars,Q,MChars).

plg_escape_('\\','\\').
plg_escape_('a','\a').
plg_escape_('b','\b').
plg_escape_('e','\e').
plg_escape_('f','\f').
plg_escape_('n','\n').
plg_escape_('r','\r').
plg_escape_('s','\s').
plg_escape_('t','\t').
plg_escape_('v','v').
plg_escape_('\'','\'').
plg_escape_('"','"').
plg_escape_('`','`').

hex_char(0,Cs,Acc,Char,Cs) :- !, 
	char_code(Char,Acc).
hex_char(Count,[C|Cs],Acc,Char,Etc) :-
	hex_value(C,V), !,
	NxtAcc is Acc*16+V,
	NxtCount is Count-1,
	hex_char(NxtCount,Cs,NxtAcc,Char,Etc).
hex_char(Count,[C|Cs],Acc,Char,Etc) :-
	Count < 0,
	char_code(Char,Acc),
	(C = '\\' -> Etc = Cs ; Etc = [C|Cs]).  % optional terminating /

hex_value(C,V) :- char_type(C,digit(V)) -> true ; char_type(C,xdigit(V)).

octal_char([C|Cs],Acc,Char,Etc) :-
	octal_value(C,V), !,
	NxtAcc is Acc*8+V,
	octal_char(Cs,NxtAcc,Char,Etc).
octal_char([C|Cs],Acc,Char,Etc) :-
	char_code(Char,Acc),
	(C = '\\' -> Etc = Cs ; Etc = [C|Cs]).  % optional terminating /

octal_value(C,V) :- char_type(C,digit(V)), 0 =< V, V =< 7.

skip_layout_([Char|Chars],MChars) :-
	char_type(Char,space) -> skip_layout_(Chars,MChars) ; MChars = [Char|Chars].

% expression utility: flatten expr, arg and elem's to a list of op definitions and values
% PrefixOp, InfixOp, and PostfixOp mapped to op/3 term; all others left as is
% Note 'op' nodes in ptree are removed so no conflict with operand node values.
flatten_exp_([],List,List).
flatten_exp_([E|Ex],LIn/[Op|Ts],LOut) :- 
	opDef(E,Op), !, 
	flatten_exp_(Ex,LIn/Ts,LOut).
flatten_exp_([arg(Sub)|Ex],LIn,LOut) :- !,
	flatten_exp_([expr(Sub)|Ex],LIn,LOut).
flatten_exp_([elem(Sub)|Ex],LIn,LOut) :- !,
	flatten_exp_([expr(Sub)|Ex],LIn,LOut).
flatten_exp_([expr(Sub)|Ex],LIn,LOut) :- !,
	flatten_exp_(Sub,LIn,LNxt),
	flatten_exp_(Ex,LNxt,LOut).
flatten_exp_([E|Ex],LIn/[E|Ts],LOut) :-
	flatten_exp_(Ex,LIn/Ts,LOut).

opDef('PrefixOp'([OpTerm]),op(P,A,OpVal)) :-
	term_op(OpTerm,Op,OpVal),
	current_op(P,A,Op),
	sub_atom(A,0,1,1,'f').  % f_ 
opDef('InfixOp'([OpTerm]),op(P,A,OpVal)) :-
	term_op(OpTerm,Op,OpVal),
	current_op(P,A,Op),
	sub_atom(A,1,1,1,'f').  % _f_  
opDef('PostfixOp'([OpTerm]),op(P,A,OpVal)) :-
	term_op(OpTerm,Op,OpVal),
	current_op(P,A,Op),
	sub_atom(A,1,1,0,'f').  % _f

term_op(op(SOp),Op,Op) :- !,
	node_to_term(atom(SOp), _, _, Op).  % convert string value to atom
term_op('List'(Arg),[],[](Arg)) :- !. 
term_op('Curly'(Arg),{},{}(Arg)). 

% expression utility: build a term from flattened expression
% use cuts to avoid multiple error messages on nested failure
build_term_([V], VarsIn, VarsOut, Term) :-                                  % just a value
	not_op(V),
	node_to_term(V,VarsIn,VarsOut,Term),
	!.

build_term_([op(_P,_A,Op), V], VarsIn, VarsOut, Term) :-                    % simple prefix
	!,
	build_term_([V], VarsIn, VarsNxt, Term1), 
	reduce_term_(Op, [Term1], VarsNxt, VarsOut, Term).

build_term_([V, op(_P,_A,Op)], VarsIn, VarsOut, Term) :-                    % simple postfix
	!,
	build_term_([V], VarsIn, NxtVars, Term1),
	reduce_term_(Op, [Term1], NxtVars, VarsOut, Term).

build_term_([op(P1,A1,Op1), op(P2,A2,Op2)|Etc], VarsIn, VarsOut, Term) :-   % prefix prefix
	op_associativity(op(P1,A1,Op1), op(P2,A2,Op2), right),
	!,
	build_term_([op(P2,A2,Op2)|Etc], VarsIn, VarsNxt, Term1),
	reduce_term_(Op1, [Term1], VarsNxt, VarsOut, Term).

build_term_([op(P1,A1,Op1), V, op(P2,A2,Op2)|Etc], VarsIn, VarsOut, Term) :-   % prefix infix/postfix
	not_op(V),
	op_associativity(op(P1,A1,Op1), op(P2,A2,Op2), Ass),
	!,
	(Ass = left
	 -> build_term_([op(P1,A1,Op1), V], VarsIn, NxtVars, Vterm),
	    build_term_(['$"'(Vterm), op(P2,A2,Op2)|Etc], NxtVars, VarsOut, Term)
	 ;  build_term_([V, op(P2,A2,Op2)|Etc], VarsIn, NxtVars, Vterm),
	    build_term_([op(P1,A1,Op1), '$"'(Vterm)], NxtVars, VarsOut, Term)
	).

build_term_([V, op(P1,A1,Op1), op(P2,A2,Op2)|Etc], VarsIn, VarsOut, Term) :-   % postfix postfix ; infix prefix
	not_op(V),
	op_associativity(op(P1,A1,Op1), op(P2,A2,Op2), Ass),
	!,
	(Ass = left
	 -> build_term_([V, op(P1,A1,Op1)], VarsIn, NxtVars, Vterm),
	    build_term_(['$"'(Vterm), op(P2,A2,Op2)|Etc], NxtVars, VarsOut, Term)
	 ;  build_term_([op(P2,A2,Op2)|Etc], VarsIn, NxtVars, Vterm),
	    build_term_([V, op(P1,A1,Op1), '$"'(Vterm)], NxtVars, VarsOut, Term)
	).

build_term_([V1, op(_P1,_A1,Op1), V2], VarsIn, VarsOut, Term) :-            % simple infix
	!,
	build_term_([V1],VarsIn,NxtVars1,Term1),
	build_term_([V2],NxtVars1,NxtVars2,Term2), 
	reduce_term_(Op1, [Term1,Term2], NxtVars2, VarsOut, Term).

build_term_([V1, op(P1,A1,Op1), V2, op(P2,A2,Op2)|Etc], VarsIn, VarsOut, Term) :-  % infix, infix/postfix
	op_associativity(op(P1,A1,Op1), op(P2,A2,Op2), Ass),
	!,
	(Ass = left
	 -> build_term_([V1, op(P1,A1,Op1), V2], VarsIn, NxtVars, Vterm),
	    build_term_(['$"'(Vterm), op(P2,A2,Op2)|Etc], NxtVars, VarsOut, Term)
	 ;  build_term_([V2, op(P2,A2,Op2)|Etc], VarsIn, NxtVars, Vterm),
	    build_term_([V1, op(P1,A1,Op1), '$"'(Vterm)], NxtVars, VarsOut, Term)
	).

build_term_(Exp, VarsIn, _VarsOut, _Term) :-
	print_message(informational, prolog_parser(op_conflict(Exp,VarsIn))),
	fail.


prolog:message(prolog_parser(op_conflict(Exp,VarsIn))) -->  % DCG
	{format_exp_(Exp, VarsIn, VarList, FExp, VFormat),
	 name_vars_(VarList),
	 term_singletons(FExp,Singles), name_singles_(Singles),
	 atomics_to_string(["prolog_parser Error, operator clash in:"|VFormat]," ",Format)
	},
	[ Format - FExp ].

reduce_term_([](Arg), Terms, VarsIn, VarsOut, Term) :-
	args_terms(Arg,VarsIn,VarsOut,OpArg),
	Term =.. [[], OpArg |Terms].
reduce_term_({}([Arg]), Terms, VarsIn, VarsOut, Term) :-
	node_to_term(Arg,VarsIn,VarsOut,OpArg),
	Term =.. [{}, {OpArg} |Terms].
reduce_term_(Op, Terms, VarsIn, VarsIn, Term) :- atomic(Op),  % includes []
	Term =.. [Op|Terms].

not_op(V) :- \+functor(V,op,_).

format_exp_([], VarsIn, VarsIn, [], []).
format_exp_([op(P,A,Op)|Exp], VarsIn, VarsOut, [op(P,A,Op)|FExp], ["~p"|VFormat]) :- !,
	format_exp_(Exp, VarsIn, VarsOut, FExp, VFormat).
format_exp_([V|Exp], VarsIn, VarsOut, [S|FExp], ["~p"|VFormat]) :-
	node_to_term(V,VarsIn,NxtVars,S),
	format_exp_(Exp, NxtVars, VarsOut, FExp, VFormat).

% generate left/right associativity for two operators	
op_associativity(op(P1,_,_), op(P2,_,_), left):-
	P1 < P2, !.
op_associativity(op(P1,_,_), op(P2,_,_), right):-
	P1 > P2, !.
op_associativity(op(P,A1,_), op(P,A2,_), Ass) :- !,
	op_associativityEq(A1,A2,Ass).  % equal precedence values

% precedence values equal, use associativity values	
% Note: missing cases fail (operator clash)

% Pattern: prefix prefix X
op_associativityEq(fx,fx,right).
op_associativityEq(fy,fx,right).
op_associativityEq(fy,fy,right).

% Pattern: prefix X postfix
op_associativityEq(fx,yf,left).
op_associativityEq(fy,xf,right).
op_associativityEq(fy,yf,left).

% Pattern: prefix X infix ..
op_associativityEq(fx,yfx,left).
op_associativityEq(fy,xfx,right).
op_associativityEq(fy,xfy,right).
op_associativityEq(fy,yfx,left).

% Pattern: X infix Y infix ..
op_associativityEq(xfx,yfx,left).
op_associativityEq(xfy,xfx,right).
op_associativityEq(xfy,xfy,right).
op_associativityEq(xfy,yfx,left).
op_associativityEq(yfx,yfx,left).

% Pattern: infix prefix Y ..
op_associativityEq(xfy,fx,right).
op_associativityEq(xfx,fy,right).
op_associativityEq(xfy,fy,right).

% Pattern: postfix infix ..
op_associativityEq(xf,yfx,left).
op_associativityEq(yf,yfx,left).

% Pattern: infix X postfix
op_associativityEq(xfx,yf,left).
op_associativityEq(xfy,xf,right).
op_associativityEq(xfy,yf,left).
op_associativityEq(yfx,yf,left).

% Pattern: postfix postfix
op_associativityEq(xf,yf,left).
op_associativityEq(yf,yf,left).
