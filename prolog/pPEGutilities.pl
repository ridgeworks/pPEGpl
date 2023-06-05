%
% pPEGutilities == SWI-Prolog module containing pPEG supporting functions
%
/*	The MIT License (MIT)
 *
 *	Copyright (c) 2021-23 Rick Workman
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
:- module(pPEGutilities,[
	ptree_json_term/2,   % map a PTree to/from a "new" JSON term
	ptree_pratt/2,       % map a pTree to a pratt tree using op rule bindings convention
	ptree_printstring/2, % pretty print string for a ptree
	ptree_printstring/3  % pretty print string for a ptree specifying a left margin indent.
]).

:-set_prolog_flag(optimise,true).  % mainly optimizes arithmetic (module scope only)

%
% Many pPEG implementations specify a ptree using a JSON format. ptree_json_term/2 in
%	conjunction with library(http/json) can be used to covert the Prolog representation
%	of a ptree to the "new" JSON format. On input, string values must be left as strings
%	using the "value_string_as(string)" option if necessary.
%
ptree_json_term(PTree,[Name, Value]) :-
	(string(Name)
	 -> atom_string(PName,Name),    % JSON  -> PTree
	    PTree =.. [PName, PVal]
	 ;  PTree =.. [PName, PVal],    % PTree -> JSON
	    atom_string(PName,Name)
	),
	ptree_val_json_value(PVal,Value).

ptree_val_json_value(Value,Value) :-
	string(Value), !.
ptree_val_json_value([], []) :- !.  % ! when first arg a var
ptree_val_json_value([PNode|PNodes], [JNode|Jnodes]) :-
	ptree_json_term(PNode, JNode),
	ptree_val_json_value(PNodes, Jnodes).

%
% ptree_pratt/2 maps a ptree produced using a grammar with Pratt rule naming conventions
% to a "pratt tree", by applying precedence values to the operators and replacing the
% original node names with the operator symbols. A simple example:
% 
% ?- pratt_parse_expr("1+2*3",Tree),ptree_pratt(Tree,Pratt).
% Tree = expr([number("1"), addOp_3L("+"), expr([number("2"), mulOp_5L("*"), number("3")])]),
% Pratt = +[number("1"), *([number("2"), number("3")])].
%
% Any sub-expression nodes, i.e., nodes with the same name as the ptree root, are 
% similarly converted:
%
% ?- pratt_parse_expr("(1+2)*3",Tree),ptree_pratt(Tree,Pratt).
% Tree = expr(['Pexpr'([expr([number("1"), addOp_3L("+"), number("2")])]), mulOp_5L("*"), number("3")]),
% Pratt = *(['Pexpr'([+[number("1"), number("2")]]), number("3")]).
%
% The algorithm requires operator rule names containing a Pratt suffix of the form "_PA"
% where 'P' character usually in the range '0-9' but can be any  character permitted
% in rule names, and 'A' is the associativity, "L" or "R". Examples:
%
%	addOp_2L   = [-+]
%	mulOp_3L   = [*/]
%	expOp_4R   = '^'
%
% Precedence values for characters: [0-9] < [A-Z] < '_' < [a-z]
%

ptree_pratt(Tree, Pratt) :-
	Tree =.. [PFunc,Args],         % separate rule functor from list of children
	(string(Args)                  % tree with value string is already a pratt tree
	 -> Pratt = Tree
	 ;  pratt_flatten_(Args,PFunc,T/T,List/[]),
	    pratt_(List,[Pratt])       % make pratt tree
	).

pratt_flatten_([],_PFunc,List,List).
pratt_flatten_([Op|Ex],PFunc,LIn/[Pratt_op|Ts],LOut) :-   % substitute pratt operator definitions
	pratt_op(Op,Pratt_op), !, 
	pratt_flatten_(Ex,PFunc,LIn/Ts,LOut).
pratt_flatten_([Tree|Ex],PFunc,LIn,LOut) :-               % sub-expr nodes get flattened
	Tree =.. [PFunc,Args], !,	
	pratt_flatten_(Args,PFunc,LIn,LNxt),
	pratt_flatten_(Ex,PFunc,LNxt,LOut).
pratt_flatten_([ValIn|Ex],PFunc,LIn/[ValOut|Ts],LOut) :-  % non pratt expression, check arguments
	ValIn  =.. [F,ArgsIn],
	(string(ArgsIn)
	 -> ValOut = ValIn
	 ;  pratt_args_(ArgsIn,PFunc,ArgsOut),
	    ValOut =.. [F,ArgsOut]
	),
	pratt_flatten_(Ex,PFunc,LIn/Ts,LOut).

pratt_args_([],_PFunc,[]).
pratt_args_([Arg|ArgsIn],PFunc,[Arg|ArgsOut]) :-
	atomic(Arg), !,
	pratt_args_(ArgsIn,PFunc,ArgsOut).	
pratt_args_([ArgIn|ArgsIn],PFunc,[ArgOut|ArgsOut]) :-
	ArgIn  =.. [F,FArgsIn],
	(F = PFunc
	 -> ptree_pratt(ArgIn,ArgOut)                          % pratt expression
	 ;  pratt_args_(FArgsIn,PFunc,FArgsOut),               % non-pratt, check arguments
	    ArgOut =.. [F,FArgsOut]
	),
	pratt_args_(ArgsIn,PFunc,ArgsOut).

% apply precedence rules to token list of operators and operands
pratt_(['$pratt_op'(OpSym,_,_), V], [Term]) :-                % simple prefix
	not_op(V),
	!,
	Term =.. [OpSym,[V]].

pratt_([V, '$pratt_op'(OpSym,_,_)], [Term]) :-                % simple postfix
	not_op(V),
	!,
	Term =.. [OpSym,[V]].

pratt_([V1, '$pratt_op'(OpSym,_,_), V2], [Term]) :-           % simple infix
	not_op(V1), not_op(V2),
	!,
	Term =.. [OpSym,[V1,V2]].

pratt_(['$pratt_op'(OpSym1,OpL1,OpR1), '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-     % prefix prefix
	OpL2 > OpR1,  % must associate to right
	!,
	pratt_right_(['$pratt_op'(OpSym2,OpL2,OpR2) |Etc], RHS),
	pratt_(['$pratt_op'(OpSym1,OpL1,OpR1) |RHS], Term).

pratt_([V, '$pratt_op'(OpSym1,OpL1,OpR1), '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-  % postfix postfix ; infix prefix
	not_op(V),
	!,
	(OpL2 > OpR1
	 -> pratt_right_(['$pratt_op'(OpSym2,OpL2,OpR2) |Etc], RHS),
	    pratt_([V, '$pratt_op'(OpSym1,OpL1,OpR1) |RHS], Term)
	 ;  pratt_([V, '$pratt_op'(OpSym1,OpL1,OpR1)], [LHS]),
	    pratt_([LHS, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term)
	).

pratt_(['$pratt_op'(OpSym1,OpL1,OpR1), V, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-  % prefix infix / postfix postfix
	not_op(V),
	!,
	(OpL2 > OpR1
	 -> pratt_right_([V, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc],RHS),
	    pratt_(['$pratt_op'(OpSym1,OpL1,OpR1) |RHS], Term)
	 ;  pratt_(['$pratt_op'(OpSym1,OpL1,OpR1), V], [LHS]),
	    pratt_([LHS, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term)
	).

pratt_([V1, '$pratt_op'(OpSym1,OpL1,OpR1), V2, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-  % infix (infix/postfix)
	not_op(V1),              % V2 check unnecessary (already handled above)
	!,
	(OpL2 > OpR1
	 -> pratt_right_([V2, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], RHS),
	    pratt_([V1, '$pratt_op'(OpSym1,OpL1,OpR1) |RHS], Term)
	 ;  pratt_([V1, '$pratt_op'(OpSym1,OpL1,OpR1), V2], [LHS]),
	    pratt_([LHS, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term)
	).

pratt_(Exp, _Term) :-
	print_message(informational, prolog_parser(op_conflict(Exp))),
	fail.

% build RHS recursively as far as possible, then return value and rest of tokens
pratt_right_(['$pratt_op'(OpSym1,OpL1,OpR1), '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-
	!,
	OpL2 > OpR1,  % must associate to right
	pratt_right_(['$pratt_op'(OpSym2,OpL2,OpR2) |Etc], RHS),
	Term = ['$pratt_op'(OpSym1,OpL1,OpR1) |RHS].

pratt_right_(['$pratt_op'(OpSym1,OpL1,OpR1), V, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-
	!,
	(OpL2 > OpR1
	 -> pratt_right_([V, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], RHS),
	    Term = ['$pratt_op'(OpSym1,OpL1,OpR1) |RHS]
	 ;  pratt_(['$pratt_op'(OpSym1,OpL1,OpR1), V], [LHS]),
	    Term = [LHS,'$pratt_op'(OpSym2,OpL2,OpR2) |Etc]
	).

pratt_right_([V, '$pratt_op'(OpSym1,OpL1,OpR1), '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-
	!,
	(OpL2 > OpR1
	 -> pratt_right_(['$pratt_op'(OpSym2,OpL2,OpR2) |Etc], RHS),
	    Term = [V, '$pratt_op'(OpSym1,OpL1,OpR1) |RHS]
	 ;  pratt_([V, '$pratt_op'(OpSym1,OpL1,OpR1)], [LHS]),
	    Term = [LHS,'$pratt_op'(OpSym2,OpL2,OpR2) |Etc]
	).

pratt_right_([V1, '$pratt_op'(OpSym1,OpL1,OpR1), V2, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], Term) :-
	!,
	(OpL2 > OpR1
	 -> pratt_right_([V2, '$pratt_op'(OpSym2,OpL2,OpR2) |Etc], RHS),
	    Term = [V1, '$pratt_op'(OpSym1,OpL1,OpR1) |RHS]
	 ;  pratt_([V1, '$pratt_op'(OpSym1,OpL1,OpR1), V2], [LHS]),
	    Term = [LHS,'$pratt_op'(OpSym2,OpL2,OpR2) |Etc]
	).

pratt_right_(Exp, Term) :- pratt_(Exp, Term).


prolog:message(prolog_parser(op_conflict(Exp))) -->  % DCG
	['Error, operator clash in: ~p\n' - [Exp]].

not_op(V) :- \+functor(V,'$pratt_op',_).

% extract binding value pair from suffix of Op functor; suffix matches '_PA'
% Op = OpRule(OpSym)
pratt_op(Op, '$pratt_op'(OpSym,OpL,OpR)) :-
	Op =.. [OpRule,SOp], 
	sub_atom(OpRule,_,3,0,PSfx),                 % P code = precedence value
	atom_codes(PSfx,[95,P,A]),
	OpL is P*2,                                  % multiply code by 2 to leave a gap
	(A = 76 -> OpR is OpL+1                      % A = 'L'
	;A = 82 -> OpR is OpL-1                      % A = 'R'
	),                                           % ; fail
	atom_string(OpSym,SOp).                      % atom form for functor, string(SOp) implied


%
% ptree_printstring/N maps any ptree to a "pretty printed" string.
% 
% ptree_printstring/3 permits the use of the `Indent` argument, a string 
% forming the prefix of all lines. 
% ptree_printstring/2 defines a default prefix of ""
% An example:
/*
?- peg_parse(pPEG,"r1 = [a-z] [A-zA-z]*",Tree),ptree_printstring(Tree,"\t",S),write(S).
	Peg
	└─rule
	  ├─id "r1"
	  └─seq
	    ├─chs "[a-z]"
	    └─rep
	      ├─chs "[A-zA-z]"
	      └─sfx "*"
Tree = 'Peg'([rule([id("r1"), seq([chs("[a-z]"), rep([chs("[A-zA-z]"), sfx("*")])])])]),
S = "\tPeg\n\t└─rule\n\t  ├─id \"r1\"\n\t  └─seq\n\t    ├─chs \"[a-z]\"\n\t    └─rep\n\t      ├─chs \"[A-zA-z]\"\n\t      └─sfx \"*\"\n".
*/
ptree_printstring(PTree, PPstring) :-               % Arity 2
	ptree_printstring(PTree, "", PPstring).         % null indent
	
ptree_printstring(PTree, Indent, PPstring) :-       % Arity 3
	ptree_printstring(PTree, [Indent], T/T, PPlist/[]),
	atomics_to_string(PPlist, PPstring).            % build final string result
	
ptree_printstring(PTree, Indent, StrIn, StrOut) :-  % Arity 4
	PTree =.. [Name, Val],                          % decompose to name and value
	ptree_printstring_(Name, Val, Indent, StrIn, StrOut).

ptree_printstring_(Name, AString, Indent, Str/Tail, Str/Etc) :-   % ptree value: string
	string(AString), !,
	format(string(OString),"~p",[AString]),  % quoted and escaped
	indent_term(Indent,[Name," ",OString,"\n"|Etc],Tail).
ptree_printstring_(Name, Children, Indent, Str/Tail, Str/Etc) :-  % ptree value: [children..]
	%  is_list(Children),
	indent_term(Indent,[Name,"\n"|Nxt],Tail),
	new_indent(Indent,NxtIndent),
	ptree_children(Children,NxtIndent,Str/Nxt,Str/Etc).     % add children
	
ptree_children([], _, Str, Str).                            % EOL, No child case
ptree_children([Term], [_|Indent], Str/Nxt, Str/Etc) :- !,  % EOL -> different prefix
	left_crn(LC),
	ptree_printstring(Term, [LC|Indent], Str/Nxt, Str/Etc).
ptree_children([Term|Terms], Indent, Str/Nxt1, Str/Etc) :- 
	ptree_printstring(Term, Indent, Str/Nxt1, Str/Nxt2),
	ptree_children(Terms, Indent, Str/Nxt2, Str/Etc).

indent_term([],Tail,Tail).
indent_term([I|Is],In,Tail) :-
	indent_term(Is,[I|In],Tail).

new_indent([LT|Indent], New) :- left_tee(LT), !,  %  "|-" ==> "| |-"
	vertical(Vrt), New = [LT,Vrt|Indent].
new_indent([LC|Indent], New) :- left_crn(LC), !,  %  "+-" ==> "  |-"
	left_tee(LT), space(SP), New = [LT,SP|Indent].
new_indent(Indent,[LT|Indent]) :-                 %  Any ==> Any "|-"  
	left_tee(LT).

left_tee("\u251C\u2500").  % "|-"
left_crn("\u2514\u2500").  % "+-"
vertical("\u2502 ").       % "| "
space("  ").               % "  "
