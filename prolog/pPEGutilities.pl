%
% pPEGutilities == SWI-Prolog module containing pPEG supporting functions
%
/*	The MIT License (MIT)
 *
 *	Copyright (c) 2021 Rick Workman
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
	pratt/2  % transform token list to nested expression using op rule bindings convention
]).
%
% The objective of pratt/2 is to reduce a list of alternating operators and values
% representing an expression to a single tree using operator precedence values.
% The tree's non-terminal nodes are the operator' symbols and the terminal nodes are the values.
% Operator recedence information is derived from the rule names acting as semantic tokens.
%
% The algorithm requires 1) a grammar producing alternating operator and value "tokens", and
% rulenames containing a Pratt suffix of the form "_PA" where 'P' is a character, usually 
% in the range '0-9' but can be any character permitted in rule names, and 'A' is the 
% associativity, "L" or "R". An example grammar rule and operator definition:
%		pratt_expr = pre? val (op val)* post?
%		op_2L      = [+-]
% Precedence values for characters: [0-9] < [A-Z] < '_' < [a-z]
%
% pratt/2 has 3 subcases: 
%	list starts with variable-operator pair
%	list starts with operator-value pair
%	list contains a single value
pratt([V1],V1) :- !.                                  % [value]
pratt([V1,O1|Exp],E) :-                               % [value, operator, ...]
	op_def(O1,OpL1,OpR1,OpSym1), !,
	pratt_v_op(Exp,V1,OpL1,OpR1,OpSym1,E).     	
pratt([O1,V1|Exp],E) :-                               % [operator, value, ...]
	op_def(O1,_OpL1,OpR1,OpSym1),                     % O1 is a 'nud'
	(Exp = [O2|Exp2]                                         
	 -> op_def(O2,OpL2,OpR2,OpSym2),                  % operator -> value, enter standard loop 
	    (OpR1 > OpL2                                  % is rbp(Op1) > lbp(Op2) ?
	     -> E1 =.. [OpSym1,[V1]],                     % left binding
            pratt_v_op(Exp2,E1,OpL2,OpR2,OpSym2,E)
	     ;  pratt_v_op(Exp2,V1,OpL2,OpR2,OpSym2,E1),  % right binding
	        E  =.. [OpSym1,[E1]]
	    ) 
	 ;  E =.. [OpSym1,[V1]]                           % Exp = [], single prefix
	).
%  V1,Op1, ... cases, next child is a value or nothing 
pratt_v_op([],V1,_OpL1,_OpR1,OpSym1,E) :- !,          % last postfix
	E =.. [OpSym1,[V1]].
pratt_v_op([V2],V1,_OpL1,_OpR1,OpSym1,E) :-  !,       % last infix
	E=..[OpSym1,[V1,V2]].
pratt_v_op([V2,O2|Exp],V1,_OpL1,OpR1,OpSym1,E) :-     % infix
	op_def(O2,OpL2,OpR2,OpSym2), !,
	(OpR1 > OpL2                                      % is rbp(Op1) > lbp(Op2) ?
	 -> E1 =.. [OpSym1,[V1,V2]],                      % left binding
        pratt_v_op(Exp,E1,OpL2,OpR2,OpSym2,E)
	 ;  pratt_v_op(Exp,V2,OpL2,OpR2,OpSym2,E1),       % right binding
	    E  =.. [OpSym1,[V1,E1]]
	).
% extract binding value pair from suffix of Op rule name; syntax matches '_PA'
% Op symbol is value of argument
op_def(Op,OpL,OpR,OpSym) :-
	Op =.. [OpRule,SOp], 
	sub_atom(OpRule,_,3,0,PSfx),                 % P code = precedence value
	atom_codes(PSfx,[95,OpL,A]),
	(A = 76 -> OpR is OpL+1                      % A = 'L'
	;A = 82 -> OpR is OpL-1                      % A = 'R'
	),                                           % ; fail
	atom_string(OpSym,SOp).                      % atom form for functor, string(SOp) implied
