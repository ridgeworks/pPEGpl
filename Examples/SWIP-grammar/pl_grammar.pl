%
% pl_grammar == SWI-Prolog module defining SWI Prolog grammar in pPEG
%
/*	The MIT License (MIT)
 *
 *	Copyright (c) 2022,2023 Rick Workman
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
:- module(pl_grammar, [prolog_grammar/1]).

/* Usage, e.g.,
?- prolog_grammar(PG), peg_parse(PG,"X is exp(42).",R).

PG = 'Peg`([ ... ]),
R = 'Prolog'([expr([var("X"), 'InfixOp'([op("is")]), 'Compound'([atom("exp"), integer("42")])])]).

Note: you usually don't want the grammar term 'PG' in a toplevel query - it's quite verbose.
*/

prolog_grammar({|pPEG||

	Prolog = _ (expr _eox)+          # one or more expressions properly terminated
	_eox   = '.' (_ws+ / _eos)       # end of expression: '.' followed by whitespace or eos
	_eos   = !(~[])                  # end of input string
	
	expr   = PrefixOp _ (&PrefixOp expr / !InfixOp expr)
	       / term _ ( InfixOp _ expr / PostfixOp _ )*
	#   Compound term arguments and List elements are restricted subsets of 'expr'
	arg    = PrefixOp _ (&PrefixOp arg / !InfixOp arg)
	       / term _ ( ![,] (InfixOp _ arg / PostfixOp _ ))*
	elem   = PrefixOp _ (&PrefixOp elem / !InfixOp elem)
	       / term _ (![,|] (InfixOp _ elem / PostfixOp _ ))*

	term   = Pexpr         # (expr)
	       / Compound      # f(x,y)
	       / List          # [X,Y|Z]
	       / QQuote        # {|syntax||content| }  - can't have qq terminator in a comment
	       / Curly         # {expr}
	       / Dict          # tag{Key:Value, ...}
	       / var           # X
	       / string        # "xyz"
	       / number        # see below
	       / atom          # xyz
	       / bquote        # `abc`
	
	Pexpr  = '(' _ expr _ ')'
	
	Compound = functor '(' _ (arg (_ ',' _ arg)*)? _ ')'
	functor  = atom / '[' _ ']' / '{' _ '}'                # block functors can include ws               

	List   = '[' _ (elem ( _ ',' _ elem)* Tail?)? _ ']'
	Tail   = _ '|' _ elem                          # improper lists allowed
	
	QQuote = '{|' _ expr _ '||' qcontent '|''}'    # can't have qq terminator in the grammar
	qcontent = ~('|''}')*                          # anything but qq terminator
	
	Curly  = '{' _ expr? _ '}'
	
	Dict   = (var / atom) '{' _ (pair (_ ',' _ pair)*)? _ '}'
	pair   = (intkey / atom) _ ':' _ arg
	intkey = '-'? _integer
	
	var    = _var_start _id_continue
	string = '"' (~["\]+ / '""' / _esc)* '"'       # catch escapes and double "s
	number = float         # 1.3e4
	       / rational      # 1r2 (canonical version only)
	       / integer       # 123
	# Note: prefix '-' is not part of number syntax
	float  = _dig+ ('.' _dig+ _exp? / _exp) ('Inf' / 'NaN')?
	rational = _dig+ 'r' _dig+
	integer = _integer                             # add wrapper for _integer
	_integer = '0o' _octal+ 
	         / '0x' _hex+ 
	         / '0b' _bin+ 
	         / '0'['] _code 
	         / _radix 
	         / _digits
	atom   = _atom                                 # add wrapper for _atom
	bquote = '`' (~[`\\]+ / '``' / _esc)* '`'
	
	_atom  = !(_eox / '/*')                                # ISO restrictions 
	               ( [!;]                                  # single char symbols
	               / _atom_start _id_continue              # lowercase alpha
	               / _symbol                               # symbols
	               / (['] (~['\]+ / [']*2 / _esc)* ['])    # single quoted text
	               )
	
	# character definitions covering UTF-16
	_var_start   = [A-Z_\u00C0-\u00D6\u00D8-\u00DE]
	_atom_start  = [a-z\u00AA\u00B5\u00BA\u00DF-\u00F6\u00F8-\u00FF]
	_id_continue = [a-zA-Z0-9_\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]*
	_symbol      = [-+~#$&*./:<=>?@\^\u00A1-\u00A9\u00AB\u00AC\u00AE-\u00B1\u00B4\u00B6-\u00B8\u00BF\u00D7\u00F7]+

	_digits = _dig (' ' / '_' _)? _digits* 
	_exp    = [eE] [+-]? _dig+
	_dig    = [0-9]
	_octal  = [0-7]
	_hex    = [0-9a-fA-F]
	_bin    = [0-1]
	_code   = _esc / [']*2 / ~[]
	_radix  = [0-9]*1..2 ['] [0-9a-zA-Z]+
	
	_esc    = '\' ( [\abcefnrstv'"`] 
	              / 'x' _hex+ '\'?
	              / _octal+ '\'?
	              / 'u' _hex*4
	              / 'U' _hex*8
	              )
	
	# operator expressions - in each case match 'op' then 
	#   test for compatible definition using extension <testOp>
	# Note: treat unary - on number as PrefixOp, prefix followed by '(' not an op (it's a functor)
	PrefixOp  = minus &[0-9] / op <pl_grammar:testOp prefix>  !'('
	InfixOp   =                op <pl_grammar:testOp infix>
	PostfixOp =                op <pl_grammar:testOp postfix>
	
	minus     = '-'
	
	# See manual 4.25: "In SWI-Prolog, a quoted atom never acts as an operator."
	op = !['] _atom / List / Curly / ',' / '|'             # include block operators, ',' and '|'
	
	# whitespace ...
	_         = _ws*                                       # optional ws
	_ws       = [ \t-\r]+                                  # [ \t\n\v\f\r]
	          / '%' ~[\n\r]*                               # line comment
	          / _blkcmt                                    # block comment
	_blkcmt   = '/*' (~[/*] / _blkcmt / ~('*/'))* '*/'

|}).

/* To restrict "Prolog' to clauses, replace Prolog rule with:
	Prolog = " " (Clause _eox)+
	Clause = expr " " (Body / DCG / SSU)?
	Body   = "->  " expr
	DCG    = "--> " expr
	SSU    = ", "   expr " => " expr
Note: untested.
*/
:- current_module(pPEG) -> true  ; use_module(library(pPEG),[peg_lookup_previous/3]).

% extension to check to see if previous 'op' match is operator of desired class (infix, prefix, or postfix)
% precedence/associativity handled later in semantic analysis
testOp(Class,Env,_Input,PosIn,PosIn,[]) :-
	peg_lookup_previous('op',Env,SOp),  % previous string match from op rule
	sub_atom(SOp,0,1,_,C),
	string_to_op(C,SOp,Op),
	current_op(_,Assoc,Op),
	op_type(Assoc,Class),
	!.

string_to_op('[',SOp,[]) :-         % block quotes operator, e.g., X[2]
	sub_atom(SOp,_,1,0,']'), !.
string_to_op('{',SOp,{}) :-         % curly quotes operator
	sub_atom(SOp,_,1,0,'}'), !.
string_to_op(_,SOp,Op) :-           % normal atom operator
	atom_string(Op,SOp).

% largely copied from prolog_operator.pl
op_type(fx,  "prefix").
op_type(fy,  "prefix").
op_type(xfx, "infix").
op_type(xfy, "infix").
op_type(yfx, "infix").
op_type(xf,  "postfix").
op_type(yf,  "postfix").
