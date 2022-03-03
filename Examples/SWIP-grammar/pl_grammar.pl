%
% pl_grammar == SWI-Prolog module defining SWI Prolog grammar in pPEG
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
:- module(pl_grammar, [prolog_grammar/1]).

/* Usage, e.g.,
?- prolog_grammar(PG), peg_parse(PG,"X is exp(42).",R).

PG = 'Peg`([ ... ]),
R = 'Prolog'([expr([var("X"), 'InfixOp'([op("is")]), 'Compound'([atom("exp"), integer("42")])])]).

Note: you usually don't want the grammar term 'PG' in a toplevel query - it's quite verbose.
*/

prolog_grammar({|pPEG||

	Prolog = " " (expr _eox)+        # one or more expressions properly terminated
	_eox   = " ." (_ws+ / _eos)      # end of expression: '.' followed by whitespace or eos
	_eos   = !(~[])                  # end of input string
	
	expr   = PrefixOp " " (&PrefixOp expr / !InfixOp expr)
	       / term " " ( InfixOp " " expr " " / PostfixOp " " )*
	#   Compound term arguments and List elements are restricted subsets of 'expr'
	arg    = PrefixOp " " (&PrefixOp arg / !InfixOp arg)
	       / term " " ( ![,] (InfixOp " " arg  " " / PostfixOp " " ))*
	elem   = PrefixOp " " (&PrefixOp elem / !InfixOp elem)
	       / term " " (![,|] (InfixOp " " elem " " / PostfixOp " " ))*

	term   = Pexpr         # (expr)
	       / Compound      # f(x,y)
	       / List          # [X,Y|Z]
	       / QQuote        # {|syntax||content| }  - can't have qq terminator in a comment
	       / Curly         # {expr}
	       / var           # X
	       / string        # "xyz"
	       / number        # see below
	       / atom          # xyz
	       / bquote        # `abc`
	
	Pexpr  = "( " expr " )"
	
	Compound = atom "( " (arg (" , " arg)*)? " )"
	
	List   = "[ " (elem (" , " elem)* Tail?) " ]"
	Tail   = " | " elem                            # improper lists allowed
	
	QQuote = "{| " expr " ||" qcontent '|''}'      # can't have qq terminator in the grammar
	qcontent = ~('|''}')*                          # anything but qq terminator
	
	Curly  = "{ " expr " }"
	
	var    = _var_start _id_continue
	string = '"' (~["\\]+ / '""' / _esc)* '"'      # catch escapes and double "s
	number = float         # 1.3e4
	       / rational      # 1r2 (canonical version only)
	       / integer       # 123
	float  = '-'? _dig+ ('.' _dig+ _exp? / _exp) ('Inf' / 'NaN')?
	rational = _dig+ 'r' _dig+
	integer = '-'? ( '0o' _octal+ 
	               / '0x' _hex+ 
	               / '0b' _bin+ 
	               / "0'" _code 
	               / _radix 
	               / _digits
	               )
	atom   = _atom                                 # add wrapper for _atom
	bquote = '`' (~[`\\]+ / '``' / _esc)* '`'
	
	_atom  = !_eox ( [!;]                                  # single char symbols
	               / _mtList / '{' " " '}'                 # empty brackets
	               / _atom_start _id_continue              # lowercase alpha
	               / _symbol                               # symbols   
	               / ("'" (~['\\]+ / "''" / _esc)* "'")    # single quoted text
	               )
	
	# character definitions covering 8 bit ASCII
	_var_start   = [A-Z_\u00C0-\u00D6\u00D8-\u00DE]
	_atom_start  = [a-z\u00AA\u00B5\u00BA\u00DF-\u00F6\u00F8-\u00FF]
	_id_continue = [a-zA-Z0-9_\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]*
	_symbol      = [-+~#$&*./:<=>?@^\\\u00A1-\u00A9\u00AB\u00AC\u00AE-\u00B1\u00B4\u00B6-\u00B8\u00BF\u00D7\u00F7]+

	_digits = _dig (' ' / "_ ")? _digits* 
	_exp    = [eE] [+-]? _dig+
	_dig    = [0-9]
	_octal  = [0-7]
	_hex    = [0-9a-fA-F]
	_bin    = [0-1]
	_code   = _esc / ~[]
	_radix  = [0-9]*1..2 "'" [0-9a-zA-Z]+
	
	_mtList = '[' " " ']'
	_esc    = '\\' ( [\\abcefnrstv'"`] 
	               / 'x' _hex+ '\\'?
	               / _octal+ '\\'?
	               / 'u' _hex*4
	               / 'U' _hex*8
	               )
	
	# operator expressions - in each case match 'op' then 
	#   test for compatible definition using extension <testOp>
	PrefixOp  = !('-' [0-9]) op <pl_grammar:testOp prefix> # '-' before number not prefix
	InfixOp   =              op <pl_grammar:testOp infix>
	PostfixOp =              op <pl_grammar:testOp postfix>
	
	# See manual 4.25: "In SWI-Prolog, a quoted atom never acts as an operator."
	op = !"'" _atom / List / Curly / ',' / '|'             # include block operators, ',' and '|'
	
	# whitespace ...
	_space_   = _ws*                                       # optional ws
	_ws       = [ \t-\r]+                                  # [ \t\n\v\f\r]
	          / '%' ~[\n\r]*                               # line comment
	          / _blkcmt                                    # block comment
	_blkcmt  = '/*' (~[/*] / _blkcmt / ~('*/'))* '*/'

|}).

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
