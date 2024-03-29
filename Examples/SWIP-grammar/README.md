#### A pPEG formal grammar for SWI-Prolog syntax

While some implementations of Prolog include a formal specification of their syntax, SWI-Prolog does not; this directory contains such a specification. While it is difficult to know how complete and bug-free it is, but it has been tested on a fair number of example program files including the top level of the SWI-Prolog library directory and the boot directory (as of V8.4.1).

In addition to the grammar, a "parser" program is included to map the ptree result of a *pPEG* parse to Prolog terms to facilitate testing and experimentation.

#### Module `pl_grammar`

This module exports the grammar term defined by the `pPEG` specification of the SWI-Prolog syntax, so it can be used directly in a `peg_parse` query:
```
?- prolog_grammar(Plg), peg_parse(Plg,"f(X).",R).
R = 'Prolog'(['Compound'([atom("f"), var("X")])]).
```
The language that is recognized by the grammar consists of one or more Prolog terms (rule `expr`), each terminated by the traditional "full stop followed by white space" Prolog terminator.
```
	Prolog = _ (expr _eox)+          # one or more expressions properly terminated
	_eox   = _ '.' (_ws+ / _eos)     # end of expression: '.' followed by whitespace or eos
	_eos   = !(~[])                  # end of input string
	
	_         = _ws*                 # optional ws
	_ws       = [ \t-\r]+            # [ \t\n\v\f\r]
	          / '%' ~[\n\r]*         # line comment
	          / _blkcmt              # block comment
	_blkcmt   = '/*' (~[/*] / _blkcmt / ~('*/'))* '*/'

```
Note that the terms are not restricted to clausal terms, so non-programs are valid input:
```
?- prolog_grammar(Plg), peg_parse(Plg,"[H|T]. 42.",R).
R = 'Prolog'(['List'([var("H"), 'Tail'([var("T")])]), integer("42")]).
```
Currently only 8-bit characters are supported for variable names and unquoted atoms; it would be fairly straight forward, albeit tedious, to extend this to cover a larger Unicode character set.

Most of the grammar is pretty self-explanatory but operator expressions are the exception. First, Prolog operator definitions are defined in a separate "table"  which can be extended and modified dynamically. Therefore the set of operators is not defined by the grammar, as would be the case in most programming languages. Instead a *pPEG* extension is defined to test whether a particular "token" is an operator of the necessary class (`prefix`, `infix`, or `postfix`):
 ```
	# operator expressions - in each case match 'op' then 
	#   test for compatible definition using extension <testOp>
	# Note: treat unary - on number as PrefixOp, prefix followed by '(' not an op (it's a functor)
	PrefixOp  = minus &[0-9] / op <pl_grammar:testOp prefix>  !'('
	InfixOp   =                op <pl_grammar:testOp infix>
	PostfixOp =                op <pl_grammar:testOp postfix>
	
	minus     = '-'
	
	# See manual 4.25: "In SWI-Prolog, a quoted atom never acts as an operator."
	op = !['] _atom / List / Curly / ',' / '|'             # include block operators, ',' and '|'
```
The `op` rules identifies possible operators syntactically and the `testOp` extension verifies that the operator is of the desired class. Parsing does not include defining new operators, so the set of possible operators defined at the start of the parse operations. Also note that parsing does not depend on any other operator properties other than name and class; properties such as precedence and associativity are part of the semantics and will be addressed in post-parsing semantic analysis.

The second issue with Prolog operator expressions is that operators and operands can be syntactically indistinguishable. For example, "`- - - .`" could mean the term `-('-','-')` or `-(-('-'))`. So the first "`-`" could be either a prefix operator or an atom. In general, a wrong choice could affect subsequent success or failure of rules. In practice this means some look-ahead is required to ensure downstream success and, even then, ambiguity may still exist. In the "`- - - .`" example, SWI-Prolog chooses the `-(-('-'))` result, so the expression rule is:
```
	expr   = PrefixOp _ (&PrefixOp expr / !InfixOp expr)
	       / term _ ( InfixOp _ expr _ / PostfixOp _ )*
```

But this is, at best, an approximation to what the built-in parser does. In general, it's quite difficult to resolve all the possible ambiguities when atoms in an expression are also defined as operators. There appears to be no common strategy between various Prolog parsers to resolve such ambiguities. (The ISO standard flat out prohibits the use of operators as operands, but most "modern" implementations of Prolog reject that approach as being too restrictive.) Since such expressions are typically hard to comprehend, most programmers use explicit parentheses to make intent clear.  

Since expressions are limited when used in compound term arguments and lists, e.g., "`,`" expressions must be parenthesized, separate rules are required to cover those cases.

Note that this parser is not a compiler or loader so to be recognized by the parser, any operator must be defined before a parse is done. Directives, including operator definitions, are just expressions to be parsed.

Now suppose we wanted to change our grammar to add the sensible restriction (probably should have been enforced by ISO) that improper lists (lists which don't terminate with a list and do not pass the `is_list/1` test) generate parse errors. This can be easily done by changing the `Tail` rule to:
 ```
 Tail   = _ '|' _ (var / List / _mtList)          # insist on a proper list
 ```
so only a variable or a list are permitted after the vertical bar. (The intent was probably to use a comma rather than a bar.)  As it turns out, there don't appear to be a lot of code examples where this restriction bites; there are only 3 files in the `swipl library` (top level) and `boot` directories which violate this rule. (Of course, it's still possible to create one dynamically by binding a `Tail` variable to a term other than a list, but that's not a restriction a parser can enforce.)

#### Module `pl_parser`

For all but the simplest input, the *ptree* result is hard to read and just as difficult to test. So module `pl_parser` implements a simple parser using the grammar defined in `pl_grammar`. Its only exported predicate is `string_termList/2` which converts an input string defining a sequence of terms to a list of terms:
```
?- string_termList("[H|T]. 42.",TL).
TL = [[H|T], 42].
```
This parser does the first level of semantic analysis on the *ptree* and maps it to a Prolog term, with any variables bound to their names. (This binding is convenient while testing, but can removed by commenting out a couple of lines in the source where indicated.) Semantic analysis is where all the precedence/associativity rules are applied to generate any operator expression term. As you might suspect, this makes up a significant portion of the parser code. The other major function it performs is mapping if the escape syntax to designated character, e.g., "`\t`" to a tab character or "`\xA9`" to the copyright symbol.

The downside to a strategy like this is that any errors due to operator precedence/associativity violations don't reference the original source location. To partially compensate, the operators are replaced by their `op/3` definition in error messages:
```
?- string_termList("x<y<z.",TL).
% prolog_parser Error, operator clash in: x op(700,xfx,<) y op(700,xfx,<) z
false.
```
But the main objective of this parser is to assist in verifying the grammar, not to replace the builtin parser, so this shouldn't be much of an inconvenience.

#### Other Utilities

The file `parser_IO.pl` just contains a couple of file system oriented utilities to assist in testing the grammar, e.g., `parse_directory/1` just parses all the files in a named directory. These predicates are fairly trivial and have limited general usefulness.
