## pPEG API for SWI-Prolog

### Introduction

The name *pPEG* stands for portable *PEG*, and *PEG* is an acronym for "Parsing Expression Grammars", a type of grammar well suited for parsing machine-oriented syntax due to its deterministic behaviour and *O(N)* performance (*N* = length of the input to be parsed). Any context free grammar, and some context sensitive grammars, can be implemented as a *PEG* grammar, which makes *PEG* considerably more expressive than regular expressions.

*pPEG* is a dialect of *PEG* that is host language independent focused just on parsing with no support for semantic actions or predicates. The result of a parsing operation is a generic structure called a *ptree* which can be realized in any general purpose programming language with strings and arrays (or lists). Any semantic processing must be done using a *ptree* instance as input.

Unlike many grammar support systems that generate source code for a target language which gets included in application source, *pPEG*, like regular expressions, uses a direct execution model. The source for a *pPEG* grammar is just a string in the host language which is "compiled" at runtime to a grammar object. This grammar object can then be used to parse input text to produce an instance of a *ptree*. (Analogous operations in the SWI-Prolog regular expression [`library(pcre)`][swip-pcre] would be `re_compile/3` and `re_matchsub/4`.)

While the source of a *pPEG* grammar is language independent, the concrete structure of *ptree*'s and the functions (or predicates) used to compile grammars and use them to parse text, are specified in a language dependent API. The focus of this document is the *pPEG* API implemented in the pack/module `pPEG` for SWI-Prolog. For information on how to write *pPEG* grammars in general, see [pPEG repo][pPEGrepo]. (For numerous examples of other language grammars specified in ANTLR, a popular grammar system (not *PEG* based), see [ANTLR Examples](https://github.com/antlr/grammars-v4)).

### `pPEG` Grammar Source

The source for a `pPEG` grammar is a Prolog string, usually a literal string. For anything other than small grammars (1 or 2 rules), the most convenient way to define a grammar is using a quasi-quoted string ([`library(strings)`][swip-libstrings]). For example here's a fact defining a grammar for CSV data ([RFC 4180](https://www.ietf.org/rfc/rfc4180.txt)):
```
csv_grammar({|string||
    CSV     = Hdr Row+
    Hdr     = Row
    Row     = field (',' field)* '\r'? '\n'
    field   = _string / _text / ''

    _text   = ~[,\n\r]+
    _string = '"' (~["] / '""')* '"'
|}).
```
Since quasi-quoted strings are "raw" strings, there's no need to escape the quotes and other control characters so the grammar is much more readable (and writeable).

For reference, here's the grammar for *pPEG* in *pPEG*:
```
peg_grammar({|string||
    Peg   = _ rule+ _
    rule  = id _ '=' _ alt
 
    alt   = seq ('/'_ seq)*
    seq   = rep*
    rep   = pre sfx? _
    pre   = pfx? term
    term  = call / sq / chs / group / extn
 
    group = '('_ alt ')'
    pfx   = [&!~]
    sfx   = [+?] / '*' range?
    range = num (dots num?)?
    num   = [0-9]+
    dots  = '..'
 
    call  = id _ !'='
    id    = [a-zA-Z_] [a-zA-Z0-9_-]*
    sq    = ['] ~[']* ['] 'i'?
    chs   = '[' ~']'* ']'
    extn  = '<' ~'>'* '>'
    _     = ('#' ~[\n\r]* / [ \t\n\r]+)*

|}).
```
(Aside: This defintion is actually used when the module `pPEG` initializes to create the parser used in compiling user defined grammars.)

A third option for defining a *pPEG* grammar in Prolog is to use a quasi-quotation with syntax `pPEG`. In this case, the quasi-quotation is replaced by a `pPEG` grammar term and no explicit compile step is required (more on this below).

For parsing the contents of files, convert the contents to a string using [`read_file_to_string/3`][swip-readfile].

### The Prolog *ptree* Definition

As stated earlier, the result of a *pPEG* parse operation is a *ptree*. A Prolog *ptree* is an arity 1 compound term whose functor is the name of the rule that generated it and the argument is either a Prolog `string` (corresponding to a terminal node in the tree) or a `list` of *ptree*'s (corresponding to the children of a non-terminal node). There are more examples below but using the `csv_grammar` above to parse some test data:
```
data_csv({|string||
Details,Month,Amount
Mid Bonus,June,"2,000"
,January,"""zippo"""
Total Bonuses,"","5,000"
|}).
```
the resulting Prolog *ptree* is:
```
'CSV'([
    'Hdr'(['Row'([field("Details"),field("Month"),field("Amount")])]),
    'Row'([field("Mid Bonus"),field("June"),field("\"2,000\"")]),
    'Row'([field(""),field("January"),field("\"\"\"zippo\"\"\"")]),
    'Row'([field("Total Bonuses"),field("\"\""),field("\"5,000\"")])
])
```
It may help to use the `ptree_printstring` predicate in module `library(pPEGutilities)` to output a "pretty printed" form of the *ptree*:
```
?- csv_grammar(CSV), peg_compile(CSV, CSVg), data_csv(Data), peg_parse(CSVg, Data, CSVTree), ptree_printstring(CSVTree,PS),write(PS).
CSV
├─Hdr
│ └─Row
│   ├─field "Details"
│   ├─field "Month"
│   └─field "Amount"
├─Row
│ ├─field "Mid Bonus"
│ ├─field "June"
│ └─field "\"2,000\""
├─Row
│ ├─field ""
│ ├─field "January"
│ └─field "\"\"\"zippo\"\"\""
└─Row
  ├─field "Total Bonuses"
  ├─field "\"\""
  └─field "\"5,000\""
```
Note that much of the detailed syntax, e.g., the literal punctuation, has been removed through judicious use of *pPEG* rule name conventions, simplifying the job performed in subsequent processing steps. The grammar not only defines the language it recognizes but also the detailed structure of the *ptree*'s that are generated by successful parsing operations. Conversely, the rule name conventions do not affect the language that is recognized, only the form of the *ptree* result.

The process of applying the rule name (the left hand side of a rule) to some value representing the syntactic content (defined by the right hand side of the rule) might be considered to be a form of semantic labelling. This label directs the semantic processing done post-parsing to the appropriate code for the value in question. For example, a '`number`' rule might be processed by a predicate using `number_string/2` to convert the string value returned by the parser to a numeric value. Using this model, semantic actions supported by some formal grammar systems, are moved out of the grammar specification to the semantic analysis phase but are linked together using rule names interpreted as semantic labels. Other semantic attributes, e.g., operator precedence, may be inferred by adapting (grammar specific) rule name syntax conventions. (See [Operator Expressions] below.)

A *ptree* in JSON format is a common convention used in other *pPEG* implementations. The Prolog *ptree* described above can be mapped to/from a JSON term that can be used with `library(http/json)` using the predicate `ptree_json_term/2` in module `pPEGutilities` (assumes  "new", rather than "classic", JSON term format). This could be used to import/export *ptree*'s between different implementations although that is not expected to be a common requirement.

### Compiling *pPEG* Grammars

"Compiling" maps the source for a *pPEG* grammar into a parser that recognizes text input in the language specified by the grammar. This is done using the predicate `peg_compile/2` which takes as arguments a string containing the grammar source, and a grammar term to be used in subsequent parsing operations. (`peg_compile/3` takes an additional argument specifying a list of options as discussed below.) If the grammar term is a variable, it is unified with the grammar term produced by compiling the source. If it is an atom, it will be interpreted as a name that can subsequently be used to lookup such a grammar term when required by the parser; the name `pPEG` is reserved for the `pPEG` grammar, which is used to compile other grammars. (It's stored  by the `pPEG` module in a global variable as a side effect of the compile.) Here are the two options using the CSV grammar previously defined:
```
﻿?-  csv_grammar(CSV), peg_compile(CSV,CSVG).
CSV = "CSV     = Hdr Row+\nHdr     = Row\nRow     = field (',' field)* '\\r'? '\\n'\nfield   = _string / _text / ''\n\n_text   = ~[,\\n\\r]+\n_string = '\"' (~[\"] / '\"\"')* '\"'\n",
CSVG = 'Peg'([rule('CSV', seq([call_O(_A), rep_O(call_O(_B), 1, -1)])), rule('Hdr', call_O(_B)), rule('Row', seq([call_O(_C), rep_O(seq([sq_O(exact, ","), call_O(_C)]), 0, -1), rep_O(sq_O(exact, "\r"), 0, 1), sq_O(exact, "\n")])), rule(field, alt([call_O(_D), call_O(_E), sq_O(exact, "")])), rule('_text', rep_O(chs_O(notin, [',', '\n', '\r']), 1, -1)), rule('_string', seq([sq_O(exact, "\""), rep_O(alt([chs_O(notin, ['"']), sq_O(exact, "\"\"")]), 0, -1), sq_O(exact, "\"")]))], [_, _A, _B, _C, _E, _D]).

  
?- csv_grammar(CSV), peg_compile(CSV,csv).
CSV = "CSV     = Hdr Row+\nHdr     = Row\nRow     = field (',' field)* '\\r'? '\\n'\nfield   = _string / _text / ''\n\n_text   = ~[,\\n\\r]+\n_string = '\"' (~[\"] / '\"\"')* '\"'\n".
```
As you can see the grammar term is quite verbose, in fact it's the actual program the `pPEG` parser virtual machine executes when parsing input text. For the most part, it's simpler for applications to provide a grammar name for future reference rather than deal with managing the grammar term directly, but that's an application choice.

In unoptimised form, the grammar term includes the *ptree* result of applying the `pPEG` grammar to the grammar source. However, by default (as shown above), the grammar is optimised to maximize runtime performance. `peg_compile`'s `optimise` option can be used to explicitly control this, but the default setting is all that's normally needed.
  
Another advantage of the optimization process is that some grammar errors will be detected resulting in "Warning" messages. These include messages for undefined and duplicate rules:
```
?-  peg_compile("rule1=s rule1='x'",G).
Warning: pPEG: duplicate rule rule1, last definition will apply
Warning: pPEG: s undefined
G = 'Peg'([rule(rule1, call_O(_)), rule(rule1, sq_O(exact, "x"))], [_, _]).
```
 Although the compile succeeds, the grammars will probably not function as intended: using an undefined rule will just fail, and the last definition of any rule will overwrite previous definitions. However, note that any errors in parsing the grammar will still result in failure. 

A grammar defined in a `pPEG` quasi-quotation does not have to be explicitly compiled since the builtin Prolog parser will automatically invoke the `pPEG` compiler and replace the quasi-quotation with the result. Any arguments will be treated as compiler options, e.g.,:
```
﻿?- G={|pPEG||rule = 'x'+ 'y'|}.
G = 'Peg'([rule(rule, seq([rep_O(sq_O(exact, "x"), 1, -1), sq_O(exact, "y")]))], [_]).
  
﻿?- G={|pPEG(optimise(false))||rule = 'x'+ 'y' rule|}.
G = 'Peg'([rule([id("rule"), seq([rep([sq("'x'"), sfx("+")]), sq("'y'"), id("rule")])])], _).
  
?- peg_parse({|pPEG||xs_then_y = 'x'+ 'y'|},"xxxy",R).
R = xs_then_y("xxxy").
```
The last example uses a "literal" grammar to parse some input text; this is covered in more detail in the next section.

### Parsing With Compiled *pPEG* Grammars

Compiled grammars are used to recognize and parse text, in the language defined by the grammar, using the predicates `peg_parse/3` and `peg_parse/5`, the latter providing additional parsing options described below. As you might expect, `peg_parse` takes a compiled grammar term, input text as a string, and produces a *ptree* result. The "pretty printed" CSV example (assumes grammar named `csv` has already been compiled and input text is defined by `data_csv/1`) :
```
﻿?- data_csv(Input), peg_parse(csv,Input,Tree), ptree_printstring(Tree,"  ",PS), write(PS).
  CSV
  ├─Hdr
  │ └─Row
  │   ├─field "Details"
  │   ├─field "Month"
  │   └─field "Amount"
  ├─Row
  │ ├─field "Mid Bonus"
  │ ├─field "June"
  │ └─field "\"2,000\""
  ├─Row
  │ ├─field ""
  │ ├─field "January"
  │ └─field "\"\"\"zippo\"\"\""
  └─Row
    ├─field "Total Bonuses"
    ├─field "\"\""
    └─field "\"5,000\""
% ... top level bindings
```
CSV is a pretty forgiving grammar but here's an example of invalid input:
```
?- peg_parse(csv,"\"\"\"",CSVtree).
% pPEG Error: _string failed, expected '"' at line 1.3:
%   1 | """
%         ^
false.
```
First note that, given input text that is not recognized by the grammar, `peg_parse` fails (as is appropriate in a logic programming language). But it does so in a "noisy" way by generating an "informational" message (see [`print_message/2`][swip-print_message])  which hopefully provides a clue as to where and why the failure occurred.

The application can choose to modify this behaviour. First, the informational messages can be suppressed by setting the `verbose` Prolog environment flag to `silent` or by specifying the option `verbose(silent)` in the `peg_parse/5` option list.

A second option is to map the failure to an exception. This can be done fairly cheaply by just throwing the exception as an alternative when `peg_parse` fails but any diagnostic information about the cause of the failure is lost. A more expensive option that preserves this information is to use [message_hook/3][swip-message_hook] to intercept the message before it's output and use the error information in constructing the exception, as in this code example:
```
:- multifile user:message_hook/3.

message_hook(peg(ErrorInfo), informational, [Form - Data]) :-
    ErrorInfo =.. [errorinfo|_],	% filter for errorInfo messages
    format(string(Msg),Form,Data),  % write the error information to a string
    throw(error(syntax_error(''),context('',Msg))).
```
`peg_parse/3` will also fail if the parse succeeds but fails to consume all the input text, as in this simple example:
```
?- peg_parse({|pPEG||r = 'x'*3|},"xxxx",R).
% pPEG Error: r fell short at line 1.4:
%   1 | xxxx
%          ^
false.
```
The parse succeeded after consuming 3 '`x`'s but there were 4 in the input. Now in some situations, the application needs to parse a chunk of text in pieces. An example is an `http` message composed of a header (grammar `http`) and a body (using some other grammar specified by the header). This requires the use of `peg_parse/5` with additional arguments to specify an `incomplete` parse option and the "residue" left by the parse. Using the previous example:
```
?- peg_parse({|pPEG||r = 'x'*3|},"xxxx",R,Residue,[incomplete(true)]).
R = r("xxx"),
Residue = "x".
```
There is one type of grammar bug which results in an immediate runtime "Resource Error" exception: infinite recursion.
```
?- peg_compile("r = r 'x'",G), peg_parse(G,"xxxx",R).
ERROR: Not enough resources: pPEG infinite recursion applying r
```
These may be caused by simple bugs, but a common cause is so-called left recursion. *PEG* grammars don't support left recursion (see Ford's paper) but rules of this form can be re-written using the repeat suffix operators.

Hopefully during the grammar development process, the error messages are sufficient to diagnose errors in the grammar (as opposed to grammar non-compliance in input text). But it can be quite difficult to accurately pinpoint the cause of the error in recursive descent parsers. To support that extra level of grammar debugging, most *pPEG*'s provide a trace feature, although the details may vary between implementations. Trace output can very large and detailed so some caution is advisable, but it captures exactly what the parser is doing so it can be indispensable in diagnosing obscure bugs.

The tracing feature in `pPEG` is enabled using the `tracing` option in the `peg_parse/5` option list. The argument to this option is a rule name, or a list of rule names to be traced. Tracing the previous `csv` error example:
```
?- peg_parse(csv,"\"\"\"",R,_,[tracing('CSV')]).
%    1.1   CSV
%    1.1   |  (Hdr Row+)
%    1.1   |  Hdr
%    1.1   |  |  Row
%    1.1   |  |  |  (field (',' field)* '\r'? '\n')
%    1.1   |  |  |  field
%    1.1   |  |  |  |  (_string / _text / '')
%    1.1   |  |  |  |  _string
%    1.1   |  |  |  |  |  ('"' (~["] / '""')* '"')
%    1.2   |  |  |  |  |  '"' == 	"\""
%    1.2   |  |  |  |  |  (~["] / '""')*
%    1.2   |  |  |  |  |  (~["] / '""')
%    1.2   |  |  |  |  |  ~["] != 	"\"\""
%    1.3   |  |  |  |  |  '""' == 	"\"\""
%    1.3   |  |  |  |  |  (~["] / '""')
%    1.3   |  |  |  |  |  ~["] != 	""
%    1.3   |  |  |  |  |  '""' != 	""
%    1.3   |  |  |  |  |  '"' != 	""
%    1.1   |  |  |  |  _string != 	"\"\"\""
%    1.1   |  |  |  |  _text
%    1.1   |  |  |  |  |  ~[,\n\r]+
%    1.2   |  |  |  |  |  ~[,\n\r] == 	"\""
%    1.3   |  |  |  |  |  ~[,\n\r] == 	"\""
%    1.3   |  |  |  |  |  ~[,\n\r] == 	"\""
%    1.3   |  |  |  |  |  ~[,\n\r] != 	""
%    1.3   |  |  |  |  _text == 	"\"\"\""
%    1.3   |  |  |  field => 	field("\"\"\"")
%    1.3   |  |  |  (',' field)*
%    1.3   |  |  |  (',' field)
%    1.3   |  |  |  ',' != 	""
%    1.3   |  |  |  '\r'?
%    1.3   |  |  |  '\r' != 	""
%    1.3   |  |  |  '\n' != 	""
%    1.1   |  |  Row != 	"\"\"\""
%    1.1   |  Hdr != 	"\"\"\""
%    1.1   CSV != 	"\"\"\""
% pPEG Error: _string failed, expected '"' at line 1.3:
%   1 | """
%         ^
false.
```
While this shows exactly how the parse failed, it's quite voluminous. The amount of trace output can be reduced by tracing a more specific rule (or list of rules):
```
?- peg_parse(csv,"\"\"\"",R,_,[tracing('_string')]).
%    1.1   _string
%    1.1   |  ('"' (~["] / '""')* '"')
%    1.2   |  '"' == 	"\""
%    1.2   |  (~["] / '""')*
%    1.2   |  (~["] / '""')
%    1.2   |  ~["] != 	"\"\""
%    1.3   |  '""' == 	"\"\""
%    1.3   |  (~["] / '""')
%    1.3   |  ~["] != 	""
%    1.3   |  '""' != 	""
%    1.3   |  '"' != 	""
%    1.1   _string != 	"\"\"\""
% pPEG Error: _string failed, expected '"' at line 1.3:
%   1 | """
%         ^
false.
```

### Implementing `pPEG` Extensions

The standard definition of *pPEG* includes an extension mechanism enabling additional functionality when necessary, e.g., to implement some context sensitive grammars within a *pPEG* framework. It should be noted that, in general, extensions are not portable between *pPEG* implementations so they should be used with discretion.

The *pPEG* extension syntax allows extensions to be defined in angle brackets containing  arbitrary syntax that doesn't itself contain an angle bracket. In `pPEG` (a Prolog implementation of *pPEG*) the text between angle brackets is interpreted as follows:
- if the text contains spaces, everything up to the first space specifies a predicate, including an optional module specifier. Everything after the space up to the closing '`>`' is trimmed of spaces and becomes the first argument in the predicate call. Five additional arguments are supplied by the parser as documented below, and the arity 6 predicate is then called.
- if there are no spaces in the text, the entire content is assumed to be the name of an arity 6 predicate and the first argument will be the empty string.
- If the above is not true, e.g., no such predicate, the text is interpreted as the content of an "information" message and printed using [`print_message/2`][swip-print_message]. (Note that "information" messages will be printed regardless of the `verbose` setting, although they can be intercepted using [`message_hook/3`][swip-message_hook].)

Module qualification on predicate names is supported.

For the first option, the implementation of the extension predicate takes five arguments in addition to any arguments specified in the extension. In order these include:
- *+Env* - the environment term for the parse, should generally be treated as an opaque term and ignored.
- *+Input* - a string containing the text being parsed
- *+PosIn* - a positive integer containing the current position in `Input` of the text being parsed
- *-PosOut* - a positive integer `>= PosIn` containing the position in `Input` after executing the extension predicate.
- *-Result* - any result (normally a *ptree* or `[]`) the extension wishes to define

Here's an example of an extension "named" `???`which just invokes the SWI-Prolog debugger:
```
???("",_Env,_Input,PosIn,PosIn,[]) :- trace.
```
Note that the cursor position is unchanged and the result is `[]` (will be ignored). So:
```
?- peg_parse({|pPEG||r = <???> 'x'*|},"xx",R).
^  Exit: (17) catch(pPEG:call(???(""), @([rule(r, seq([extn(???(...)), rep_O(..., ..., ...)]))], [], r, 1,  (([]), ([]), ([]))), "xx", 0, 0, []), _16038, pPEG:extn_error(_16038, ???(""), @([rule(r, seq([extn(???(...)), rep_O(..., ..., ...)]))], [], r, 1,  (([]), ([]), ([]))), "xx", 0, 0, [])) ? no debug
R = r("xx").
```
Unless you're debugging `pPEG` itself this isn't a very useful extension, so here's a more practical example. In addition to extending the domain of grammars that *pPEG* can express, extensions can be used to improve performance. SWI-Prolog provides a regular expression library ([`library(pcre)`][swip-pcre]) which provides an interface to a low level implementation in C. Provided enough can be done in C to overcome any cost in navigating the built-in primitive interface, performance improvements can be had. Here's an extension which takes a regular expression and recognizes  matching text in the `Input` (the `pPEG` pack contains the module `library(rexp_pPEGxt)` implementing this extension):
```
:- module(rexp_pPEGxt, [re_match/6]).
  
:- use_module(library(pcre),[re_matchsub/4]).
  
re_match(RExp,_Env,Input,PosIn,PosOut,[]) :-
    string_length(Input,ILen), PosIn < ILen,  % guard against domain error
    re_matchsub(RExp,Input,Sub,[start(PosIn),anchored(true)]),  % pcre caches compiled RE's
    string_length(Sub.0,Len),  % length of matched string (0th entry of dict Sub)
    PosOut is PosIn+Len.       % move cursor
```
Now:
```
?- peg_parse({|pPEG||num = <re_match ((-?[1-9][0-9]*)|(-?0))([.][0-9]+)?([eE][+-]?[0-9]+)? >|},"12.34e56",R).
R = num("12.34e56").
 
?- peg_parse({|pPEG||num = <re_match ((-?[1-9][0-9]*)|(-?0))([.][0-9]+)?([eE][+-]?[0-9]+)? >|},"fred",R).
% pPEG Error: num.num failed, expected num.num at line 1.1:
%   1 | fred
%       ^
false.
```
This particular example isn't likely to buy much in terms of performance (number strings aren't that long) but one can imagine other scenarios where the gains might be significant, e.g., long strings or a custom whitespace rule processing text with long comments. In any case, the intent of this section is not to encourage the use of extensions for portability reasons, but demonstrate how they can be implemented when necessary.

### Operator Expressions in *pPEG*

(See also [Operator Expressions][pPEG_OpExps].) Most programming languages support operator expressions like `1+2*3` which are normally parsed as a sequence of operands (`1`, `2`, and `3`) and operators (`+` and `*`). But a problem arises with the semantics of such an expression; does it mean `1+(2*3)` or `(1+2)*3`? A grammar can be defined to (syntactically) insist on the use of parentheses to avoid the problem but this significantly impacts readability (and writeability). Alternatively, the grammar rules can be be used explicitly parse the expression as in the following simple grammar:
```
term   = factor (addop factor)*
factor = value (mulop value)*
addop  = [+-]
mulop  = [*/]
value  = [0-9]+
```
Each "precedence level" has it's own rules and the rules are ordered by the calling sequence, e.g., `term` calls `factor` so `mulop` has a higher precedence (binds tighter) than `addop`. This works pretty well for a small number of such levels but becomes a bit unwieldy  when the number of levels exceeds 3 or 4. But note that any solution essentially pushes semantic issues into the parsing process which, ideally, should only be concerned with syntax. (I.e., shouldn't the issue of whether `1+2*3` means `1+(2*3)` or `(1+2)*3` really be handled by "post-parsing" semantic analysis?)

Many parsers support the use of precedence and associativity properties of an operator (originally proposed by [Pratt, 1973](https://tdop.github.io/)). Operators of higher precedence "bind" their operands tighter than those of lower precedence. When precedences are equal, associativity  (left or right) is used to break the tie. Unfortunately these operator properties are usually defined in a separate table so they get decoupled from the actual grammar specification. In a sense, this isn't a terrible thing  (since operator precedence is really about semantics). On the other hand, we've also seen how to define a grammar with the precedence "baked in" (using multiple rule order to define precedence).

While assigning precedence properties to operators with an external table decouples precedence from the grammar, it's hard to see how to do otherwise with *pPEG*, which is designed to be just a syntactic parser with semantic analysis performed in a subsequent step. But viewing the rule name as a semantic label (see *ptree* discussion above) raises an interesting possibility. Can the rule name be used to convey operator precedence?

A simple scheme which does this is to define a rule name suffix which can be interpreted as a precedence/associativity pair, for example `_2L` for precedence 2 with left associativity, or `_5R` for precedence 5 with right associativity. Given a rule such as `addop_2L = [+-]`, the *ptree* result will carry the precedence information along with the actual operator in the expression, e.g., `addop_2l("+")`. Then subsequent semantic analysis, using standard techniques (like the Pratt parsing algorithm) can use this information without needing a separate table; the grammar defines the values in the rule names. At the same time, this is just a naming convention, so nothing need be added to the *pPEG* parser to support this functionality. Note that prefix operators should always have an asociativity value of `R` and postfix a value of `L`.

Here's a concrete example of a very simple expression grammar using this approach:
```
eg_expr_grammar({|pPEG||
	pratt_expr = pre? val (op val)* post?
	op     = _ (op_2L / op_3L / op_4R) _
	pre    = _ pop_5R _
	post   = _ opp_5L _
	val    = [0-9]+
	op_2L  = [-+]
	op_3L  = [*/]
	op_4R  = '^'
	pop_5R = [-+~]
	opp_5L = '++' / '--'
	_      = [ \t\n\r]*   # optional whitespace
|}).
```
The *ptree* result of the rule `pratt_expr` is a sequence of values and operators, e.g.,:
```
?- parse_eg_expr("1+2*3+4",Tree).
﻿Tree = pratt_expr([val("1"), op_2L("+"), val("2"), op_3L("*"), val("3"), op_2L("+"), val("4")]).
 
?- parse_eg_expr("-2*3++",Tree).
﻿Tree = pratt_expr([pop_5R("-"), val("2"), op_3L("*"), val("3"), opp_5L("++")]).
```
Module `pPEGutilities` exports a predicate that will reformat the *ptree* to a "Pratt tree" which enforces the precedence defined by the rule names and replaces the operator rule names with their symbol:
```
?- parse_eg_expr("1+2*3+4",Tree), ptree_pratt(Tree,Pratt).
﻿Tree = pratt_expr([val("1"), op_2L("+"), val("2"), op_3L("*"), val("3"), op_2L("+"), val("4")]),
Pratt = +[+[val("1"), *([val("2"), val("3")])], val("4")].
 
?- parse_eg_expr("-2*3++",Tree), ptree_pratt(Tree,Pratt).
﻿Tree = pratt_expr([pop_5R("-"), val("2"), op_3L("*"), val("3"), opp_5L("++")]),
Pratt = *([-[val("2")], ++([val("3")])]).
``` 
It's fairly straight forward to write a predicate to map the Pratt tree to a compatible Prolog arithmetic term suitable for evaluation:
```
eg_pratt_term((val(S), N) :- !,            % terminal value
	number_string(N,S).
eg_pratt_term(Func, Exp) :-                % any operator node
	Func =.. [F,Args],
	eg_pratt_termList(Args,Args1),
	Exp =.. [F|Args1].
  
eg_pratt_termList([],[]).
eg_pratt_termList([A|Args],[A1|Args1]) :-
	eg_pratt_term(A,A1),
	eg_pratt_termList(Args,Args1).
```
Examples:
```
?- parse_eg_expr("1+2*3+4",Tree), ptree_pratt(Tree,Pratt), eg_pratt_term(Pratt,Exp), Result is Exp.
﻿Tree = pratt_expr([val("1"), op_2L("+"), val("2"), op_3L("*"), val("3"), op_2L("+"), val("4")]),
Pratt = +[+[val("1"), *([val("2"), val("3")])], val("4")],
Exp = 1+2*3+4,
Result = 11.
 
?- parse_eg_expr("-2*3+4",Tree), ptree_pratt(Tree,Pratt), eg_pratt_term(Pratt,Exp), Result is Exp.
﻿Tree = pratt_expr([pop_5R("-"), val("2"), op_3L("*"), val("3"), op_2L("+"), val("4")]),
Pratt = +[*([-[val("2")], val("3")]), val("4")],
Exp = - 2*3+4,
Result = -2.
```
(Arithmetic expressions with postfix operators can't be evaluated until the arithmetic functions '`++`' and '`--`' are defined - see `library(arithmetic)`.)

A more elaborate example is in the `Examples/Expression_grammars` directory. The general message here is to show how semantic labelling can be used with a purely syntactic parser to support the common problem of operator precedence in expressions, without relying on a separate operator definition table - the precedence information is encoded in the grammar along with the associated syntax. 

A final note: the precedence value `P` in the suffix '`_PA`' can be any character supported by the syntax of the rule name in a *pPEG* grammar; `[0-9] < [A-Z] < [_] < [a-z]`, i.e., the precedence order is equivalent to the ASCII code order of the precedence character value, which provides more than enough levels for any useful expression grammar.

### Using *pPEG* for (some) Context Sensitive Grammars

(See also [Context Sensitive Grammars][pPEG_CSGs].) In general, *PEG*'s cannot be used to express Context Sensitive Grammars (*CSG*'s). The *PEG* "`&`" and "`!`" operators provide unbounded lookahead, and so can express some grammars which cannot be expressed using a Context Free Grammar and may be useful in some circumstances. And sometimes context sensitivity issues can be postponed to the semantic analysis phase. A common example of a problem that can exploit this technique is matching begin and end tags in XML. As long as the parser can recognize the begin and end tag syntax, semantic analysis can enforce the requirement that they match for any given element. But this technique does not work when the parsing process itself is affected, e.g., matching begin and end quotes in a Markdown back-tick quoted string.

The Markdown example is typical of the need to use a *CSG* when parsing machine oriented languages. The underlying requirement is the need to match the same string matched earlier in the parsing process. This can't be expressed in a *PEG* but we can define an extension to support this functionality.

The extension `<@ name>` exactly matches the current input text with the previous match by rule `name`. If `name` has not previously matched anything, the extension matches nothing, i.e., `""`. If there is a previous match, but it doesn't match the current input text it fails. The result of the `@` extension is equivalent to matching the literal containing the same text.

The implementation of the `@` extension can be found in `library(csg_pPEGxt)`:
```
@(Name,Env,Input,PosIn,PosOut,[]) :-
    (peg_lookup_previous(Name,Env,Match)
     -> sub_string(Input,PosIn,Len,_,Match),             % same as previous result
        PosOut is PosIn+Len
     ;  PosOut = PosIn                                   % no previous, match nothing
    ).
```
Note: `peg_lookup_previous/3` is a public interface predicate of `pPEG` which can be used to build other custom *CSG* extensions when "exact" matching doesn't fit the need.

Although you can use semantic analysis to enforce XML tag matching, here's a "tiny" XML grammar which does this:
```
xmlelem_grammar({|string||
    elem    = '<' tag '>' content '</' <@ tag> '>'
    content = (text / elem)*
    tag     = [a-zA-Z]+
    text    = ~[<]+
|}).
```
After compiling this to `xelm`:
```
?- peg_parse(xelm, "<div> abc <p>par</p></div>", R).
R = elem([tag("div"), content([text(" abc "), elem([tag("p"), text("par")])])]).
  
?- peg_parse(xelm, "<div> abc <p>par</f></div>", R).
% pPEG Error: elem.elem failed, expected <@ tag> at line 1.19:
%   1 | <div> abc <p>par</f></div>
%                         ^
false.
```
Using `﻿ptree_printstring`:
```
﻿?- peg_parse(xelm, "<div> abc <p>par</p></div>", R), ptree_printstring(R,PS),write(PS).
elem
├─tag "div"
└─content
  ├─text " abc "
  └─elem
    ├─tag "p"
    └─text "par"
  ...
```
A slightly more complicated example which, like the previously mentioned Markdown example, does require a *CSG* is [Rust raw strings](https://doc.rust-lang.org/rust-by-example/std/str.html). A corresponding "tiny" grammar:
```
rustRaw_grammar({|string||
    Raw   = 'r' _fence '"' raw '"' <@ _fence>
    raw   = ~('"' <@ _fence>)*
    _fence = '#'*
|}).
```
Anonymous rules are used to remove `_fences` from the output. When compiled to `rraw`:
```
?- peg_parse(rraw,"r##\"raw \"#string\"##",R).
R = 'Raw'([raw("raw \"#string")]).
```
Note that the "`\`" escapes are required for the Prolog parser and are not part of the input text.

A considerably more complicated example that can be handled with the same extension is indent matching, such as you might find in parsing Python (see [Python indentation](https://en.wikipedia.org/wiki/Python_syntax_and_semantics#Indentation)). A "tiny" grammar using just space characters for indentation:
```
pyBlock_grammar({|string||
    py     = line Blk
    Blk    = &(<@ _inset> ' ') _inset line (<@ _inset> !' ' line / Blk)*
    _inset  = ' '+
    line   = ~[\n\r]* '\r'? '\n'
|}).
```
This is a good time to point out the scoping nature of an `@` reference. It must refer to a previously called rule either in the same rule or in a rule which called that rule directly or indirectly, i.e., a (dynamic) ancestor, and matching is only attempted against the first such instance encountered. In this particular example, the first time `Blk` is called, there is no previously defined `_inset` so it succeeds, matching nothing (see the above definition of the `@` extension). Then `_inset` is matched so references in the following repeat loop will attempt to match that result. If that fails, `Blk` is called recursively, but now there is an existing match string for the first `_inset` that can be used to ensure that a new `_inset` value is longer.

Some example test data:
```
py_test1({|string||
 block 1 start
 line B11
  block 2 start
 line B12
  block 3 start
   block 4 start
 line B13
|}).
```
The "pretty printed" parse result:
```

?- py_test1(T), peg_parse(pyb,T,R), ptree_printstring(R,PS), write(PS).
py
├─line " \n"
└─Blk
  ├─line "block 1 start\n"
  ├─line "line B11\n"
  ├─Blk
  │ └─line "block 2 start\n"
  ├─line "line B12\n"
  ├─Blk
  │ ├─line "block 3 start\n"
  │ └─Blk
  │   └─line "block 4 start\n"
  └─line "line B13\n"
 ...
```
Hopefully these few examples demonstrate how a generic `@` extension can address many of the machine oriented languages where *CSG*'s may be necessary. In cases where an exact match isn't what's needed, e.g., matching multiple brackets, a custom extension is fairly easy to construct using the supplied `peg_lookup_previous/3` predicate.

### `pPEG` Reference

#### Interface and Dependencies (from source):
```
:- module(pPEG,[            % module pPEG exports:
	 peg_compile/2,         % create a grammar from a source string
	 peg_compile/3,         % as above with option list
	 peg_parse/3,           % use a pPEG grammar to parse an Input string to a ptree Result
	 peg_parse/5,           % as above with unmatched residue and option list
	 peg_grammar/1,         % pPEG grammar source
	 peg_lookup_previous/3, % used by CSG extensions to lookup previous matches
	 pPEG/4                 % quasi-quotation hook for pPEG
	]).
  
:- use_module(library(strings),[string/4]).         % for quasi-quoted strings
:- use_module(library(debug)).                      % for tracing (see peg_trace/0)
:- use_module(library(option),[option/3]).          % for option list processing
:- use_module(library(pcre),[re_matchsub/4]).       % uses a regular expression for error & trace output
:- use_module(library(quasi_quotations), [          % pPEG as quasi-quotation
    quasi_quotation_syntax/1, 
    with_quasi_quotation_input/3
]).
```
#### peg_compile(*+String, ?Grammar*)
Succeeds if *String* is valid *pPEG* source text and can be compiled to *Grammar*. *Grammar* is either a variable which will be unified with the grammar term usable when calling `peg_parse`, or is the name of the grammar (an atom) which will be used to retrieve the grammar by `peg_parse`. A `peg_compile` failure will be accompanied by an `informational` message, and is normally the result of invalid *pPEG* source. In addition, `peg_compile` may succeed but output `warning` messages indicating anomalies in the grammar which may cause unexpected results when the grammar is used in parsing. These warnings should be addressed while the grammar is in development.

Grammars defined in `pPEG` quasi-quotations are compiled at load time; in effect the quasi-quotation is replaced by the compiled grammar, so they do not need to be explicitly compiled. Standard default options apply (see `peg_compile/3`).

By default, the grammar term contains an optimized version of the grammar (not a *ptree*); this can be overridden using the `optimise` option (see below).

Note: Named grammars are kept in SWI-Prolog [global variables][swip-gvar]. This has a couple of practical implications:
- a named grammar must be re-compiled in each thread that uses them (each thread has it's own global stack, i.e., it's own copy of global variables).
- when module `pPEG` is reloaded any existing named grammars will be lost.

#### peg_compile(*+String, ?Grammar, +OptionList*)
Semantics are the same as `peg_compile/2` with an additional list of options with each option of the form `Name(Value)`, (i.e., arity 1). The only option specific to the compile predicate is:
- `optimize(Opt)` - If `Opt = true` (default = `true`), an optimized grammar with be generated; otherwise the (acyclic) *ptree* form defined by the *pPEG* grammar will be generated. Either form can be used for parsing but the optimized version is typically 2-3 times faster.

Since the compile operation largely consists of parsing the source text using the *pPEG* grammar, any other `peg_parse` options provided (see below) will also be in effect.

#### peg_parse(*+Grammar, +String, -PTree*)
Succeeds if *String* can be entirely parsed using *Grammar* and the resulting *ptree* unifies with *PTree*. Otherwise it fails, or throws a resource error if parsing detects an infinite recursion.

A failure is accompanied by an `informational` message (see [`print_message/2`][swip-print_message]) pointing at the location of the failure. If the failure is due to "falling short" (not consuming the entire *String*), the failure message will point to the location of the unparsed test.

#### peg_parse(*+Grammar, +String, -PTree, -String, +OptionList*)
Semantics are the same as `peg_parse/3` with additional arguments specifying the "residue" left from an "incomplete" parse (argument 4) and an *OptionList*. If the `incomplete` parsing option is `true` (see options below), the fourth argument will be unified with a string containing the unparsed content os the input string.

The *OptionList* has the same format as that used in `peg_compile/3`; the following options can be used to override the `peg_parse` defaults:
- `incomplete(Bool)` - if `Bool = true` (default = `false`), parsing does not fail if the entire input string is not parsed. The unparsed substring of the input is unified with the "residue" string.
- `verbose(Verb)` - if `Verb = normal` (default defined as  `current_prolog_flag(verbose,Verb)`), `informational` messages will be generated on failure.
- `tracing(Rules)` - `Rules` is a rule name (an atom), or list of rule names, which will be traced during the parse operation (default = `[]`). Unrecognized rule names are ignored.

#### peg_grammar(*-String*)
Succeeds when *String* unifies with the string defining the *pPEG* grammar. Externally this is largely for documentation purposes, but it specifies the actual grammar used by `peg_compile` to produce grammar terms.

#### peg_lookup_previous(*?Name,+Env,-String*)
*Name* is a rule name (an atom or a string) or a variable; if a variable, it will be unified with the most recent successful rule name as a string. *Env* is the environment argument provided by the second argument in an extension call. `peg_lookup_previous/3` succeeds unifying *String* with the string previously matched by rule *Name*; otherwise it fails.

`peg_lookup_previous/3` is used to support extensions which require "look-behind" to match a previous match, e.g., an indentation or an opening bracket sequence.

#### pPEG(*+Content,+Args,+Binding,-Grammar*)
Implements  the  quasi  quotation  syntax `pPEG`; only invoked by the builtin Prolog parser. This predicate compiles the *Content* to *Grammar* using options *Args*. *Binding* is currently not used.
 
[`pPEGpl` repository](https://github.com/ridgeworks/pPEGpl)

[pPEGrepo]: https://github.com/pcanz/pPEG
[pPEG_CSGs]: https://github.com/pcanz/pPEG/blob/master/docs/context-sensitive-grammars.md
[pPEG_OpExps]: https://github.com/pcanz/pPEG/blob/master/docs/operator-expressions.md

[swip-readfile]: https://www.swi-prolog.org/pldoc/doc_for?object=read_file_to_string/3
[swip-print_message]: https://www.swi-prolog.org/pldoc/doc_for?object=print_message/2
[swip-message_hook]: https://www.swi-prolog.org/pldoc/doc_for?object=message_hook/3
[swip-callable]: https://www.swi-prolog.org/pldoc/doc_for?object=callable/1
[swip-libstrings]: https://www.swi-prolog.org/pldoc/man?section=strings
[swip-pcre]: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pcre.html%27)
[swip-gvar]: https://www.swi-prolog.org/pldoc/man?section=gvar
