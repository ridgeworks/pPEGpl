## Pack `pPEG` for SWI-Prolog

#### What is *PEG*?

*PEG* is an acronym for "Parsing Expression Grammar" described in Bryan Ford's original 2004 paper ["Parsing Expression Grammars: A Recognition-Based Syntactic Foundation"](https://bford.info/pub/lang/peg.pdf). From the abstract:

>	*For decades we have been using Chomsky’s generative system of grammars, particularly context-free grammars (CFGs) and regular expressions (REs), to express the syntax of programming languages and protocols. The power of generative grammars to express ambiguity is crucial to their original purpose of modelling natural languages, but this very power makes it unnecessarily difficult both to express and to parse machine-oriented languages using CFGs. Parsing Expression Grammars (PEGs) provide an alternative, recognition-based formal foundation for describing machine-oriented syntax, which solves the ambiguity problem by not introducing ambiguity in the first place. Where CFGs express nondeterministic choice between alternatives, PEGs instead use prioritized choice. PEGs address frequently felt expressiveness limitations of CFGs and REs, simplifying syntax definitions and making it unnecessary to separate their lexical and hierarchical components. A linear-time parser can be built for any PEG, avoiding both the complexity and fickleness of LR parsers and the inefficiency of generalized CFG parsing.*

So the main difference between *PEG*s and *CFG*s is that *PEG*s do not support alternative "matches" on backtracking, hence the deterministic, linear time parsing behaviour of *PEG* parsers. This has two main implications as follows.

First, the "OR" operation ("`|`" in most CFGs) is replaced by the *PEG* infix operator "`/`" signifying a committed choice, i.e., once a choice is made, no backtracking is triggered on later failure to try any alternative choices. This means the order of the expressions in a PEG choice expression is significant. (Sound familiar?)

Secondly, repeat operators like "`*`", "`+`" and "`?`" are greedy (match the longest possible string) and shorter strings are not matched on backtracking. One consequence of this is that repeat expressions may require a "look-ahead guard" to prevent overly greedy matches. *PEG* provides the look-ahead prefix operators "`&`" and "`!`" for this purpose (among others).

#### What is *pPEG*?

[*pPEG*][pPEGrepo] is a member of the *PEG* family with the following attributes:
- portable. A grammar written in the "pure" subset of *pPEG* (no extensions) is language and implementation independent. The design intent is that *pPEG* is relatively easy to implement in any general purpose programming language. (See [A pPEG Virtual Machine](https://github.com/pcanz/pPEG/blob/master/docs/pPEG-machine.md)).

- simple. A *pPEG* grammar only recognizes the syntax of a string producing a generic tree data structure (defined by the language specific API) called a *ptree* which is representable in pretty much any programming language using just arrays (or lists) and strings. Application dependent semantic analysis is performed as a subsequent step using the generated trees as input.

- directly executable. The source of a *pPEG* grammar is a string which is "compiled" to a *pPEG* grammar "object" which is used directly to parse input strings in the language specified by the grammar. This is analogous to how regular expressions are generally used but different from most formal grammar systems which generate source code for a parser in a specific target language.

- "scannerless". The input to the parser is a sequence of characters (a string) rather than a sequence of tokens produced by a lexer. Processing of insignificant whitespace, which is one of the more valuable functions commonly performed by a lexer, is supported by a feature extension to the core (i.e., Ford's) [*PEG*](https://bford.info/pub/lang/peg.pdf) specification.

- extendable. While all context free grammars and some context sensitive grammars (using '`&`' and '`!`' prefix operators) can be implemented in "pure" *pPEG*, other grammars may require the use of extensions which are hooks into the underlying programming language. The syntax and semantics of particular extensions may be implementation dependent and generally compromise portability.

For the *pPEG* specification see [*pPEG*][pPEGrepo]. An online "dingus" based on the Javascript implementation can be found [here](https://pcanz.github.io/pPEGjs/dingus.html).

#### A *pPEG* Pack for SWI-Prolog

`pPEG` is an add-on pack for SWI-Prolog implemented entirely in Prolog which complements the parsing capabilities of regular expressions (`library(pcre)`) and DCG's already available to the SWI-Prolog community. It consists of  module (`pPEG`) with three core interface predicates: `peg_compile`, for constructing a parser term from the grammar source (analogous to `pcre:re_compile`), `peg_parse`, for parsing a input string in the language specified by the parser term (analogous to `pcre:re_matchsub`), and `peg_grammar` for accessing the `pPEG` source grammar, largely for documentation/specification purposes. (A second module, `pPEGutilities`, contains miscellaneous predicates that facilitate processing the results of a `peg_parse` operation and modules `csg_pPEGxt` and `rexp_pPEGxt` contain useful extensions.) The remainder of this section is an overview of how to use `pPEG` in Prolog applications; for the nitty-gritty details see the [`pPEG` API reference][pPEGref].

The easiest way to define a grammar is with a quasi-quoted string with syntax '`pPEG`'. A simple grammar to recognize 1 or more '`x`'s followed by a '`y`':
```
?- G={|pPEG||xs_y = 'x'+ 'y'|}.
G = 'Peg'([rule(xs_y, seq([rep_O(sq_O(exact, "x"), 1, -1), sq_O(exact, "y")]))], []).
```
When this is parsed by the Prolog system, the result is a grammar term that can then be used to parse text content in the language defined by the grammar (see below). Any arguments to the "Syntax" term, e.g., `pPEG(optimise(true))`, are treated as compile options.

Another option is to define the grammar in a quasi-quoted "raw" string using  `library(strings)` and then use `peg_compile/3`; this generates the same grammar term as using the `pPEG` quasi-quoted syntax.

And the last option is just to use a Prolog string, but you have to be cognizant of of the necessary character escapes.

Here's two ways to express a [*pPEG*][pPEGrepo] grammar for a quasi-quoted string, first as a `pPEG` grammar (no explicit compile required) and than as a quasi-quoted string which will require a call to `peg_compile` to create the grammar term:

		qqs_grammar({|pPEG||
			qq_string = '{|string||' rawstring _eos
			rawstring = ~_eos*
			_eos      = '|' '}'
		|}).

		qqs_grammar_def({|string||
			qq_string = '{|string||' rawstring _eos
			rawstring = ~_eos*
			_eos      = '|' '}'
		|}).

(Note that a quasi-quoted string cannot contain the substring "`|}`", hence the use of two literals in the `_eos` rule.) Using `peg_compile/2` can be used to compile `qqs_grammar_def` to produce a grammar term equivalent to `qqs_grammar`:

		?- qqs_grammar_def(QS), peg_compile(QS,qqs).
		QS = "qq_string = '{|string||' rawstring _eos\nrawstring = ~_eos*\n_eos      = '|' '}'\n".

In this example, the actual grammar term is stored internally by `pPEG` along with its designated name '`qqs`' (an atom). This name can be used to parse Prolog strings in the "`qqs`" language:

		?- peg_parse(qqs,"{|string|| A quasi-quoted string in a normal Prolog string.\\u00A9 |}",QQSTree).
		QQSTree = rawstring(" A quasi-quoted string in a normal Prolog string.\\u00A9 ").

or using the pre-compiled `pPEG` quasi-quoted defintion:

		?- qqs_grammar(QQG), peg_parse(QQG,"{|string|| A quasi-quoted string in a normal Prolog string.\\u00A9 |}",QQSTree).
		QQSTree = rawstring(" A quasi-quoted string in a normal Prolog string.\\u00A9 ").

The result of the parse unifies `QQSTree` with the *ptree* generated by the parsing operation. A Prolog *ptree* is a compound term with its functor being the rule name that matched the input and whose single argument is either a string (for a terminal node in the tree) or a list of *ptree*s (representing the children of a non-terminal node). In this trivial example, there is just a single `rawstring` terminal node containing the raw (unescaped) string. To convert it to the equivalent Prolog string with "`\\u00A9`" replaced by the copyright character requires additional "semantic processing". Perhaps the simplest way of doing this is to just use [`read_term_from_atom/3`][swip-readatom] to invoke the Prolog parser:

		?- peg_parse(qqs,"{|string|| A quasi-quoted string in a normal Prolog string.\\u00A9 |}",rawstring(R)),
		atomics_to_string(['"',R,'"'],P), read_term_from_atom(P,String,[]).

		R = " A quasi-quoted string in a normal Prolog string.\\u00A9 ",
		P = "\" A quasi-quoted string in a normal Prolog string.\\u00A9 \"",
		String = " A quasi-quoted string in a normal Prolog string.© ".

[swip-readatom]: https://www.swi-prolog.org/pldoc/doc_for?object=read_term_from_atom/3
This example doesn't do much that's useful other than provide a simple example of usage so let's look at a more practical example, a `pPEG` parser for JSON. Without explaining the details, here's the grammar which will be compiled and stored under the name '`json`':

		json_grammar({|string||
			json    = " " value
			Object  = "{ " pair (", " pair)* "} " / "{ } "
			pair    = string " : " value
			Array   = "[ " value (", " value)* "] " / "[ ] "
			value   = string / number / Object / Array / lit

			string  = '"' (~["\\] / _esc)* "\u0022 "              # \u0022 = '"'
			_esc    = '\\' (["\\/bfnrt] / 'u' [0-9a-fA-F]*4)
			number  = '-'? _int ('.' [0-9]+)? ([eE] [+-]? _int)? " "
			_int    = '0' / [1-9] [0-9]*
			lit     = ('true' / 'false' / 'null') " "
		|}).

This grammar has all three rule types: default rules, whose names start with a lowercase letter and whose value may be either a string or a list of child nodes, anonymous rules (names start with '`_`') which never appear in a *ptree*, and explicit rules (names start with uppercase letter) whose value is always a (perhaps empty) list of children. The grammar not only specifies the language accepted by the parser but also the form of the *ptree* that is generated. But the rule naming conventions can never change the "language" that is recognized by the grammar. This example also demonstrates the use of a space character in double quoted literals to represent any amount of insignificant whitespace.

An example input string to be parsed:

		json_test1({|string||
		    { "answer": 42,
		      "mixed": [1, 2.3, "a string", true, [4, 5]],
		      "empty": {}
		    }
		|}).
and the (hand-formatted) result of parsing:

		?- json_test1(T), peg_parse(json,T,JSONTree).
		T = " \n    { \"answer\": 42,\n      \"mixed\": [1, 2.3, \"a string\", true, [4, 5]],\n      \"empty\": {}\n    }\n",
		JSONTree =
		    'Object'([
		        pair([string("\"answer\""),
		            number("42")
		        ]),
		        pair([string("\"mixed\""),
		            'Array'([
		                number("1"),
		                number("2.3"),
		                string("\"a string\""),
		                lit("true"),
		                'Array'([
		                    number("4"),
		                    number("5")
		                ])
		            ])
		        ]),
		        pair([string("\"empty\""),
		        'Object'([])
		        ])
		    ]).
Note that all the terminal values are strings which may, or may not, be perfectly acceptable for the next step in the application. But writing a *ptree* walker is incredibly easy in Prolog. As an example, here's a walker that converts a JSON *ptree* to the classical term form produced by the JSON parser in `library(http/json)`:
```
	ptree_to_json_term('Object'(Pairs),json(NameValues)) :-
	    pairs_to_nameValues(Pairs,NameValues).
	ptree_to_json_term('Array'(PList),List) :-
	    ptree_list_to_json_list(PList,List).
	ptree_to_json_term(string(String),Atom) :-
	    unescape_solidus(String,String1),         % unique JSON escape
	    read_term_from_atom(String1,String2,[]),  % parse to a string
	    atom_string(Atom,String2).                % and convert to atom
	ptree_to_json_term(number(NumString),Num) :-
	    number_string(Num,NumString).
	ptree_to_json_term(lit(String), @(Atom))
	    atom_string(Atom,String).

	pairs_to_nameValues([],[]).
	pairs_to_nameValues([pair([PName,PValue])|Members], [Name=Value|NameValues]) :-
	    ptree_to_json_term(PName,Name),
	    ptree_to_json_term(PValue,Value),
	    pairs_to_nameValues(Members,NameValues).

	ptree_list_to_json_list([],[]).
	ptree_list_to_json_list([P|PList],[J|List]) :-
	    ptree_to_json_term(P,J),
	    ptree_list_to_json_list(PList,List).

	% special case for odd \/ JSON escape
	unescape_solidus(In,Out) :-
	    unescape_solidus_(In,Atomics),
	    atomics_to_string(Atomics,"/",Out).
	unescape_solidus_(In,[Pre|List]) :-
	    sub_string(In,B,2,A,"\\/"), !,
	    sub_string(In,0,B,_,Pre),
	    sub_string(In,_,A,0,NxtIn),
	    unescape_solidus_(NxtIn,List).
	unescape_solidus_(In,[In]).
```
The only tricky bit is the use of the internal Prolog parser (`read_term_from_atom/3`) to map escape sequences in JSON strings to their real character values in the Prolog strings. (Unfortunately, the JSON escape solidus ('`\/`') must be treated as a special case.) Now:

		?- json_test1(T), peg_parse(json,T,JSONTree), ptree_to_json_term(JSONTree,JSONTerm).
produces:

		JSONTerm = json([answer=42,mixed=[1,2.3,'a string',@(true),[4,5]],empty=json([])]).
This also isn't a particularly practical example; the JSON parser from `library(http/json)` is more than adequate. The intent of this section is to provide a minimal introduction to developing practical, portable grammars using a simple three step define-compile-parse process (or just define-parse when using `pPEG` quasi-quotations). These grammars are embedded in the host Prolog application (like regular expressions) and require no additional support tools.

#### A Formal Grammar for SWI-Prolog Syntax

For a more extensive example, see the `Examples` directory fo a formal specification of the SWI-Prolog syntax (current as of March, 2022), including a small semantic analyser which converts the *ptree* result to the corresponding Prolog terms. This has been fairly extensively tested including all files in the `swipl boot` directory and the (almost 200) top level files in the `library`. The biggest obstacle to its use is ensuring the the necessary operators are defined prior to parsing a file. (It's just a parser so only pre-existing operators are recognized.)

This parser is not intended to be used as an alternative to the builtin parser, but having a complete, concise, and testable specification of the SWIP syntax is valuable in its own right (not to mention its intrinsic value as documentation).

#### `pPEG` Performance

If parsing text is a significant activity in the application and performance is critical, it's worth investing the time and effort in a custom parser. For most other situations, `pPEG` provides decent performance, typically in the range of 2-4 microseconds per character depending on the language being parsed. Usually the ability to rapidly implement a parser with a formal specification outweighs any performance penalty incurred. Even when a custom parser is justifiable, starting with a formal, and testable, grammar definition is generally a good idea.

#### Getting Started

If SWI-Prolog has not been installed, see [downloads](http://www.swi-prolog.org/Download.html).

`pPEG` can be installed from the URL `https://github.com/ridgeworks/pPEGpl.git`. Once installed, it can be loaded with `use_module/1` as in:
```
	﻿?- pack_install(pPEG,[url('https://github.com/ridgeworks/pPEGpl.git')]).

	﻿?- use_module(library(pPEG)).
```
In addition to the core `pPEG` module, modules `csg_pPEGxt` (extension supporting some context sensitive grammars), `rexp_pPEGxt` (extension for matching text with regular expressions), and `pPEGutilities` are installed in the library. See [`pPEG` API reference][pPEGref] for further details and examples.

[pPEGrepo]: https://github.com/pcanz/pPEG
[pPEGref]: https://ridgeworks.github.io/pPEGpl/pPEG_API_Guide.html
