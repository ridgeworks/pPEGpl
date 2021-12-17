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
:- module(test_pPEG,
	  [ test_pPEG/0
	  ]).
:- use_module(library(plunit)).
%:- if(exists_source(library(pPEG))).
:- if(true).
:- (current_module(pPEG) -> true ; use_module(library(pPEG))).
/*
:- module(pPEG,[       % module pPEG exports:
	 peg_compile/2,    % create a grammar from a source string
	 peg_compile/3,    % as above with option list
	 peg_parse/3,      % use a pPEG grammar to parse an Input string to a ptree Result
	 peg_parse/5,      % as above with unmatched residue and option list
	 peg_grammar/1     % pPEG grammar source
	]).
*/
%:- set_test_options([silent(true)]).

test_pPEG :-
	run_tests([peg_functions]).

:- begin_tests(peg_functions).

parse_test(Src,Input,Result,Options) :-
	 peg_compile(Src,G,Options),
	 peg_parse(G,Input,Result).

test(identity, R=PG) :-  % if this works, many things are already working
	peg_grammar(S),
	peg_compile(S,PG,[optimise(false)]),
	peg_parse(PG,S,R).

test(basic_rule, G='Peg'([rule([id("rule1"), id("Rule2_")])])) :-
	S="rule1 = Rule2_",
	peg_compile(S,G,[optimise(false)]).

test(basic_rule_O, G='Peg'([rule(rule1, call_O(rule('Rule2_', _)))])) :-
	S="rule1 = Rule2_",
	peg_compile(S,G,[optimise(true)]).

test(basic_fail, fail) :-
	S="rule1 = $",
	peg_compile(S,_).

test(dup_fail, true) :-  % should succeed, Warning message only
	S="rule1 = 'x' rule1 = 'y'",
	peg_compile(S,_,[optimise(true)]).

test(sq, R=rule1("a")) :-
	parse_test("rule1 = 'a'", "a", R, []).
test(sq, fail) :-
	parse_test("rule1 = 'a'", "b", _R, []).
test(sq, R=rule1("a")) :-
	parse_test("rule1 = '' 'a'", "a", R, []).
test(sq, R=rule1("a")) :-
	parse_test("rule1 = 'a' ''", "a", R, []).
test(sq, fail) :-
	parse_test("rule1 = 'a'", "b", _R, []).
test(sq, R=rule1("a")) :-
	parse_test("rule1 = 'a'", "a", R, [optimise(true)]).
test(sq, R = rule1("")) :-  % succeed but match nothing, Residue=Input
	Src="rule1 = ''", Inp = "abc",
	peg_compile(Src,G),
	peg_parse(G,Inp,R,Inp,[incomplete(true)]).

test(dq, R=rule1("a")) :-
	parse_test("rule1 = \"a\"", "a", R, []).
test(dq, fail) :-
	parse_test("rule1 = \"a\"", " b ", _R, []).
test(dq, R=rule1("a")) :-
	parse_test("rule1 = \"\" 'a'", "a", R, []).
test(dq, R=rule1("a")) :-
	parse_test("rule1 = 'a' \"\"", "a", R, []).
test(dq, R=rule1(" a \n")) :-
	parse_test("rule1 = \" a \"", " a \n", R, []).
test(dq, R=rule1(" a \n")) :-
	parse_test("rule1 = \" a \"  _space_ = [ \t\n\r]*", " a \n", R, [optimise(true)]).
test(dq, R=rule1("\n a")) :-
	parse_test("rule1 = \" a\"  _space_ = [ \t\n\r]*", "\n a", R, [optimise(true)]).

test(chs, R=rule1("c")) :-
	parse_test("rule1 = [a-z]", "c", R, []).
test(chs, fail) :-
	parse_test("rule1 = [a-z]", "$", _R, []).
test(chs, R=rule1("a")) :-
	parse_test("rule1 = [a-z]", "a", R, [optimise(true)]).
test(chs, R=rule1("C")) :-
	parse_test("rule1 = ~[a-z]", "C", R, []).
test(chs, fail) :-
	parse_test("rule1 = ~[a-z]", "c", _R, []).
test(chs, R=rule1("A")) :-
	parse_test("rule1 = ~[a-z]", "A", R, [optimise(true)]).
test(chs, fail) :-
	parse_test("rule1 = []", "", _R, []).
test(chs, fail) :-
	parse_test("rule1 = []", "x", _R, []).

test(name, R=rule2("a")) :-
	parse_test("rule1 = rule2\nrule2='a'", "a", R, []).
test(name, fail) :-
	parse_test("rule1 = rule2\nrule3='a'", "a", _R, []).
test(name, R=rule2("a")) :-
	parse_test("rule1 = rule2\nrule2='a'", "a", R, [optimise(true)]).
test(name, R="a") :-
	parse_test("_rule1 = rule2\nrule2='a'", "a", R, []).
test(name, R='Rule1'([rule2("a")])) :-
	parse_test("Rule1 = rule2\nrule2='a'", "a", R, []).

test(seq, R=rule1("ab")) :-
	parse_test("rule1 = 'a' 'b'", "ab", R, []).
test(seq, fail) :-
	parse_test("rule1 = 'a' 'b'", "aa", _R, []).

test(sel, R=rule1("a")) :-
	parse_test("rule1 = 'a' / 'b'", "a", R, []).
test(sel, R=rule1("b")) :-
	parse_test("rule1 = 'a' / 'b'", "b", R, []).
test(seq, fail) :-
	parse_test("rule1 = 'a' / 'b'", "c", _R, []).

test(rep, R=rule1("")) :-
	parse_test("rule1 = 'a'*", "", R, []).
test(rep, R=rule1("aaa")) :-
	parse_test("rule1 = 'a'*", "aaa", R, []).
test(rep, fail) :-
	parse_test("rule1 = 'a'*", "b", _R, []).
test(rep, R=rule1("aaa")) :-
	parse_test("rule1 = 'a'*", "aaa", R, [optimise(true)]).
test(rep, R=rule1("aaa")) :-
	parse_test("rule1 = 'a'*3", "aaa", R, []).
test(rep, fail) :-
	parse_test("rule1 = 'a'*2", "aaa", _R, []).
test(rep, R=rule1("aaa")) :-
	parse_test("rule1 = 'a'*2..", "aaa", R, []).
test(rep, R=rule1("aaa")) :-
	parse_test("rule1 = 'a'*2..4", "aaa", R, []).
test(rep, R=rule1("aa")) :-
	parse_test("rule1 = 'a'*2..4", "aa", R, []).
test(rep, R=rule1("aaaa")) :-
	parse_test("rule1 = 'a'*2..4", "aaaa", R, []).
test(rep, fail) :-
	parse_test("rule1 = 'a'*2..4", "aaaaa", _R, []).
test(rep, R=rule1("a")) :-
	parse_test("rule1 = 'a'+", "a", R, []).
test(rep, R=rule1("aaa")) :-
	parse_test("rule1 = 'a'+", "aaa", R, []).
test(rep, fail) :-
	parse_test("rule1 = 'a'+", "b", _R, []).
test(rep, R=rule1("a")) :-
	parse_test("rule1 = 'a'+", "a", R, [optimise(true)]).
test(rep, R=rule1("")) :-
	parse_test("rule1 = 'a'?", "", R, []).
test(rep, R=rule1("a")) :-
	parse_test("rule1 = 'a'?", "a", R, []).
test(rep, R=rule1("b")) :-
	parse_test("rule1 = 'a'? 'b'", "b", R, [optimise(true)]).

test(pre, R=rule1("ab")) :-
	parse_test("rule1 = &'a' 'ab'", "ab", R, []).
test(pre, fail) :-
	parse_test("rule1 = &'a' 'ba'", "ba", _R, []).
test(pre, R=rule1("ba")) :-
	parse_test("rule1 = !'a' 'ba'", "ba", R, []).
test(pre, fail) :-
	parse_test("rule1 = !'a' 'ab'", "ab", _R, []).
test(pre, R=rule1("ba")) :-
	parse_test("rule1 = ~'a' 'a'", "ba", R, []).
test(pre, fail) :-
	parse_test("rule1 = ~'a' 'a'", "aa", _R, []).
test(pre, R=rule1("x")) :-
	parse_test("rule1 = ~[]", "x", R, []).
test(pre, R=rule1("12")) :-
	parse_test("rule1 = _rule2 \n _rule2 = d d \n d = [0-9]", "12", R, []).

test(group, R=rule1("ab")) :-
	parse_test("rule1 = 'a' ('b' / 'c')", "ab", R, []).
test(group, R=rule1("acd")) :-
	parse_test("rule1 = 'a' ('b' / 'c') 'd'", "acd", R, []).

test(rec, R=='Err') :-
	catch(parse_test("rule1=rule2\nrule2=rule1","",R,[]),error(resource_error(_),_),R='Err').

test(esc, R=rule1("\n\r\t\\\u005d")) :-
	parse_test("rule1 = '\n\r\t\\\u005d'", "\n\r\t\\\u005d", R, []).

test(cws, R=rule1([text("abc"),text("def")])) :-
	parse_test("rule1 = (text \" \")* text = [a-z]*_space_ = [ \t\n\r?]*", "abc ? def  \t", R, []).


:- end_tests(peg_functions).

:- else.

test_pPEG.

:- endif.