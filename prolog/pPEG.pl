%
% pPEG == SWI-Prolog module for parsing with pPEG grammars
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
:- module(pPEG,[            % module pPEG exports:
	 peg_compile/2,         % create a grammar from a source string
	 peg_compile/3,         % as above with option list
	 peg_parse/3,           % use a pPEG grammar to parse an Input string to a ptree Result
	 peg_parse/5,           % as above with unmatched residue and option list
	 peg_grammar/1,         % pPEG grammar source
	 peg_lookup_previous/3  % used by CSG extensions to lookup previous matches
	]).
	
:- use_module(library(strings),[string/4]).    % for quasi-quoted strings
:- use_module(library(debug)).                 % for tracing (see peg_trace/0)
:- use_module(library(option),[option/3]).     % for for option list processing
:- use_module(library(pcre),[re_matchsub/4]).  % regular expression support

%
% the "standard" pPEG grammar source for bootstrapping and reference, e.g.,
% ?- peg_grammar(S), write_term(S,[]).
%
peg_grammar({|string||
    Peg   = " " (rule " ")+
    rule  = id " = " alt

    alt   = seq (" / " seq)*
    seq   = rep (" " rep)*
    rep   = pre sfx?
    pre   = pfx? term

    term  = call / sq / dq / chs / group / extn
    call  = id !" ="
    sq    = "'" ~"'"* "'" 'i'?
    dq    = '"' ~'"'* '"' 'i'?
    chs   = '[' ~']'* ']'
    group = "( " alt " )"
    extn  = '<' ~'>'* '>'
    
    id    = [a-zA-Z_] [a-zA-Z0-9_]*
    pfx   = [&!~]
    sfx   = [+?] / '*' range?
    range = num (dots num?)?
    num   = [0-9]+
    dots  = '..'

    _space_ = ([ \t\n\r]+ / '#' ~[\n\r]*)*
|}).

%
% Bootstrap grammar in ptree form
%
boot_grammar_def('Peg'([
	rule([id("Peg"),rep([alt([id("rule"),dq("\" \"")]),sfx("+")])]),
	rule([id("rule"),seq([id("id"),dq("\" = \""),id("alt")])]),
	
	rule([id("alt"),seq([id("seq"),rep([seq([dq("\" / \""),id("seq")]),sfx("*")])])]),
	rule([id("seq"),seq([id("rep"),rep([seq([rep([sq("' '"),sfx("*")]),id("rep")]),sfx("*")])])]),
	rule([id("rep"),seq([id("pre"),rep([id("sfx"),sfx("?")])])]),
	rule([id("pre"),seq([rep([id("pfx"),sfx("?")]),id("term")])]),
	rule([id("term"),alt([id("id"),id("sq"),id("dq"),id("chs"),id("group")])]),

	rule([id("id"),rep([chs("[a-zA-Z_]"),sfx("+")])]),
	rule([id("sq"), seq([dq("\"'\""),rep([pre([pfx("~"),dq("\"'\"")]),   sfx("*")]),dq("\"'\"")])]),
	rule([id("dq"), seq([sq("'\"'"), rep([pre([pfx("~"),sq("'\"'")]),sfx("*")]),sq("'\"'")])]),
	rule([id("chs"),seq([sq("'['"),  rep([pre([pfx("~"),sq("']'")]), sfx("*")]),sq("']'")])]),
	rule([id("group"),seq([dq("\" ( \""),id("alt"),dq("\" )\"")])]),
	
	rule([id("sfx"),chs("[*+?]")]),
	rule([id("pfx"),chs("[!&~]")])
])).

/* Corresponds to:
peg_bootstrap_grammar({|string||
	Peg   = (rule / " ")+
	rule  = id " = " alt

	alt   = seq (" / " seq)*
	seq   = rep (' '* rep)*
	rep   = pre sfx?
	pre   = pfx? term
	term  = id / sq / dq / chs / group

	id    = [a-zA-Z_]+
	sq    = "'" ~"'"* "'"
	dq    = '"' ~'"'* '"'
	chs   = '[' ~']'* ']' 
	group = " ( " alt " )"
	
	sfx   = [*+?]
	pfx   = [!&~]
|}).
*/

%
% initialization code
%
:-  (current_prolog_flag(optimise,Opt),
	 nb_setval('pPEG:temp',Opt),      % save current value to restore on :- initialization/1
	 set_prolog_flag(optimise,false)  % mainly optimizes arithmetic
	).

restore_optimise :-  % restore "optimise" flag at :- initialization.
	nb_current('pPEG:temp',Opt) -> set_prolog_flag(optimise,Opt) ;  true.

% provide debug support before turning optimization on 
debug_peg_trace(FString,Args) :- debug(pPEG(trace),FString,Args).

:-set_prolog_flag(optimise,true).  % mainly optimizes arithmetic

% called from :- initialization.
init_peg :-
	restore_optimise,                  % restore global flag after loading
	foreach((nb_current(Key,_), atom_concat('pPEG:',_,Key)), nb_delete(Key)),  % clear pPEG globals
	nodebug(pPEG(trace)),              % init trace
	bootstrap_grammar.                 % initial load
	
user:exception(undefined_global_variable,'pPEG:$pPEG',retry) :-
	bootstrap_grammar.                 % support multi-threads (need copy for each thread)              

bootstrap_grammar :-
	boot_grammar_def(BootPeg),         % bootstrap and optimize
	nb_setval('pPEG:$pPEG',BootPeg),
	peg_grammar(PegSrc),
	peg_compile(PegSrc,pPEG,[optimise(true)]).  % if successful, will overwrite boot parser

%
% peg_compile/2, peg_compile/3 :create a grammar from a source string
%
peg_compile(Src, GrammarSpec) :-              % create an unoptimized parser (a ptree)
	peg_compile(Src, GrammarSpec, []).
	
peg_compile(Src, GrammarSpec, OptionList) :-  % create parser, optionally optimized
	peg_parse(pPEG, Src, Ptree, _, OptionList),
	option_value(optimise(Opt),OptionList,true),
	(Opt = true -> optimize_peg(Ptree,Grammar) ; Grammar=Ptree),
	(Grammar = GrammarSpec
	 -> true                   % Grammar Spec can be Grammar
	 ;  (atom(GrammarSpec)     % or name of a Grammar
	     -> atomic_concat('pPEG:$',GrammarSpec,GKey),
	        nb_setval(GKey,Grammar)
	     ;  current_prolog_flag(verbose,GVrbse),
	        option_value(verbose(Vrbse),OptionList,GVrbse), % default = global setting
	        peg_fail_msg(peg(argError('GrammarSpec',GrammarSpec)),Vrbse)
	    )
	).

%
% peg_parse/3 :use a Peg grammar to parse an Input string to a ptree Result
% peg_parse/5 :parse/3 with Match string and Options	
%
peg_parse(GrammarSpec, Input, Result) :-
	peg_parse(GrammarSpec, Input, Result, _Residue, []).

peg_parse(GrammarSpec, Input, Result, Residue, OptionList) :-
	% process options
	option_value(incomplete(Incomplete),OptionList,false),  % default = complete parse option
	option_value(tracing(TRules),OptionList,[]),            % default = no tracing
	current_prolog_flag(verbose,GVrbse),
	option_value(verbose(Vrbse),OptionList,GVrbse),         % default = global setting
	(Vrbse = normal
	 -> nb_linkval('pPEG:errorInfo',errorInfo([],[],0))     % init error info - ground so use linkval
	 ;  nb_linkval('pPEG:errorInfo',[])  % verbose \= normal so disable collection
	),
	peg_setup_parse_(GrammarSpec,Input,Vrbse,TRules,GName,Env,Eval),  % setup initial Env and Eval
	(eval_(Eval, Env, Input, 0, PosOut, Result0)   % parse using Eval
	 -> (Result0 = [] -> sub_string(Input,0,PosOut,_,Result) ; Result = Result0)  % parse successful, map [] to matched
	 ;  (nb_getval('pPEG:errorInfo',errorInfo(Name,Inst,Pos))  % parse unsuccessful, check errorInfo
	     -> peg_fail_msg(peg(errorInfo(GName,Name,Inst,Pos,Input)),Vrbse)
	     ;  fail                                   % and fail
	    )
	),
	(string_length(Input,PosOut)                   % did parse consume all input?	
	 -> Residue=""                                 % yes, set residue to empty
	 ;  (Incomplete = true                         % no, incomplete allowed?
	     -> sub_string(Input,PosOut,_,0,Residue)   % yes, set Residue to remaining
	     ;  ((nb_getval('pPEG:errorInfo',errorInfo(Name,Inst,Pos)), PosOut =< Pos)
	         -> peg_fail_msg(peg(errorInfo(GName,Name,Inst,Pos,Input)),Vrbse)  % use errorInfo if relevant
	         ;  peg_fail_msg(peg(incompleteParse(GName,Input,PosOut)),Vrbse)   % else use incomplete
	        )
		)
	).

option_value(Option, Options, Default) :-
	(Options = []
	 -> arg(1,Option,Default)                      % faster option when empty list
	 ;  option(Option, Options, Default)           % else use option/3
	).

peg_setup_parse_(GrammarSpec,Input,Vrbse,TRules,GName,@(Grammar,WS,[],0,([],[])),Eval) :-
	(string(Input)
	 -> true
	 ;  peg_fail_msg(peg(argError('Input',Input)),Vrbse)
	),
	(GrammarSpec='Peg'(Grammar0)
	 -> true
	 ; (atom(GrammarSpec), atomic_concat('pPEG:$',GrammarSpec,GKey), nb_current(GKey,'Peg'(Grammar0))
	    -> true
	    ;  peg_fail_msg(peg(argError('Grammar',GrammarSpec)),Vrbse)
	   )
	),
	peg_add_tracing(TRules,Grammar0,Grammar),      % add required tracing
	peg_set_ws('_space_',Grammar,WS),              % init for custom whitespace	
	Grammar = [FirstRule|_],                       % first rule
	(FirstRule = rule([Eval|_])                    % GName is name of first rule
	 -> Eval = id(GName)                           % non-optimized version: id(Name)
	 ;  Eval = call_O(FirstRule),                  % optimized version
	    FirstRule = rule(GName,_)
	).

peg_fail_msg(Msg, normal) :-                       % only print if verbose = normal
	print_message(informational, Msg),
	fail.

:- multifile prolog:message/1.

prolog:message(peg(argError(Arg,Value))) -->  % DCG
	[ "pPEG Error: invalid argument, ~w = ~w" - [Arg,Value] ].

prolog:message(peg(errorInfo(GName,Rule,Inst,Pos,Input))) -->  % DCG
	{rule_elements(Rule,GName,Elems),
	 atomics_to_string(Elems,".",RName),		 
	 peg_line_pos(Input,Pos,0,1,Text,EPos,ELineNum),    % source text information
	 CPos is EPos+1,                                    % cursor position is 1 based
	 vm_instruction(Inst,Exp),
	 rule_elements(Exp,GName,FElems),
	 atomics_to_string(FElems,".",FExp),	
	 (FExp = "" -> Expct = "" ; Expct = ", expected ")
	},
	% a bit of format magic using tab settings to right justify LineNo and position cursor
	[ 'pPEG Error: ~w failed~w~w at line ~w.~w:\n% ~|~` t~d~3+ | ~w\n% ~|~` t~3+   ~|~` t~*+^' 
	        - [RName,Expct,FExp,ELineNum,CPos,ELineNum,Text,EPos]
	].

prolog:message(peg(incompleteParse(GName,Input,PosOut))) -->  % DCG
	{peg_line_pos(Input,PosOut,0,1,Text,EPos,ELineNum),  % source text information
	 CPos is EPos+1                                      % cursor position is 1 based
	},
	% more format magic using tab settings to right justify LineNo and position cursor
	[ 'pPEG Error: ~w fell short at line ~w.~w:\n% ~|~` t~d~3+ | ~w\n% ~|~` t~3+   ~|~` t~*+^'
	        - [GName,ELineNum,CPos,ELineNum,Text,EPos]
	].

prolog:message(peg(undefined(RuleName))) -->  % DCG
	[ 'pPEG: ~w undefined' - [RuleName] ].               % from VM so limited info

rule_elements([],GName,[GName,GName]) :- !.  % nil rule name, use grammar name (twice)
rule_elements(Rule,GName,[GName,Rule]) :- 
	sub_atom(Rule,0,1,_,RType),              % is it a rule name (starts with alpha)
	char_type(RType,alpha),
	!.
rule_elements(Rule,_GName,[Rule]).           % nothing else qualified by grammar name

% 
% lookup previous match of rule Name in Env
%
peg_lookup_previous(Name,Env,Match) :-
	(atom(Name) -> RName = Name ; atom_string(RName,Name)),
	arg(5,Env,Ctxt),
	lookup_match_(Ctxt,RName,Match).

lookup_match_((Matches,Parent),Name,Match) :-
	(memberchk((Name,Match),Matches)
	 -> true
	 ;  lookup_match_(Parent,Name,Match)
	).

% 
% peg VM implementation - 9 native plus 6 "optimized" instructions
%
eval_(id(Name), Env, Input, PosIn, PosOut, R) :-                % id "instruction"
	% map to call_O(Rule)
	atom_string(PName,Name),
	arg(1,Env,Grammar),  % Env[1] = Grammar
	(memberchk(rule([id(Name), Exp]), Grammar)   % linear search, can be slow
	 -> eval_(call_O(rule(PName,Exp)), Env, Input, PosIn, PosOut, R) % continue with call_O
	 ;  print_message(warning, peg(undefined(PName))),  % undefined rule, fail with warning
	 	fail 
	). 

eval_(alt(Ss), Env, Input, PosIn, PosOut, R) :-                 % alt "instruction"
	alt_eval(Ss, Env, Input, PosIn, PosOut, R).

eval_(seq(Ss), Env, Input, PosIn, PosOut, R) :-                 % seq "instruction"
	seq_eval(Ss, PosIn, Env, Input, PosIn, PosOut, R).
		
eval_(rep([Exp, ROp]), Env, Input, PosIn, PosOut, R) :-         % rep "instruction"
	rep_counts(ROp,Min,Max), !,                  % green cut for rep_counts
	repeat_eval(0, Min, Max, Exp, Env, Input, PosIn, PosOut, R).

eval_(pre([pfx(POp), Exp]), Env, Input, PosIn, PosOut, []) :-   % pre "instruction"
	% requires help with managing errorInfo
	nb_getval('pPEG:errorInfo',ErrorInfo),       % save errorInfo
	nb_linkval('pPEG:errorInfo',[]),             % disable errorInfo collection
	(eval_(Exp, Env, Input, PosIn, _PosOut, _R)
	 -> nb_linkval('pPEG:errorInfo',ErrorInfo),  % restore previous errorInfo
	    % eval_(Exp) succeeded
	    (POp = "&" -> PosOut = PosIn) % ; fail)
	  ; nb_linkval('pPEG:errorInfo',ErrorInfo),  % restore previous errorInfo
	    % eval_(Exp) failed
	    (POp = "!" -> PosOut = PosIn
	    ;POp = "~" -> (string_length(Input,PosIn) -> fail ; PosOut is PosIn+1)  % no match, bump position
	    )
	).

eval_(sq(S), _Env, Input, PosIn, PosOut, []) :-                 % sq "instruction"
	(sub_string(S,_,1,0,"i")                  % case insensitive match test
	 -> sub_string(S,0,_,1,S1),               % strip i
	    literal_match_(S1,SMatch),            % string to match
	    string_upper(SMatch,UMatch),
	    string_length(SMatch,Len),
	    sub_string(Input,PosIn,Len,_,Match),
	    string_upper(Match,UMatch)	          % case insensitive match ... 
	 ;  literal_match_(S,Match),              % string to match
	    sub_string(Input,PosIn,Len,_,Match)   % case sensitive match
	),
	PosOut is PosIn+Len.

eval_(dq(S), Env, Input, PosIn, PosOut, []) :-                  % dq "instruction" 
	(sub_string(S,_,1,0,"i")                  % case insensitive match test
	 -> sub_string(S,0,_,1,S1),               % strip i
	    literal_match_(S1,SMatch),            % string to match
	    string_upper(SMatch,Match),
	    dq_match_list(Match,Matches),         % construct match list
	    dq_match(Matches, upper, Env, Input, PosIn, PosOut)  % then match
	 ;  literal_match_(S,Match),              % string to match
	    dq_match_list(Match,Matches),         % construct match list
	    dq_match(Matches, exact, Env, Input, PosIn, PosOut)  % then match
	).

eval_(chs(MatchSet), _Env, Input, PosIn, PosOut, []) :-        % chs "instruction"
	sub_atom(Input, PosIn, 1, _, R),          % input char, fails if end of Input
	match_chars(MatchSet,MChars),             % convert Match string to MChars list
	chars_in_match(MChars,R),                 % character in set
	PosOut is PosIn+1.                        % match succeeded, consume 1 char

eval_(extn(S), Env, Input, PosIn, PosOut, R) :-                % extn "instruction"
	extn_pred(S,T),
	extn_call(T,Env,Input,PosIn,PosOut,R).
	
% additional instructions produced by optimizer
eval_(call_O(rule(Name, Exp)), @(Grammar,WS,_RName,Dep,Ctxt), Input, PosIn, PosOut, R) :-  % call_O "instruction"
	% also called from id instruction after lookup in non-optimised grammars
	nonvar(Exp),    % test for undefined rule called, warning would have been printed by optimizer
	Dep1 is Dep+1,  % increment call depth
	% recursion check - expensive, so use sparingly
	(Dep1 >= 64  % only check when call depth exceeds 64
	 -> recursive_loop_check(eval_(call_O(rule(Name,_)),_,_,P,_,_),P,PosIn,Name)
	 ;  true
	),
	eval_(Exp, @(Grammar,WS,Name,Dep1,([],Ctxt)), Input, PosIn, PosOut, Res),  % with new context
	(Res = ?(R)
	 -> true  % exit, already done during trace
	 ;  Len is PosOut-PosIn,
	    sub_string(Input,PosIn,Len,_,Match),  % string matched
	    % add Match to siblings in context (undo on backtrack)
	    arg(1,Ctxt,Matches), setarg(1,Ctxt,[(Name,Match)|Matches]),
	    sub_atom(Name,0,1,_,RType),  % first character of name determines rule type
	    (RType = '_'
	     -> R = []                   % optimise passthru rule => null result
	     ;  % ptree result  
	        flatten_(Res,[],RRs),                % flatten args list
	        build_ptree(RRs,RType,Match,Name,R)  % and build
	    )
	).

eval_(rep_O(Exp, Min, Max), Env, Input, PosIn, PosOut, R) :-    % rep_O "instruction"
	repeat_eval(0, Min, Max, Exp, Env, Input, PosIn, PosOut, R).	

eval_(sq_O(Case,Match), _Env, Input, PosIn, PosOut, []) :-      % sq_O "instruction"
	(Case = exact
	 -> sub_string(Input,PosIn,Len,_,Match)   % will match "" with Len=0
	 ;  % assume Case=upper
	    string_length(Match,Len),
	    sub_string(Input,PosIn,Len,_,S),
	    string_upper(S,Match)
	),   
	PosOut is PosIn+Len.

eval_(dq_O(Case,Matches), Env, Input, PosIn, PosOut, []) :-     % dq_O "instruction" 
	dq_match(Matches, Case, Env, Input, PosIn, PosOut).

eval_(chs_O(In,MChars), _Env, Input, PosIn, PosOut, []) :-      % chs_O "instruction"
	sub_atom(Input, PosIn, 1, _, R),          % input char, fails if end of Input
	(chars_in_match(MChars,R) -> In = in ; In = notin),  % character in/notin set
	PosOut is PosIn+1.	                      % match succeeded, consume 1 char

eval_(extn_O(T), Env, Input, PosIn, PosOut, R) :-               % extn_O "instruction"
	extn_call(T,Env,Input,PosIn,PosOut,R).

%
% Support for VM instructions
%

% alt instruction
alt_eval([S|Ss], Env, Input, PosIn, PosOut, R) :- 
	eval_(S, Env, Input, PosIn, PosOut, R)
	 -> true                                         % committed choice
	 ;  alt_eval(Ss, Env, Input, PosIn, PosOut, R).  % first failed, keep trying


% seq instruction
% responsible for capturing error info on failure 
seq_eval([], _Start, _Env, _Input, PosIn, PosIn, []).
seq_eval([S|Ss], Start, Env, Input, PosIn, PosOut, R) :-	
	eval_(S, Env, Input, PosIn, PosNxt, Re), !,
	(Re = [] -> R = Rs ; R = [Re|Rs]),  % don't accumulate empty results
    seq_eval(Ss, Start, Env, Input, PosNxt, PosOut, Rs).  % recurse to next in sequence
seq_eval([S|_], Start, Env, _Input, PosIn, _PosOut, _R) :-
	PosIn > Start,  % something consumed in this sequence
	nb_getval('pPEG:errorInfo',errorInfo(_,_,HWM)),
	PosIn > HWM,
	arg(3,Env,FName),  % Env[3] = current rule name from environment
	nb_linkval('pPEG:errorInfo',errorInfo(FName,S,PosIn)),
	fail.	


% rep instruction 
% counts for repeat a match, -1 signifies any number for Max  
rep_counts(sfx("?"),0, 1).
rep_counts(sfx("+"),1,-1).
rep_counts(sfx("*"),0,-1).                         % *
rep_counts(num(StrN),N,N) :-                       % *N
	number_string(N,StrN).
rep_counts(range([num(StrN),_]),N,-1) :-           % *N..
	number_string(N,StrN).
rep_counts(range([num(StrM),_,num(StrN)]),M,N) :-  % *M..N
	number_string(M,StrM),
	number_string(N,StrN).

% repeat evaluation loop, evaluates to a list
repeat_eval(Max, _Min, Max, _Exp, _Env, _Input, PosIn, PosIn, []) :- !.  % terminate if C=Max
repeat_eval(C,    Min, Max,  Exp,  Env,  Input, PosIn, PosOut, R) :- 
	eval_(Exp, Env, Input, PosIn, PosN, Re),
	PosN  > PosIn, % expressions in loops must consume
	!,
	C1 is C+1,     % increment count
	(Re = [] -> R = Rs ; R = [Re|Rs]),  % don't accumulate empty results
	repeat_eval(C1, Min, Max, Exp, Env, Input, PosN, PosOut, Rs).
repeat_eval(C,    Min,_Max, _Exp, _Env, _Input, PosIn, PosIn, []) :-  % eval failed
	C >= Min.        % C greater than or equal Min, else fail


% sq instruction (also dq)
% strip outer quotes and map escapes
literal_match_(S,Match) :-
	match_chars(S,Chars),                    % convert S string to escaped Chars list
	string_chars(Match,Chars).               % string to match


% dq instruction
% construct a list of dq match components from original match string
% "" component matches whitespace, anything else does literal match
dq_match_list("",[]) :- !.
dq_match_list(S,Ms) :-
	split_string(S," "," ",RawMs),   % splits on space(s) and removes all space characters
	RawMs \= [""], !,                % more than just space characters
	% conditionally restore leading space indicator
	(sub_string(S,0,1,_," ") -> Ms = [""|Ms1] ;  Ms = Ms1),
	sub_string(S,_,1,0,Trl),         % trailing space ?
	dq_match_list_(RawMs,Trl,Ms1).   % RawMs has at least 1 non-empty string
dq_match_list(_S,[""]).              % split yields nothing but whitespace.
	
dq_match_list_([M1,M2|Ms],Trl,[M1,""|Ms1])	:-  !,   
	dq_match_list_([M2|Ms],Trl,Ms1).
dq_match_list_([M1]," ",[M1,""]) :- !.  % trailing space indicator
dq_match_list_(M,_,M).               % no trailing ws

% Match input against a dq match list (see below)
% Grammar required in case there's user defined whitespace
dq_match([], _Case, _Env, _Input, PosIn, PosIn).
dq_match([Match|Matches], Case, Env, Input, PosIn, PosOut) :-
	(Match = ""  % whitespace here?
	 -> skip_ws(Env,Input,PosIn,Pos1)              % yes
	 ;  (Case = exact                              % no
	     -> sub_string(Input,PosIn,Len,_,Match)    % case sensitive match
	     ;  % assume Case = upper
	        string_length(Match,Len),              % case insensitive match
	        sub_string(Input,PosIn,Len,_,R),
	        string_upper(R,Match)
	    ),
	    Pos1 is PosIn+Len
	),
	dq_match(Matches, Case, Env, Input, Pos1, PosOut).

% skip white space in Input moving curser
skip_ws(@(Grammar,WSExp,_,Dep,Ctxt),Input,PosIn,PosOut) :-
	(WSExp = []
	 -> skip_ws_(Input,PosIn,PosOut)   % apply default whitespace
	 ;  % pseudo call to _space_ expression
	    Dep1 is Dep+1,  % increment call depth
	    % recursion check - from call_O instruction 
	    (Dep1 >= 64  % only check when call depth exceeds 64
	     -> recursive_loop_check(skip_ws(_,_,Last,_),Last,PosIn,'_space_')
	     ; true
	    ),
	    % need to ignore any error information generated by _space_
 	    nb_getval('pPEG:errorInfo',ErrorInfo),   % save errorInfo
 	    nb_linkval('pPEG:errorInfo',[]),         % disable errorInfo collection
	    debugging(pPEG(trace),Trace),            % disable tracing while parsing whitespace
	    (Trace = true -> peg_notrace ; true),
	    (eval_(WSExp, @(Grammar,WSExp,'_space_',Dep1,Ctxt), Input, PosIn, PosOut, _R)  % Note: Result discarded
	     -> true
	     ;  PosOut = PosIn                       % eval failed, i.e., no (optional) whitespace
	    ),
	    (Trace = true -> peg_trace ; true),      % restore trace
	    nb_linkval('pPEG:errorInfo',ErrorInfo)   % restore previous errorInfo
	).

skip_ws_(Input,PosIn,PosOut) :-                  % default ws
	((sub_atom(Input,PosIn,1,_,C), ws_char(C))   % peek at char, fails if none left or not ws
	 -> P is PosIn+1,
	    skip_ws_(Input,P,PosOut)
	 ;  PosOut = PosIn
	).

ws_char(' ').
ws_char('\t').
ws_char('\n').
ws_char('\r').

% init custom whitespace expression, e.g., for comments. Name is an atom
peg_set_ws(Name,Grammar,Exp) :-
	memberchk(rule(Name,Exp),Grammar), !.          % optimized rule
peg_set_ws(Name,Grammar,Exp) :-
	atom_string(Name,SName),                       % convert atom to string
	memberchk(rule([id(SName),Exp]),Grammar), !.   % unoptimized rule
peg_set_ws(_Name,_Grammar,[]).                     % non-existant rule, set to 'default'


% chars instruction
% construct list of MChars for matching
match_chars(MatchSet, MChars) :- 
	sub_string(MatchSet,1,_,1,Str),  % strips []
	string_chars(Str,Chars),
	unescape_(Chars,MChars).

unescape_([],[]).	
unescape_(['\\',u,C1,C2,C3,C4|NxtChars],[Esc|MChars]) :-
	hex_value(C1,V1), hex_value(C2,V2), hex_value(C3,V3), hex_value(C4,V4), !,
	VEsc is ((V1*16+V2)*16+V3)*16+V4,
	char_code(Esc,VEsc),
	unescape_(NxtChars,MChars).
unescape_(['\\',CEsc|Chars],[Esc|MChars]) :-
	std_escape_(CEsc,Esc), !,
	unescape_(Chars,MChars).
unescape_([Char|Chars],[Char|MChars]) :-
	unescape_(Chars,MChars).

std_escape_('n','\n').
std_escape_('r','\r').
std_escape_('t','\t').
%%std_escape_('b','\n').
%%std_escape_('f','\n').
std_escape_('\\','\\').

hex_value(C,V) :- char_type(C,digit(V)) -> true ; char_type(C,xdigit(V)).

% search for Ch in list of MChars (including escapes and ranges)
chars_in_match([Cl,'-',Cu|MChars],Ch) :- !,                   % range
	(Cl@=<Ch,Ch@=<Cu -> true ; chars_in_match(MChars,Ch)).
chars_in_match([Cl|MChars],Ch) :-                             % equivalence
	(Cl=Ch -> true ; chars_in_match(MChars,Ch)).


% id/call instruction
% recursive loop check - SWI Prolog specific (also used by skip_ws/4)
recursive_loop_check(Goal,Last,Pos,Name) :-
	prolog_current_frame(F),                % this frame
	prolog_frame_attribute(F,parent,IPF),   % caller's frame
	prolog_frame_attribute(IPF,parent,GPF), % caller's predecessor's frame
	(once(prolog_frame_attribute(GPF,parent_goal,Goal)), Last=Pos 
	 -> % found a parent call with identical cursor position ==> infinte recursion
	    peg_notrace,
	    format(string(Message),"pPEG infinite recursion applying ~w",[Name]),
	    throw(error(resource_error(Message),_))
	 ;  true
	).

% flatten arguments and remove [] (uses difference lists)	
flatten_([], Tl, Tl) :-
    !.
flatten_([Hd|Tl], Tail, List) :-
    !,
    flatten_(Hd, FlatHeadTail, List),
    flatten_(Tl, Tail, FlatHeadTail).
flatten_(NonList, Tl, [NonList|Tl]).

% build a ptree from a flattened list of args
build_ptree([],RType,Match,PName,R) :- !,      % no args, 2 cases
	(char_type(RType,lower)
	 -> R =.. [PName,Match]                    % string result
	 ;  R =.. [PName,[]]                       % empty args
	).
build_ptree([Arg],RType,_Match,_PName,Arg) :-  % single arg
	compound(Arg),
	char_type(RType,lower),                    % cull case, don't wrap
	!.                
build_ptree(Arg,_RType,_Match,PName,R) :-      % general case, construct ptree node   
	R =.. [PName,Arg].


% extn instruction
% convert extension contents to callable Func(StringArg)
extn_pred(S,T) :-
	(sub_string(S,Pos,1,_," ")                 % contains a space at Pos
	 -> FLen is Pos-1,                         % functor length
	    sub_atom(S,1,FLen,_,Func),             % strip <
	    APos is Pos+1,                         % StringArg pos           
	    sub_string(S,APos,_,1,S1),             % also strip >    
	    split_string(S1,""," ",[StringArg])    % and trim whitespace from Arg 
	 ;  sub_atom(S,1,_,1,Func),                % empty StringArg
	    StringArg = ""
	),
	T =.. [Func,StringArg].

% extensions calls T/6 if defined, else just a tracepoint with nothing returned
extn_call(T,Env,Input,PosIn,PosOut,R) :-
	functor(T,F,_),
	current_predicate(F/6), !,                % succeed or fail on call
	call(T,Env,Input,PosIn,PosOut,R).
extn_call(T,_Env,Input,PosIn,PosIn,[]) :-     % default - assume string, treat as trace message
	sub_string(Input,PosIn,_,0,Rem),
	print_message(information, peg(extension(T,Rem))).
	
prolog:message(peg(extension(T,Rem))) -->  % DCG
	[ "Extension ~w parsing: ~p\n" - [T,Rem] ].

% pre-defined extension to enter Prolog debugger
???("",_Env,_Input,PosIn,PosIn,[]) :- trace. 

% pre-defined extension for tracing (see add_tracing/3)
?(Exp,Env,Input,PosIn,PosOut,R) :-
	(debugging(pPEG(trace),true)
	 -> Exp = call_O(rule(Name,RExp)),  % already tracing, just eval rule expression
	    arg(3,Env,Name),
	    eval_(RExp,Env,Input,PosIn,PosOut,R)
	 ;  current_prolog_flag(debug,DF),  % save debug state
	    peg_trace,                      % enable tracing and repeat call_O
	    nb_linkval('pPEG:indent'," "),  % two spaces to start
	    (eval_(Exp,Env,Input,PosIn,PosOut,Rt)
	     -> peg_notrace,                % success, disable tracing and return a result
	        set_prolog_flag(debug,DF),  % restore saved debug state
	        % result wrapped in '?' to prevent double wrapping - see call_O instruction
	        R = ?(Rt)
	     ;  peg_notrace,                % fail, just disable tracing
	        set_prolog_flag(debug,DF),  % restore saved debug state
	        fail
	    )
	).

% tracing support
peg_add_tracing(TRules,Grammar,GrammarT) :-
	(TRules = []
	 -> GrammarT = Grammar                    % short circuit if nothing to trace
	 ;  (Grammar = [rule(_,_)|_]
	     ->	duplicate_term(Grammar,GrammarC)  % create duplicate of optimized grammar
	     ;  GrammarC = Grammar                % unoptimized case copies as needed
	    ),
	    add_tracing(TRules,GrammarC,GrammarT)
	).

add_tracing([],Grammar,Grammar) :- !.
add_tracing([Name|Names],Grammar,GrammarT) :- !,
	add_tracing(Name,Grammar,NxtGrammar), 
	add_tracing(Names,NxtGrammar,GrammarT).
add_tracing(Name,Grammar,GrammarT) :-
	add_trace(Grammar,Name,GrammarT).
	
add_trace([],_SName,[]).
add_trace([rule([id(SName), Exp])|Rules], Name, 
          [rule([id(SName), extn_O(?(call_O(rule(AName,Exp))))])|Rules]) :-
	nonvar(Exp),              % must be defined
	atom_string(AName,SName), % SName and Name equivalent to AName
	atom_string(AName,Name),
	!.
add_trace([Rule|Rules], Name, 
          [Rule|Rules]) :-
	Rule = rule(AName, Exp),  % optimized Rule
	nonvar(Exp),              % must be defined
	atom_string(AName,Name),  % name matches
	!,
	% overwrite expression in place so all call references persist
	setarg(2,Rule,extn_O(?(call_O(rule(AName,Exp))))). 
add_trace([R|Rules],SName,[R|RulesT]) :-
	add_trace(Rules,SName,RulesT).

% enable tracing
peg_trace :-
	debug(pPEG(trace)),
	current_prolog_flag(verbose,V),      % suppress informational messages when enabling spy point
	set_prolog_flag(verbose,silent),
	spy(pPEG:eval_),
	set_prolog_flag(verbose,V).

% disable tracing
peg_notrace :-
	(debugging(pPEG(trace),true)
	 -> current_prolog_flag(verbose,V),  % suppress informational messages when disabling spy point
	    set_prolog_flag(verbose,silent),
	    nospy(pPEG:eval_),
	    set_prolog_flag(verbose,V),
	    nodebug(pPEG(trace))
	 ;  true
	).

:- multifile user:prolog_trace_interception/4.

user:prolog_trace_interception(Port,Frame,_Choice,continue) :-
	debugging(pPEG(trace),true),  % peg(trace) enabled?
	prolog_frame_attribute(Frame,goal,Goal),
	peg_trace_port(Port,Goal),
	!.  % defensive, remove any CP's

peg_trace_port(Port,pPEG:eval_(Inst, _Env, Input, PosIn, PosOut, R)) :-  % only trace pPEG:eval_/6
	peg_inst_type(Inst,Type),
	vm_instruction(Inst,TInst),
	peg_trace_port_(Type, Port, TInst, Input, PosIn, PosOut, R),
	!.
	
peg_trace_port_(call, call, TInst, Input, PosIn, _PosOut, _R) :- !,
	peg_cursor_pos(Input,PosIn,Cursor),
	peg_trace_msg(postInc, "~w~w~w", [Cursor,TInst]).            % with indent parm
peg_trace_port_(call, fail, TInst, Input, PosIn, _PosOut, _R) :- !,
	peg_cursor_pos(Input,PosIn,Cursor),
	peg_trace_input(Input,PosIn,Str),
	peg_trace_msg(preDec, "~w~w~w != \t~p", [Cursor,TInst,Str]). % with indent parm
peg_trace_port_(call, exit, TInst, Input, PosIn, PosOut, R) :- !,
	peg_cursor_pos(Input,PosOut,Cursor),		
	(R = []  % if null result (_rule), print matching string
	 -> Len is PosOut-PosIn,
	    sub_string(Input,PosIn,Len,_,RT) 
	 ;  RT = R
	),
	(string(RT) -> MatchOp = "==" ; MatchOp = "=>"),
	peg_trace_msg(preDec, "~w~w~w ~w \t~p", [Cursor,TInst,MatchOp,RT]). % with indent parm
peg_trace_port_(meta, call, TInst, Input, PosIn, _PosOut, _R) :- !,
	peg_cursor_pos(Input,PosIn,Cursor),
	peg_trace_msg(indent, "~w~w~w", [Cursor,TInst]).             % with indent parm
peg_trace_port_(terminal, fail, TInst, Input, PosIn, _PosOut, _R) :- !,
	peg_cursor_pos(Input,PosIn,Cursor),
	peg_trace_input(Input,PosIn,Str),
	peg_trace_msg(indent, "~w~w~w != \t~p", [Cursor,TInst,Str]). % with indent parm
peg_trace_port_(terminal, exit, TInst, Input, PosIn, PosOut, _R) :- !,
	peg_cursor_pos(Input,PosOut,Cursor),
	Len is PosOut-PosIn,
	sub_string(Input,PosIn,Len,_,RT),
	peg_trace_msg(indent, "~w~w~w == \t~p", [Cursor,TInst,RT]).  % with indent parm
peg_trace_port_(_Other, _, _, _, _, _, _).  % else no trace message

peg_inst_type(alt(_),meta).
peg_inst_type(seq(_),meta).
peg_inst_type(pre(_),call).
peg_inst_type(rep(_),meta).
peg_inst_type(rep_O(_,_,_),meta).
peg_inst_type(sq(_),terminal). 
peg_inst_type(sq_O(_,_),terminal).
peg_inst_type(dq(_),terminal).
peg_inst_type(dq_O(_,_),terminal).
peg_inst_type(chs(_),terminal).
peg_inst_type(chs_O(_,_),terminal).
peg_inst_type(extn(_),terminal).
peg_inst_type(extn_O(Ext),Type) :-
	(Ext = ?(_) -> Type = notrace ; Type = terminal).
peg_inst_type(id(_),notrace).  % not traced
peg_inst_type(call_O(_),call).

peg_cursor_pos(Input,Pos,Cursor) :-
	peg_line_pos(Input,Pos,0,1,_Text,LinePos,LineNo),    % source text information
	CPos is LinePos +1,                                  % cursor position is 1 based
	format(string(Cursor),"~` t~d~4+.~d~4+",[LineNo,CPos]).  % more format tab magic

peg_line_pos(Input,Pos,LinePos,LineNum,Text,EPos,ELineNum) :-
	Pos > LinePos,
	re_matchsub("[^\n\r]*(\n|\r\n?)",Input,Match,[start(LinePos)]), !,  % match a line
	string_length(Match.0,Len),
	NxtLinePos is LinePos+Len,
	(Pos<NxtLinePos                            % Pos is within this line?
	 -> string_concat(Text,Match.1,Match.0),   % yes
	    EPos is Pos-LinePos,
	    ELineNum = LineNum
	 ;  NxtLineNum is LineNum+1,               % no
	    peg_line_pos(Input,Pos,NxtLinePos,NxtLineNum,Text,EPos,ELineNum) 
	).
peg_line_pos(Input,Pos,LinePos,LineNum,Text,EPos,LineNum) :-  % last line
	sub_string(Input,LinePos,_,0,Text),
	EPos is Pos-LinePos.

peg_trace_input(Input,PosIn,Str) :-
	sub_string(Input,PosIn,L,0,SStr),          % current residue
	(L =< 32
	 -> Str = SStr
	  ; sub_string(SStr,0,32,_,SStr1),
	    string_concat(SStr1," ... ",Str)
	).

peg_trace_msg(postInc, Msg, [Cursor|Args]) :-
	nb_getval('pPEG:indent', Indent),
	debug_peg_trace(Msg,[Cursor,Indent|Args]),
	string_concat(Indent,"|  ",NxtIndent),      % add "|  " to current indent
	nb_linkval('pPEG:indent',NxtIndent).
peg_trace_msg(preDec, Msg, [Cursor|Args]) :-
	nb_getval('pPEG:indent', Indent),
	sub_string(Indent,_,_,3,NxtIndent),         % subtract 3 chars from end of current indent
	debug_peg_trace(Msg,[Cursor,NxtIndent|Args]),
	nb_linkval('pPEG:indent',NxtIndent).
peg_trace_msg(indent, Msg, [Cursor|Args]) :-
	nb_getval('pPEG:indent', Indent),
	debug_peg_trace(Msg,[Cursor,Indent|Args]).

%
% de-compile VM instructions of interest, used for tracing and error messages
%
vm_instruction([], "").
vm_instruction(id(Name), Name).
vm_instruction(call_O(rule(Name,_Exp)), Name). 
vm_instruction(seq(Exps), Is) :-
	vm_instruction_list(Exps,LIs),
	atomics_to_string(LIs," ",Is0),
	atomics_to_string(["(",Is0,")"],Is).
vm_instruction(alt(Exps), Is) :-
	vm_instruction_list(Exps,LIs),
	atomics_to_string(LIs," / ",Is0),
	atomics_to_string(["(",Is0,")"],Is).
vm_instruction(rep([Exp, sfx(ROp)]), Is) :-
	vm_instruction(Exp,I),
	(I = [] -> Is = I ; string_concat(I,ROp,Is)).
vm_instruction(rep_O(Exp, Min, Max), Is) :-
	rep_counts(sfx(ROp), Min, Max), !,	
	vm_instruction(Exp,I),
	(I = [] -> Is = I ; string_concat(I,ROp,Is)).
vm_instruction(rep_O(Exp, N, N), Is) :-
	rep_counts(num(StrN), N, N), !,	
	vm_instruction(Exp,I),
	(I = [] -> Is = I ; atomics_to_string([I,'*',StrN],Is)).
vm_instruction(rep_O(Exp, N, -1), Is) :-
	rep_counts(range([num(StrN),_]),N,-1), !,	
	vm_instruction(Exp,I),
	(I = [] -> Is = I ; atomics_to_string([I,'*',StrN,".."],Is)).
vm_instruction(rep_O(Exp, M, N), Is) :-
	rep_counts(range([num(StrM),_,num(StrN)]),M,N),	
	vm_instruction(Exp,I),
	(I = [] -> Is = I ; atomics_to_string([I,'*',StrM,"..",StrN],Is)).	
vm_instruction(pre([pfx(Chs),Exp]), Is) :-
	vm_instruction(Exp,I),
	(I = [] -> Is = I ; string_concat(Chs,I,Is)).
vm_instruction(sq(Match), Is) :-
	unescape_std(Match,Is).
vm_instruction(sq_O(Case,Match), Is) :-
	(Case = exact -> Sens = "" ; Sens = "i"),
	unescape_std(Match,S1),
	unescape_string(S1,"'","\\u0027",S),
	atomics_to_string(["'",S,"'",Sens],Is).
vm_instruction(dq(Match), Is) :-
	unescape_std(Match,Is).
vm_instruction(dq_O(Case,Matches), Is) :- 
	(Matches = [""]
	 -> Match = " "                           % was a single " "
	 ;  atomics_to_string(Matches," ",Match)  % insert space as separator between elements
	),
	(Case = exact -> Sens = "" ; Sens = "i"),
	unescape_std(Match,S1),
	unescape_string(S1,"\"","\\u0022",S),
	atomics_to_string(['"',S,'"',Sens],Is).
vm_instruction(chs(Match), Is) :-
	unescape_std(Match,Is).
vm_instruction(chs_O(In,MChars), Is) :-
	(In = notin -> Pfx = '~' ; Pfx = ''),
	string_chars(MStr,MChars),
	unescape_std(MStr,S),
	unescape_string(S,"]","\\u005d",S1),
	atomics_to_string([Pfx,"[",S1,"]"],Is).	
vm_instruction(extn(Is), Is).
vm_instruction(extn_O(Ext), Is) :-
	(string(Ext)
	 -> atomics_to_string(['<',Ext,'>'],Is)
	 ;  Ext =.. [Func,StringArg],  % Sexp format
	    (Func = ? -> Is=[] ; atomics_to_string(['<',Func,' ',StringArg,'>'],Is))
	).

vm_instruction_list([],[]).
vm_instruction_list([Exp|Exps],[Is|LIs]) :-
	vm_instruction(Exp,Is),
	vm_instruction_list(Exps,LIs).

unescape_string(Sin,Esc,Usc,Sout) :-
	split_string(Sin,Esc,"",L),
	atomics_to_string(L,Usc,Sout).
	
unescape_std(Sin,Sout) :-
	string_chars(Sin,CharsIn),
	escape_chars(CharsIn,CharsOut),
	string_chars(Sout,CharsOut).

escape_chars([],[]).	
escape_chars([C|CharsIn],[C|CharsOut]) :-
	char_code(C,CS), between(32,126,CS), !,     % ASCII
	escape_chars(CharsIn,CharsOut).	
escape_chars([ECh|CharsIn],['\\',Ch|CharsOut]) :- 
	std_escape_(Ch,ECh),!,                      % escapes
	escape_chars(CharsIn,CharsOut).	
escape_chars([C|CharsIn],['\\','u',X1,X2,X3,X4|CharsOut]) :-
	char_code(C,CS), % (CS =< 31 ; CS >= 127),  % outside ASCII, but not std escape
	divmod(CS,16,Q4,R4),
	divmod(Q4,16,Q3,R3),
	divmod(Q3,16,R1,R2),
	hex_value(X1,R1), hex_value(X2,R2), hex_value(X3,R3), hex_value(X4,R4),
	escape_chars(CharsIn,CharsOut).
	
%
% optimizing compiler for use with peg_compile
% normally takes unoptimized ptree as input, but it's idempotent
% produces an optimized grammar object which is faster but not a ptree (it's cyclic!)
%
optimize_peg('Peg'(Rules),'Peg'(RulesO)) :-
	(optimize_rules(Rules,RDefs,RulesO)
	 -> chk_RDefs(RDefs)  % must be done after optimize so as to not corrupt refs
	 ;  (Rules = [rule([id(GName),_])|_Rules] -> true ; GName = "?unknown?"),
	    print_message(warning,peg(optimize_fail(GName))),  % ensures failure msg               
	    fail
	 ).

chk_RDefs([]) :- !.
chk_RDefs([Name:Rule|RDefs]) :-
	(var(Rule)
	 -> print_message(warning, peg(undefined(Name))),  % also caught by id instruction	 
	    atom_string(PName,Name), 
	    Rule = rule(PName,_)  % bind a name with var to catch if used
	 ;  true
	), chk_RDefs(RDefs).

prolog:message(peg(optimize_fail(GName))) -->  % DCG
	[ "pPEG: grammar ~w optimization failed" - [GName] ].
	
optimize_rules([],_RDefs,[]).
optimize_rules([Rule|Rules],RDefs,[RuleO|RulesO]) :-
	optimize_rule(Rule,RDefs,RuleO),
	optimize_rules(Rules,RDefs,RulesO).

optimize_rule(rule([id(Name),Exp]), RDefs, RuleO) :- !, % unoptimized rule 
	atom_string(PName,Name),  % optimised rule name is atom for building ptrees
	RuleO = rule(PName,ExpO),
	memberchk(Name:RuleO, RDefs),
	(var(ExpO)
	 -> optimize_exp(Exp, RDefs, ExpO)                  % OK, rule not already defined
	 ;  print_message(warning,peg(duplicate(Name)))     % duplicate rule               
	).
optimize_rule(rule(Name,Exp), _RDefs, rule(Name,Exp)).  % already optimized?

prolog:message(peg(duplicate(Name))) -->  % DCG
	[ "pPEG: rule ~w already defined" - [Name] ].
	
optimize_exp(id(Name), RDefs, call_O(Rule)) :-          % id(Name) ==> call_O(Rule)
	memberchk(Name:Rule, RDefs).
	
optimize_exp(seq(Ins), RDefs, seq(Opt)) :-
	optimize_exp_list(Ins,RDefs,Opt).	
	
optimize_exp(alt(Ins), RDefs, alt(Opt)) :-
	optimize_exp_list(Ins,RDefs,Opt).	
	
optimize_exp(rep([Exp, ROp]), RDefs, rep_O(ExpO, Min, Max)) :-
	rep_counts(ROp,Min,Max), !,
	optimize_exp(Exp,RDefs,ExpO).
	
optimize_exp(pre([pfx("~"), chs(MatchSet)]), RDefs, chs_O(notin,MChars)) :- !,
	optimize_exp(chs(MatchSet), RDefs, chs_O(_,MChars)).
optimize_exp(pre([pfx(POp), Exp]), RDefs, pre([pfx(POp), ExpO])) :-
	optimize_exp(Exp,RDefs,ExpO).

optimize_exp(chs(MatchSet), _RDefs, chs_O(in,MChars)) :- 
	match_chars(MatchSet, MChars).

optimize_exp(sq(QS), _RDefs, sq_O(Case,Match)) :-
	(sub_string(QS,_,1,0,"i")                  % case insensitive match test
	 -> Case = upper,
	    sub_string(QS,0,_,1,S),                % strip i
	    literal_match_(S,AMatch),              % string to match
	    string_upper(AMatch,Match)
	 ;  Case = exact,
	    literal_match_(QS,Match)               % string to match
	).

optimize_exp(dq(QS), _RDefs, I) :-
	(sub_string(QS,_,1,0,"i")                  % case insensitive match test
	 -> Case = upper,
	    sub_string(QS,0,_,1,S),                % strip i
	    literal_match_(S,SMatch),              % string to match
	    string_upper(SMatch,Match)
	 ;  Case = exact,
	    literal_match_(QS,Match)               % string to match
	),
	dq_match_list(Match,Matches),              % split into submatches
	dq_instruction(Matches,Case,I),
	!.

optimize_exp(extn(S), _RDefs, extn_O(T)) :-    % extn "instruction"
	extn_pred(S,T).

optimize_exp(call_O(Rule), _RDefs, call_O(Rule)).                 % already optimized?
optimize_exp(rep_O(Exp, Min, Max), _RDefs, rep_O(Exp, Min, Max)). % already optimized?
optimize_exp(sq_O(C,M), _RDefs, sq_O(C,M)).                       % already optimized?
optimize_exp(dq_O(C,M), _RDefs, dq_O(C,M)).                       % already optimized?
optimize_exp(chs_O(M), _RDefs, chs_O(M)).                         % already optimized?
optimize_exp(extn_O(T), _RDefs, extn_O(T)).                       % already optimized?

optimize_exp_list([],_RDefs,[]).
optimize_exp_list([Exp|Exps],RDefs,[ExpO|ExpOs]) :-
	optimize_exp(Exp,RDefs,ExpO),
	optimize_exp_list(Exps,RDefs,ExpOs).	 

% optimize dq cases
dq_instruction([],Case,sq_O(Case,"")).            % empty string -> sq_O
dq_instruction([""],Case,dq_O(Case,[""])).        % ws match -> dq_O
dq_instruction([S],Case,sq_O(Case,S)).            % single non ws -> sq_O
dq_instruction(Matches,Case,dq_O(Case,Matches)).  % everything else -> dq_O

%
% extensions from colocated files
%
:- include(pPEG/re_match_extn).
:- include(pPEG/csg_extn).

%
% time to initialize...
%
:- initialization(init_peg,now).