%
% extension to match regular expression (recognizer only, i.e., on success just move cursor)
%
re_match(RExp,_Env,Input,PosIn,PosOut,[]) :-
	string_length(Input,ILen), PosIn < ILen,  % guard against domain error
	re_matchsub(RExp,Input,Sub,[start(PosIn),anchored(true)]),  % pcre caches compiled RE's
	string_length(Sub.0,Len),  % length of matched string (0th entry of dict Sub)
	PosOut is PosIn+Len.       % move cursor

