%
% @ extension : Generic exact match to back reference
%
@(Name,Env,Input,PosIn,PosOut,[]) :-
	(peg_lookup_previous(Name,Env,Match)
	 -> string_length(Match,Len),
	    sub_string(Input,PosIn,Len,_,Match),             % same as previous result
	    PosOut is PosIn+Len
	 ;  PosOut = PosIn                                   % no previous, match nothing
	).
