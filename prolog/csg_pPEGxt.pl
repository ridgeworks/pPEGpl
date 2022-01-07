%
% @ extension : Generic exact match to back reference
%
:- module(csg_pPEGxt, ['@'/6]).

:- (current_module(pPEG) -> true ; use_module(library(pPEG),[peg_lookup_previous/3])).  % for retrieving previous match

@(Name,Env,Input,PosIn,PosOut,[]) :-
	(peg_lookup_previous(Name,Env,Match)
	 -> string_length(Match,Len),
	    sub_string(Input,PosIn,Len,_,Match),             % same as previous result
	    PosOut is PosIn+Len
	 ;  PosOut = PosIn                                   % no previous, match nothing
	).
