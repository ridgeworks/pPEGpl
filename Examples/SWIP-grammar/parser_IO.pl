swipl_dir(Name,Dir) :-     % construct swipl directory path
	current_prolog_flag(home,H),
	atomics_to_string([H,'/',Name],Dir).

parse_directory(Path) :-   % parse all files in a directory (results discarded.)
	directory_files(Path,Files),
	forall((member(F,Files), atomics_to_string([Path,'/',F],File)), parse_file(File,_)).

parse_file(Path,Terms) :-   % parse .pl file contents to list of terms
	(sub_string(Path,_,3,0,".pl")
	 -> open(Path,read,Stream),
	    read_string(Stream,_,String),
	    close(Stream),
	    format("Parsing ~p ...\n",[Path]),
	    (string_termList(String,Terms) -> true ; true)
	 ;  format("~p not a Prolog file name.\n",[Path])
	).
	
/*  
?- working_directory(Old,"//Users/rworkman/Documents/PrologDev/SWI_Prolog/pPEG").
Old =  (//).

?- working_directory(WD,WD).
WD = '/Users/rworkman/Documents/PrologDev/SWI_Prolog/pPEG/'.


?- parse_file("pl_grammar.pl",Ts).
Ts = [
	(:-module(pl_grammar, [(prolog_grammar/1, testOp/6)])),  
	(:-use_module(library(strings), [string/4])), 
	prolog_grammar('$quasi_quotation'("{|string||\n\n\tProlog = 
...
	\n\n|}")),  
	(testOp("infix", A, B, C, C, []):-currentOp_([D, f, E], A, F)),  
	(testOp("arg_infix", A, B, C, C, []):-currentOp_([D, f, E], A, F), F\=(',')),  
	(testOp("prefix", A, B, C, C, []):-currentOp_([f, D], A, E)),  
	(testOp("postfix", A, B, C, C, []):-currentOp_([D, f], A, E)),  
	(currentOp_(A, B, C):-peg_lookup_previous('_atom', B, D), atom_string(C, D), current_op(E, F, C), atom_chars(F, A))
	].

?- directory_files(".",Files),forall(member(F,Files),parse_file(F,_)).

  operators for library:

op(1150, fx, record).
op(200, fy, @).
op(900, xfx, @).
op(10, fx, #).
  and from XML package
op(400, fx, //).

*/