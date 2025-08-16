%
% A simple example of an extension "named" `???`which just invokes the SWI-Prolog debugger
%
:- module(trace_pPEGxt, ['???'/6]).

???("",_Env,_Input,PosIn,PosIn,[]) :- trace.