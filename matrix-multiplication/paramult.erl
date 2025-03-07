-module(paramult).
-export([matmult/3]).
-import(matmult,[transpose/1]).

matmult(L1,L2,NoProcesses) ->
	StartTime = erlang:timestamp(),
	Work = trunc(length(L1) / NoProcesses),
	%Remainder = length(L1) rem NoProcesses,
	spawnprocesses(NoProcesses,L1,transpose(L2),Work),
	loop(NoProcesses,[],StartTime).

spawnprocesses(N,L1,L2,Work) when N > 0 ->
	LSplit = lists:split(Work,L1),
	Pid = spawn(matmult,threadmatnobin, []),
	Pid ! {self(),element(1,LSplit),L2,N},
	spawnprocesses(N-1,element(2,LSplit),L2,Work);
spawnprocesses(0,_,_,_) ->
	[].
		
loop(N,L,StartTime) when N > 0 ->
	receive
		{Pid, Msg, N} ->
			CL = lists:append(L,[{Msg,N}])
	end,
Pid ! stop,
loop(N-1,CL,StartTime);

loop(0,L,StartTime) ->
	combinelists(tupletolist(lists:keysort(2,L))),
	io:format("Time to complete multiplication: ~w~n", [timeinsecs(StartTime)]).

tupletolist([H|T]) ->
	[element(1,H)|tupletolist(T)];
	tupletolist([]) -> [].	

combinelists([H|T]) ->
	H ++ combinelists(T);
	combinelists([]) -> [].

timeinsecs(StartTime) ->
	timer:now_diff(erlang:timestamp(),StartTime)/(1*math:pow(10,6)).

%go(L1,L2) ->
%	register(process, spawn(matmult, threadmatnobin, [])),
%	process ! {self(),L1,L2},
%	receive
%		{Pid, Msg} ->
%			io:format("~w~n", [Msg])
%end,
%Pid ! stop.
	
%threadmatnobin() ->
%	receive
%		{From, Mat1, Mat2} ->
%			From ! {self(),matdot(Mat1,Mat2)},
%			threadmatnobin();
%		stop ->
%			true
%end.

%dimbin([H|T]) ->
%	[list_to_binary(H)|dimbin(T)];
%	dimbin([]) -> [].
	
%reconmatrix([H|T]) ->
%	[binary_to_list(H)|reconmatrix(T)];
%	reconmatrix([]) -> [].
	
%flatbin(L) ->
%	list_to_binary(L).

%threadmat() ->
%	receive
%		{From, Mat1, Mat2, Size} ->
%			From ! {}
%			loop();
%		stop ->
%			true
%end.

%binmatmult(Mat1,Mat2,Size) ->
	%<<S:Size/binary,R/binary>> = Mat1,
	%<<C:Size/binary,D/binary>> = Mat2,
	%D.