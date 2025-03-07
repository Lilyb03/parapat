-module(farm).
-export([farm/3]).
-export([work/0]).

farm(L,Fun,NoProcesses) ->
	Work = trunc(length(L)/NoProcesses),
	spawnProcesses(NoProcesses,L,Fun,Work),
	loop(NoProcesses,[]).

spawnProcesses(N,L,Fun,Work) when N > 1 ->
	LSplit = lists:split(Work,L),
	Pid = spawn(farm, work, []),
	Pid ! {self(),element(1,LSplit),Fun,N},
	spawnProcesses(N-1,element(2,LSplit),Fun,Work);
spawnProcesses(1,L,Fun,_) ->
	Pid = spawn(farm, work, []),
	Pid ! {self(),L,Fun,1}.

loop(N,L) when N > 0 ->
	receive
		{Pid,Msg,I} ->
			CL = lists:append(L,[{Msg,I}])
	end,
Pid ! stop,
loop(N-1,CL);

loop(0,L) ->
	combinelists(tupletolist(lists:reverse(lists:keysort(2,L)))).

tupletolist([H|T]) ->
	[element(1,H)|tupletolist(T)];
	tupletolist([]) -> [].	

combinelists([H|T]) ->
	H ++ combinelists(T);
	combinelists([]) -> [].

work() ->
	receive
		{From, L, Fun, I} ->
			From ! {self(),Fun(L),I},
			work();
		stop ->
			true
end.