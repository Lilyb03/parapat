-module(matmult).
-import(lists,[sum/1]).
-export([matrixmult/2]).
-export([matrixmult/3]).
-export([transpose/1]).
-export([threadmatnobin/0]).
-export([matdot/2]).

matrixmult(L1,L2,NoProcesses) ->
	StartTime = erlang:timestamp(),
	A = farm:farm(L1,fun(X) -> apply(matmult,matdot,[X,transpose(L2)]) end,NoProcesses),
	io:format("Time to complete multiplication: ~w~n", [timeinsecs(StartTime)]),
	A.

matrixmult(L1,L2) ->
	StartTime = erlang:timestamp(),
	A = matdot(L1,transpose(L2)),
	io:format("Time to complete multiplication: ~w~n", [timeinsecs(StartTime)]),
	A.

timeinsecs(StartTime) ->
	timer:now_diff(erlang:timestamp(),StartTime)/(1*math:pow(10,6)).

%Gets dot product of the rows 
%of two matrices together
matdot([H|T],L2) ->
	[dotlists(H,L2)|matdot(T,L2)];
	matdot([],_) -> [].

%Gets the dot product of H against each element
%in the second list (which is a list of lists)
dotlists(H,[HB|TB]) ->
	[dot(H,HB)|dotlists(H,TB)];
	dotlists(_,[]) -> [].
	
%Transpose Matrix
transpose([[]|_]) -> [];
transpose(L1) ->
	[getcolumn(L1)|transpose(removecolumn(L1))].

%Takes a list of lists and creates a list
%comprised of the tails of each list
removecolumn([H|T]) ->
	[tl(H)|removecolumn(T)];
	removecolumn([]) -> [].

%Takes a list of lists and creates a list
%comprised of the first elements in each list
getcolumn([H|T]) ->
	[hd(H)|getcolumn(T)];
	getcolumn([]) -> [].

%Multiply terms of two lists together
listmult([HA|TA],[HB|TB]) ->
	[HA*HB|listmult(TA,TB)];
	listmult([],[]) -> [].
	
%Calculate dot product of A and B
dot(A,B) ->
	sum(listmult(A,B)).
	
threadmatnobin() ->
	receive
		{From, Mat1, Mat2, I} ->
			From ! {self(),matdot(Mat1,Mat2),I},
			threadmatnobin();
		stop ->
			true
end.