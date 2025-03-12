-module(matrix).
-export([lmatget/3]).
-export([twodfromlist/1]).
-export([aux/1]).

lmatget(Row,Col,Mat) ->
	L = lists:nth(Row,Mat),
	lists:nth(Col,L).

%amatget(Row,Col,Arr) ->
%	A = array

aux(L) ->
	X = array:from_list(L),
	array:get(14,X).
	
twodfromlist([H|T]) ->
	[array:from_list(H)|twodfromlist(T)];
	twodfromlist([]) -> [].
	%array:from_list(List).