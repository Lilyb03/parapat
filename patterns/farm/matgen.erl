-module(matgen).
-export([genmatrix/1]).

genmatrix(T) ->
	outerMatrix([],T).

outerMatrix(L,T) ->
	if
		length(L) =/= T ->
			outerMatrix((L ++ [innerMatrix([],T)]),T);
		true ->
			L
	end.
	
innerMatrix(L,T) ->
	if 
		length(L) =/= T ->
			innerMatrix((L ++ [(trunc(rand:uniform()*100))]),T);
		true ->
			L
	end.