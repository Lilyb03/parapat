-module(gauss).
-export([gauss1D/2]).
-export([gauss2D/3]).
-export([normalize/2]).

gauss1D(Sigma,X) ->
	((1/(math:sqrt(2*math:pi()*math:pow(Sigma,2)))) 
	* math:exp(-((math:pow(X,2))/(math:pow(2*Sigma,2))))).
	
gauss2D(Sigma,X,Y) ->
	((1/(2*math:pi()*math:pow(Sigma,2))) 
	* math:exp(-((math:pow(X,2) + math:pow(Y,2))/(math:pow(2*Sigma,2))))).
	
normalize(Sigma,Range) ->
	normalLoop(Sigma,0,Range*Range,0).

normalLoop(Sigma,N,Count,Total) when N < Count ->
	X = trunc(N rem trunc(math:sqrt(Count))) - 50,
	Y = trunc(N/trunc(math:sqrt(Count))) - 50,
	io:format("~w~n",[[X,Y]]),
	normalLoop(Sigma,N+1,Count,Total + gauss2D(Sigma,
	X,
	Y));
normalLoop(_,_,_,Total) ->
	Total.