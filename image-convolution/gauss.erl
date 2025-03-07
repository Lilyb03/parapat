-module(gauss).
-export([gauss1D/2]).
-export([gauss2D/3]).

gauss1D(Sigma,X) ->
	((1/(math:sqrt(2*math:pi()*math:pow(Sigma,2)))) 
	* math:exp(-((math:pow(X,2))/(math:pow(2*Sigma,2))))).
	
gauss2D(Sigma,X,Y) ->
	((1/(2*math:pi()*math:pow(Sigma,2))) 
	* math:exp(-((math:pow(X,2) + math:pow(Y,2))/(math:pow(2*Sigma,2))))).