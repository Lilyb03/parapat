-module(example).
-export([go/0, loop/0]).
go() ->
	register(echo, spawn(example, loop, [])),
	echo ! {self(), 12, 41},
	receive
		{Pid, Msg} ->
			io:format("~w~n",[Msg])
 end,
 Pid ! stop.
 
loop() ->
	receive
		{From, Msg, Msg2} ->
			From ! {self(), Msg+Msg2},
			loop();
		stop ->
			true
 end.