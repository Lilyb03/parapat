-module(paraimage).
-export([parablur/2]).
-import(proplists,[get_value/2]).

parablur(File,NoProcesses) ->
	StartTime = erlang:timestamp(),
	
	ImageData = imagecon:file_r(File),
	Work = trunc((get_value("Height",ImageData) *
	get_value("Width",ImageData))/ NoProcesses),
	spawnprocesses(NoProcesses,ImageData,Work,NoProcesses),
	NewData = loop(NoProcesses,[],StartTime),
	
	FileHeader = get_value("FHeader",ImageData),
	DIBHeader = get_value("DHeader", ImageData),

	FinalImage = <<FileHeader/binary,DIBHeader/binary,NewData/binary>>,
	
	file:write_file("ParaBlur.bmp", FinalImage).
	
spawnprocesses(N,ImageData,Work,NoProcesses) when N > 0 ->
	Pid = spawn(imagecon,threadBlur,[]),
	Pid ! {self(),ImageData,4,(Work*(N-1)),N,NoProcesses},
	spawnprocesses(N-1,ImageData,Work,NoProcesses);
spawnprocesses(0,_,_,_) ->
	[].
	
loop(N,L,StartTime) when N > 0 ->
	receive
		{Pid,Msg,N} ->
			io:format("Time for Loop: ~w~n", [timeinsecs(StartTime)]),
			CL = lists:append(L,[{Msg,N}])
	end,
Pid ! stop,
loop(N-1,CL,StartTime);

loop(0,L,StartTime) ->
	io:format("Time to complete convolution: ~w~n", [timeinsecs(StartTime)]),
	list_to_binary(combinelists(tupletolist(lists:keysort(2,L)))).

tupletolist([H|T]) ->
	[element(1,H)|tupletolist(T)];
	tupletolist([]) -> [].	

combinelists([H|T]) ->
	binary_to_list(H) ++ combinelists(T);
	combinelists([]) -> [].

timeinsecs(StartTime) ->
	timer:now_diff(erlang:timestamp(),StartTime)/(1*math:pow(10,6)).