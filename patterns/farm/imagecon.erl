-module(imagecon).
-export([blur/1,file_r/1,blur/2]).
-export([splitWork/2]).
-export([doWork/3]).
-import(gauss,[gauss2D/3]).
-import(proplists,[get_value/2]).

file_r(Filename) ->
	Bits = file:read_file(Filename),
	<<FileHeader:14/binary, DibSize:4/little-signed-integer-unit:8, _/binary>> = element(2,Bits),
	<<_Signature:2/binary, 
	_FileSize:4/binary,
	_Reserved:4/binary,
	DataOffset:4/little-signed-integer-unit:8>> = FileHeader,
	
	<<_:14/binary, DIBHeader:DibSize/binary, _/binary>> = element(2,Bits),
	
	<<_:4/binary, Width:4/little-signed-integer-unit:8, 
	Height:4/little-signed-integer-unit:8,
	_Planes:2/binary, Bitspp:2/little-signed-integer-unit:8,
	_/binary>> = DIBHeader,
	
	<<_:DataOffset/binary, PixelData/binary>> = element(2,Bits),
	
	[{"PData", PixelData},
	{"Height", Height},
	{"Width", Width},
	{"Bytespp", trunc(Bitspp/8)},
	{"FHeader", FileHeader},
	{"DHeader", DIBHeader}].
	
blur(Filename) ->
	StartTime = erlang:timestamp(),
	Data = file_r(Filename),
	
	NewData = blurImage(get_value("PData",Data),
	get_value("Bytespp",Data),
	get_value("Width",Data),
	get_value("Height",Data),2),

	FileHeader = get_value("FHeader",Data),
	DIBHeader = get_value("DHeader", Data),

	FinalImage = <<FileHeader/binary,DIBHeader/binary,NewData/binary>>,
	
	file:write_file("blur.bmp", FinalImage),
	io:format("Time to complete convolution: ~w~n", [timeinsecs(StartTime)]).
	
	%pixelsToList([],PixelData,trunc(Bitspp/8),Width,Height).

blur(Filename,NoProcesses) ->
	Data = file_r(Filename),
	StartTime = erlang:timestamp(),
	
	NewData = list_to_binary(farm:farm(Data,fun(X,Y,Z) -> doWork(X,Y,Z) end,fun(X,Y) -> splitWork(X,Y) end,NoProcesses)),
	
	io:format("Time to complete convolution: ~w~n", [timeinsecs(StartTime)]),
	FileHeader = get_value("FHeader",Data),
	DIBHeader = get_value("DHeader", Data),

	FinalImage = <<FileHeader/binary,DIBHeader/binary,NewData/binary>>,
	
	file:write_file("blur.bmp", FinalImage).
	
	
doWork(ImageData,Work,I) ->
			Width = get_value("Width",ImageData),
			Height = get_value("Height",ImageData),
			NumberOfPixels = Width*Height,
			binary_to_list(blurLoop([],get_value("PData",ImageData),
			get_value("Bytespp",ImageData),
			Width,
			Height,
			4, NumberOfPixels - (Work * (I-1)), Work*((trunc(NumberOfPixels/(Work)))-I))).

splitWork(ImageData,NoProcesses) ->
	trunc(((get_value("Height", ImageData) * get_value("Width",ImageData))/NoProcesses)).

timeinsecs(StartTime) ->
	timer:now_diff(erlang:timestamp(),StartTime)/(1*math:pow(10,6)).
	
blurImage(Pixels,Bytespp,Width,Height,Range) ->
	NumberOfPixels = Width*Height,
	blurLoop([],Pixels,Bytespp,Width,Height,Range,NumberOfPixels,0).
	
blurLoop(L,Pixels,Bytespp,Width,Height,Range,NumberOfPixels,Count) when Count < NumberOfPixels ->
	X = trunc(Count rem Width),
	Y = trunc(Count/Width),
	blurLoop([applyBlurToPixel(X,Y,Pixels,Bytespp,Width,Height,Range)|L],
	Pixels,Bytespp,Width,Height,Range,NumberOfPixels,Count+1);
blurLoop(L,_,_,_,_,_,_,_) ->
	list_to_binary(lists:reverse(L)).
	
applyBlurToPixel(X,Y,Pixels,Bytespp,Width,Height,Range) ->
	NumberOfPixels = math:pow(Range*2 + 1,2),
	pixelsLoop([],X,Y,Pixels,Bytespp,Width,Height,Range,NumberOfPixels,0).

pixelsLoop(L,X,Y,Pixels,Bytespp,Width,Height,Range,NumberOfPixels,Count) when Count < NumberOfPixels ->
	TargetX = trunc(X + ((Count rem (Range*2+1)) - Range)),
	TargetY = trunc(Y + ((math:floor(Count/(Range*2 + 1))) - Range)),
	
	if 
		TargetX < 0 ->
			SignX = -1;
		TargetX >= Width ->
			SignX = -1;
		true ->
			SignX = 1
	end,
	
	if 
		TargetY < 0 ->
			SignY = -1;
		TargetY >= Height ->
			SignY = -1;
		true ->
			SignY = 1
	end,
	
	Data = getPixel(trunc(X + (((Count rem (Range*2 + 1)) - Range) * SignX)),
			trunc(Y + ((((math:floor(Count/(Range*2 + 1))) - Range)) * SignY)),
			Pixels,Width,Bytespp),
	
	pixelsLoop(listadd(L,applyGaussianFunc(Data,trunc((((Count rem (Range*2 + 1)) - Range) * SignX)),
	trunc(((((math:floor(Count/(Range*2 + 1))) - Range)) * SignY)), (Range*2+1)/3)), X,Y,Pixels,Bytespp,Width,Height,Range,NumberOfPixels,Count+1);
pixelsLoop(L,_,_,_,_,_,_,_,_,_) ->
	list_to_binary(L).
	
applyGaussianFunc(Bin,X,Y,Sigma) ->
	L = binary_to_list(Bin),
	applyGauss(L,X,Y,Sigma).

applyGauss([H|T],X,Y,Sigma) ->
	[trunc(gauss2D(Sigma,X,Y) * H)|applyGauss(T,X,Y,Sigma)];
applyGauss([],_,_,_) ->
	[].

listadd([HA|TA],[HB|TB]) ->
	[min(HA+HB,255)|listadd(TA,TB)];
	listadd([],[]) -> [];	
	listadd([],L) ->
		L.

getPixel(X,Y,PixelData,Width,Bytespp) ->
	%io:format("~w~n",[[X,Y]]),
	Offset = trunc(X*Bytespp + (Width*Y*Bytespp)),
	<<_:Offset/binary, Result:Bytespp/binary, _/binary>> = PixelData,
	Result.