-module(imagecon).
-export([blur/1,file_r/1]).
-export([threadBlur/0]).
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
	
%normalizeConstant(Sigma,KernelSize) ->
%	NoPixels = KernelSize*KernelSize,
%	normLoop(Sigma,KernelSize,0,0,NoPixels).

%normLoop(Sigma,KernelSize,Count,Total,NoPixels) when Count < NoPixels ->
%	normLoop(Sigma,KernelSize,Count+1,Total+
%	gauss2D(Sigma,trunc((Count rem (KernelSize)) - (KernelSize)),
%	trunc((math:floor(Count/KernelSize) - KernelSize))), NoPixels);
%normLoop(_,_,_,Total,NoPixels) ->
%	io:format("~w~n", [[Total/NoPixels]]),
%	(Total/NoPixels).	

listadd([HA|TA],[HB|TB]) ->
	[min(HA+HB,255)|listadd(TA,TB)];
	listadd([],[]) -> [];	
	listadd([],L) ->
		L.

threadBlur() ->
	receive
		{From, ImageData, Range, StartCount, I, NoProcesses} ->
			Width = get_value("Width",ImageData),
			Height = get_value("Height",ImageData),
			NumberOfPixels = Width*Height,
			From ! {self(),blurLoop([],get_value("PData",ImageData),
			get_value("Bytespp",ImageData),
			Width,
			Height,
			Range, trunc((NumberOfPixels/NoProcesses)*I), StartCount),I},
			threadBlur();
		stop ->
			true
end.

%pixelsToList(L,Pixels,Bytespp,Width,Height) when Height > 0 ->
%	RowLength = trunc(Bytespp*Width),
%	<<Data:RowLength/binary, X/binary>> = Pixels,
%	pixelsToList([pixelsToRow([],Data,Bytespp,Width)|L],X,Bytespp,Width,Height-1);
%pixelsToList(L,_,_,_,0) ->
%	lists:reverse(L).

%pixelsToRow(L,Pixels,Bytespp,Width) when Width > 0 ->
%	<<Data:Bytespp/binary, X/binary>> = Pixels,
%	pixelsToRow([binary_to_list(Data)|L],X,Bytespp,Width-1);
%pixelsToRow(L,_,_,0) ->
%	lists:reverse(L).

%getRows(L,X,Y,Range,Width,Bytespp,PixelData,Counter) when Counter < ((Range*2)+1) ->
%	getRows([getPixel(X,Y,PixelData,Width,Bytespp) | L]
%	,X + 1, Y, Range, Width, Bytespp, PixelData, Counter+1);
%getRows(L,_,_,_,_,_,_,_) ->
%	L.

%getSurroundingPixels(X,Y,Width,Height,Bytespp,Range,PixelData) ->
%	getRows([],X,Y,Range,Width,Bytespp,PixelData).
	
%getRows(L,X,Y,Range,Width,Bytespp,PixelData) when Y =< Range ->
%	getRows([functioName([],X - Range,Y,Range,Width,Bytespp,PixelData) | L],X,Y+1,Range,Width,Bytespp,PixelData);
%getRows(L,_,_,_,_,_,_) ->
%	L.
	
%functioName(L,X,Y,Range,Width,Bytespp,PixelData) when X =< Range ->
%	functioName(,X+1,Y,Range,Width,Bytespp,PixelData);
%functioName(L,_,_,_,_,_,_) ->
%	lists:reverse(L).

getPixel(X,Y,PixelData,Width,Bytespp) ->
	%io:format("~w~n",[[X,Y]]),
	Offset = trunc(X*Bytespp + (Width*Y*Bytespp)),
	<<_:Offset/binary, Result:Bytespp/binary, _/binary>> = PixelData,
	Result.

%getSurroundingPixelsNew(X,Y,NoAdjacent,PixelData,Width,Bytespp) ->

%	NegOffset = trunc(((X - NoAdjacent - EdgeDetectL)*Bytespp) + (Width*Bytespp*(Y-1))),
%	NOffset = trunc((X - NoAdjacent - 1)*Bytespp + (Width*Bytespp*Y)),
%	PosOffset = trunc(((X - NoAdjacent - 1)*Bytespp) + (Width*Bytespp*(Y+1))),
%	SizeOfBinary = trunc(NoAdjacent*Bytespp*2 + Bytespp),
%	<<_:NegOffset/binary, PixelsTop:SizeOfBinary/binary, _/binary>> = PixelData,
%	<<_:NOffset/binary, PixelsMiddle:SizeOfBinary/binary, _/binary>> = PixelData,
%	<<_:PosOffset/binary, PixelsBottom:SizeOfBinary/binary, _/binary>> = PixelData,
%	{PixelsTop, PixelsMiddle, PixelsBottom}.
	
%getSurroundingPixels(N,PixelData,Width,Bytespp) ->
%	NegOffset = trunc((N*Bytespp - Bytespp*2 - (Width*Bytespp))),
%	NOffset = trunc((N*Bytespp) - Bytespp*2),
%	PosOffset = trunc((N*Bytespp - Bytespp*2 + (Width*Bytespp))),
%	<<_:NegOffset/binary, A:Bytespp/binary, B:Bytespp/binary, C:Bytespp/binary, _/binary>> = PixelData,
%	<<_:NOffset/binary, D:Bytespp/binary, E:Bytespp/binary, F:Bytespp/binary, _/binary>> = PixelData,
%	<<_:PosOffset/binary, G:Bytespp/binary, H:Bytespp/binary, I:Bytespp/binary, _/binary>> = PixelData,
%	{A,B,C,D,E,F,G,H,I}.