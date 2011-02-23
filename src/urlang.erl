%%% Copyright (c) 2011 Tim 'Shaggy' Bielawa <timbielawa@gmail.com>
%%% 
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without
%%% restriction, including without limitation the rights to use,
%%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following
%%% conditions:
%%% 
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% File    : urlang.erl
%%% Description : URL Shortener in Erlang

-module(urlang).
-export([init/0]).
-include_lib("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    register(url_cache, spawn(fun() -> url_store() end)),
    start().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    start(5678, {attempt, 1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
url_store(State, Max) ->
    receive
	{store, Longurl} ->
	    io:format("Storing a new URL~n"),
	    url_store([#tinyurl{long=Longurl, short=(Max + 1)}|State], (Max + 1));
	{recall, Caller, Hash} ->
	    io:format("Recalling a stored URL: ~p~n", [Hash]),
	    Results = [X#tinyurl.long || X <- State, string:equal(X#tinyurl.short, Hash)],
	    case length(Results) of
		1 ->
		    [Longurl] = Results,
		    Caller ! {ok, Longurl};
		_ ->
		    Caller ! {fail}
	    end,
	    url_store(State, Max)
    end.
url_store() ->
    Test1 = #tinyurl{long="http://peopleareducks.com", short="1"},
    Test2 = #tinyurl{long="http://afrolegs.com", short="2"},
    Test3 = #tinyurl{long="http://tektosterone.com", short="3"},
    url_store([Test1, Test2, Test3], 3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Port, {attempt, Attempt}) ->
    case gen_tcp:listen(Port, [binary, {packet, 0},
			       inet, {active, false},
			       {reuseaddr, true}]) of
	{ok, LSocket} -> 
	    server(LSocket);
	{error,eaddrinuse} ->
	    io:format("MSG: Socket in use, attempt #~p~n", [Attempt]),
	    retry_listener(Port, {attempt, Attempt+1});
	{error,Error} ->
	    io:format("MSG: Other error received:~n~p~n", [Error])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
server(LSocket) ->
    io:format("TCP Listener activated~n"),
    io:format("Waiting for incoming socket.~n"),
    {ok, Sock} = gen_tcp:accept(LSocket),
    gen_tcp:close(LSocket),
    io:format("TCP Socket connection made~n"),
    {ok, {Address, Port}} = inet:peername(Sock),
    io:format("Connection from: ~p on port ~p~n", [Address, Port]),
    PacketHandler = spawn(fun() -> get_packets() end),
    spawn(fun() -> do_recv(PacketHandler, Sock) end),
    io:format("Spawned do_recv packet slurper~n"),
    start().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
retry_listener(Port, {attempt, Attempt}) ->
    case Attempt of
	5 ->
	    io:format("Max number of attempts (5) reached to open socket listener.~n"),
	    io:format("Exiting....~n");
	_ ->
	    start(Port, {attempt,Attempt})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_recv(PPid, Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, B} ->
	    io:format("do_recv: received binary transfer~n"),
	    io:format("Reading request from remote connection~n"),
	    PPid ! {ok, B, Sock};
	{error, closed} ->
	    ok = gen_tcp:close(Sock),
	    PPid ! {done}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_packets() ->
    io:format("get_packets() waiting...~n"),
    receive
	{ok, Pkt, Sock} ->
	    Request = parse_request(Pkt),
	    case string:tokens(Request, " ") of
		["GET", Resource |_] -> 
		    handle_request(strip_leading_slash(Resource), Sock);
		_ ->
		    close_conn_failure(Sock)
	    end;
	{done} ->
	    io:format("MSG: Connection closed~n")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_request(Request) ->
    Strreq = binary:bin_to_list(Request),
    [Method|_] = [string:strip(X) || X <- string:tokens(Strreq, "\r\n")],
    Method.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
strip_leading_slash(Astring) ->
    [_|Rest] = Astring,
    Rest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close_conn_failure(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 500 Internal Service Error\r\nContent-type: text/html\r\nContent-Length: 0\r\nConnection: close\r\nServer: urlang/0.1\r\n\r\n"),
    gen_tcp:close(Sock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close_conn_not_found(Sock) ->
    io:format("Uncached Resource Requested~n"),
    gen_tcp:send(Sock, "HTTP/1.1 404 Short URL Not Found\r\nContent-type: text/html\r\nContent-Length: 0\r\nConnection: close\r\nServer: urlang/0.1\r\n\r\n"),
    gen_tcp:close(Sock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close_conn_forward(Longurl, Sock) ->
    Headers = string:join(["HTTP/1.1 301 Moved Permanently\r\nLocation: ", Longurl, "\r\nContent-type: text/html\r\nContent-Length: 0\r\nConnection: close\r\nServer: urlang/0.1\r\n\r\n"], ""),
    gen_tcp:send(Sock, Headers),
    gen_tcp:close(Sock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_request(Resource, Sock) ->
    io:format("Resource requested: ~p~n", [Resource]),
    url_cache ! {recall, self(), Resource},
    receive
	{ok, Longurl} ->
	    close_conn_forward(Longurl, Sock);
	{fail} ->
	    close_conn_not_found(Sock)
    end.
