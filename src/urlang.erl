%%% File    : urlang.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : URL Shortener
%%% Created : 22 Feb 2011 by Tim Bielawa <timbielawa@gmail.com>

-module(urlang).
-export([start/0]).
-include_lib("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    start(5678, {attempt, 1}).

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
get_packets() ->
    io:format("get_packets() waiting...~n"),
    receive
	{ok, Pkt, Sock} ->
	    Request = parse_request(Pkt),
	    case string:tokens(Request, " ") of
		["GET", Resource |_] -> 
		    handle_request(Resource, Sock);
		_ ->
		    close_conn_failure(Sock, Request)
	    end;
	{done} ->
	    io:format("MSG: Connection closed~n")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_request(Request) ->
    Strreq = binary:bin_to_list(Request),
    [Method|_] = [string:strip(X) || X <- string:tokens(Strreq, "\r\n")],
    %% case string:tokens(Method, " ") of
    %% 	["GET", Resource, Version] ->
    %% 	    io:format("MSG: parse_request found Resource/Version: ~p/~p~n", [Resource, Version]);
    %% 	_ ->
    %% 	    io:format("MSG: parse_request has no idea whats going on~n")
    %% end,
    Method.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close_conn_failure(Sock, Request) ->
    io:format("Couldn't tokenize!~n"),
    io:format("HTTP Headers Sent To Us: ~p~n", [Request]),
    gen_tcp:send(Sock, "HTTP/1.1 500 Internal Service Error\r\nContent-type: text/html\r\nContent-Length: 0\r\nConnection: close\r\nServer: urlang/0.1\r\n\r\n"),
    gen_tcp:close(Sock).

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
handle_request(Resource, Sock) ->
    io:format("Resource requested: ~p~n", [Resource]),
    %% TODO: Add a function to handle lookups
    gen_tcp:send(Sock, "HTTP/1.1 301 Moved Permanently\r\nLocation: http://csee.wvu.edu\r\nContent-type: text/html\r\nContent-Length: 0\r\nConnection: close\r\nServer: urlang/0.1\r\n\r\n"),
    gen_tcp:close(Sock).


