%%% File    : test_tcp_accept.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : Testing a simple server listening on socket.
%%% Created : 14 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>
%%% Based off the example code for gen_tcp online,
%%% modified to accept simultaneous connections.
%%%
%%% Source: http://www.erlang.org/doc/man/gen_tcp.html

-module(test_tcp_accept).
-export([start/0]).

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
	    Request = binary:bin_to_list(Pkt),
	    case string:tokens(Request, " ") of
		[Method, Resource] ->
		    %% TODO: Validate Method, POST/GET
		    io:format("Request type: ~p~nResource requested:~p~n", [Method, Resource]),
		    io:format("MSG: Responding to connection~n"),
		    %% TODO: Add a function to handle lookups
		    %% Should accept Resource, Socket
		    gen_tcp:send(Sock, "HTTP/1.1 301 Moved Permanently\r\nLocation: http://csee.wvu.edu\r\nContent-type: text/html\r\nContent-Length: 0\r\nConnection: close\r\nServer: urlang/0.1\r\n\r\n"),
		    gen_tcp:close(Sock);
		_ ->
		    io:format("Couldn't tokenize~n")
	    end;
	{done} ->
	    io:format("MSG: Connection closed~n")
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
    
