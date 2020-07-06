-module(server). 

-export([server/1]).
-export([server/0]).
-export([servers/1]).

server(Pid) -> 
    receive
        {check, Str} ->
            case palin:palindrome(Str) of
                true ->
                    Pid ! {result, "\"" ++ Str ++ "\" is a palindrome."}, 
                    server(Pid);
                _ ->
                    Pid ! {result, "\"" ++ Str ++ "\" is not a palindrome."}, 
                    server(Pid)
            end;
        _ ->
            %% stop the server
            stop
    end.

server() -> 
    receive
        {check, Str, From} ->
            logger:notice("server/0 got ~p from Pid ~p ~n", [Str, From]),
            case palin:palindrome(Str) of
                true ->
                    From ! {result, "\"" ++ Str ++ "\" is a palindrome."}, 
                    server();
                _ ->
                    From ! {result, "\"" ++ Str ++ "\" is not a palindrome."}, 
                    server()
            end;
        Msg ->
            %% stop the server
            logger:notice("server/0 stopping, msg ~p ~n", [Msg]),
            stopped
    end.

servers(N) -> 
    Servers = [ spawn(server, server, []) || _X <- lists:seq(1, N) ],
    logger:notice("servers/1 spawned ~p workers ~n", [N]),
    dispatch(Servers).
    
dispatch([FirstServer | OtherServers] = L) ->
    receive
        {check, Str, From} ->
            logger:notice("dispatch/1 got ~p from Pid ~p ~n", [Str, From]),
            FirstServer ! {check, Str, From},
            dispatch(lists:append(OtherServers, [FirstServer]));
        Msg ->
            logger:notice("dispatch/1 stopping servers, msg ~p ~n", [Msg]),
            F = fun
                (Server) ->
                    Server ! stop
            end,
            ok = lists:foreach(F, L)
    end.

% Shell test output :
% ==============
% 
% 13> S = spawn(server,servers,[2]).
% <0.388.0>
% 
% 14> C = spawn(client,client,[S]).
% <0.392.0>
% 
% 15> C ! {check, "lol", self()}.
% {check,"lol",<0.182.0>}
% 
% 16> 2020-07-07T01:04:45.039944+02:00 notice: client/1 got "lol" from Pid <0.182.0>
% 2020-07-07T01:04:45.040193+02:00 notice: dispatch/1 got "lol" from Pid <0.392.0>
% 2020-07-07T01:04:45.040369+02:00 notice: server/0 got "lol" from Pid <0.392.0>
% 2020-07-07T01:04:45.040527+02:00 notice: client/1 got response {result,"\"lol\" is a palindrome."}, sending back to caller <0.182.0>
% 
% 16> receive X -> X end.
% {result,"\"lol\" is a palindrome."}
%
%
% 9> length(erlang:processes()).
% 78 ------------------------------------> initial number of processes
% 
% 10> {S,C} = palin:run(2).
% 2020-07-07T01:32:27.109964+02:00 notice: servers/1 spawned 2 workers
% {<0.216.0>,<0.217.0>}
% 
% 11> length(erlang:processes()).
% 82 ----------------------------> 78 + dispatcher + client + 2 servers
% 
% 12> C ! stop.
% 2020-07-07T01:32:37.238220+02:00 notice: client/1 got message stop
% stop
% 2020-07-07T01:32:37.238416+02:00 notice: dispatch/1 stopping servers, msg stop
% 13> 2020-07-07T01:32:37.238610+02:00 notice: server/0 stopping, msg stop
% 2020-07-07T01:32:37.238542+02:00 notice: server/0 stopping, msg stop
% 
% 13> length(erlang:processes()).
% 78 ------------------------------------> 82 - (dispatcher + client + 2 servers)
%  |
%  |---> all palin processes were correctly stopped 