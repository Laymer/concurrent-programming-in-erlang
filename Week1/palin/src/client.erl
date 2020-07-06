-module(client). 

-export([client/1]).
% -export([send_req/1]).

client(Server) ->
    receive
        {check, Str, From} ->
            logger:notice("client/1 got ~p from Pid ~p ~n", [Str, From]),
            Server ! {check, Str, self()},
            receive 
                Response -> 
                    logger:notice("client/1 got response ~p, sending back to caller ~p ~n", [Response, From]),
                    From ! Response
            after
                5000 ->
                    stop
            end,
            client(Server);
        Msg ->
            logger:notice("client/1 got message ~p ~n", [Msg]),
            Server ! stop,
            stop
    end.

% send_req(Server) ->
%     receive
%         {check, Str} ->
%             Server ! {check, Str, self()},
%             case palin:palindrome(Str) of
%                 true ->
%                     Pid ! {result, "\"" ++ Str ++ "\" is a palindrome."}, 
%                     client(Pid);
%                 _ ->
%                     Pid ! {result, "\"" ++ Str ++ "\" is not a palindrome."}, 
%                     client(Pid)
%             end;
%         _ ->
%             %% stop the client
%             stop
%     end.