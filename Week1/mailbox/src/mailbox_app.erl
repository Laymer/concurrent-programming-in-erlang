%%%-------------------------------------------------------------------
%% @doc mailbox public API
%% @end
%%%-------------------------------------------------------------------

-module(mailbox_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([receiver/1]).
-export([receiver_case/1]).
-export([ordered_receiver/0]).

start(_StartType, _StartArgs) ->
    mailbox_sup:start_link().

stop(_State) ->
    ok.

receiver(Delay) ->
    timer:sleep(Delay),
    receive
        stop ->
            logger:notice("receiver/0 stopping");
        Msg ->
            logger:notice("receiver/0 got ~p", [Msg]),
            receiver(Delay)
    end.

receiver_case(Delay) ->
    timer:sleep(Delay),
    receive
        Msg ->
            case Msg of
            	stop ->
            		logger:notice("receiver_case/0 stopping");
            	_ ->
		            logger:notice("receiver_case/0 got ~p", [Msg]),
		            receiver_case(Delay)
            end
    end.

ordered_receiver() ->
    receive
        {first, FirstString} ->
            logger:notice("FirstString : ~p", [FirstString])
    end,
    receive
        {second, SecondString} ->
            logger:notice("SecondString : ~p", [SecondString])
    end.

% 31> R = spawn(mailbox_app, receiver, [1000]), R ! m1, R ! m2, R ! stop.
% stop
% 32> 2020-07-07T09:49:22.732284+02:00 notice: receiver/0 got m1
% 2020-07-07T09:49:23.733581+02:00 notice: receiver/0 got m2
% 2020-07-07T09:49:24.734505+02:00 notice: receiver/0 stopping

% 32> RC = spawn(mailbox_app, receiver_case, [1000]), RC ! m1, RC ! m2, RC ! stop.
% stop
% 33> 2020-07-07T09:49:45.089385+02:00 notice: receiver_case/0 got m1
% 2020-07-07T09:49:46.090069+02:00 notice: receiver_case/0 got m2
% 2020-07-07T09:49:47.091105+02:00 notice: receiver_case/0 stopping

% 34> OR = spawn(mailbox_app, ordered_receiver, []), OR ! {second, "hi2"}, OR ! {first, "hi"}.
% {first,"hi"}
% 35> 2020-07-07T09:52:08.371013+02:00 notice: FirstString : "hi"
% 2020-07-07T09:52:08.371202+02:00 notice: SecondString : "hi2"
