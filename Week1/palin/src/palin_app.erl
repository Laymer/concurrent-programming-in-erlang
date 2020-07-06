%%%-------------------------------------------------------------------
%% @doc palin public API
%% @end
%%%-------------------------------------------------------------------

-module(palin_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    palin_sup:start_link().

stop(_State) ->
    ok.

%% internal functions