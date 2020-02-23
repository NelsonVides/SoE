%%%-------------------------------------------------------------------
%% @doc school_of_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(school_of_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    school_of_erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
