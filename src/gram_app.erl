%%%-------------------------------------------------------------------
%% @doc gram public API
%% @end
%%%-------------------------------------------------------------------

-module(gram_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gram_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
