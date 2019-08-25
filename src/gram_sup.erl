-module(gram_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(gram_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    SpamDetector = #{id => spam_detector,
                    start => {spam_detector, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [spam_detector]},
    {ok, {SupFlags, [SpamDetector]}}.

