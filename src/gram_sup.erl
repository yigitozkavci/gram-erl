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

    Bot = <<"GramBot">>,
    Token = list_to_binary(utils:read_token()),
    pe4kin:launch_bot(Bot, Token, #{receiver => false}),
    Pe4kin =       #{id => pe4kin_receiver,
                    start => {pe4kin_receiver, start_link, [Bot, Token, #{receiver => true}]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [pe4kin_receiver]},
    {ok, {SupFlags, [Pe4kin, SpamDetector]}}.

