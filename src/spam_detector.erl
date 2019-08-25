-module(spam_detector).
-behaviour(gen_server).
-export([handle_call/3, start_link/0, handle_cast/2, init/1, get_request/1, handle_info/2, get_updates_url/2, get_json/1, user_of_update/1]).

-define(SERVER, ?MODULE).

-record(state, {token, next_update_id, updates}).
-record(spam, {user, messages}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

spam_check(Updates, window) ->
  done.

get_updates(Token, UpdateId) ->
  UpdatesUrl = get_updates_url(Token, UpdateId),
  JsonObj = get_json(UpdatesUrl),
  Updates = proplists:get_value(<<"result">>, JsonObj, []),
  case lists:reverse(Updates) of
    [{[{<<"update_id">>, LastUpdateId} | _]} | _] -> 
      {Updates, LastUpdateId + 1};
    [] ->
      {[], UpdateId}
  end.

init([]) ->
  io:format("Starting spam detector..."),
  inets:start(),
  ssl:start(),
  self() ! flush,
  Token = read_token(),
  {ok, #state{token=Token, next_update_id=0, updates=[]}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(flush, State) ->
  io:format("Flushing...~n"),
  {Updates, NextUpdateId} = get_updates(State#state.token, State#state.next_update_id),
  case Updates of
	[] -> 
	  self() ! detect_spam,
    {noreply, State};
	_ -> 
    self() ! flush,
    {noreply, State#state{next_update_id = NextUpdateId, updates = lists:append(State#state.updates, Updates)}}
  end;

handle_info(detect_spam, State) ->
  io:format("Detecting spam with ~p updates...", [length(State#state.updates)]),
  UserToUpdates = maps:to_list(group_by(fun user_of_update/1, State#state.updates)),
  {Spams, Updates} = detect_spams(UserToUpdates),
  io:format("Found some spams? ~p~n...", [Spams]),
  timer:send_after(10000, self(), detect_spam),

  {NewUpdates, NextUpdateId} = get_updates(State#state.token, State#state.next_update_id),
  {noreply, State#state{ updates = NewUpdates, next_update_id = NextUpdateId }}.

detect_spams([]) ->
  {[], []};

detect_spams([{ User, Updates } | Xs]) ->
  {OtherSpams, OtherUpdates} = detect_spams(Xs),
  {SpamUpdates, NonSpamUpdates} = extract_spam_updates(Updates),
  case SpamUpdates of
    [] ->
      {OtherSpams, OtherUpdates ++ NonSpamUpdates};
    _ ->
      {[#spam{user=User, messages=SpamUpdates} | OtherSpams], OtherUpdates ++ NonSpamUpdates}
  end.

extract_spam_updates(Updates) ->
  extract_spam_updates(Updates, []).

extract_spam_updates([], NonSpamUpdates) ->
  {[], NonSpamUpdates};

extract_spam_updates(Updates, NonSpamUpdates) ->
  if
    length(Updates) < 3 ->
      {[], Updates ++ NonSpamUpdates};
    true -> 
      {Window, Rest} = lists:split(3, Updates),
      [ First | RestWindow ] = Window,
      Last = lists:last(Window),
      Date_diff = date_of_update(Last) - date_of_update(First),
      if
        Date_diff < 60 ->
          {Window, Rest ++ NonSpamUpdates};
        true ->
          extract_spam_updates(RestWindow ++ Rest, NonSpamUpdates ++ [First])
      end
  end.

user_of_update({[{<<"update_id">>, _}, {<<"message">>, {Message}} |_]}) ->
  {From} = proplists:get_value(<<"from">>, Message),
  binary_to_list(proplists:get_value(<<"first_name">>, From)).

date_of_update({[{<<"update_id">>, _}, {<<"message">>, {Message}} |_]}) ->
  proplists:get_value(<<"date">>, Message).

group_by(Fun, List) ->
  lists:foldl(
    fun(E, Acc) ->
        maps:update_with(Fun(E), fun(U) -> [E | U] end, [], Acc)
    end,
    maps:new(),
    List
  ).

base_url(Token) ->
  "https://api.telegram.org/bot" ++ Token.

get_json(Url) ->
  Response = get_request(Url),
  {JsonObj} = jiffy:decode(Response),
  JsonObj.

get_updates_url(Token, Offset) ->
  base_url(Token) ++ "/getUpdates?offset=" ++ integer_to_list(Offset).

read_token() ->
  {ok, Directory} = file:get_cwd(),
  {ok, Data} = file:read_file(Directory ++ "/token.tok"),
  binary_to_list(Data).

get_request(Url) ->
  {ok, {_, _, Body}} = request(get, {Url, []}),
  Body.

post_request(Url, Data) ->
  Response = request(post, {Url, [], "application/x-www-form-urlencoded", Data}),
  {ok, {{"HTTP/1.1",ReturnCode, State}, _Head, _Body}} = Response,
  io:format("~w / ~w~n", [ReturnCode, State]).

request(Method, Body) ->
  httpc:request(Method, Body, [{ssl,[{verify,0}]}], []).

