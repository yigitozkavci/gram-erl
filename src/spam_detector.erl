-module(spam_detector).
-behaviour(gen_server).
-export([handle_call/3, start_link/0, handle_cast/2, init/1, handle_info/2, user_of_update/1, group_by/2]).

-define(SERVER, ?MODULE).


-record(state, {updates}).
-record(spam, {user, messages}).
-record(text_message, {user :: string(), message :: string(), chat_id :: integer(), date :: integer()}).

% Private chat id: 564929761
% Public chat id: -1001330413002
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  io:format("Starting spam detector..."),
  inets:start(),
  ssl:start(),
  pe4kin_receiver:subscribe(<<"GramBot">>, self()),
  pe4kin_receiver:start_http_poll("GramBot", #{limit=>100, timeout=>60}),

  % Give pe4kin some time to poll chat updates into state then
  % start detecting spams.
  timer:send_after(5000, self(), detect_spam),

  {ok, #state{updates=[]}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({pe4kin_update, BotName, Update}, State) ->
  io:format("[Info] Got update for bot ~p~n", [BotName]),
  case pe4kin_types:update_type(Update) of
    message ->
      {ok, ChatId} = pe4kin_types:chat_id(message, Update),
      {ok, #{<<"first_name">> := Username}} = pe4kin_types:user(message, Update),
      #{<<"message">> := Message} = Update,
      case pe4kin_types:message_type(Message) of
        text ->
          #{<<"text">> := Text, <<"date">> := Date} = Message,
          ParsedUpdate = #text_message{user=binary_to_list(Username), message=Text, chat_id=ChatId, date=Date},
          io:format("[Info] Added text message: ~p~n", [ParsedUpdate]),
          {noreply, State#state{updates = [ParsedUpdate | State#state.updates]}};
      _ ->
          io:format("[Warning] Non-text messages are not supported yet.~n", []),
          {noreply, State}
      end;
    _ ->
        io:format("[Warning] Non-message updates are not supported yet.~n", []),
        {noreply, State}
  end;

handle_info(detect_spam, State) ->
  io:format("Detecting spam with ~p updates...", [length(State#state.updates)]),
  UserToUpdates = maps:to_list(group_by(fun(U) -> U#text_message.user end, State#state.updates)),
  {Spams, Updates} = detect_spams(UserToUpdates),
  lists:foreach(fun notify_spam_owner/1, Spams),
  io:format("Found some spams? ~p~n...", [Spams]),
  timer:send_after(3 * 60 * 1000, self(), detect_spam),

  {noreply, State#state{updates=Updates}};

handle_info(wow, State) ->
  Wow = tdlib:send_sync(session1, [{<<"@type">>, <<"getChatHistory">>}, {<<"chat_id">>, -1001330413002}, {<<"from_message_id">>, 1074790400}, {<<"limit">>, 100}], 30000),
  % lists:foreach(fun(M) -> io:format("~p~n", [M]) end, proplists:get_value(<<"messages">>, Wow)),
  lists:foreach(fun(M) -> io:format("~p~n", [M]) end, lists:map(fun get_message_texts/1, proplists:get_value(<<"messages">>, Wow))),
  Resp = tdlib:send_sync(session1, [{<<"@type">>, <<"getChats">>}, {<<"offset_order">>, <<"9223372036854775807">>}, {<<"limit">>, 100}], 30000),
  ChatIds = proplists:get_value(<<"chat_ids">>, Resp),
  Infos = lists:map(fun get_chat_info/1, ChatIds),
  io:format("~p~n", [Infos]),
  {noreply, State}.

get_chat_info(ChatId) ->
  Resp = tdlib:send_sync(session1, [{<<"@type">>, <<"getChat">>}, {<<"chat_id">>, ChatId}], 30000),
  Type = proplists:get_value(<<"type">>, Resp),
  case proplists:get_value(<<"@type">>, Type) of
    <<"chatTypePrivate">> ->
      {proplists:get_value(<<"user_id">>, Type), proplists:get_value(<<"title">>, Resp)};
    _ ->
      undefined
  end.

get_message_texts(Message) ->
  case proplists:get_value(<<"@type">>, Message) of
    <<"message">> ->
      Content = proplists:get_value(<<"content">>, Message),
      case proplists:get_value(<<"@type">>, Content) of
        <<"messageText">> ->
          Text = proplists:get_value(<<"text">>, Content),
          case proplists:get_value(<<"@type">>, Text) of
            <<"formattedText">> ->
              MessageText = proplists:get_value(<<"text">>, Text),
              Cond = string:rstr(binary_to_list(MessageText), "open.spotify.com"),
              if
                Cond == 0 ->
                  MessageText;
                true ->
                  Content
              end;
            _ ->
              <<"no_formatted_text">>
          end;
      _ ->
        <<"no_message_text">>
      end;
    _ ->
      <<"no_message">>
  end.

        

notify_spam_owner(#spam{user=User, messages=Messages}) ->
  Text=iolist_to_binary(io_lib:format("User ~s has spammed with ~p messages in 3 minutes!", [list_to_binary(User), length(Messages)])),
  pe4kin:send_message(<<"GramBot">>, #{chat_id=>-1001330413002, text=>Text}),
  done.

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

spam_interval_secs() ->
  60 * 3.

extract_spam_updates(Updates) ->
  extract_spam_updates(Updates, []).

extract_spam_updates([], NonSpamUpdates) ->
  {[], NonSpamUpdates};

extract_spam_updates([U|US], NonSpamUpdates) ->
  {LatestFrequentUpdates, Rest} = lists:splitwith(fun(RU) -> date_of_update(RU) + spam_interval_secs() > date_of_update(U) end, [U|US]),
  if
    length(LatestFrequentUpdates) >= 5 ->
      {LatestFrequentUpdates, Rest ++ NonSpamUpdates};
    true ->
      extract_spam_updates(US, NonSpamUpdates ++ [U])
  end.

user_of_update({[{<<"update_id">>, _}, {<<"message">>, {Message}} |_]}) ->
  {From} = proplists:get_value(<<"from">>, Message),
  proplists:get_value(<<"first_name">>, From).

date_of_update(#text_message{date=Date}) ->
  Date.

group_by(Fun, List) ->
  % Reversing to preserve ordering.
  maps:map(fun(_, V) -> lists:reverse(V) end,
    lists:foldl(
      fun(E, Acc) ->
          maps:update_with(Fun(E), fun(U) -> [E | U] end, [E], Acc)
      end,
      maps:new(),
      List
    )
  ).

