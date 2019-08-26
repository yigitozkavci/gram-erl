-module(spam_detector_test).
-include_lib("eunit/include/eunit.hrl").

base_url_test_() ->
  [
    ?_assert("https://api.telegram.org/bot<token>" =:= spam_detector:base_url("<token>"))
  ].

get_updates_url_test_() ->
  [
    ?_assert("https://api.telegram.org/bot<token>/getUpdates?offset=4" =:= spam_detector:get_updates_url("<token>", 4))
  ].

read_token_test_() ->
  {ok, Dir} = file:get_cwd(),
  TestDir = Dir ++ "/test/testdata",
  [
    ?_assert("<test_token>" =:= spam_detector:read_token(TestDir))
  ].

group_by_test_() ->
  [
    ?_assert(
       spam_detector:group_by(
         fun({K, _}) -> K end,
         []
       )
       =:=
       #{}
    ),
    ?_assert(
       spam_detector:group_by(
         fun({K, _}) -> K end,
         [{"red", "lamp"}]
       )
       =:=
       #{"red" => [{"red", "lamp"}]}
    ),
    ?_assert(
       spam_detector:group_by(
         fun({K, _}) -> K end,
         [{"red", "lamp"}, {"red", "sofa"}, {"blue", "carpet"}]
       )
       =:=
       #{"red" => [{"red", "lamp"}, {"red", "sofa"}], "blue" => [{"blue", "carpet"}]}
    )
  ].
