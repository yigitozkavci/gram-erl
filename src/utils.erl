-module(utils).

-export([read_token/0, read_token/1]).

-spec read_token() -> string().
read_token() ->
  {ok, Directory} = file:get_cwd(),
  read_token(Directory).

-spec read_token(Directory :: string()) -> string().
read_token(Directory) ->
  {ok, Data} = file:read_file(Directory ++ "/token.tok"),
  binary_to_list(Data).

