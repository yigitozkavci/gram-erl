-module(utils).

-export([read_credentials/0]).

read_credentials() ->
  {ok, Directory} = file:get_cwd(),
  {ok, Token} = file:read_file(Directory ++ "/token.tok"),
  {ok, ApiId} = file:read_file(Directory ++ "/api_id.tok"),
  {ok, ApiHash} = file:read_file(Directory ++ "/api_hash.tok"),
  { Token, ApiId, ApiHash }.
