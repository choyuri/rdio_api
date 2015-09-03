-module(tokens_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

suite() ->
    [{require, rdio_api_refresh_token},
     {timetrap, {minutes, 1}}].

all() ->
    [invalid_request_token,
     expired_access_token, invalid_access_token,
     expired_implicit_grant_tokens,
     expired_client_credentials, invalid_client_credentials,
     {group, parallel_expired_access_token}].

groups() ->
    [{parallel_expired_access_token, [parallel], lists:duplicate(11, expired_access_token)}].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(rdio_api),
    [{apps_started, Started}|Config].

end_per_suite(Config) ->
    lists:map(fun application:stop/1, lists:reverse(?config(apps_started, Config))).

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

invalid_request_token(_Config) ->
    Tokens = rdio_api_authorization:tokens("expired", 0, undefined, {refresh_token, "invalid"}),
    {error, _Error} = rdio_api:request("currentUser", [], Tokens). % TODO: Test
      % for specific error.

expired_access_token(_Config) ->
    Tokens = rdio_api_authorization:tokens("expired", 0, undefined, {refresh_token, ct:get_config(rdio_api_refresh_token)}),
    {ok, _Result, NewTokens} = rdio_api:request("currentUser", [], Tokens),
    true = Tokens =/= NewTokens.

invalid_access_token(_Config) ->
    Tokens = rdio_api_authorization:tokens("invalid", now_seconds() + 60*60, undefined, {refresh_token, ct:get_config(rdio_api_refresh_token)}),
    {ok, _Result, NewTokens} = rdio_api:request("currentUser", [], Tokens),
    true = Tokens =/= NewTokens.

expired_implicit_grant_tokens(_Config) ->
    Tokens = rdio_api_authorization:tokens_with_implicit_grant("expired", 1),
    timer:sleep(2000),
    {error, #{refresh_tokens_error := implicit_grant}} = rdio_api:request("currentUser", [], Tokens).

expired_client_credentials(_Config) ->
    Tokens = rdio_api_authorization:tokens("expired", 0, undefined, client_credentials),
    {ok, _Result, NewTokens} =
        rdio_api:request(
          "search",
          [{"query", "Chiddy Bang"},
           {"types", "Artist"}],
          Tokens),
    true = Tokens =/= NewTokens.

invalid_client_credentials(_Config) ->
    Tokens = rdio_api_authorization:tokens("invalid", now_seconds() + 60*60, undefined, client_credentials),
    {ok, _Result, NewTokens} =
        rdio_api:request(
          "search",
          [{"query", "Chiddy Bang"},
           {"types", "Artist"}],
          Tokens),
    true = Tokens =/= NewTokens.

now_seconds() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.
