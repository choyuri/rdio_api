-module(tokens_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [invalid_request_token, expired_access_token, invalid_access_token, {group, parallel_expired_access_token}].

groups() ->
    [{parallel_expired_access_token, [parallel], lists:duplicate(11, expired_access_token)}].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(rdio_api),
    [{require, rdio_api_refresh_token}, {apps_started, Started}|Config].

end_per_suite(Config) ->
    lists:map(fun application:stop/1, lists:reverse(?config(apps_started, Config))).

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

invalid_request_token(_Config) ->
    Tokens = rdio_api_authorization:tokens("invalid", "expired", 0),
    {error, _Error} = rdio_api:request("currentUser", [], Tokens). % TODO: Test
      % for specific error.

expired_access_token(_Config) ->
    Tokens = rdio_api_authorization:tokens(ct:get_config(rdio_api_refresh_token), "expired", 0),
    {ok, _Result, NewTokens} = rdio_api:request("currentUser", [], Tokens),
    true = Tokens =/= NewTokens.

invalid_access_token(_Config) ->
    Tokens = rdio_api_authorization:tokens(ct:get_config(rdio_api_refresh_token), "expired", now_seconds() + 60*60),
    {ok, _Result, NewTokens} = rdio_api:request("currentUser", [], Tokens),
    true = Tokens =/= NewTokens.

now_seconds() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.
