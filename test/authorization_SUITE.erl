-module(authorization_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [client_credentials
     %% Disabled so testing does not require interaction. Worked 15-07-22:
     %% authorization_code
    ].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(rdio_api),
    [{apps_started, Started}|Config].

end_per_suite(Config) ->
    lists:map(fun application:stop/1, lists:reverse(?config(apps_started, Config))).

authorization_code(_Config) ->
    RedirectUri = "http://localhost:8080/oauth-callback",
    Url = rdio_api_authorization:code_authorization_url(RedirectUri),
    Code = receive_authorization_code(Url),
    ct:timetrap({seconds, 10}),
    {ok, Tokens} = rdio_api_authorization:tokens_with_authorization_code(Code, RedirectUri),
    {ok, _Result, _NewTokens} = rdio_api:request("currentUser", [], Tokens).

receive_authorization_code(Url) ->
    ct:print(" Open ~n~s~n in a browser and send the \"code\" query parameter of the URI you are redirected to, to ~p using~nlist_to_pid(\"~p\") ! Code.", [Url, self(), self()]),
    ct:timetrap(infinity),
    Code = receive Msg -> Msg end,
    ct:print("Received code ~p.", [Code]),
    Code.

client_credentials(_Config) ->
    {ok, Tokens} = rdio_api_authorization:tokens_with_client_credentials(),
    {ok, _Result, _NewTokens} = 
        rdio_api:request(
          "search", 
          [{"query", "Chiddy Bang"},
           {"types", "Artist"}], 
          Tokens).
    

