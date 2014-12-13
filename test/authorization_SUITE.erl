-module(authorization_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [token_endpoint_and_current_user_request].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(rdio_api),
    [{apps_started, Started}|Config].

end_per_suite(Config) ->
    lists:map(fun application:stop/1, lists:reverse(?config(apps_started, Config))).

token_endpoint_and_current_user_request(_Config) ->
    RedirectUri = "http://localhost:8080/oauth-callback",
    Url = rdio_api_authorization:authorization_url(RedirectUri),
    ct:print(" Open ~n~s~n in a browser and send the \"code\" query parameter of the URI you are redirected to, to ~p using~nlist_to_pid(\"~p\") ! Code.", [Url, self(), self()]),
    ct:timetrap(infinity),
    Code = receive Msg -> Msg end,
    ct:print("Received code ~p.", [Code]),
    ct:timetrap({seconds, 10}),
    {ok, Tokens} = rdio_api_authorization:tokens_with_authorization_code(Code, RedirectUri),
    ct:log("Tokens: ~p", [Tokens]),
    Return = rdio_api:request("currentUser", [], Tokens),
    ct:log("Current user request with tokens ~p returned ~p.", [Tokens, Return]).
