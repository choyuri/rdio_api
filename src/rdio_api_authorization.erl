-module(rdio_api_authorization).

%% Tokens Refresh (App intern)
-export([refresh_tokens_with_request_fun/2, refresh_tokens_when_expired_with_request_fun/2]).

%% Authorization Code Grant
-export([authorization_url/1, tokens_with_authorization_code/2]).

%% Client
-export([client_id/0, client_secret/0]).
-export_type([client_id/0, client_secret/0]).
%% Tokens
-export([tokens/3, 
         refresh_token/1, access_token/1, expires/1]).
-export_type([tokens/0, 
              refresh_token/0, access_token/0]).

%% ===================================================================
%% URLs
%% ===================================================================

-define(RdioTokenEndpointUrl, "https://services.rdio.com/oauth2/token").
-define(RdioAuthorizationEndpointUrl, "https://www.rdio.com/oauth2/authorize").

%% ===================================================================
%% Types
%% ===================================================================

-record(tokens, {refresh_token :: refresh_token(),
                 access_token :: access_token(),
                 expires = 0 :: unix_timestamp()}).

-type refresh_token() :: string().
-type access_token() :: string().
-type unix_timestamp() :: non_neg_integer(). % Unix timestamp in seconds.

-opaque tokens() :: #tokens{}.
-type maybeTokens() :: {ok, tokens()} | {error, {binary(), binary()}}.

-type client_id() :: string().
-type client_secret() :: string().

%% ===================================================================
%% Tokens API
%% ===================================================================

-spec refresh_token(tokens()) -> refresh_token().
refresh_token(#tokens{refresh_token = RT}) ->
    RT.

-spec access_token(tokens()) -> access_token().
access_token(#tokens{access_token = AT}) ->
    AT.

-spec expires(tokens()) -> unix_timestamp().
expires(#tokens{expires = E}) ->
    E.

tokens(RefreshToken, AccessToken, ExpiresAt)
  when is_list(RefreshToken),
       is_list(AccessToken),
       is_integer(ExpiresAt),
       ExpiresAt >= 0 ->
    #tokens{access_token = AccessToken,
            refresh_token = RefreshToken,
            expires = ExpiresAt}.

%% ===================================================================
%% Client API
%% ===================================================================

-define(APPLICATION, rdio_api).

-spec client_id() -> client_id().
client_id() ->
    {ok, ClientId} = application:get_env(?APPLICATION, client_id),
    ClientId.

-spec client_secret() -> client_secret().
client_secret() ->
    {ok, ClientSecret} = application:get_env(?APPLICATION, client_secret),
    ClientSecret.

%% ===================================================================
%% Authorization Code Grant
%% ===================================================================

-spec authorization_url(string()) -> string().
authorization_url(RedirectUri) ->
    ?RdioAuthorizationEndpointUrl ++
        "?response_type=code" ++
        "&client_id=" ++ client_id() ++
        "&redirect_uri=" ++ http_uri:encode(RedirectUri).

-spec tokens_with_authorization_code(string(), string()) -> maybeTokens().
tokens_with_authorization_code(Code, RedirectUri) ->
    {ok, {{_Version, HttpCode, _Msg}, _Header, Body}} =
        rdio_api_requester_manager:request(
          ?RdioTokenEndpointUrl,
          [{<<"grant_type">>, <<"authorization_code">>},
           {<<"code">>, Code},
           {<<"redirect_uri">>, RedirectUri}],
          [basic_http_auth_client_verification_header()]),
    handle_token_endpoint_response(HttpCode, Body).

%% ===================================================================
%% Tokens Refresh
%% ===================================================================

%% With request fun, otherwise the there could be deadlocks.
refresh_tokens_when_expired_with_request_fun(#tokens{expires = E} = Tokens, RequestFun) ->
    Now = now_seconds(),
    if E - Now > 0 ->
            {ok, Tokens};
       true ->
            refresh_tokens_with_request_fun(Tokens, RequestFun)
    end.

refresh_tokens_with_request_fun(#tokens{refresh_token = RT}, RequestFun) ->
    tokens_with_refresh_token_and_request_fun(RT, RequestFun).

%% ===================================================================
%% Private
%% ===================================================================


tokens_with_refresh_token_and_request_fun(RefreshToken, RequestFun) ->
    {ok, {{_Version, Code, _Msg}, _Header, Body}} =
        RequestFun(
          ?RdioTokenEndpointUrl,
          [{<<"grant_type">>, <<"refresh_token">>},
           {<<"refresh_token">>, RefreshToken}],
          [basic_http_auth_client_verification_header()]),
    handle_token_endpoint_response(Code, Body).

handle_token_endpoint_response(200, Body) ->
    #{<<"token_type">> := <<"bearer">>,
      <<"access_token">> := AccessToken,
      <<"refresh_token">> := RefreshToken,
      <<"expires_in">> := AccessTokenLifetime} = jiffy:decode(Body, [return_maps]),
    ExpiresAt = now_seconds() + AccessTokenLifetime,
    {ok, #tokens{access_token = binary_to_list(AccessToken),
                 refresh_token = binary_to_list(RefreshToken),
                 expires = ExpiresAt}};
handle_token_endpoint_response(400, Body) ->
    #{<<"error">> := Error,
      <<"error_description">> := ErrorDescription} = jiffy:decode(Body, [return_maps]),
    {error, {Error, ErrorDescription}}.

now_seconds() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.

basic_http_auth_client_verification_header() ->
    {"Authorization", "Basic " ++ base64:encode_to_string(client_id() ++ ":" ++ client_secret())}.

