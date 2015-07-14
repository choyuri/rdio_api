-module(rdio_api_authorization).

%% Tokens Refresh
-export([refresh_tokens_with_request_fun/2, refresh_tokens_when_expired_with_request_fun/2]).

-export_type([maybe_tokens/0]).

%% All following exports are part of the applications public API. Feel free to
%% use them anywhere you want.

%% Authorization Code Grant
-export([code_authorization_url/1, code_authorization_url/2, code_authorization_url/3, 
         tokens_with_authorization_code/2]).

%% Implicit Grant
-export([token_authorization_url/1, token_authorization_url/2, token_authorization_url/3]).

%% Resource Owner Credential
-export([tokens_with_resource_owner_credentials/2, tokens_with_resource_owner_credentials/3]).

%% Client Credentials
-export([tokens_with_client_credentials/0, tokens_with_client_credentials/1]).

%% Device Code
-export([start_device_code_grant/0, start_device_code_grant/1,
         tokens_with_device_code/1]).

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
-define(RdioDeviceCodeEndpointUrl, "https://services.rdio.com/oauth2/device/code/generate").

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

code_authorization_url(RedirectUri) ->
    code_authorization_url(RedirectUri, undefined, undefined).

code_authorization_url(RedirectUri, Scope) ->
    code_authorization_url(RedirectUri, Scope, undefined).

code_authorization_url(RedirectUri, Scope, State) ->
    authorization_url("code", RedirectUri, Scope, State).

-spec tokens_with_authorization_code(string(), string()) -> maybe_tokens().
tokens_with_authorization_code(Code, RedirectUri) ->
    handle_token_endpoint_response(
      rdio_api_requester_manager:request(
        ?RdioTokenEndpointUrl,
        [{"grant_type", "authorization_code"},
         {"code", Code},
         {"redirect_uri", RedirectUri}],
        [basic_http_auth_client_verification_header()])).

%% ===================================================================
%% Implicit Grant
%% ===================================================================

token_authorization_url(RedirectUri) ->
    token_authorization_url(RedirectUri, undefined, undefined).

token_authorization_url(RedirectUri, Scope) ->
    token_authorization_url(RedirectUri, Scope, undefined).

token_authorization_url(RedirectUri, Scope, State) ->
    authorization_url("token", RedirectUri, Scope, State).

%% ===================================================================
%% Resource Owner Credential
%% ===================================================================

tokens_with_resource_owner_credentials(Username, Password) ->
    tokens_with_resource_owner_credentials(Username, Password, undefined).

tokens_with_resource_owner_credentials(Username, Password, Scope) ->
    handle_token_endpoint_response(
      rdio_api_requester_manager:request(
        ?RdioTokenEndpointUrl,
        [{"grant_type", "password"},
         {"username", Username},
         {"password", Password}] ++ 
            case Scope of undefined -> []; _ -> [{"scope", Scope}] end,
        [basic_http_auth_client_verification_header()])).

%% ===================================================================
%% Client Credentials
%% ===================================================================

tokens_with_client_credentials() ->
    tokens_with_client_credentials(undefined).

tokens_with_client_credentials(Scope) ->
    handle_token_endpoint_response(
      rdio_api_requester_manager:request(
        ?RdioTokenEndpointUrl,
        [{"grant_type", "client_credentials"}] ++ 
            case Scope of undefined -> []; _ -> [{"scope", Scope}] end,
        [basic_http_auth_client_verification_header()])).

%% ===================================================================
%% Device Code
%% ===================================================================

start_device_code_grant() ->
    start_device_code_grant(undefined).

start_device_code_grant(Scope) ->
    handle_device_code_grant_response(
      rdio_api_requester_manager:request(
        ?RdioDeviceCodeEndpointUrl,
        [{"client_id", client_id()}] ++ 
            case Scope of undefined -> []; _ -> [{"scope", Scope}] end,
        [])).

handle_device_code_grant_response({ok, {{_Version, 200, _Msg}, _Header, Body}}) ->
    #{<<"device_code">> := DeviceCode,
      <<"verification_url">> := VerificationUrl,
      <<"expires_in_s">> := ExpiresIn,
      <<"interval_s">> := Interval} = jiffy:decode(Body, [return_maps]),
    {ok, DeviceCode, VerificationUrl, now_seconds() + ExpiresIn, Interval};
handle_device_code_grant_response({ok, Res}) ->
    {error, {unexpected_response, Res}};
handle_device_code_grant_response({error, Reason}) ->
    {error, {httpc, Reason}}.

tokens_with_device_code(DeviceCode) ->
    handle_token_endpoint_response(
      rdio_api_requester_manager:request(
        ?RdioTokenEndpointUrl,
        [{"grant_type", "device_code"},
         {"device_code", DeviceCode}],
        [basic_http_auth_client_verification_header()])).

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

-spec authorization_url(string(), string(), string() | undefined, string() | undefined) -> string().
authorization_url(Type, RedirectUri, Scope, State) ->
    ?RdioAuthorizationEndpointUrl ++ "?" ++
        rdio_api_request:uri_params_encode(
          [{"response_type", Type},
           {"client_id", client_id()},
           {"redirect_uri", RedirectUri}] ++
              case Scope of undefined -> []; _ -> [{"scope", Scope}] end ++
              case State of undefined -> []; _ -> [{"state", State}] end).

tokens_with_refresh_token_and_request_fun(RefreshToken, RequestFun) ->
    handle_token_endpoint_response(
      RequestFun(
        ?RdioTokenEndpointUrl,
        [{<<"grant_type">>, <<"refresh_token">>},
         {<<"refresh_token">>, RefreshToken}],
        [basic_http_auth_client_verification_header()])).

-type tokens_error() :: {rdio, binary(), binary()} | {unexpected_response, any()} | {httpc, any()}.
-type maybe_tokens() :: {ok, tokens()} | {error, tokens_error()}.

-spec handle_token_endpoint_response(any()) -> maybe_tokens().
handle_token_endpoint_response({ok, {{_Version, 200, _Msg}, _Header, Body}}) ->
    #{<<"token_type">> := <<"bearer">>,
      <<"access_token">> := AccessToken,
      <<"refresh_token">> := RefreshToken,
      <<"expires_in">> := AccessTokenLifetime} = jiffy:decode(Body, [return_maps]),
    ExpiresAt = now_seconds() + AccessTokenLifetime,
    {ok, #tokens{access_token = binary_to_list(AccessToken),
                 refresh_token = binary_to_list(RefreshToken),
                 expires = ExpiresAt}};
handle_token_endpoint_response({ok, {{_Version, 400, _Msg}, _Header, Body}}) ->
    #{<<"error">> := Error,
      <<"error_description">> := ErrorDescription} = jiffy:decode(Body, [return_maps]),
    {error, {rdio, Error, ErrorDescription}};
handle_token_endpoint_response({ok, Res}) ->
    {error, {unexpected_response, Res}};
handle_token_endpoint_response({error, Reason}) ->
    {error, {httpc, Reason}}.

now_seconds() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.

basic_http_auth_client_verification_header() ->
    {"Authorization", "Basic " ++ base64:encode_to_string(client_id() ++ ":" ++ client_secret())}.
