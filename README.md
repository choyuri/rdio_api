# Rdio API

[![Build Status](https://travis-ci.org/luisgerhorst/rdio_api.svg)](https://travis-ci.org/luisgerhorst/rdio_api)

This is an Erlang wrapper for the [Rdio API](http://www.rdio.com/developers/). It automatically limits the number of requests according to the API's rate limit and refreshes the OAuth 2.0 access tokens when they expire.

## Config

Set your applications Client ID and Client Secret as environment variables in string format before you start the application.

```erl
application:set_env(rdio_api, client_id, "my-client-id-here")
application:set_env(rdio_api, client_secret, "my-client-secret-here"),
```

Optionally, you may specify the rate limit for your app (default is 10 calls per second, `{10, 1000}`).

```erl
application:set_env(rdio_api, rate_limit, {MaxCalls, TimeframeInMilliseconds})
```

## Start

Start the application before making requests:

```erl
application:ensure_all_started(rdio_api)
```

## Usage

### `rdio_api_authorization`

Using the `tokens_with_AuthorizationMethod` or `tokens/3,5` functions you can obtain the opaque type `tokens()`. You pass the type into every API request and get potentially refreshed tokens back.

#### Types

```erl
-opaque tokens().

-type refresh_token() :: string().
-type access_token() :: string().

-type client_id() :: string().
-type client_secret() :: string().
```

In addition to that, the following types apply in the documentation:

```erl
TokenEndpointResponse = {ok, tokens()} | {error, {rdio, Error :: binary(), ErrorDescription :: binary()} | {unexpected_response, HttpcRequestResult} | {httpc, HttpcErrorReason}}
```

where `HttpcRequestResult` is the second part of the `ok`-Tuple returned by `httpc:request/1,2,4,5` and `HttpcErrorReason` the second part of the `error`-Tuple. `{rdio, Error, ErrorDescription}` represents an error returned by the Rdio API, as documented [here](http://www.rdio.com/developers/docs/web-service/oauth2/overview/ref-failure).

#### Accessors

Manually create tokens or access the internals of the opaque type:

```erl
refresh_token(Tokens) -> undefined | string()
access_token(Tokens) -> string()
expires(Tokens) -> non_neg_integer()
scope(Tokens) -> undefined | string()
grant(Tokens) -> other | client_credentials
tokens(RefreshToken :: undefined | string(), AccessToken :: string(), ExpirationTimestamp :: non_neg_integer()) -> tokens()
tokens(RefreshToken :: undefined | string(), AccessToken :: string(), ExpirationTimestamp :: non_neg_integer(), Scope :: undefined | string(), Grant :: other | client_credentials) -> tokens()
```

In case you need them:

```erl
client_id() -> string()
client_secret() -> string()
```

#### Authorization Code Grant

Obtain the URL you have to redirect the user to. Note that you can also obtain the Client ID usign `rdio_api_authorization:client_id/0)` and construct the URL manually.

```erl
code_authorization_url(RedirectUri :: string()) -> string()
code_authorization_url(RedirectUri :: string(), Scope :: string() | undefined) -> string()
code_authorization_url(RedirectUri :: string(), Scope :: string() | undefined, State :: string() | undefined) -> string()
```

When the user has allowed your application to access their account and you have received the authorization code, turn it into the opaque tokens type:

```erl
tokens_with_authorization_code(Code :: string(), RedirectUri :: string()) -> TokenEndpointResponse
```

#### Implicit Grant

Get the URL you have to redirect the user to:

```erl
token_authorization_url(RedirectUri :: string()) -> string()
token_authorization_url(RedirectUri :: string(), Scope :: string() | undefined) -> string()
token_authorization_url(RedirectUri :: string(), Scope :: string() | undefined, State :: string() | undefined) -> string()
```

#### Resource Owner Credential

After polling the user for his username and password, you can retreive the tokens:

```erl
tokens_with_resource_owner_credentials(Username :: string(), Password :: string()) -> TokenEndpointResponse
tokens_with_resource_owner_credentials(Username :: string(), Password :: string(), Scope :: string() | undefined) -> TokenEndpointResponse
```

#### Client Credentials

```erl
tokens_with_client_credentials() -> TokenEndpointResponse
tokens_with_client_credentials(Scope :: string() | undefined) -> TokenEndpointResponse
```

#### Device Code Grant

```erl
start_device_code_grant() -> Return
start_device_code_grant(Scope :: string() | undefined) -> Return
```

where

```erl
Return = {ok, DeviceCode :: binary(), VerificationUrl :: binary(), ExpirationTimestamp, PollingInterval} | {error, {unexpected_response, HttpcRequestResult} | {httpc, HttpcErrorReason}}
```

Note that `ExpirationTimestamp` is not the number of seconds until the device code expires, but instead the Unix time (in seconds, obtained with `now/0`) when it is expected to expire.

You can poll the token endpoint with:

```erl
tokens_with_device_code(DeviceCode :: binary() | string()) -> TokenEndpointResponse
```

### `rdio_api`

A simple API request:

```erl
request(Method :: string(), Arguments :: [{Key :: string(), Value :: string()}], Tokens :: tokens()) -> {ok, MethodResult :: map(), NewTokens :: tokens()} | {error, #{ErrorType => ErrorReason} | #{tokens => NewTokens, ErrorType => ErrorReason}}
```

`MethodResult` is the parsed response from Rdio (using [`jiffy:decode/2`](https://github.com/davisp/jiffy#jiffydecode12) with `return_maps` option).

Sometimes you may want to perform multiple requests in quick succession, for that you can use:

```erl
run(fun (Request) ->
    {ok, _MethodResult, Tokens2} = Request(Method1, Args1, Tokens1),
    {ok, _MethodResult, Tokens3} = Request(Method2, Args2, Tokens2),
    ...
end)
```

`Request` behaves like `request/3`, but here the (forced) delay between the requests is _always_ the timeframe specified in the applications rate limit, e.g. one second for regular applications. Note that this method may under low load actually be slower then calling `request/3` multiple times. Anyway, if you want a guarantee, use this method.

## Examples

### Authorization Code Grant and API request

Create an app at [www.rdio.com/developers/your-apps](http://www.rdio.com/developers/your-apps/). Add `http://localhost/` as redirect URI.

```
$ cd /path/to/rdio_api
$ rebar get-deps
$ rebar compile
$ rebar shell
1> application:set_env(rdio_api, client_id, "MyAppsClientIDHere").
2> application:set_env(rdio_api, client_secret, "MyAppsClientSecretHere").
3> application:ensure_all_started(rdio_api).
4> RedirectUri = "http://localhost/".
5> rdio_api_authorization:authorization_url(RedirectUri).
```

Open the shown URL in your browser and allow your app to access your account. You will be redirected to a URL that looks like this `http://localhost/?code=Code`, copy the `Code` part to your clipboard and proceed.

```
6> Code = "MyCopiedCodeHere".
7> {ok, Tokens} = rdio_api_authorization:tokens_with_authorization_code(Code, RedirectUri).
8> {ok, #{<<"firstName">> := UserFirstNameBinary, <<"lastName">> := UserLastNameBinary}, Tokens2} = rdio_api:request("currentUser", [], Tokens).
```

# Todo

- Test device code grant and implicit grant
- Fix dialyzer types
