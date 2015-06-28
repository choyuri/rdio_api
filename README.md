# Rdio API

This is an Erlang wrapper for the [Rdio API](http://www.rdio.com/developers/). It automatically limits the number of requests according to the API's rate limit and refreshes the OAuth 2.0 access tokens when they expire.

## Config

Set your applications Client ID and Client Secret as environment variables in string format before you start the application.

```erl
application:set_env(rdio_api, client_secret, "my-client-secret-here"),
application:set_env(rdio_api, client_id, "my-client-id-here")
```

Optionally you may specify the rate limit for your app (default is 10 calls per second, `{10, 1000}`).

```erl
application:set_env(rdio_api, rate_limit, {MaxCalls, TimeframeInMilliseconds})
```

## Start

Start the application before making requests:

```erl
application:ensure_all_started(rdio_api)
```

## Usage

### Authorization

Using the `rdio_api_authorization:tokens_with_AuthorizationMethod` or `rdio_api_authorization:tokens/3` functions you can obtain the opaque type `tokens()`. You pass the type into every API request and get potentially refreshed tokens back.

#### Accessors

Manually create tokens or access the internals of the opaque type:

```erl
rdio_api_authorization:refresh_token(Tokens) -> string()
rdio_api_authorization:access_token(Tokens) -> string()
rdio_api_authorization:expires(Tokens) -> non_neg_integer()
rdio_api_authorization:tokens(RefreshToken :: string(), AccessToken :: string(), ExpirationTimestamp :: non_neg_integer()) -> tokens()
```

#### Authorization Code Grant

Obtain the URL you have to redirect the user to. You can also obtain the Client ID usign `rdio_api_authorization:client_id() -> string()` and construct the URL manually.

```erl
rdio_api_authorization:authorization_url(RedirectUri :: string()) -> string()
```

When the user has allowed your application to access their account and you have received the authorization code, turn it into the opque tokens type:

```erl
rdio_api_authorization:tokens_with_authorization_code(Code :: string(), RedirectUri :: string()) -> {ok, tokens()} | {error, Reason}
```

### Requests

A simple API request:

```erl
rdio_api:request(Method :: string(), Arguments :: [{Key :: string(), Value :: string()}], Tokens :: tokens()) -> {ok, MethodResult :: map(), NewTokens :: tokens()} | {error, #{ErrorType => ErrorReason} | #{tokens => NewTokens, ErrorType => ErrorReason}}
```

Sometimes you may want to perform multiple requests in quick succession, for that you can use:

```erl
rdio_api:run(fun (Request) ->
    {ok, _MethodResult, Tokens2} = Request(Method1, Args1, Tokens1),
    {ok, _MethodResult, Tokens3} = Request(Method2, Args2, Tokens2),
    ...
end)
```

`Request` takes the same arguments as `rdio_api:request/3`. The maximal (forced) delay between the requests is the timeframe specified in the applications rate limit, e.g. one second for regular applications. Note that this method may, under low load, actually be slower then calling `rdio_api:request/3` multiple times. Anyway, if you want a garentee, use this function.
