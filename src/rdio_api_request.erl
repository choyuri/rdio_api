-module(rdio_api_request).

-export([request/1, uri_params_encode/1]).

%% ===================================================================
%% Raw function used to make a request.
%% ===================================================================

request({Url, Args, Headers}) ->
    Req = {Url, Headers, "application/x-www-form-urlencoded", uri_params_encode(Args)},
    httpc:request(post, Req, [], []).

%% ===================================================================
%% Encode URI Params
%% ===================================================================

uri_params_encode(Params) ->
    intercalate("&", [uri_join([K, V], "=") || {K, V} <- Params]).

intercalate(Sep, Xs) ->
    lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) ->
    [];
intersperse(_, [X]) ->
    [X];
intersperse(Sep, [X | Xs]) ->
    [X, Sep | intersperse(Sep, Xs)].

uri_join(Values, Separator) ->
    string:join(lists:map(fun encode/1, Values), Separator).

encode(Binary) when is_binary(Binary) ->
    http_uri:encode(binary_to_list(Binary));
encode(List) when is_list(List) ->
    http_uri:encode(List).
