-module(rdio_api_request).

%% Requester API
-export([request/1]).

%% ===================================================================
%% Raw function used to make a request.
%% ===================================================================

request({Url, Args, Headers}) ->
    Req = {Url, Headers, "application/x-www-form-urlencoded", uri_params_encode(Args)},
    httpc:request(post, Req, [], []).

%% ===================================================================
%% Encode URI Params
%% via https://github.com/tim/erlang-oauth/blob/master/src/oauth.erl
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
    string:join(lists:map(fun uri_encode/1, Values), Separator).

uri_encode(Term) when is_integer(Term) ->
    integer_to_list(Term);
uri_encode(Term) when is_atom(Term) ->
    uri_encode(atom_to_list(Term));
uri_encode(Term) when is_binary(Term) ->
    uri_encode(binary_to_list(Term));
uri_encode(Term) when is_list(Term) ->
    uri_encode(lists:reverse(Term, []), []).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

uri_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
    uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
    NewAcc = [$%, dec2hex(X bsr 4), dec2hex(X band 16#0f) | Acc],
    uri_encode(T, NewAcc);
uri_encode([], Acc) ->
    Acc.

dec2hex(N) when N >= 10 andalso N =< 15 ->
    N + $A - 10;
dec2hex(N) when N >= 0 andalso N =< 9 ->
    N + $0.
