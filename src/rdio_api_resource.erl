%% Make requests to the 'real' API aka the resource server.

-module(rdio_api_resource).

-export([request_fun/1]).

-define(RdioAPIURL, "https://services.rdio.com/api/1/").

request_fun(RequestFun) ->
    fun (Method, Args, Tokens) ->
            api_request_with_retries(Method, Args, Tokens, RequestFun, 10)
    end.

-type args() :: [{string(), string()}].
-type request_error() :: {error,
                          #{refresh_tokens_error => rdio_api_authorization:tokens_error()} |
                          #{httpc_request_error => any(),
                            tokens => rdio_api_authorization:tokens()} |
                          #{unexpected_response => any(),
                            tokens => rdio_api_authorization:tokens()} |
                          #{rdio_error => binary(),
                            tokens => rdio_api_authorization:tokens()}}.
-type request_return() :: {ok, #{}, rdio_api_authorization:tokens()} | request_error().

-spec api_request_with_retries(string(), args(), rdio_api_authorization:tokens(), rdio_api_requester_manager:request_fun(), non_neg_integer()) -> request_return().
api_request_with_retries(Method, Args, Tokens, RequestFun, Retry) ->
    handle_api_request_try(
      api_request_with_fresh_tokens_and_request_fun(
        Method, Args, rdio_api_authorization:refresh_tokens_when_expired_with_request_fun(Tokens, RequestFun), RequestFun),
      Retry,
      Method, Args, Tokens, RequestFun).

-spec handle_api_request_try(request_return(), non_neg_integer(), string(), args(), rdio_api_authorization:tokens(), rdio_api_requester_manager:request_fun()) -> request_return().
handle_api_request_try({error, #{unexpected_response := {{_Version, 401, _Msg}, _Header, Body},
                                 tokens := FreshTokens}} = Return,
                       Retry,
                       Method, Args, _Tokens, RequestFun) when Retry > 0 ->
    case catch jiffy:decode(Body, [return_maps]) of
        #{<<"error">> := <<"invalid_token">>} ->
            handle_api_request_try(
              api_request_with_fresh_tokens_and_request_fun(
                Method, Args, rdio_api_authorization:refresh_tokens_with_request_fun(FreshTokens, RequestFun), RequestFun),
              Retry-1,
              Method, Args, FreshTokens, RequestFun);
        _ ->
            Return
    end;
handle_api_request_try(Return,
                       _Retry,
                       _Method, _Args, _Tokens, _RequestFun) ->
    Return.

-spec api_request_with_fresh_tokens_and_request_fun(string(), args(), rdio_api_authorization:maybe_tokens(), rdio_api_requester_manager:request_fun()) -> request_return().
api_request_with_fresh_tokens_and_request_fun(Method, Args, {ok, FreshTokens}, RequestFun) ->
    handle_api_httpc_return(
      RequestFun(
        ?RdioAPIURL,
        [{"method", Method}|Args],
        [bearer_access_token_http_auth_header(rdio_api_authorization:access_token(FreshTokens))]),
      FreshTokens);
api_request_with_fresh_tokens_and_request_fun(_Method, _Args, {error, Reason}, _RequestFun) ->
    {error, #{refresh_tokens_error => Reason}}.

handle_api_httpc_return({ok, {{_Version, Code, _Msg}, _Header, Body} = Result}, FreshTokens) ->
    handle_code(Code, Body, Result, FreshTokens);
handle_api_httpc_return({error, Reason}, FreshTokens) ->
    {error, #{httpc_request_error => Reason,
              tokens => FreshTokens}}.

%% TODO: With OAuth 2 error codes like 401 also seem to return valid JSON with
%% error information.
handle_code(200, Body, _Response, FreshTokens) ->
    handle_body(
      jiffy:decode(Body, [return_maps]),
      FreshTokens);
handle_code(_Code, _Body, Response, FreshTokens) ->
    {error, #{unexpected_response => Response,
              tokens => FreshTokens}}.

handle_body(#{<<"status">> := <<"ok">>,
              <<"result">> := Result}, FreshTokens) ->
    {ok, Result, FreshTokens};
handle_body(#{<<"status">> := <<"error">>,
              <<"message">> := ErrMsg}, FreshTokens) ->
    {error, #{rdio_error => ErrMsg,
              tokens => FreshTokens}}. % Rdio returned an error message.

%% Helper

bearer_access_token_http_auth_header(AccessToken) ->
    {"Authorization", "Bearer " ++ AccessToken}.
