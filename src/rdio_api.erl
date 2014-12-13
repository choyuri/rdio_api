-module(rdio_api).

%% Application API
-export([request/3, run/1]).

%% ===================================================================
%% Resource API
%% ===================================================================

request(Method, Args, Tokens) ->
    run(
      fun (Request) ->
              Request(Method, Args, Tokens)
      end).

run(Fun) ->
    rdio_api_requester_manager:run_with_request_fun(
      fun (RequestFun) ->
              APIRequestFun = rdio_api_resource:request_fun(RequestFun),
              Fun(APIRequestFun)
      end).
