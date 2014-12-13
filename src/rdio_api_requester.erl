-module(rdio_api_requester).

%% API
-export([start_link/0, request/2]).
%% Gen Rate Limiter
-behaviour(gen_rate_limiter).
-export([init/1, before_run/1, run/2, to_wait/1]).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_rate_limiter:start_link(?MODULE, undefined).

request(Pid, Req) ->
	gen_rate_limiter:run(Pid, Req).

%% ===================================================================
%% Rate Limiter
%% ===================================================================

init(_State) -> 
    {ok, {_, Pause}} = application:get_env(rate_limit),
    Pause.

before_run(Pause) ->
    {timestamp_ms(), Pause}.

run(Req, State) ->
    {rdio_api_request:request(Req),
     State}.

to_wait({Started, Pause}) ->
    {Pause - (timestamp_ms() - Started),
     Pause}.

%% TODO fix when new OTP release, now() is bad for timespans
timestamp_ms() ->
    {Mega, Sec, Micro} = now(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).
