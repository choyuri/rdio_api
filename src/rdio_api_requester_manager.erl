-module(rdio_api_requester_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, request/3, run_with_request_fun/1]).
%% Gen Server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================================================================
%% API
%% ===================================================================

request(Url, Args, Headers) ->
    run_with_request_fun(
      fun (RequestFun) ->
              RequestFun(Url, Args, Headers)
      end).

run_with_request_fun(Fun) ->
    {RequestFun, Ref} = gen_server:call(?MODULE, borrow, infinity),
    Return = Fun(RequestFun),
    gen_server:cast(?MODULE, {return, Ref}),
    Return.

%% ===================================================================
%% Supervisor API
%% ===================================================================

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    gen_server:cast(?MODULE, {init, self()}),
    {ok, Pid}.

%% ===================================================================
%% Gen Server
%% ===================================================================

init([]) ->
    register(?MODULE, self()),
    {ok, undefined}.

handle_call(borrow, From, {Queue, Rs, Given}) ->
    share(queue:in(From, Queue), Rs, Given).

handle_cast({init, Sup}, _State) ->
    RSup = rdio_api_sup:start_requester_sup(Sup),
    {ok, {RequesterCount, _}} = application:get_env(rate_limit),
    Rs = repeat(
           RequesterCount,
           fun () ->
                   {ok, Pid} = supervisor:start_child(RSup, []),
                   req_fun(Pid)
           end),
    {noreply, {queue:new(), Rs, maps:new()}};

handle_cast({return, MRef}, {Queue, Rs, Given}) ->
    demonitor(MRef, [flush]),
    take(MRef, Queue, Rs, Given).

handle_info({'DOWN', MRef, process, _Pid, _Reason},
            {Queue, Rs, Given}) ->
    take(MRef, Queue, Rs, Given).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ===================================================================
%% Private
%% ===================================================================

req_fun(Pid) ->
    fun (Url, Args, Headers) ->
            rdio_api_requester:request(Pid, {Url, Args, Headers})
    end.

take(MRef, Queue, Rs, Given) ->
    R = maps:get(MRef, Given),
    Given2 = maps:remove(MRef, Given),
    share(Queue, [R|Rs], Given2).

share(Queue, Rs, Given) ->
    case {queue:out(Queue), Rs} of
        {{{value, From}, Queue2}, [R|Rs2]} ->
            share(Queue2, Rs2, give(From, R, Given));
        _ ->
            {noreply, {Queue, Rs, Given}}
    end.

give({Pid, _Tag} = From, R, Given) ->
    MRef = monitor(process, Pid),
    gen_server:reply(From, {R, MRef}),
    maps:put(MRef, R, Given).

%% Run Fun N times and return results as list.
repeat(N, Fun) ->
    repeat(N, Fun, []).

repeat(0, _, Response) ->
    Response;
repeat(N, Fun, Response) ->
    repeat(N-1, Fun, [Fun()|Response]).
