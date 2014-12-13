-module(rdio_api_requester_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor Callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 60}, % Do not restart.
          [{rdio_api_requester,
            {rdio_api_requester, start_link, []},
            temporary, 5*60*1000, worker, [rdio_api_requester]}]}}.
