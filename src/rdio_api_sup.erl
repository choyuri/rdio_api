-module(rdio_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_requester_sup/1]).
%% Supervisor Callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
	{ok, {{one_for_all, 5, 60},
		  [{rdio_api_requester_manager,
            {rdio_api_requester_manager, start_link, []},
            permanent, 5000, worker, [rdio_api_requester_manager]}]}}.

start_requester_sup(Sup) ->
    Spec = {rdio_api_requester_sup,
            {rdio_api_requester_sup, start_link, []},
            temporary, 5000, supervisor, [rdio_api_requester_sup]},
    {ok, RSup} = supervisor:start_child(Sup, Spec),
    RSup.
