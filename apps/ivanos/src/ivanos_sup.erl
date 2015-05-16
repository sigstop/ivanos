-module(ivanos_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    EmbYawsSup = {emb_yaws_sup,
		  {emb_yaws_sup, start_link, []},
		  permanent, infinity, supervisor, [emb_yaws_sup]},
    IvanosGuiHub = {gui_client_hub,
		    {gui_client_hub, start_link, []},
		    permanent, 5000, worker, [ivanos_gui_hub]},
    Children = [%%EmbYawsSup,
		%%IvanosGuiHub
	       ],
    {ok, {{one_for_one, 5, 10}, Children}}.

