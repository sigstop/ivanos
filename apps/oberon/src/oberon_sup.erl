-module(oberon_sup).

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
    OberonGuiHub = {oberon_gui_hub,
		    {oberon_gui_hub, start_link, []},
		    permanent, 5000, worker, [oberon_gui_hub]},
    Children = [EmbYawsSup,
		OberonGuiHub
	       ],
    {ok, {{one_for_one, 5, 10}, Children}}.

