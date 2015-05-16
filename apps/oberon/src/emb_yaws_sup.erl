-module(emb_yaws_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    YBed = {emb_yaws,
                    {emb_yaws, start_link, [self()]},
                    permanent, 5000, worker, [emb_yaws]},
    {ok, {{one_for_all, 0, 1}, [YBed]}}.
