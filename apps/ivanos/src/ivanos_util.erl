-module(ivanos_util).

-export([pid2ivanos_id/1]).

-include("ivanos.hrl").

%% @type ivanos_id() = binary().

-spec pid2ivanos_id(Pid::pid())->ivanos_id().
pid2ivanos_id(Pid)->
    Bin = erlang:md5(erlang:pid_to_list(Pid)),
    <<"ivanos_actor/",Bin/binary>>.
