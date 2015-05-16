-module(ivanos_actor).

-export([spawn/3,publish/2]).


spawn(ModuleName,Function,ArgList)->
    Pid = erlang:spawn(ModuleName,Function,ArgList),
    ivanos_util:pid2ivanos_id(Pid).

publish(Target,Metadata)->
    ok.


