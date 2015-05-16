%% @doc These are the primary system calls available for an IvanOS application developer.
-module(ivanos).

-export([spawn_actor/3,publish/2]).

-include("ivanos.hrl").

%% @type ivanos_id() = binary().

-spec spawn_actor(ModuleName::atom(),FunctionName::atom(),ArgList::list())->IvanosID::binary().
%% @doc Spawns an IvanOS Actor execution context.
%%      <p>Command Line Example:</p>
%%      <code>1> Actor = ivanos:spawn(hot_potato,actor,[1,0,[]]).</code>
spawn_actor(ModuleName,Function,ArgList)->
    ok.


-spec publish(Target::ivanos_id(),Metadata::tuple())->Results::tuple().
%% @doc Attempts to publish legal metadata on a Target such as an IvanOS Actor, Flow, etc.
%%      Next line.
%% <p></p>
%% <b><pre>
%% Metadata = { actor, ActorMetadata }
%%
%% ActorMetadata = { restart_on_crash, Hot } |
%%                 { n_way_available,N, Election } | // not yet implemented
%%                 { scale_flow, Match, HighWaterMark, N} |   // not yet implemented
%%                 { scale_compute, HighWaterMark, N}   //not yet implemented
%% </pre></b>
%% == Actor Metadata ==
%% === { restart_on_crash, Hot } ===
%% <b><pre>Hot = true | false</pre></b>
%% Command Line Example:<p></p><code>
%% 1> ivanos:publish(Actor,{restart_on_crash,true}).
%% </code>

publish(Target,Metadata)->
    ok.
