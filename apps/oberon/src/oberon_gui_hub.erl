%%------------------------------------------------------------------------------
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%%
%% @doc
%% Attach point and data distribution and control hub for
%% GUI clients. Receives data from the backend
%% and pushes the data to the GUI clients.

-module(oberon_gui_hub).

-behavior(gen_server).

-export([start_link/0,
         new_client/1,
	 new_tick/1,
	 new_tick_order_book/1,
	 new_exp/1,
	 new_strike_sort/1,
	 new_session/1,
	 check_session/2,
	 authenticate_session/4,
	 logoff_session/3,
	 delete_session_by_pid/1,
	 delete_session_by_cookie/1,
	 show_sessions/0,
	 broadcast/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("oberon_logger.hrl").

-define(STATE, gui_client_hub_state).
-record(?STATE,{start_time,
                clients = [],
		sessions = [],
		options_store}).

-define(SESSION, gui_client_hub_session).
-record(?SESSION,{pid = unbound,
		  cookie = unbound,
		  permissions = unbound,
		  session_bin = unbound}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_client(Pid) ->
    gen_server:cast(?MODULE, {new_client, Pid}).

new_session(Cookie) ->
    gen_server:cast(?MODULE, {new_session, Cookie}).

authenticate_session(Pid,Cookie,Permission,SessionId) ->
    gen_server:cast(?MODULE, {authenticate_session, {Pid,Cookie,Permission,SessionId}}).

logoff_session(Pid,Cookie,SessionId) ->
    gen_server:cast(?MODULE, {logoff_session, {Pid,Cookie,SessionId}}).
    

check_session(Pid,Cookie)->
    gen_server:cast(?MODULE, {check_session, {Pid,Cookie}}).

delete_session_by_pid(Pid)->
    gen_server:cast(?MODULE, {delete_session_by_pid, Pid}).

delete_session_by_cookie(Cookie)->
    gen_server:cast(?MODULE, {delete_session_by_pid, Cookie}).

show_sessions()->
    gen_server:cast(?MODULE, show_sessions).

new_tick(skip)->
    ok;
new_tick(Msg)->
    %%?INFO("NBBO Msg=(~p)~n", [ Msg ]),
    gen_server:cast(?MODULE, {new_tick, Msg}).

new_tick_order_book(Msg)->
    gen_server:cast(?MODULE, {new_tick_order_book, Msg}).

new_strike_sort(Msg)->
    gen_server:cast(?MODULE, {new_strike_sort, Msg}).

new_exp(NewExp)->
    gen_server:cast(?MODULE, {new_exp, NewExp}).

broadcast(Msg)->
    gen_server:cast(?MODULE, {broadcast, Msg}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([])->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{options_store = dict:new()}}.

% for debugging
handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    StartTime = calendar:universal_time(),
    Time = list_to_binary(rfc3339(StartTime)),
    {noreply, State#?STATE{start_time = StartTime}};
handle_cast({new_client, Pid}, State = #?STATE{clients = Clients,
                                               start_time = StartTime}) ->
    monitor(process, Pid),
    ?DEBUG("gui_client_hub: new client ~p~n",[Pid]),
    HELLO = jiffy:encode({[{<<"start_time">>,
                                list_to_binary(rfc3339(StartTime))},
                           {<<"current_time">>,
                                list_to_binary(rfc3339(calendar:universal_time()))}]}),
    %%gui_client_sock:send(Pid, HELLO),
    {noreply, State#?STATE{clients = [{Pid,<<"AAPL">>,<<"150320">>,<<"low_to_high">>} | Clients]}};
handle_cast(show_sessions, State = #?STATE{sessions = Sessions}) ->
    ?DEBUG("gui_client_hub: show_sessions ~p~n",[Sessions]),
    {noreply, State};
handle_cast({new_session, Cookie}, State = #?STATE{sessions = Sessions}) ->
    ?DEBUG("gui_client_hub: new session ~p~n",[Cookie]),
    case lists:keyfind(Cookie,3,Sessions) of
	#?SESSION{pid = AnyPid, cookie = Cookie, permissions = Permissions} ->
	    ?DEBUG("gui_client_hub: session exists Pid = ~p, Cookie = ~p, Permissions = ~p~n",[AnyPid,Cookie,Permissions]),
	    {noreply, State};
	_ ->
	    ?DEBUG("gui_client_hub: starting new session Cookie = ~p~n",[Cookie]),
	    {noreply, State#?STATE{sessions = [#?SESSION{cookie = Cookie}| Sessions]}}
    end;
handle_cast({check_session, {Pid,Cookie}}, State = #?STATE{sessions = Sessions}) ->
    ?DEBUG("gui_client_hub: check session, Pid = ~p Cookie = ~p~n",[Pid,Cookie]),
    case lists:keyfind(Cookie,3,Sessions) of
	#?SESSION{pid = unbound, cookie = Cookie, permissions = Permissions} ->
	    ?DEBUG("gui_client_hub: unbound session Cookie = ~p, Permissions = ~p~n",[Cookie,Permissions]),
	    Pid ! unbound,
 	    {noreply, State};
	#?SESSION{pid = AnyPid, cookie = Cookie, permissions = Permissions} ->
	    ?DEBUG("gui_client_hub: session Pid = ~p, Cookie = ~p, Permissions = ~p~n",[AnyPid,Cookie,Permissions]),
	    Pid ! {permission,Permissions},
	    {noreply, State};
	_ -> 
	    ?DEBUG("gui_client_hub: BAD SESSION! ~p~n",[Cookie]),
	    Pid ! no_auth,
	    {noreply, State}
   end;
handle_cast({authenticate_session, {Pid,Cookie,Permissions,SessionBin}}, State = #?STATE{sessions = Sessions}) ->
    ?DEBUG("gui_client_hub: authenticate session SessionBin = ~p, Cookie = ~p, Permission = ~p~n",[SessionBin,Cookie,Permissions]),
    case lists:keyfind(Cookie,3,Sessions) of
	Session = #?SESSION{pid = unbound, cookie = Cookie, permissions = unbound} ->
	    case SessionBin of
		unknown -> 	 
		    ?DEBUG("gui_client_hub: No existing Session for Cookie = ~p",[Cookie]),
		    {noreply,State};
		_ ->
		    ?DEBUG("gui_client_hub: binding Pid ~p to Cookie = ~p, Permission = ~p~n",[Pid,Cookie,Permissions]),
		    TempList = lists:delete(Session, Sessions),
		    NewSessions = [Session#?SESSION{pid = Pid,permissions = Permissions,session_bin = SessionBin}|TempList],
		    gui_client_sock:send(Pid,jiffy:encode({[{<<"session">>,SessionBin}]})),
		    {noreply, State#?STATE{sessions = NewSessions}}
	    end;
	Session = #?SESSION{cookie = Cookie} -> 
	    ?DEBUG("gui_client_hub: resuming session Pid = ~p, Cookie = ~p, Permission = ~p, SessionBin = ~p~n",
		   [Session#?SESSION.pid,Cookie,Session#?SESSION.permissions,Session#?SESSION.session_bin]),
	    TempList = lists:delete(Session, Sessions),
	    NewSessions = [Session#?SESSION{pid = Pid,session_bin = Session#?SESSION.session_bin}|TempList],
	    gui_client_sock:send(Pid,jiffy:encode({[{<<"session">>,Session#?SESSION.session_bin}]})),
	    {noreply, State#?STATE{sessions = NewSessions}};
   	_ -> 	    
	    ?DEBUG("gui_client_hub: no session found for Cookie = ~p in Sessions = ~p~n",[Cookie,Sessions]),
	    {noreply,State}
    end;
handle_cast({logoff_session, {Pid,Cookie,SessionId}}, State = #?STATE{sessions = Sessions}) ->
    ?DEBUG("gui_client_hub: logoff session Pid = ~p, Cookie = ~p, SessionId = ~p~n",[Pid,Cookie,SessionId]),
    case lists:keyfind(Cookie,3,Sessions) of
	Session = #?SESSION{pid = Pid, cookie = Cookie} ->
	    ?DEBUG("gui_client_hub: logging off Pid ~p to Cookie = ~p, SessionId = ~p~n",[Pid,Cookie,SessionId]),
	    TempList = lists:delete(Session, Sessions),
	    NewSessions = [#?SESSION{cookie = Cookie}|TempList],
	    {noreply, State#?STATE{sessions = NewSessions}};
	Session = #?SESSION{cookie = Cookie} -> 
	    ?DEBUG("gui_client_hub: BAD SESSION! Pid = ~p, Cookie = ~p, SessionId = ~p~n",[Pid,Cookie,SessionId]),
	    {noreply,State};
   	_ -> {noreply,State}
    end;
handle_cast({delete_session_by_cookie,Cookie}, State = #?STATE{sessions = Sessions})->
    case lists:keyfind(Cookie,3,Sessions) of
	Session = #?SESSION{cookie = Cookie} ->
	    ?DEBUG("gui_client_hub: Deleting (by Cookie) Session Pid = ~p, Cookie = ~p, Permission = ~p~n",[Session#?SESSION.pid,Cookie,Session#?SESSION.permissions]),
	    NewSessions = lists:delete(Session, Sessions),
	    {noreply, State#?STATE{sessions = NewSessions}};
	_ -> {noreply,State}
    end;
handle_cast({delete_session_by_pid,Pid}, State = #?STATE{sessions = Sessions})->
    case lists:keyfind(Pid,1,Sessions) of
	Session = #?SESSION{pid = Pid} ->
	    ?DEBUG("gui_client_hub: Deleting (by Pid) Session Pid = ~p, Cookie = ~p, Permission = ~p~n",[Pid,Session#?SESSION.cookie,Session#?SESSION.permissions]),
	    NewSessions = lists:delete(Session, Sessions),
	    {noreply, State#?STATE{sessions = NewSessions}};
	_ -> {noreply,State}
    end;
handle_cast({broadcast, Msg}, State = #?STATE{clients = Clients}) ->
    broadcast_msg(Clients,Msg),
    {noreply,State};
handle_cast({new_tick, Msg}, State = #?STATE{clients = Clients,options_store = CurStore}) ->
    NewStore = seatbelt_options_store:add_option(Msg,CurStore),
    
    [new_tick_client_update(Client,NewStore) || Client <- Clients],
    %%broadcast_msg(Clients,Msg),
    {noreply,State#?STATE{options_store = NewStore}};
handle_cast({new_tick_order_book, Msg}, State = #?STATE{clients = Clients}) ->
    %?INFO("Callback=(~p)~n", [ Msg ]),
    new_tick_order_book_msg(Clients,Msg),
    {noreply,State};
handle_cast({new_exp,Tuple}, State = #?STATE{clients = Clients, options_store = Store}) ->
    {Pid,NewExp} = Tuple,
    {Pid,UnderL,_Exp,Sort} = lists:keyfind(Pid,1,Clients),
    NewClient = {Pid,UnderL,NewExp,Sort},
    NewClients = lists:keystore(Pid,1,Clients,NewClient),
    new_tick_client_update(NewClient,Store),
    {noreply,State#?STATE{clients = NewClients}};
handle_cast({new_strike_sort,Tuple}, State = #?STATE{clients = Clients, options_store = Store}) ->
    {Pid,NewSort} = Tuple,
    {Pid,UnderL,Exp,_Sort} = lists:keyfind(Pid,1,Clients),
    NewClient = {Pid,UnderL,Exp,NewSort},
    NewClients = lists:keystore(Pid,1,Clients,NewClient),
    new_tick_client_update(NewClient,Store),
    {noreply,State#?STATE{clients = NewClients}};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info({'DOWN', _, process, Pid, _}, State = #?STATE{clients = Clients}) ->
    ?DEBUG("client down ~p~n", [Pid]),
    {noreply, State#?STATE{clients = lists:delete(Pid, Clients)}};
handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%------------------------------------------------------------------------------
% local functions
%------------------------------------------------------------------------------

broadcast_msg(Clients, Msg) ->
    [gui_client_sock:send(C, Msg) || C <- Clients].

new_tick_order_book_msg(Clients, ok) -> ok;

new_tick_order_book_msg(Clients, Msg) ->
    %?INFO("SEND: (~p)~n", [ Msg ]),
    [gui_client_sock:send(Pid, jiffy:encode({ Msg })) || {Pid,_UnderL,_Exp,Sort} <- Clients].

new_tick_client_update({_Pid,_UnderL,no_select,_Sort} = _Client,_Options)->
    no_data;
new_tick_client_update(Client,Options)->
    {Pid,_UnderL,_Exp,_Sort} = Client,
        ?INFO("NBBO Options = ~p",[Options]),
    Data = seatbelt_options_store:select_to_gui(Options,Client),
    ?INFO("NBBO Table = ~p",[Data]),
    case Data of
	[] -> no_data;
	_ -> gui_client_sock:send(Pid,jiffy:encode(Data)),
	     ok
    end.


    
now() ->
    os:timestamp().

since(A) ->
    diff(gui_client_hub:now(), A).

diff(A, B) ->
    diff_millis(A, B) div 1000.

diff_millis(A, B) ->
    timer:now_diff(A, B) div 1000.

universal(T) ->
    calendar:now_to_universal_time(T).

rfc3339({{Year, Month, Day}, {Hour, Minute, Second}})->
    lists:flatten(
        io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ",
            [Year, Month, Day, Hour, Minute, Second])).

rfc3339_to_epoch(Timestamp)->
    {ok, [Year,Month,Day,Hour,Minute,Second],[]} =
                        io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2dZ", Timestamp),
    {{Year,Month,Day},{Hour, Minute, Second}}.
