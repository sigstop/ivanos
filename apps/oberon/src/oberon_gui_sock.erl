
%% @doc Client Socket module - callback from yaws

-module(oberon_gui_sock).

-include("oberon_logger.hrl").

-export([handle_message/1, send/2, bin_to_float/1]).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

send(Pid,Message) when is_pid(Pid) ->
    %% turn off debug logging to stdout
    %%?DEBUG("Sending ~p to ~p~n",[Message,Pid]),
    yaws_api:websocket_send(Pid, {text, Message}).

%------------------------------------------------------------------------------
% yaws callback
%------------------------------------------------------------------------------

handle_message({text, <<"PING">>}) ->
    ?DEBUG("Received PING:~n"),
    {reply, {text, <<"PONG">>}};
handle_message({text, <<"SEATBELT_GUI">>}) ->
    ?DEBUG("Pid ~p Received SEATBELT_GUI:~n",[self()]),
    {reply, {text, seatbelt_test_gui:test_traders()}};
handle_message({text, <<"VOLATILITY_GUI">>}) ->
    ?DEBUG("Pid ~p Received VOLATILITY_GUI:~n",[self()]),
    send(self(),volatility_test_gui:test_tabledata()),
    noreply;
handle_message({text, <<"SYS2_GUI">>}) ->
    ?DEBUG("Pid ~p Received SYS2_GUI:~n",[self()]),
    gui_client_hub:new_client(self()),
    %%send(self(),sys2_test_gui:test_options("trades.log")),
    noreply;
handle_message({text, <<"DCM_GUI">>}) ->
    ?DEBUG("Pid ~p Received DCM_GUI:~n",[self()]),
    %%gui_client_hub:new_client(self()),
    %%send(self(),sys2_test_gui:test_options("trades.log")),
    noreply;
handle_message({text, MessageBits}) when is_bitstring(MessageBits) ->
    ?DEBUG("Received:~p~n",[MessageBits]),
    Message = jiffy:decode(MessageBits),
    ?DEBUG("Decoded:~p~n",[Message]),
    request(Message),
    noreply;
handle_message({close, _, _})->
    ?DEBUG("Closing:~p~n",[self()]),
    %%gui_client_hub:delete_session_by_pid(self()),
    % ignore close, not needed
    noreply;
handle_message(A)->
    ?INFO("UNKNOWN MESSAGE RECEIVED:~n~p~n",[A]),
    ?DEBUG("My Pid = ~p~n",[self()]),
    noreply.

%------------------------------------------------------------------------------
% local functions
%------------------------------------------------------------------------------
request({[{<<"request">>,<<"register">>},{<<"first_name">>,FirstNameBin},{<<"last_name">>,LastNameBin},{<<"company">>,CompanyBin},{<<"email">>,EmailBin},{<<"login">>,LoginBin},{<<"identity">>,IdentityBin},{<<"cookie">>,CookieBin}]})->
    AuthCred = list_to_integer(binary:bin_to_list(IdentityBin),16),
    ?DEBUG("Registration requested Login = ~p, AuthCred = ~p~n",[LoginBin,AuthCred]),
   case dcm_db:insert_dcm_prospect_with_cred(binary:bin_to_list(LoginBin), binary:bin_to_list(FirstNameBin), binary:bin_to_list(LastNameBin), binary:bin_to_list(CompanyBin), binary:bin_to_list(EmailBin), AuthCred ) of 
       success -> gui_client_sock:send(self(),jiffy:encode({[{<<"register">>,<<"success">>}]}));
       user_exists -> gui_client_sock:send(self(),jiffy:encode({[{<<"register">>,<<"user_exists">>}]}));
       _ -> ok
   end;
request({[{<<"request">>,<<"login">>},{<<"session">>,SessionBin},{<<"cookie">>,CookieBin}]})->
    Cookie = binary:bin_to_list(CookieBin),
    case SessionBin of
	<<"onload">> ->
	    gui_client_hub:authenticate_session(self(),Cookie,unknown,unknown);
	<<"unknown">> ->
	    gui_client_hub:authenticate_session(self(),Cookie,unknown,unknown);
	_ ->
	    SessionId = list_to_integer(binary:bin_to_list(SessionBin),16),
	    ?DEBUG("Login requested SessionId = ~p, Cookie = ~p~n",[SessionId,Cookie]),
	    case dcm_db:get_auth_by_session_cookie(SessionId,Cookie) of
		no_auth -> gui_client_sock:send(self(),jiffy:encode({[{<<"session">>,<<"no_auth">>}]}));
		pending -> gui_client_sock:send(self(),jiffy:encode({[{<<"session">>,<<"pending">>}]}));
		invalid -> gui_client_sock:send(self(),jiffy:encode({[{<<"session">>,<<"invalid">>}]}));
		Permissions -> gui_client_hub:authenticate_session(self(),Cookie,Permissions,SessionBin)
	    end
    end;
request({[{<<"request">>,<<"logoff">>},{<<"session">>,SessionBin},{<<"cookie">>,CookieBin}]})->
    Cookie = binary:bin_to_list(CookieBin),
    Session = binary:bin_to_list(SessionBin),
    ?DEBUG("Logging off Pid = ~p, Cookie = ~p, Session = ~p~n",[self(),Cookie,Session]),
    gui_client_hub:logoff_session(self(),Cookie,Session),
    gui_client_sock:send(self(),jiffy:encode({[{<<"session">>,<<"end">>}]}));
%%    Session = list_to_integer(binary:bin_to_list(SessionBin),16),
%%   case dcm_db:get_auth_by_session_cookie(Session,Cookie) of
%%	no_auth -> ok;
%%	Permissions -> gui_client_hub:logoff_session(self(),Cookie,Permissions,SessionBin)
request({[{<<"request">>,<<"contact_message">>},{<<"email">>,Email},{<<"msg">>,Msg}]})->
    ?DEBUG("Received contact message request from: ~p ~n",[Email]),
    seatbelt_smtp:send_contact_message(Email,Msg);
request({[{<<"request">>,<<"user_message">>},{<<"msg">>,Msg},{<<"session">>,SessionBin},{<<"cookie">>,CookieBin}]})->
    Session = list_to_integer(binary:bin_to_list(SessionBin),16),
    Cookie = binary:bin_to_list(CookieBin),
    ?DEBUG("Received user message request for Pid = ~p, Session = ~p Cookie = ~p~n",[self(),Session,Cookie]),
    UserName = dcm_db:get_user_by_session_cookie(Session, Cookie),
    seatbelt_smtp:send_user_message(UserName,Msg);
%%    end;
request({[{<<"request">>,<<"account_info">>},{<<"session">>,<<"bycookie">>},{<<"cookie">>,CookieBin}]})->
    Cookie = binary:bin_to_list(CookieBin),
    ?DEBUG("Received account info request for Pid = ~p, Session = <<\"bycookie\">>, Cookie = ~p~n",[self(),Cookie]);
request({[{<<"request">>,<<"show_users">>}]})->
    ?DEBUG("Received show user request~n"),
    Users = dcm_db:show_users(),
    gui_client_sock:send(self(),jiffy:encode({[{<<"show_users">>,binary:list_to_bin(Users)}]}));
request({[{<<"request">>,<<"show_prospects">>}]})->
    ?DEBUG("Received show prospects request~n"),
    Prospects = dcm_db:show_prospects(),
    gui_client_sock:send(self(),jiffy:encode({[{<<"show_prospects">>,binary:list_to_bin(Prospects)}]}));
request({[{<<"request">>,<<"reset_user">>},{<<"email">>,EmailBin}]})->
    ?DEBUG("Received reset user request for ~p~n",[EmailBin]),
    Result = dcm_db:user_to_lower(binary:bin_to_list(EmailBin)),
    case Result of
	success ->
	    gui_client_sock:send(self(),jiffy:encode({[{<<"reset_user">>,<<"success">>}]}));
	_ -> 
	    gui_client_sock:send(self(),jiffy:encode({[{<<"reset_user">>,<<"failed">>}]}))
    end;
request({[{<<"request">>,<<"delete_user">>},{<<"email">>,EmailBin}]})->
    ?DEBUG("Received delete user request for ~p~n",[EmailBin]),
    Result = dcm_db:delete_user_by_email(binary:bin_to_list(EmailBin)),
    case Result of
	success ->
	    gui_client_sock:send(self(),jiffy:encode({[{<<"delete_user">>,<<"success">>}]}));
	_ -> 
	    gui_client_sock:send(self(),jiffy:encode({[{<<"delete_user">>,<<"failed">>}]}))
    end;
request({[{<<"request">>,<<"delete_prospect">>},{<<"email">>,EmailBin}]})->
    ?DEBUG("Received delete prospect request for ~p~n",[EmailBin]),
    Result = dcm_db:delete_prospect_by_email(binary:bin_to_list(EmailBin)),
    case Result of
	success ->
	    gui_client_sock:send(self(),jiffy:encode({[{<<"delete_prospect">>,<<"success">>}]}));
	_ -> 
	    gui_client_sock:send(self(),jiffy:encode({[{<<"delete_prospect">>,<<"failed">>}]}))
    end;
request({[{<<"request">>,<<"change_password">>},{<<"password">>,PasswordBin},{<<"session">>,SessionBin},{<<"cookie">>,CookieBin}]})->
    Session = list_to_integer(binary:bin_to_list(SessionBin),16),
    Cookie = binary:bin_to_list(CookieBin),
    Password = binary:bin_to_list(PasswordBin),
    ?DEBUG("Received change password request for Pid = ~p, Session = ~p, Cookie = ~p, Password = ~p~n",[self(),Session,Cookie,Password]),
    Result = dcm_db:change_password_by_session_cookie(Session,Cookie,Password),
    case Result of
	{success,Login,Permissions} ->
	    IdentityInt = crypto:hash(md5,Login ++ Password),
	    Identity = string:to_lower(integer_to_list(crypto:bytes_to_integer(IdentityInt),16)),
	    NewSessionInt = crypto:hash(md5,Identity ++ Cookie),
	    NewSession = integer_to_list(crypto:bytes_to_integer(NewSessionInt),16),
	    NewSessionBin = binary:list_to_bin(string:to_lower(NewSession)),
	    gui_client_hub:authenticate_session(self(),Cookie,Permissions,NewSessionBin),
	    gui_client_sock:send(self(),jiffy:encode({[{<<"change_password">>,<<"success">>}]}));
	_ -> 
	    gui_client_sock:send(self(),jiffy:encode({[{<<"change_password">>,<<"failed">>}]}))
    end;
request({[{<<"request">>,<<"account_info">>},{<<"session">>,SessionBin},{<<"cookie">>,CookieBin}]})->
    Session = list_to_integer(binary:bin_to_list(SessionBin),16),
    Cookie = binary:bin_to_list(CookieBin),
    ?DEBUG("Received account info request for Pid = ~p, Session = ~p Cookie = ~p~n",[self(),Session,Cookie]),
    [ UserName ] = dcm_db:get_user_by_session_cookie(Session, Cookie),
    UserRecord = dcm_db:get_dcm_user_by_login(UserName),
    ?DEBUG("User Record:~p~n",[UserRecord]),
    %% HAIM TODO: Having trouble getting record into encode function
    %%[{dcm_user,Email,undefined,FirstName,LastName,Company,Email,_,_}] = UserRecord,
    [ {dcm_user,ULEmail,_,FirstName,LastName,Company,_,_,Permissions} ] = UserRecord,
    Output = "<PRE>First Name:" ++ FirstName ++ "\n" ++ "Last Name: " ++ LastName ++ "\n" ++ "Email: " ++ ULEmail ++ "\nCompany: " ++ Company ++ "\n" ++ "Relationship Manager: " ++ "Not Assigned\n<PRE>",
    ?DEBUG("Output:~p~n",[Output]),
    Encode = jiffy:encode({[{<<"account_info">>,list_to_binary(Output)}]}),
    gui_client_sock:send(self(),Encode);
%%    end;
request({[{<<"request">>,<<"passwordreset">>},{<<"email">>,EmailBin},{<<"cookie">>,CookieBin}]})->
    Email = binary:bin_to_list(EmailBin),
    Cookie = binary:bin_to_list(CookieBin),
    ?DEBUG("Password Reset Request: Email = ~p, Cookie = ~p~n",[Email,Cookie]),
    dcm_db:insert_dcm_pw_reset(Email);
request({[{<<"request">>,<<"pwd_change">>},{<<"token">>,TokenBin},{<<"identity">>,IdentityBin}]})->
    ?DEBUG("Password Change Request: token = ~p, identity = ~p~n",[TokenBin,IdentityBin]),
    Result = dcm_db:change_password(binary:bin_to_list(TokenBin),binary:bin_to_list(IdentityBin)),
    case Result of
	success ->
	    gui_client_sock:send(self(),jiffy:encode({[{<<"pwd_change">>,<<"success">>}]}));
	_ -> 
	    gui_client_sock:send(self(),jiffy:encode({[{<<"pwd_change">>,<<"failed">>}]}))
    end;
request({[{<<"request">>,<<"new_account">>},{<<"first_name">>,FirstNameBin},{<<"last_name">>,LastNameBin},{<<"company">>,CompanyBin},{<<"email">>,EmailBin}]})->
    ?DEBUG("New account requested Login = ~p, First Name = ~p, Last Name = ~p, Company = ~p, Email = ~p~n",[EmailBin,FirstNameBin,LastNameBin,CompanyBin,EmailBin]),
    case dcm_db:add_dcm_user_direct(binary:bin_to_list(EmailBin), binary:bin_to_list(FirstNameBin), binary:bin_to_list(LastNameBin), binary:bin_to_list(CompanyBin), binary:bin_to_list(EmailBin)) of 
       success -> gui_client_sock:send(self(),jiffy:encode({[{<<"new_account">>,<<"success">>}]}));
	user_exists -> gui_client_sock:send(self(),jiffy:encode({[{<<"new_account">>,<<"user_exists">>}]}));
       _ -> ok
   end;
request({[{<<"request">>,<<"new_sort">>},{<<"strike_sort">>,NewSort}]})->
    ?DEBUG("New strike_sort requested ~p~n",[NewSort]),
    gui_client_hub:new_strike_sort({self(),NewSort});
request({[{<<"request">>,<<"new_exp">>},{<<"exp">>,<<>>}]})->
    ?DEBUG("Empty exp requested~n");
request({[{<<"request">>,<<"new_exp">>},{<<"exp">>,NewExp}]})->
    ?DEBUG("New exp ~p requested~n",[NewExp]),
    gui_client_hub:new_exp({self(),NewExp});
request({[{<<"request">>,<<"orders">>},{<<"Account1">>,Account1}]})->
    ?DEBUG("Orders for ~p requested~n",[Account1]),
    Orders = seatbelt_test_gui:test_orders(Account1),
    ?DEBUG("Orders for ~p are~n",[Account1]),
    send(self(),jiffy:encode(Orders));
request({[{<<"request">>,<<"vol_series">>}]}) ->
    ?DEBUG("Volatility requested~n"),
    Series = volatility_test_gui:get_vol_series(),
    ?DEBUG("Series for ~p are~n",[Series]),
    send(self(),Series);
request([{[{<<"id">>,1},{<<"col1">>,<<"Exp">>},{<<"col3">>,_},{<<"col4">>,_},{<<"col5">>,_}]},{[{<<"id">>,2},{<<"col1">>,<<"ATMV">>},{<<"col3">>,_},{<<"col4">>,_},{<<"col5">>,_}]},{[{<<"id">>,3},{<<"col2">>,<<"Mkt">>},{<<"col3">>,Mkt1},{<<"col4">>,Mkt2},{<<"col5">>,Mkt3}]},{[{<<"id">>,4},{<<"col2">>,<<"Adj">>},{<<"col3">>,Adj1},{<<"col4">>,Adj2},{<<"col5">>,Adj3}]},{[{<<"id">>,5},{<<"col2">>,<<"Used">>},{<<"col3">>,_},{<<"col4">>,_},{<<"col5">>,_}]},{[{<<"id">>,6},{<<"col1">>,<<"Skew">>},{<<"col3">>,Skew1},{<<"col4">>,Skew2},{<<"col5">>,Skew3}]},{[{<<"id">>,7},{<<"col1">>,<<"Curve">>},{<<"col3">>,Curve1},{<<"col4">>,Curve2},{<<"col5">>,Curve3}]},{[{<<"id">>,8},{<<"col1">>,<<"Rot">>},{<<"col3">>,Rot1},{<<"col4">>,Rot2},{<<"col5">>,Rot3}]}]) -> 
    try
    M1 = bin_to_float(Mkt1),
    M2 = bin_to_float(Mkt2),
    M3 = bin_to_float(Mkt3),
    Ad1 = bin_to_float(Adj1),
    Ad2 = bin_to_float(Adj2),
    Ad3 = bin_to_float(Adj3),
    S1 = bin_to_float(Skew1),
    S2 = bin_to_float(Skew2),
    S3 = bin_to_float(Skew3),
    C1 = bin_to_float(Curve1),
    C2 = bin_to_float(Curve2),
    C3 = bin_to_float(Curve3),
    R1 = bin_to_float(Rot1),
    R2 = bin_to_float(Rot2),
    R3 = bin_to_float(Rot3),
    Series = volatility_test_gui:get_vol_series(M1,M2,M3,Ad1,Ad2,Ad3,S1,S2,S3,C1,C2,C3,R1,R2,R3),
    ?DEBUG("Series for ~p are~n",[Series]),
    send(self(),Series),
    NewATMV1 = binary:list_to_bin(io_lib:format("~.2f",[M1+Ad1])),
    NewATMV2 = binary:list_to_bin(io_lib:format("~.2f",[M2+Ad2])),
    NewATMV3 = binary:list_to_bin(io_lib:format("~.2f",[M3+Ad3])),
    NewUsed1 = binary:list_to_bin(io_lib:format("~.2f",[M1+Ad1])),
    NewUsed2 = binary:list_to_bin(io_lib:format("~.2f",[M2+Ad2])),
    NewUsed3 = binary:list_to_bin(io_lib:format("~.2f",[M3+Ad3])),
    NewMkt1 = binary:list_to_bin(io_lib:format("~.2f",[M1])),
    NewMkt2 = binary:list_to_bin(io_lib:format("~.2f",[M2])),
    NewMkt3 = binary:list_to_bin(io_lib:format("~.2f",[M3])),
    NewAdj1 = binary:list_to_bin(io_lib:format("~.2f",[Ad1])),
    NewAdj2 = binary:list_to_bin(io_lib:format("~.2f",[Ad2])),
    NewAdj3 = binary:list_to_bin(io_lib:format("~.2f",[Ad3])),
    NewSkew1 = binary:list_to_bin(io_lib:format("~.2f",[S1])),
    NewSkew2 = binary:list_to_bin(io_lib:format("~.2f",[S2])),
    NewSkew3 = binary:list_to_bin(io_lib:format("~.2f",[S3])),
    NewCurve1 = binary:list_to_bin(io_lib:format("~.2f",[C1])),
    NewCurve2 = binary:list_to_bin(io_lib:format("~.2f",[C2])),
    NewCurve3 = binary:list_to_bin(io_lib:format("~.2f",[C3])),
    NewRot1 = binary:list_to_bin(io_lib:format("~.2f",[R1])),
    NewRot2 = binary:list_to_bin(io_lib:format("~.2f",[R2])),
    NewRot3 = binary:list_to_bin(io_lib:format("~.2f",[R3])),
    Tabledata = jiffy:encode([{[{<<"id">>,1},{<<"col1">>,<<"Exp">>},{<<"col3">>,<<"Oct 24 2014">>},{<<"col4">>,<<"Nov 21 2014">>},{<<"col5">>,<<"Dec 19 2014">>}]},{[{<<"id">>,2},{<<"col1">>,<<"ATMV">>},{<<"col3">>,NewATMV1},{<<"col4">>,NewATMV2},{<<"col5">>,NewATMV3}]},{[{<<"id">>,3},{<<"col2">>,<<"Mkt">>},{<<"col3">>,NewMkt1},{<<"col4">>,NewMkt2},{<<"col5">>,NewMkt3}]},{[{<<"id">>,4},{<<"col2">>,<<"Adj">>},{<<"col3">>,NewAdj1},{<<"col4">>,NewAdj2},{<<"col5">>,NewAdj3}]},{[{<<"id">>,5},{<<"col2">>,<<"Used">>},{<<"col3">>,NewUsed1},{<<"col4">>,NewUsed2},{<<"col5">>,NewUsed3}]},{[{<<"id">>,6},{<<"col1">>,<<"Skew">>},{<<"col3">>,NewSkew1},{<<"col4">>,NewSkew2},{<<"col5">>,NewSkew3}]},{[{<<"id">>,7},{<<"col1">>,<<"Curve">>},{<<"col3">>,NewCurve1},{<<"col4">>,NewCurve2},{<<"col5">>,NewCurve3}]},{[{<<"id">>,8},{<<"col1">>,<<"Rot">>},{<<"col3">>,NewRot1},{<<"col4">>,NewRot2},{<<"col5">>,NewRot3}]}]),
    ?DEBUG("New table data is ~p~n",[Tabledata]),
    send(self(),Tabledata)
    of
	_ -> ok
    catch
	error:Error -> 			  
	    ?DEBUG("caught ~p~n",[Error]),
	    send(self(),jiffy:encode({[{<<"bad_data">>,<<"true">>}]}))
    end;
request(Req)->
    ?DEBUG("Unknown request: ~p~n",[Req]),
    ok.
    


bin_to_float(Bin) ->
    N = binary_to_list(Bin),
    Num = case string:to_float(N) of
	      {error,no_float} -> 
		  float(list_to_integer(N));
	      {F,_Rest} -> F
	  end,
    %%?DEBUG("~p = ~p~n",[Bin,Num]),
    Num.
    

 
    
