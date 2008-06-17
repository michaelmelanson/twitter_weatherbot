%%%-------------------------------------------------------------------
%%% File    : twitter_status.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(twitter_status).

-behaviour(gen_server).

%% API
-export([start_link/0, weather_update/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).

-define(TWITTER_USERNAME, "wx_canada").
-define(TWITTER_PASSWORD, "pleasedonthackme").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

weather_update(City, Province, Message) ->
    gen_server:cast(?SERVER, {weather_update, City, Province, Message}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    twitter_client:start(?TWITTER_USERNAME, ?TWITTER_PASSWORD),
    
    io:format("Authenticating with Twitter..."),
    case twitter_client:call(?TWITTER_USERNAME, account_verify_credentials) of
        true ->
            io:format("Success!~n"),
            {ok, #state{}};
            
        false ->
            io:format("Failed!~n"),
            {stop, twitter_authentication_failed}
    end.
    
%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({weather_update, City, Province, Message}, State) ->
    Status = City ++ ", " ++ Province ++ ": " ++ Message,
    
    io:format("Would update Twitter: ~p~n", [Status]),
    %twitter_client:call(?TWITTER_USERNAME, status_update, 
    %                    [{"status", Status}]), 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
