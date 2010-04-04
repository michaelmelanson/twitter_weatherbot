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
-export([start_link/0, tweet/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-include("twitter_client.hrl").

-define(SERVER, ?MODULE).

-define(NATIONAL_ACCOUNT, "wx_canada").
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

tweet(Province, Account, Tweet) ->
    gen_server:cast(?SERVER, {tweet, Province, Account, Tweet}).

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
    twitter_client:start(),
    twitter_client:add_session(?NATIONAL_ACCOUNT, ?TWITTER_PASSWORD),
    
    {ok, #state{}}.
    
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
handle_cast({tweet, Province, _ProvinceAccount, Tweet}, State) ->
    post_to_twitter(?NATIONAL_ACCOUNT, Province ++ ": " ++ Tweet),
    %post_to_twitter(ProvinceAccount, Tweet),
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
post_to_twitter(AccountName, Status) ->
    Delay = twitter_client:delay(),
    case Delay of
      0 -> ok; % no throttling needed
      _ ->
        error_logger:info_msg(
            "Throttling: Waiting ~p seconds to update Twitter.~n",
            [Delay]),

        timer:sleep(Delay*1000)
    end,

    error_logger:info_msg("Updating @~s: ~p~n", [AccountName, Status]),

    Reply = twitter_client:call(AccountName, status_update, 
                                [{"status", Status}]), 
    case Reply of
      [#status{}] ->
        error_logger:info_msg("Twitter update successful~n");
      _ ->
        error_logger:error_msg("Update failed: ~p~n", [Reply])
    end,
    
    ok.
    
all_accounts() -> ["wx_canada", "wx_bc", "wx_alberta", "wx_sasatchewan",
                   "wx_manitoba", "wx_ontario", "wx_quebec", "wx_newbrunswick",
                   "wx_pei", "wx_novascotia", "wx_newfoundland", "wx_nunavut",
                   "wx_nwt", "wx_yukon"].
                   