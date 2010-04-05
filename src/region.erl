%%%-------------------------------------------------------------------
%%% File    : region.erl
%%% Author  : Michael Melanson
%%% Description : Monitor process for a region (province/territory)
%%%-------------------------------------------------------------------
-module(region).

-behaviour(gen_server).

%% API
-export([start_link/2, update/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, account, updates}).


-define(SERVER, ?MODULE).
-define(UPDATE_INTERVAL, 1000*60*10).  %% Five minutes


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name, Account) ->
    gen_server:start_link(?MODULE, [Name, Account], []).

update(City, Region, Update) ->
    Children = supervisor:which_children(region_sup),
    
    lists:map(fun({ChildRegion, Pid, _, _}) when Region =:= ChildRegion ->
                  gen_server:cast(Pid, {update, City, Update});
                 (_) -> undefined
              end, Children),
    ok.

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
init([Name, Account]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("~s: started~n", [Name]),

    set_timer(?UPDATE_INTERVAL),
    {ok, #state{name=Name, account=Account, updates=[]}}.

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
handle_cast(post_updates, State) ->
    lists:map(fun({Update, Sites}) ->
        HashtagSet = sets:union(lists:map(fun(Site) -> sets:from_list(hashtags(Site)) end, Sites)),
        Hashtags = string:join(sets:to_list(HashtagSet), " "),
        SiteList = string:join(Sites, ", "),
        Tweet = Update ++ " for " ++ SiteList ++ " " ++ Hashtags,
        twitter_status:tweet(State#state.name, State#state.account, Tweet)
    end, State#state.updates),
            
    set_timer(?UPDATE_INTERVAL),
    {noreply, State#state{updates=[]}};
    
handle_cast({update, City, Notice}, State) ->
    error_logger:info_msg("~s: Received update for ~s: ~s~n",
                          [State#state.name, City, Notice]),
                          
    NewUpdate =
        case lists:keysearch(Notice, 1, State#state.updates) of
            false -> {Notice, [City]};
            {value, {Notice, Cities}} ->
                case lists:member(City, Cities) of
                    true -> {Notice, Cities};
                    false -> {Notice, Cities ++ [City]}
                end
        end,
        
    NewUpdates = lists:keystore(Notice, 1, State#state.updates, NewUpdate),
    {noreply, State#state{updates=NewUpdates}}.

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
set_timer(Interval) ->
    timer:apply_after(Interval, gen_server, cast, [self(), post_updates]).

hashtags("Vancouver") -> ["#vancouver"];
hashtags("Calgary") -> ["#calgary"];
hashtags("Regina") -> ["#regina"];
hashtags("Winnipeg") -> ["#winnipeg"];
hashtags("Ottawa (Kanata - Orléans)")    -> ["#ottawa"];
hashtags("Ottawa (Richmond - Metcalfe)") -> ["#ottawa"];
hashtags("Toronto") -> ["#toronto"];
hashtags("Montréal") -> ["#montreal"];
hashtags("Halifax") -> ["#halifax"];
hashtags(_) -> [].
