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

-record(state, {name, account, updates, batch_start}).


-define(SERVER, ?MODULE).
-define(MIN_AGGREGATE_INTERVAL, 1000*60*10). % Time to wait after an update
-define(MAX_AGGREGATE_INTERVAL, 1000*60*30). % Maximum time to let messages wait


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

    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    
    case State#state.batch_start of
        undefined -> % new batch
            {noreply, State#state{updates=NewUpdates, batch_start=Now}, ?MIN_AGGREGATE_INTERVAL};
             
        BatchStart -> % there are other messages in this batch            
            Elapsed = Now - BatchStart,
            Timeout = lists:min([?MIN_AGGREGATE_INTERVAL, ?MAX_AGGREGATE_INTERVAL-Elapsed]),
            {noreply, State#state{updates=NewUpdates}, Timeout}
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    lists:map(fun({Update, Sites}) ->
        HashtagSet = sets:union(lists:map(fun(Site) -> sets:from_list(hashtags(Site)) end, Sites)),
        Hashtags = string:join(sets:to_list(HashtagSet), " "),
        SiteList = string:join(Sites, ", "),
        Tweet = Update ++ " for " ++ SiteList ++ " " ++ Hashtags,
        twitter_status:tweet(State#state.name, State#state.account, Tweet)
    end, State#state.updates),
    
    {noreply, State#state{updates=[], batch_start=undefined}}.

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
