%%%-------------------------------------------------------------------
%%% File    : http_master.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-06-17 by Michael Melanson
%%%-------------------------------------------------------------------
-module(http_master).

-behaviour(gen_server).

%% API
-export([start_link/0, available/0, request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {worker_queue, task_queue}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, #state{worker_queue = queue:new(),
                task_queue = queue:new()}}.

available() ->
    gen_server:cast(?SERVER, {insert_worker, self()}).
    
request(Task) ->
    gen_server:call(?SERVER, {insert_task, Task}, infinity).

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({insert_task, Task}, From, State) ->
    case queue:is_empty(State#state.worker_queue) of
        true ->
            Q2 = queue:in_r({Task, From}, State#state.task_queue),
            {noreply, State#state{task_queue=Q2}};
            
        false ->
            {{value, Worker}, Q2} = queue:out(State#state.worker_queue),
            gen_server:cast(Worker, {Task, From}),
            {noreply, State#state{worker_queue=Q2}}
    end.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({insert_worker, Worker}, State) ->
    case queue:is_empty(State#state.task_queue) of
        true ->
            Q2 = queue:in_r(Worker, State#state.worker_queue),
            {noreply, State#state{worker_queue=Q2}};
            
        false ->
            {{value, {Task, From}}, Q2} = queue:out(State#state.task_queue),
            gen_server:cast(Worker, {Task, From}),
            {noreply, State#state{task_queue=Q2}}
    end.
    
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
