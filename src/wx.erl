%%%-------------------------------------------------------------------
%%% File    : wx.erl
%%% Author  : Michael Melanson
%%% Description : Monitor process for a weather site
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(wx).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {site,
                interval,
                etag}).

-include("envcan_api.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Site, UpdateInterval) ->
    gen_server:start_link(?MODULE, [Site, UpdateInterval], []).

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
init([Site, UpdateInterval]) ->
    gen_server:cast(self(), update), % Start the ball rolling!
    {ok, #state{site=Site, interval=UpdateInterval}}.

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
handle_cast(update, State) ->
    Site = State#state.site,
    NewETag = try envcan_api:get(State#state.site, State#state.etag) of
        unmodified ->
            State#state.etag;
        
        {ok, SiteData, ETag} ->
            process_data(Site, SiteData),
            ETag
    catch
        Error -> io:format("Caught error: ~p~n", [Error])
    end,
    
    set_timer(State#state.interval),
    {noreply, State#state{etag=NewETag}}.

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
    timer:apply_after(Interval, gen_server, cast, [self(), update]).
    
process_data(Site, SiteData) ->
    Events = SiteData#sitedata.events,
    AllEvents = Events#events.watches ++ Events#events.warnings ++ Events#events.ended,
    
    
    lists:foreach(fun(#event{description=Desc}) ->
                      % Don't include "CONTINUED" messages
                      case (string:str(Desc, "CONTINUED") /= 0) or (string:str(Desc, "MAINTENU") /= 0) of
                          0 -> ok;
                          _ ->
                              twitter_status:weather_update(Site#site.city,
                                                            Site#site.province,
                                                            Desc)
                      end
                  end, AllEvents).