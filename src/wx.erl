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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {site, interval, events, etag}).

-include("envcan_api.hrl").

-define(SERVER, ?MODULE).
-define(MIN_UPDATE_INTERVAL, 1000*60*5).  %% Five minutes
-define(MAX_UPDATE_INTERVAL, 1000*60*60). %% One hour

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Site) ->
    Delay = random:uniform(?MAX_UPDATE_INTERVAL),
    gen_server:start_link(?MODULE, [Site, Delay], []).

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
init([Site, Delay]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("~s, ~s: started~n",
		      [Site#site.city, Site#site.province]),
    set_timer(Delay),
    {ok, #state{site=Site}}.

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
    WxPid = self(),

    F = fun() ->
		case envcan_api:get(State#state.site, State#state.etag) of
		    unmodified -> WxPid ! unmodified;
        
		    {ok, SiteData, ETag} ->			
			WxPid ! {updated, ETag, SiteData}
		end
	end,

    Pid = spawn_link(F),
    {NewETag, NewEvents, NewInterval} =
	receive
	    unmodified -> {State#state.etag,
			   State#state.events,
			   lists:min([State#state.interval*2,
				      ?MAX_UPDATE_INTERVAL])};

	    {updated, ETag, SiteData} ->
		case {State#state.etag, extract_events(SiteData)} of
		    {undefined, []}     -> {ETag, [],     ?MAX_UPDATE_INTERVAL};
		    {undefined, Events} -> {ETag, Events, ?MIN_UPDATE_INTERVAL};

		    {_, Events} when Events =:= State#state.events ->
			{ETag, Events,
			 lists:min([State#state.interval*2,
				    ?MAX_UPDATE_INTERVAL])};

		    {_, Events} ->
			lists:foreach(
			  fun(E) ->
				  EventText = E#event.description,
				  twitter_status:weather_update(Site#site.city,
								Site#site.province,
								EventText)
			  end, Events),
			{ETag, Events, ?MIN_UPDATE_INTERVAL}
		end;
	    
	    {'EXIT', Pid, Error} ->
		%% WarningText ="Error updating this city/" ++
		%%	      "Erreur de mise à  jour de cette ville",
		%% twitter_status:weather_update(Site#site.city,
		%%		      	       Site#site.province,
		%%			       WarningText),
		error_logger:error_msg("Error updating ~s, ~s: ~p~n", 
				       [Site#site.city,
					Site#site.province,
					Error]),

		{State#state.etag, State#state.events, ?MIN_UPDATE_INTERVAL}

	after 5000 ->
		error_logger:warning_msg("~s, ~s timed out; will try again in ~p minutes~n",
					 [Site#site.city, Site#site.province,
					  ?MIN_UPDATE_INTERVAL div (1000*60)]),
		exit(Pid, timeout),
		set_timer(?MIN_UPDATE_INTERVAL),
		{undefined, undefined, undefined}
	end,
    
    case NewInterval of
	undefined -> {noreply, State};
	_ -> 
	    error_logger:info_msg("~s, ~s updated; next in ~p minutes~n",
				  [Site#site.city, Site#site.province,
				   NewInterval div (1000*60)]),
	    set_timer(NewInterval),
	    {noreply, State#state{etag=NewETag,
				  events=NewEvents,
				  interval=NewInterval}}
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
set_timer(Interval) ->
    timer:apply_after(Interval, gen_server, cast, [self(), update]).
    
extract_events(SiteData) ->
    Events    = SiteData#sitedata.events,
    AllEvents = Events#events.watches ++
	        Events#events.warnings ++
	        Events#events.ended,    

    %% Don't include "CONTINUED" messages    
    lists:filter(
      fun(#event{description=Desc}) ->
	      ((string:str(Desc, "CONTINUED") =:= 0) and
	       (string:str(Desc, "MAINTENU")  =:= 0))
      end, AllEvents).
