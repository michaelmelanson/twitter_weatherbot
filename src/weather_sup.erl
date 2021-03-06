%%%-------------------------------------------------------------------
%%% File    : weather_sup.erl
%%% Author  : Michael Melanson
%%% Description : Top supervisor
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(weather_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Children = [{http_sup,       {http_sup,      start_link,[2]},permanent,2000,supervisor,[http_sup]},
                {twitter_status, {twitter_status,start_link,[]}, permanent,2000,worker,    [twitter_status]},
                {site_sup,       {site_sup,      start_link,[]}, permanent,2000,supervisor,[site_sup]},
                {region_sup,     {region_sup,    start_link,[]}, permanent,2000,supervisor,[region_sup]}],

    {ok,{{one_for_all,2,3}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
