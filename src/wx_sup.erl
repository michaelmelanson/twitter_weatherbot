%%%-------------------------------------------------------------------
%%% File    : wx_sup.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(wx_sup).

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
    io:format("~p: Retrieving site list from Environment Canada...~n",
              [?MODULE]),
    {ok, Sites} = envcan_api:list(),
    io:format("~p: There are a total of ~p sites~n", [?MODULE, length(Sites)]),


    Children = lists:map(fun(Site) ->
                            {Site, {wx, start_link, [Site, 1000*60*5]},
                             permanent, 2000, worker, [wx]}
                         end, Sites),
    {ok,{{one_for_all,100,3}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
