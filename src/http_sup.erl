%%%-------------------------------------------------------------------
%%% File    : http_sup.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-06-17 by Michael Melanson
%%%-------------------------------------------------------------------
-module(http_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
start_link(Workers) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Workers).

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
init(Workers) ->
    Master = {http_master, {http_master, start_link, []},
              permanent,2000,worker,[http_master]},
              
    Children = lists:map(fun(X) ->
                             {list_to_atom(lists:concat(["httpc_",X])),{httpc,start_link,[X]},
                              permanent,2000,worker,[httpc]}
                         end, lists:seq(1, Workers)),
    {ok,{{one_for_all,1,1}, [Master] ++ Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
