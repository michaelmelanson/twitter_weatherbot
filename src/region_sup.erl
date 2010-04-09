%%%-------------------------------------------------------------------
%%% File    : region_sup.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(region_sup).

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
    Regions = [{"BC", "BC",            "wx_bc"},          
               {"AB", "Alberta",       "wx_alberta"},
               {"SK", "Saskatchewan",  "wx_saskatchewan"},
               {"MB", "Manitoba",      "wx_manitoba"},
               {"ON", "Ontario",       "wx_ontario"},
               {"QC", "Quebec",        "wx_quebec"},
               {"NB", "New Brunswick", "wx_newbrunswick"},
               {"PE", "PEI",           "wx_pei"},
               {"NS", "Nova Scotia",   "wx_novascotia"},
               {"NL", "Newfoundland",  "wx_newfoundland"},
               {"YK", "Yukon",         "wx_nunavut"},
               {"NT", "NWT",           "wx_nwt"},
               {"NU", "Nunavut",       "wx_yukon"}],

    Children = lists:map(fun({Region, Name, Account}) ->
                            {Region, {region, start_link, [Name, Account]},
                             permanent, 2000, worker, [region]}
                         end, Regions),

    case supervisor:check_childspecs(Children) of
        ok -> {ok,{{one_for_one,100,3}, Children}};
        {error, Error} ->
            error_logger:info_msg("Invalid child specifications: ~p~n",
				  [Error]),
            {error, Error}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
