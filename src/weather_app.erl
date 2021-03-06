%%%-------------------------------------------------------------------
%%% File    : weather_app.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(weather_app).
-behaviour(application).

%% API
-export([start/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, Args)
%% Description: Starts the bot
%%--------------------------------------------------------------------
start(normal, []) ->
    application:start(inets),
    {ok, _Pid} = weather_sup:start_link().
    
stop(_) ->
    exit(weather_sup).

%%====================================================================
%% Internal functions
%%====================================================================
