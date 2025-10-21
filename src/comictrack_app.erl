%%%-------------------------------------------------------------------
%% @doc comictrack public API
%% @end
%%%-------------------------------------------------------------------

-module(comictrack_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    comictrack_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
