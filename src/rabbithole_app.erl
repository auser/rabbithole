-module(rabbithole_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
  rabbithole_sup:start_link(StartArgs).

stop(_State) ->
  ok.
