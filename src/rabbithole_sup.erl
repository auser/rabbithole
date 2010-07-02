
-module(rabbithole_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/1, start_interface/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIME, 16#ffffffff).
-define(MAX_RESTART,  10).
-define(MAX_TIME,     60).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(NAMED_CHILD(Id, M, Args, Type), {Id, {M, start_link, Args}, temporary, ?SHUTDOWN_TIME, Type, [M]}).
-define(SUP_CHILD (I, Args), {I, {supervisor, start_link, Args}, permanent, ?SHUTDOWN_TIME, supervisor, []}).

%% ===================================================================
%% API functions
%% ===================================================================

start_child(Args) ->
  supervisor:start_child(workers_sup, [Args]).
  
start_interface(Mod, Args) ->
  case catch supervisor:start_link(?MODULE, [Mod, Args]) of
    {ok, _} = T -> T;
    Else ->
      erlang:display({init, Else}),
      Else
  end.

start_link(_Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Server    =  ?NAMED_CHILD(rabbithole, rabbithole, [[]], worker),
  WorkerSup  = ?SUP_CHILD(workers_sup, [{local, workers_sup}, ?MODULE, [[]]]), 
    
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Server, WorkerSup]}};

init([_Props]) ->
  Child = ?NAMED_CHILD(undefined, rabbithole_srv, [], worker),
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Child]}};

init([Mod, _Props]) ->
  % Child = ?NAMED_CHILD(Mod, Mod, [], worker),
  Child = {Mod, {Mod, start_link, []}, transient, ?MAX_TIME, worker, [Mod]},
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Child]}}.
