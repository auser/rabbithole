%%% rabbithole.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 06/28/10 Ari Lerner <arilerner@mac.com>
%% @doc Rabbit hole
-module (rabbithole).
-include ("rabbithole.hrl").

-behaviour(gen_server).

%% API
-export([
  subscribe/1, subscribe/2, % subscribe to a queue
  publish/2, publish/3,     % publish a message to a queue
  start_link/1              % start it up
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link([])                  -> start_link("rabbitmq");
start_link(Interface)           -> gen_server:start_link({local, ?SERVER}, ?MODULE, [Interface], []).

subscribe(QueueName)            -> subscribe(QueueName, []).
subscribe(QueueName, Props)     -> gen_server:call(?SERVER, {subscribe, {QueueName, Props}}).

publish(QueueName, Msg)         -> publish(QueueName, Msg, []).
publish(QueueName, Msg, Props)  -> gen_server:call(?SERVER, {publish, {QueueName, Msg, Props}}).

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
init(InterfaceName) ->
  Interface = list_to_atom(lists:flatten([InterfaceName, "_interface"])),
  RealInterface = case (catch Interface:start_link()) of
    {ok, _} = T -> Interface;
    Else ->
      squirrel_interface:start_link(),
      squirrel_interface
  end,
  % case catch rabbithole_sup:start_interface(Interface, []) of
  %   {ok, _} = T -> T;
  %   {'EXIT', P} ->
  %     erlang:display({error, P}),
  %     rabbithole_sup:start_interface(squirrel_interface, []);
  %   E ->
  %     erlang:display({got, Interface, E}),
  %     rabbithole_sup:start_interface(squirrel_interface, [])
  % end,
  {ok, RealInterface}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({subscribe, Args}, _From, Interface) ->
  Reply = Interface:subscribe(Args),
  {reply, Reply, Interface};
  
handle_call({publish, Args}, _From, Interface) ->
  Reply = Interface:publish(Args),
  {reply, Reply, Interface};

handle_call(_Req, _From, Interface) ->
  {reply, unknown, Interface}.
  
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

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
terminate(_Reason, Interface) ->
  Interface:stop(),
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