%%% rabbitmq_interface.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 07/01/10 Ari Lerner <arilerner@mac.com>
%% @doc 
-module (rabbitmq_interface).

-behaviour(gen_server).
-include ("rabbithole.hrl").
-include("amqp_client.hrl").

-export ([
  start_link/0,
  stop/0,
  subscribe/1,
  publish/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-record(state, {
  connection,
  channel
}).
    
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe({QueueName, Props}) -> gen_server:call(?SERVER, {subscribe, binify(QueueName), Props}).

publish({QueueName, Msg, Props}) -> gen_server:call(?SERVER, {publish, binify(QueueName), binify(Msg), Props}).

stop() -> gen_server:call(?MODULE, stop).

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
init([]) ->
  case catch amqp_connection:start_network() of
    {error, P} ->
      erlang:display({exit, P}),
      {stop, P};
    Conn ->
      Channel = amqp_connection:open_channel(Conn),
      {ok, #state{connection = Conn, channel = Channel}}
  end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({subscribe, QueueName, Props}, {Pid, _} = _From, #state{channel = Chan} = State) ->
  % First create the exchange
  Type = get_value(exchange_type, <<"topic">>, Props),
  amqp_channel:call(Chan, #'exchange.declare'{exchange = QueueName, type=Type}),
  % Next create the queue
  #'queue.declare_ok'{queue = Q} = amqp_channel:call(Chan, #'queue.declare'{queue = QueueName}),
  
  Key = get_value(key, QueueName, Props),
  % Finally, bind the route  
  Route = #'queue.bind'{queue = Q, exchange = QueueName, routing_key = Key},
  amqp_channel:call(Chan, Route),
  
  BaseProps = [{channel, Chan}, {queue, Q}, {to, Pid}],
  WatcherProps = case get_value(callback, undefined, Props) of
    undefined -> BaseProps;
    E -> [{callback, E}|BaseProps]
  end,
  
  Reply = case amqp_channel:call(Chan, Route) of
    {'queue.bind_ok'} -> 
      rabbithole_sup:start_child(WatcherProps),
      ok;
    Msg -> {error, Msg}
  end,
  
  {reply, Reply, State};
  
handle_call({publish, QueueName, Msg, Props}, _From, #state{channel = Chan} = State) ->
  Exchange = QueueName,
  Key = get_value(key, QueueName, Props),
  
  Publish = #'basic.publish'{exchange = Exchange, routing_key = Key},
  AmqpMsg = #amqp_msg{payload = Msg},
  Reply = amqp_channel:call(Chan, Publish, AmqpMsg),
  {reply, Reply, State};
handle_call(stop, _From, State) ->
  {stop, shutdown, normal, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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
terminate(_Reason, #state{connection = Conn, channel = Chan} = _State) ->
  amqp_channel:close(Chan),
  amqp_connection:close(Conn),
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
binify(Tuple) when is_tuple(Tuple) -> binify(erlang:term_to_binary(Tuple));
binify(List)  when is_list(List) -> binify(erlang:list_to_binary(List));
binify(Atom)  when is_atom(Atom) -> binify(erlang:atom_to_list(Atom));
binify(Bin)   when is_binary(Bin) -> Bin.

% Just a wrapper around the proplists moduel
get_value(Key, Default, Props) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default;
    E -> E
  end.