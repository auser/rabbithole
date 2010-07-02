%%% rabbithole_srv.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 06/30/10 Ari Lerner <arilerner@mac.com>
%% @doc Receiver loop
-module (rabbithole_srv).
-include("amqp_client.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-record(state, {
  queue,
  channel,
  to,
  tag,
  callback
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).

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
init([Props]) ->
  Queue       = get_value(queue, 'rabbithole.queue', Props),
  Channel     = get_value(channel, undefined, Props),
  ForwardPid  = get_value(to, undefined, Props),
  Callback    = get_value(callback, undefined, Props),
  
  #'basic.consume_ok'{consumer_tag = Tag} = 
      amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue}, self()),
  
  {ok, #state{queue = Queue, to = ForwardPid, channel = Channel, tag = Tag, callback = Callback}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
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
handle_info({'basic.consume_ok', Bin}, State) ->
  send(State, {subscribed, erlang:binary_to_list(Bin)}),
  {noreply, State};
handle_info('basic.cancel_ok', State) ->
  send(State, {cancelled}),
  {stop, normal, State};
handle_info({#'basic.deliver'{delivery_tag = DeliveryTag}, #amqp_msg{payload=P}}, #state{channel = Chan} = State) ->
  amqp_channel:call(Chan, #'basic.ack'{delivery_tag = DeliveryTag}),
  send(State, {msg, P}),
  {noreply, State};
handle_info(Info, State) ->
  erlang:display({got_info, Info}),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{channel = Chan, tag = Tag} = _State) ->
  amqp_channel:call(Chan, #'basic.cancel'{consumer_tag = Tag}),
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
% Just a wrapper around the proplists moduel
get_value(Key, Default, Props) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default;
    E -> E
  end.

send(#state{callback = C, to = To}, Msg) ->
  case C of
    undefined -> To ! Msg;
    Fun ->
      Fun(Msg)
  end.