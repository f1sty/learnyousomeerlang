-module(kitty_gen_server).

-behaviour(gen_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(cat, {name, color = green, description}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).

return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
  gen_server:call(Pid, terminate).

%% callbacks
init([]) ->
  {ok, []}.

handle_call({order, Name, Color, Description}, _From, State) ->
  if State =:= [] ->
       {reply,
        #cat{name = Name,
             color = Color,
             description = Description},
        State};
     State =/= [] ->
       {reply, hd(State), tl(State)}
  end;
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({return, Cat = #cat{}}, State) ->
  {noreply, [Cat | State]}.

handle_info(Msg, State) ->
  io:format("Unknown message: ~p~n", [Msg]),
  {noreply, State}.

terminate(normal, State) ->
  [io:format("cat ~s was set free!~n", [C#cat.name]) || C <- State],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
