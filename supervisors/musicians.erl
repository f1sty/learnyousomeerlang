-module(musicians).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
  gen_server:stop(Role).

init([Role, Skill]) ->
  process_flag(trap_exit, true),
  rand:seed(default),
  TimeToPlay = rand:uniform(3000),
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("Musician ~ts, playing the ~s entered the room~n", [Name, StrRole]),
  {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

pick_name() ->
  lists:nth(rand:uniform(10), firstnames())
  ++ " " ++
  lists:nth(rand:uniform(10), lastnames()).

firstnames() ->
  ["Анна", "Софія", "Марія", "Мілана", "Соломія", "Артем", "Дмитро", "Максим", "Данило",
   "Андрій"].

lastnames() ->
  ["Мельник", "Шевченко", "Коваленко", "Бондаренко", "Бойко", "Ткаченко", "Кравченко",
   "Ковальчук", "Коваль", "Олійник"].

handle_call(_Message, _From, S) ->
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.

handle_info(timeout, S = #state{name=N, skill=good}) ->
  io:format("~ts produced sound!~n", [N]),
  {noreply, S, ?DELAY};
handle_info(timeout, S = #state{name=N, skill=bad}) ->
  case rand:uniform(5) of
    1 ->
      io:format("~ts played a false note. Uh oh~n", [N]),
      {stop, bad_note, S};
    _ ->
      io:format("~ts produced sound!~n", [N]),
      {noreply, S, ?DELAY}
  end;
handle_info(_Message, S) ->
  {noreply, S, ?DELAY}.

terminate(normal, S) ->
  io:format("~ts left the room (~s)~n", [S#state.name, S#state.role]);
terminate(bad_note, S) ->
  io:format("~ts sucks! kicked that member out of the band! (~s)~n", [S#state.name, S#state.role]);
terminate(shutdown, S) ->
  io:format("The manager is mad and fired the whole band! "
            "~ts just got back to playing in the subway~n",
            [S#state.name]);
terminate(_Reason, S) ->
  io:format("~ts has been kicked out (~s)~n", [S#state.name, S#state.role]).
