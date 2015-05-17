%%%-------------------------------------------------------------------
%%% @author richard
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. May 2015 21:50
%%%-------------------------------------------------------------------
-module(event).
-author("richard").
%% -compile(export_all).
-record(state, {server, name="", to_go=[]}).

%% API
-export([loop/1, normalize/1, make_limit_tail/2, cancel/1, start_link/2, start/2, init/3]).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

%%% Event's innards
init(Server, EventName, DateTime) ->
  loop(#state{server=Server,
          name=EventName,
          to_go=normalize(time_to_go(DateTime))
      }).


cancel(Pid) ->
%% Monitor in case the process is already dead
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      io:format("demonitor~n"),
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      io:format("cancelling~n"),
      Server ! {Ref, ok}
  after T*1000 ->
    if Next =:= [] ->
      io:format("done ~n"),
      Server ! {done, S#state.name};
      Next =/= []->
        io:format("looping ~p ~n", [length(Next)]),
        loop(S#state{to_go=Next})
    end
  end.

%% Hack as Erlang timeout no greater that 49 days

normalize(N) ->
%%   Limit = 49*24*60*60,
  Limit = 2,
  [N rem Limit | make_limit_tail(N div Limit, Limit)].

make_limit_tail(NoElems, ElemVal)
  when is_integer(NoElems), NoElems >=0 -> make_limit_tail(NoElems, ElemVal,[]).

make_limit_tail(0,_,L) -> L;
make_limit_tail(N,X,L) -> make_limit_tail(N-1,X,[X|L]).

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
          calendar:datetime_to_gregorian_seconds(Now),
  if ToGo > 0  -> ToGo;
     ToGo =< 0 -> 0
   end.