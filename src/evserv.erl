%%%-------------------------------------------------------------------
%%% @author richard
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2015 22:46
%%%-------------------------------------------------------------------
-module(evserv).
-author("richard").

%% API
-export([loop/1, init/0]).

-record(event,{name="",
          description="",
          pid,
          timeout={{1970,1,1},{0,0,0}}}
        ).
-record(state,{events,clients} ).


init() ->
  loop(#state{events=maps:new(),
              clients=maps:new()}).

loop(S=#state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = maps:put(Ref,Client,S#state.clients),
      Pid ! {MsgRef,ok},
      loop(S#state{clients = NewClients});

    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      case valid_datetime(TimeOut) of
        true ->
          EventPid = event:start_link(Name,TimeOut),
          NewEvents = maps:put(Name, #event{
                                            description = Description,
                                            name = Name,
                                            pid = EventPid,
                                            timeout = TimeOut
                                          },
                                      S#state.events),
          Pid ! {MsgRef,ok},
          loop(S#state{events = NewEvents});
        false ->
          Pid ! {MsgRef,bad_timeout},
          loop(S)
      end;

    {Pid, MsgRef, {cancel, Name}} ->
      UpdEvents = case maps:get(Name, S#state.events) of
                 {ok, E} ->
                   event:cancel(Pid),
                   maps:remove(E, S#state.events);
                 {error} ->
                   S#state.events
              end,
      Pid ! {MsgRef, ok},
      loop(S#state{events = UpdEvents});

    {done, Name} ->
      case maps:get(Name, S#state.events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description},
                          S#state.clients),
          NewEvents = maps:remove(Name, S#state.events),
          loop(S#state{events = NewEvents});
        error ->
          loop(S)
        end;

    shutdown ->
      exit(shutdown);

    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(S#state{clients = maps:remove(Ref, S#state.clients)});

    code_change ->
      ?MODULE:loop(S);

    Unknown ->
    io:format("Unknown message: ~p~n",[Unknown]),
    loop(S)
  end.


%%% Internal Functions
valid_datetime({Date,Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> %% not in {{Y,M,D},{H,Min,S}} format
      false
  end;
valid_datetime(_) ->
  false.

valid_time({H,M,S}) ->
  valid_time(H,M,S).

valid_time(H,M,S) when H >= 0, H < 24,
                      M >= 0, M < 60,
                      S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

send_to_clients(Msg, ClientMap) ->
  maps:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientMap).
