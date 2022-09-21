%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 2:45 pm
%%%-------------------------------------------------------------------
-module(mine_supervisor).
-author("ganesonravichandran").

-export([supervise/1, main_loop/3]).

supervise(K) ->
  io:format("K is ~p", [K]),
  Start = util:get_timestamp(),
  Cores = erlang:system_info(logical_processors_available),
  MaxProcesses = Cores * os:getenv("PROCESS_PER_CORE", 2500000),
  lists:foreach(
    fun (_) ->
       _Pid = spawn_link(worker, main, [K, self()])
      end,
    lists:seq(1, MaxProcesses)
  ),
  main_loop(0, 0, MaxProcesses),
  End= util:get_timestamp(),
  io:format("[~p] processes took [~p] milli seconds running on [~p] logical cores~n", [MaxProcesses, End - Start, Cores]).

main_loop(WorkerCount, SuccessCount, MaxProcesses) ->
  case WorkerCount < MaxProcesses of
    true -> receive
      {success, WorkerPid, HashInputString, Sha256} ->
        io:format("~p ~p ~p~n", [HashInputString, Sha256, WorkerPid]),
        main_loop(WorkerCount + 1, SuccessCount + 1, MaxProcesses);
      {nosuccess, _} ->
        main_loop(WorkerCount + 1, SuccessCount, MaxProcesses)
    end;
    false ->
      io:format("Successfully Mined [~p] coins", [SuccessCount])
end.


