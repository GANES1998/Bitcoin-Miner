-module(mine_supervisor).

-export([supervise/1, main_loop/3]).
-define(work_unit_property, "WORK_UNIT").
-define(max_processes_property, "MAX_PROCESSES").

supervise(K) ->
  %% Start CPU time
  statistics(runtime),
  %% Start Wallclock Time
  Start = util:get_timestamp(),
  %% Get Work Unit (Number of attepmts by each process before giving up)  from Env
  WorkUnit = list_to_integer(os:getenv(?work_unit_property, "100")),
  %% Max Processes to be spun to mine bitcoin. Defaults to 100000
  MaxProcesses = list_to_integer(os:getenv(?max_processes_property, "100000")),
  %% Get all dedicated ready to serve the servers request
  DedicatedWorkers = util:get_dedicated_workers(),
  %% Connect to all workers
  lists:foreach(
    fun(Worker) ->
      net_kernel:connect_node(list_to_atom(Worker))
    end,
    DedicatedWorkers
  ),
  %% Find All Workers
  AllWorkers = lists:append(nodes(), [node()]),
  %% Get total number of workers
  WorkersCount = lists:flatlength(AllWorkers),
  io:format("Starting to spawn process from supervisor [~p] to workers in [~p]", [node(), AllWorkers]),
  %% Spun MaxProcesses in Round robin fashion across all worker nodes. Supervisor node will also have workers.
  lists:foreach(
    fun(Index) ->
      WorkerIndex = (Index rem WorkersCount) + 1,
      _Pid = spawn_link(lists:nth(WorkerIndex, AllWorkers), worker, main, [K, self()])
    end,
    lists:seq(1, MaxProcesses)
  ),
  %% Start the event listening part.
  main_loop(0, 0, MaxProcesses),
  %% Stop the CPU Monitoring.
  {_, CPUTime} = statistics(runtime),
  %% Stop Wall Clock timer
  End = util:get_timestamp(),
  %% Calculate wall clock time
  WallClockTime = End - Start,
  %% Print the result with stats
  io:format("[~p] processes took [~p] milli seconds CPU Time and [~p] milli seconds Wall clock time with Work Unit [~p] ~n", [MaxProcesses, CPUTime, WallClockTime, WorkUnit]),
  Speedup = CPUTime/WallClockTime,
  io:format("Speedup Achieved: ~p~n", [Speedup]).

main_loop(WorkerCount, SuccessCount, MaxProcesses) ->
  %% Check if all workers have reported result.
  case WorkerCount < MaxProcesses of
    %% If there are are more workers yet to report result,
    true -> receive
              %% If the worker has successfully mined a bitcoin
              {success, _WorkerPid, HashInputString, Sha256} ->
                %% Print the result from that worker
                io:format("~p ~p~n", [HashInputString, Sha256]),
                %% Since, we have more workers yet to report result, again listen for messages from worker.
                main_loop(WorkerCount + 1, SuccessCount + 1, MaxProcesses);
              %% If the worker isn't able to mine bitcoin
              {nosuccess, _} ->
                %% Since, we have more workers yet to report result, again listen for messages from worker.
                main_loop(WorkerCount + 1, SuccessCount, MaxProcesses)
            end;
    false ->
      %% If all workers have reported the result, its time to stop the process.
      io:format("Successfully Mined [~p] coins~n", [SuccessCount])
  end.


