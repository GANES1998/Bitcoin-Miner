-module(mine_supervisor).

-export([supervise/1, main_loop/3]).

supervise(K) ->
  io:format("K is ~p", [K]),
  %%Start Wallclock Time
  Start = util:get_timestamp(),
  %%CPU Monitoring Initialization
  _CpuStart = cpu_sup:util(),
  %% Get Work Unit (Number of attepmts by each process before giving up)  from Env
  WorkUnit = list_to_integer(os:getenv("WORK_UNIT", "100")),
  %% Get Total Logical Cores in the system
  Cores = erlang:system_info(logical_processors_available),
  %% Calculate Max Processes to be spun using Cores * Process per core (Taken form ENV Vars)
  MaxProcesses = Cores * os:getenv("PROCESS_PER_CORE", 2500),
  %% Spun MaxProcesses times individual processes (workers)
  lists:foreach(
    fun(_) ->
      _Pid = spawn_link(worker, main, [K, self()])
    end,
    lists:seq(1, MaxProcesses)
  ),
  %% Start the event listening part.
  main_loop(0, 0, MaxProcesses),
  %% Calculate End Wall Clock time.
  End = util:get_timestamp(),
  %% Stop the CPU Monitoring.
  CpuEnd = cpu_sup:util(),
  %% Print the CPU Utilization
  io:format("Cup Time [~p]", [CpuEnd]),
  %% Print the result with stats
  io:format("[~p] processes took [~p] milli seconds running on [~p] logical cores with Work Unit [~p] ~n", [MaxProcesses, End - Start, Cores, WorkUnit]).

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


