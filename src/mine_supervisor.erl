-module(mine_supervisor).

-export([supervise/1, main_loop/3, spawn_worker/5, spawn_loop/5]).
-define(work_unit_property, "WORK_UNIT").
-define(max_processes_property, "MAX_PROCESSES").

supervise(K) ->
  %% Register the supervisor process
  register(?MODULE, self()),
  %% Start CPU time
  statistics(runtime),
  %% Start Wallclock Time
  statistics(wall_clock),
  %% Get Work Unit (Number of attepmts by each process before giving up)  from Env
  WorkUnit = list_to_integer(os:getenv(?work_unit_property, "100")),
  %% Max Processes to be spun to mine bitcoin. Defaults to 100000
  MaxProcesses = list_to_integer(os:getenv(?max_processes_property, "100000")),
  %% Spun MaxProcesses in Round robin fashion across all worker nodes. Supervisor node will also have workers.
  spawn_loop(K, WorkUnit, MaxProcesses, 1, [node()]),
  %% Listening for result messages
  io:format("Listening for result messages from workers [~p]~n", [lists:append([node()], nodes())]),
  %% Start the event listening part.
  main_loop(0, 0, MaxProcesses),
  %% Stop workers
  io:format("Stoping all joined Dedicated worker nodes [~p]~n", [nodes()]),
  stop_all_workers(nodes()),
  %% Stop the CPU Monitoring.
  {_, CPUTime} = statistics(runtime),
  %% Calculate wall clock time
  {_, WallClockTime} = statistics(wall_clock),
  %% Print the result with stats
  io:format("[~p] processes took [~p] milli seconds CPU Time and [~p] milli seconds Wall clock time with Work Unit [~p] ~n", [MaxProcesses, CPUTime, WallClockTime, WorkUnit]),
  Speedup = CPUTime / WallClockTime,
  io:format("Speedup Achieved: ~p~n", [Speedup]).

spawn_worker(K, WorkUnit, MaxProcesses, CurrentWorkerIndex, Workers) ->
  %% Get total workers count.
  WorkersCount = lists:flatlength(Workers),
  %% Get the index of the worker in round robin fashion - chose one of the workers
  WorkerIndex = (CurrentWorkerIndex rem WorkersCount) + 1,
  %% Spawn and link a new process in the chosen worker
  _Pid = spawn_link(lists:nth(WorkerIndex, Workers), worker, main, [K, WorkUnit, self()]),
  %% Again spawn the new process, while listening for new worker to join.
  spawn_loop(K, WorkUnit, MaxProcesses, CurrentWorkerIndex + 1, Workers).

spawn_loop(K, WorkUnit, MaxProcesses, CurrentWorkerIndex, Workers) ->
  %% Check if all process have been spun
  case CurrentWorkerIndex =< MaxProcesses of
    %% If more process needs to be spun
    true ->
      if
        %% For every 100 spawning execute the true block
        (CurrentWorkerIndex rem 100) == 0 ->
          %% Listen for any worker to send message with {connect, node()} format.
          receive
            {connect, Worker} ->
              %% Add the worker to the list of known workers.
              io:format("Successfully registered worker [~p]~n", [Worker]),
              spawn_worker(K, WorkUnit, MaxProcesses, CurrentWorkerIndex, lists:append(Workers, [Worker]))
          after 10 ->
            %% No new worker has requested to join the network, so, again spawn with same workers.
            spawn_worker(K, WorkUnit, MaxProcesses, CurrentWorkerIndex, Workers)
          end;
        true ->
          %% If this spawning is not one of 100 spawning. Again spawn the next process.
          spawn_worker(K, WorkUnit, MaxProcesses, CurrentWorkerIndex, Workers)
      end;
    %% After all the processes [MaxProcesses] have been spun, check output the details to console.
    false -> io:format("All [~p] processes successfully spun among workers [~p]~n", [MaxProcesses, Workers])
  end.



stop_all_workers(Workers) ->
  lists:foreach(
    fun(Worker) ->
      io:format("Stoping Worker [~p]~n", [Worker]),
      {worker, Worker} ! {stop}
    end,
    Workers
  ).

main_loop(WorkerCount, SuccessCount, MaxProcesses) ->
  %% Check if all workers have reported result.
  case WorkerCount < MaxProcesses of
    %% If there are are more workers yet to report result,
    true -> receive
            %% If the worker has successfully mined a bitcoin
              {success, _WorkerPid, HashInputString, Sha256, Node} ->
                %% Print the result from that worker
                io:format("~p\t~p\t~p~n", [HashInputString, Sha256, Node]),
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


