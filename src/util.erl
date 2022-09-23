-module(util).

%% API
-export([generate_random_str/1, concat_string/2, hash_sha256/1, get_zero_string/1, get_timestamp/0, get_workers/0, get_dedicated_workers/0, get_next_in_round_robbin/2]).

generate_random_str(ByteCount) ->
  binary:bin_to_list(base64:encode(crypto:strong_rand_bytes(ByteCount))).

concat_string(First, Second) ->
  StrConcatChar = os:getenv("STRING_CONCAT", ":"),
  string:concat(First, string:concat(StrConcatChar, Second)).

hash_sha256(InputString) ->
  io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, InputString))]).

get_zero_string(StrLen) ->
  lists:duplicate(StrLen, $0).

get_dedicated_workers() ->
  WorkerNode = os:getenv("DEDICATED_WORKER_NODE", ""),
  case string:length(WorkerNode) > 1 of
     true -> [WorkerNode];
     false -> []
  end.

get_workers() ->
  DedicatedWorkers = get_dedicated_workers(),
  MasterNode = os:getenv("MASTER_NODE", "one@127.0.0.1"),
  lists:append([MasterNode], DedicatedWorkers).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

get_next_in_round_robbin(CurrentElement, TotalElement) ->
  ((CurrentElement - 1) rem TotalElement) + 1.