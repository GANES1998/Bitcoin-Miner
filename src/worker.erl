-module(worker).
-export([mine/3, main/2, mine_until_found/6]).

-record(state, {
  recurse_level,
  user_id
}).

mine(K, CallerPid,  #state{recurse_level = _RecurseLevel, user_id = UserId} = _) ->
  %% Get Random Char count from environment variables.
  RandomCharCount = os:getenv("RANDOM_SUFFIX_LEN", 12),
  %% Get Work Unit from Env Vars.
  WorkUnit = list_to_integer(os:getenv("WORK_UNIT", "100")),
  %% Mine until a successful mine or Work Unit attempts are exhausted.
  mine_until_found(1, WorkUnit, RandomCharCount, UserId, K, CallerPid).

mine_until_found(AttemptNumber, WorkUnit, RandomCharCount, UserId, K, SupervisorPid) ->
  %% Generate Random String
  RandomString = util:generate_random_str(RandomCharCount),
  %% Generate Input String using format UserId:RandomString
  HashInputString = util:concat_string(UserId, RandomString),
  %% Generate Hash Sha256
  Sha256Hash = util:hash_sha256(HashInputString),
  %% Generate Expected String (K counts of Zeros(0)).
  ZeroStr = util:get_zero_string(K),
  %% Check if the leading K chars of Sha256 is Zeros
  case string:left(Sha256Hash, K) == ZeroStr of
    true ->
      %% If so, we have successfully mined a bitcoin and thus report to supervisor and die.
      SupervisorPid ! {success, self(), HashInputString, Sha256Hash};
    false ->
      %% Check if we can attempt again using Work Unit and Attempt Number
      case AttemptNumber < WorkUnit of
        true ->
          %% Again Attempt Mining.
          mine_until_found(AttemptNumber + 1, WorkUnit, RandomCharCount, UserId, K, SupervisorPid);
        false ->
          SupervisorPid ! {nosuccess, self()}
      end
  end.



main(K, SupervisorPid) ->
  mine(K, SupervisorPid, #state{recurse_level = 0, user_id = "g.ravichandran"}).

