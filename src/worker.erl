%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 1:29 pm
%%%-------------------------------------------------------------------
-module(worker).
-author("ganesonravichandran").
-export([mine/3, main/2, mine_until_found/6]).

-record(state, {
  recurse_level,
  user_id
}).

mine(K, CallerPid,  #state{recurse_level = _RecurseLevel, user_id = UserId} = _) ->
  RandomCharCount = os:getenv("RANDOM_SUFFIX_LEN", 12),
  WorkUnit = list_to_integer(os:getenv("WORK_UNIT", "100")),
  mine_until_found(1, WorkUnit, RandomCharCount, UserId, K, CallerPid).

mine_until_found(AttemptNumber, WorkUnit, RandomCharCount, UserId, K, SupervisorPid) ->
  RandomString = util:generate_random_str(RandomCharCount),
  HashInputString = util:concat_string(UserId, RandomString),
  Sha256Hash = util:hash_sha256(HashInputString),
  ZeroStr = util:get_zero_string(K),
  case string:left(Sha256Hash, K) == ZeroStr of
    true ->
      SupervisorPid ! {success, self(), HashInputString, Sha256Hash};
    false ->
      case AttemptNumber < WorkUnit of
        true ->
          mine_until_found(AttemptNumber + 1, WorkUnit, RandomCharCount, UserId, K, SupervisorPid);
        false ->
          SupervisorPid ! {nosuccess, self()}
      end
  end.



main(K, SupervisorPid) ->
  mine(K, SupervisorPid, #state{recurse_level = 0, user_id = "g.ravichandran"}).

