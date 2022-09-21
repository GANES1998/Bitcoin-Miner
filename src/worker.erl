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
-export([mine/3, main/2]).

-record(state, {
  recurse_level,
  user_id
}).

mine(K, CallerPid,  #state{recurse_level = _RecurseLevel, user_id = UserId} = _) ->
  RandomCharCount = os:getenv("RANDOM_SUFFIX_LEN", 12),
  RandomString = util:generate_random_str(RandomCharCount),
  HashInputString = util:concat_string(UserId, RandomString),
  Sha256Hash = util:hash_sha256(HashInputString),
  ZeroStr = util:get_zero_string(K),
%%  io:format("~p ~p ~p ~n", [Sha256Hash, string:left(Sha256Hash, K), ZeroStr]),
  case string:left(Sha256Hash, K) == ZeroStr of
    true ->
      CallerPid ! {success, self(), HashInputString, Sha256Hash};
    false ->
      CallerPid ! {nosuccess, self()}
  end.

main(K, SupervisorPid) ->
  mine(K, SupervisorPid, #state{recurse_level = 0, user_id = "g.ravichandran"}).

