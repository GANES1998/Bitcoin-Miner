rm ebin/*.beam

erlc -o ebin src/*.erl

source constants.env

export DEDICATED_WORKER_NODE=$DEDICATED_WORKER_NODE

/opt/homebrew/Cellar/erlang/25.0.2/lib/erlang/bin/erl -pa "ebin" -setcookie "${ERLANG_COOKIE}" -name "${DEDICATED_WORKER_NODE}"