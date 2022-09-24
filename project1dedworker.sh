rm ebin/*.beam

erlc -o ebin src/*.erl

source constants.env

export DEDICATED_WORKER_NODE=$DEDICATED_WORKER_NODE

$ERLANG_BIN -pa "ebin" -setcookie "${ERLANG_COOKIE}" -name "${DEDICATED_WORKER_NODE}" -eval "worker:main_dedicated(\"$1\")." -s init stop -noshell
