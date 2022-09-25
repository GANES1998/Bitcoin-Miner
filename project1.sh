rm -f ebin/*.beam

erlc -o ebin src/*.erl

source constants.env

export MASTER_NODE=$MASTER_NODE

$ERLANG_BIN -pa "ebin" -setcookie "${ERLANG_COOKIE}" -name "${MASTER_NODE}" -eval "mine_supervisor:supervise($1)." -s init stop -noshell