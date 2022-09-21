rm ebin/*.beam

erlc -o ebin src/*.erl

/opt/homebrew/Cellar/erlang/25.0.2/lib/erlang/bin/erl -pa "ebin" -eval "mine_supervisor:supervise($1)." -s init stop -noshell