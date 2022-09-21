rm ebin/*.beam

erlc -o ebin src/*.erl

erl -pa ebin -noshell -s mine_supervisor supervise $1

