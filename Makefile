all:
	test -d deps || rebar get-deps
	rebar compile
	@erl -noshell -pa './ebin' -s erlplay start
