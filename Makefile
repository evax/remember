all: compile

deps:
	@rebar get-deps

compile: deps
	@rebar compile

clean:
	@rebar clean
	@rm -Rf deps

release:
	@make clean
	@make compile
	@relx release tar

relup:
	@make clean
	@make compile
	@relx release relup tar

