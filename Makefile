all:
#	@rebar -vv compile
	@rebar compile
	@rebar escriptize

clean:
	@rebar clean
	rm -f erl_crash.dump

distclean:
	@rm -f core* vgcore.*
