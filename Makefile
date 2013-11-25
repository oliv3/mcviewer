all:
#	@rebar -vv compile
	@rebar compile
	@rebar escriptize

clean:
	@rebar clean
	@./clean-dump.sh

distclean:
	@rm -f core* vgcore.*
