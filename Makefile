APPS = kernel stdlib sasl erts ssl tools runtime_tools crypto inets \
	public_key mnesia syntax_tools compiler
COMBO_PLT = $(HOME)/.ivanos_combo_dialyzer_plt

.PHONY: all compile deps docs test clean distclean ct

all: generate

generate: compile
	./rebar skip_deps=true generate -f

compile: deps 
	./rebar -r compile 

deps:
	./rebar get-deps

updates:
	./rebar update-deps

eunit: compile
	./rebar -v skip_deps=true eunit

docs:
	cd apps/ivanos; ../../rebar doc skip_deps=true
	cp apps/ivanos/priv/ivanos.png apps/ivanos/doc/erlang.png


ct: compile
	./rebar -v ct $(CTARGS)

distclean: clean
	./rebar delete-deps

clean:
	./rebar clean

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	dialyzer --plt $(COMBO_PLT) ebin


compile test clean: rebar

rebar:
	wget -c http://github.com/rebar/rebar/wiki/rebar
	chmod +x $@
