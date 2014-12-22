PROJECT = expr

# Standard targets.

include erlang.mk

deps/horse:
	git clone -n -- https://github.com/extend/horse $(DEPS_DIR)/horse
	cd $(DEPS_DIR)/horse ; git checkout -q master
	$(MAKE) -C $(DEPS_DIR)/horse

perfs: ERLC_OPTS += -DPERF=1 +'{parse_transform, horse_autoexport}'
perfs: clean deps deps/horse app ebin/benchmarks_test.beam
	$(gen_verbose) erl -noshell -pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'

fprof.cgrind: deps app ebin/benchmarks_test.beam
	@erl \
	  -noshell \
	  -pa ebin \
	  -pa deps/*/ebin \
	  -eval "benchmarks_test:execute(), fprof:start(), fprof:apply(benchmarks_test, execute, []), fprof:profile()." \
	  -s init stop
	@erlgrind fprof.trace


test: eunit

ebin/%.beam: test/%.erl
	@erlc -o ebin $<

eunit: app ebin/expr_test.beam test/data/*
	@erl \
	  -noshell \
	  -pa ebin \
	  -eval "eunit:test($(PROJECT)_test, [verbose])" \
	  -s init stop

.PHONY: eunit
