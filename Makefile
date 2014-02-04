PROJECT = lgx

# Options.

CT_SUITES = lgx

# Dependencies

PKG_FILE_URL = https://gist.github.com/camshaft/815c139ad3c1ccf13bad/raw/packages.tsv

DEPS = fast_key
dep_fast_key = pkg://fast_key master

# Standard targets.

include erlang.mk

deps/horse:
	git clone -n -- https://github.com/extend/horse $(DEPS_DIR)/horse
	cd $(DEPS_DIR)/horse ; git checkout -q master
	$(MAKE) -C $(DEPS_DIR)/horse

perfs: ERLC_OPTS += -DPERF=1 +'{parse_transform, horse_autoexport}'
perfs: clean deps deps/horse app
	$(gen_verbose) erl -noshell -pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'
