PROJECT = pdata

# Options.

CT_SUITES = pdata

# Dependencies

PKG_FILE_URL = https://gist.github.com/CamShaft/815c139ad3c1ccf13bad/raw/packages.tsv

DEPS = fast_key
dep_fast_key = pkg://fast_key master

# Standard targets.

include erlang.mk
