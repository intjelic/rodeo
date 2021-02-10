PROJECT = rodeo
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = jiffy cowboy
dep_cowboy_commit = 2.8.0

TEST_DEPS = meck gun

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
