PROJECT = rodeo
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
