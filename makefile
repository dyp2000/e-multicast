#!/usr/bin/colormake
PATH=$PATH:/bin:/usr/bin:/usr/local/lib/erlang/bin:/usr/local/lib/erlang/lib:/usr/local/bin
PRJ_ROOT=$(shell pwd)

DEBUG=+debug_info
ERLC_FLAGS=-W -I ./include 
#-I ./deps/gpb/include
REBAR=/usr/local/bin/rebar
NONE = "\033[0m"
WARNING = "\033[33m"
ERROR = "\033[1;32m"
INFO = "\033[1;36m"

all: compile

jsonx:
	@echo "== Recompile jsonx.so =="
	make --directory=deps/jsonx all

compile:
	@echo $(WARNING)"== Server compile =="$(NONE)
	@cd $(PRJ_ROOT)
	@echo $(INFO)"Current dir: "$(PRJ_ROOT)$(NONE)
	@echo $(WARNING)"Clear ebin..."$(NONE)
	@rm -f $(PRJ_ROOT)/ebin/*.*
	@cp $(PRJ_ROOT)/src/emc_app.app.src $(PRJ_ROOT)/ebin/emc_app.app
	@echo $(WARNING)"Application compile..."$(NONE)
	erlc $(DEBUG) $(ERLC_FLAGS) -o $(PRJ_ROOT)/ebin $(PRJ_ROOT)/src/*.erl

release:
	@echo "== Server compile =="
	@cd $(PRJ_ROOT)
	@echo "Clear ebin..."
	@rm -f $(PRJ_ROOT)/ebin/*.*
	@cp $(PRJ_ROOT)/src/emc_app.app.src $(PRJ_ROOT)/ebin/emc_app.app
	@echo "Application compile..."
	erlc $(ERLC_FLAGS) -o $(PRJ_ROOT)/ebin $(PRJ_ROOT)/src/*.erl

get-deps:
	$(REBAR) get-deps compile

deps-recompile:
	$(REBAR) compile

update:
	$(REBAR) update-deps compile

doc:
	$(REBAR) doc

clean:
	rm -f $(PRJ_ROOT)/erl_crash.dump
	$(REBAR) clean
