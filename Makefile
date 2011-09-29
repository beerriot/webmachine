ERL          ?= erl
APP          := webmachine

.PHONY: deps

all: deps
	@(./rebar compile)

deps:
	@(./rebar get-deps)

clean:
	@(./rebar clean)

distclean: clean
	@(./rebar delete-deps)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all
	@(./rebar skip_deps=true eunit)

graph:
	escript scripts/wdc_graph.erl \
	src/webmachine_decision_core.erl \
	docs/wdc_graph.dot
	fdp -Tpng -odocs/wdc_graph.png docs/wdc_graph.dot