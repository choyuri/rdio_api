REBAR?=rebar

CTARGS=""

build:
	$(REBAR) compile

check: typecheck test

typecheck:
	$(REBAR) dialyzer

test:
	mkdir -p logs; \
	ct_run $(CTARGS) -dir ./ -logdir logs -pa ebin -pa deps/*/ebin -config test/tokens.config -erl_args -config rdio_api

.PHONY: build check typecheck test
