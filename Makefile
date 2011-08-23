check:
	rebar clean compile
	rebar eunit skip_deps=true && rebar ct skip_deps=true
