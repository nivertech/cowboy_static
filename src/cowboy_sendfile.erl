-module(cowboy_sendfile).
-behaviour(cowboy_http_handler).

%% cowboy callbacks
-export([init/3, handle/2, terminate/2]).

-type opt() :: {dir, string()}.

-record(state, {
    dir :: [string()]}).

init({tcp, http}, Req, Opts) ->
    {dir, Dir} = lists:keyfind(dir, 1, Opts),
    Dir1 = filename:split(Dir),
    {ok, Req, #state{dir=Dir1}}.

handle(Req, #state{dir=Dir}=State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2, State}.

terminate(Req, State) ->
    ok.

%% @private Normalize a path.
-spec abs_path(Dir::[string()], Path::[string()]) -> [string()].
abs_path(Dir, Path) ->
    Path0 = Dir ++ Path,
    abs_path_(Path0, []).

abs_path_(["."|T], Stack) ->
    abs_path_(T, Stack);
abs_path_([".."|T], [_|Stack]) ->
    abs_path_(T, Stack);
abs_path_([".."|_], _Stack) ->
    invalid;
abs_path_([H|T], Stack) ->
    abs_path_(T, [H|Stack]);
abs_path_([], Stack) ->
    lists:reverse(Stack).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

abs_path_test_() ->
    TestDir = ["tmp", "static"],
    Tests = [
        {["tmp", "static", "foo.css"], ["foo.css"]},
        {["tmp", "foo.css"], ["..", "foo.css"]},
        {["foo.css"], ["..", "..", "foo.css"]},
        {invalid, ["..", "..", "..", "foo.css"]}
    ],
    [?_assertEqual(Exp, abs_path(TestDir, Path)) || {Exp, Path} <- Tests].

-endif.
    


