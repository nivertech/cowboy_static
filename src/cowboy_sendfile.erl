-module(cowboy_sendfile).
-behaviour(cowboy_http_handler).

%% cowboy callbacks
-export([init/3, handle/2, terminate/2]).

-record(state, {
    dir :: [string()],
    chunk_size :: pos_integer(),
    fd  :: term()}).

init({tcp, http}, Req, Opts) ->
    {dir, Dir} = lists:keyfind(dir, 1, Opts),
    {chunk_size, Size} = lists:keyfind(chunk_size, 1, Opts),
    Dir1 = filename:split(Dir),
    {ok, Req, #state{dir=Dir1, chunk_size=Size}}.

handle(Req0, State0) ->
    try handle_request(Req0, State0) of
        {ok, Req1, #state{fd=FD, chunk_size=Size}=State1} ->
            Hdrs = [{<<"Content-type">>, <<"text/plain">>}],
            {ok, Req2} = cowboy_http_req:chunked_reply(200, Hdrs, Req1),
            send_chunked_reply(FD, Size, Req2, State1)
    catch
        throw:{code, Code, Error, Req1} ->
            {ok, Req2} = cowboy_http_req:reply(Code, [], Error, Req1),
            {ok, Req2, State0}
    end.

%% @private Handle a request for static files.
handle_request(Req0, #state{dir=Dir}=State) ->
    {Path, Req1} = cowboy_http_req:path(Req0),
    AbsPath = abs_path(Dir, Path),
    AbsPath =/= invalid orelse throw({code, 404, "File not found", Req1}),
    WithinDir = lists:prefix(Dir, AbsPath),
    WithinDir orelse throw({code, 404, "File not found", Req1}),
    case file:open(filename:join(AbsPath), [read, binary, raw]) of
        {ok, FD} ->
            {ok, Req1, State#state{fd=FD}};
        {error, enoent} ->
            throw({code, 404, <<"File not found">>, Req1});
        {error, Reason} ->
            throw({code, 500, ["Error opening file: ", Reason], Req1})
    end.

terminate(_Req, _State) ->
    ok.


%% @private Return an absolute file path based on the static file root.
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

%% @private Send the contents of a file to a client.
send_chunked_reply(FD, Size, Req, State) ->
    case file:read(FD, Size) of
        eof ->
            ok = file:close(FD),
            {ok, Req, State#state{fd=undefined}};
        {ok, <<>>} ->
            ok = file:close(FD),
            {ok, Req, State#state{fd=undefined}};
        {ok, Data} ->
            ok = cowboy_http_req:chunk(Data, Req),
            send_chunked_reply(FD, Size, Req, State)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

abs_path_test_() ->
    TestDir = ["tmp", "static"],
    Tests = [
        %% Tests for ..
        {["tmp", "static", "foo.css"], ["foo.css"]},
        {["tmp", "foo.css"], ["..", "foo.css"]},
        {["foo.css"], ["..", "..", "foo.css"]},
        {invalid, ["..", "..", "..", "foo.css"]},
        %% Tests for .
        {["tmp", "static", "foo.css"], [".", "foo.css"]}
    ],
    [?_assertEqual(Exp, abs_path(TestDir, Path)) || {Exp, Path} <- Tests].

-endif.
    


