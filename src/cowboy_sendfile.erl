-module(cowboy_sendfile).
-behaviour(cowboy_http_handler).

%% cowboy callbacks
-export([init/3, handle/2, terminate/2]).

-record(state, {
    dir :: [binary()],
    prefix :: [binary()],
    chunk_size :: pos_integer(),
    fd  :: term()}).

init({tcp, http}, Req, Opts) ->
    {_, Dir} = lists:keyfind(dir, 1, Opts),
    Size = case lists:keyfind(chunk_size, 1, Opts) of
        {_, ISize} -> ISize;
        false -> 10240
    end,
    Prefix = case lists:keyfind(prefix, 1, Opts) of
        {_, IPrefix} -> IPrefix;
        false -> []
    end,
    {ok, Req, #state{dir=Dir, chunk_size=Size, prefix=Prefix}}.

handle(Req0, State0) ->
    try handle_request(Req0, State0) of
        {ok, Req1, #state{fd=FD, chunk_size=Size}=State1} ->
            {ok, Req2} = cowboy_http_req:chunked_reply(200, [], Req1),
            send_chunked_reply(FD, Size, Req2, State1)
    catch
        throw:{code, Code, Error, Req1} ->
            {ok, Req2} = cowboy_http_req:reply(Code, [], Error, Req1),
            {ok, Req2, State0}
    end.

%% @private Handle a request for static files.
handle_request(Req0, #state{dir=Dir, prefix=Prefix}=State) ->
    {Path0, Req1} = cowboy_http_req:path(Req0),
    Path1 = strip_prefix(Prefix, Path0),
    AbsPath = abs_path(Dir, Path1),
    AbsPath =/= invalid orelse throw({code, 403, <<"Permission denied">>, Req1}),
    case lists:prefix(Dir, AbsPath) of
        true -> ok;
        false -> throw({code, 403, <<"Permission denied">>, Req1})
    end,
    case file:open(filename:join(AbsPath), [read, binary, raw]) of
        {ok, FD} ->
            {ok, Req1, State#state{fd=FD}};
        {error, eacces} ->
            throw({code, 403, <<"Permission denied">>, Req1});
        {error, eisdir} ->
            throw({code, 403, <<"Permission denied">>, Req1});
        {error, enoent} ->
            throw({code, 404, <<"File not found">>, Req1});
        {error, Reason} ->
            throw({code, 500, ["Error opening file: ", Reason], Req1})
    end.

terminate(_Req, _State) ->
    ok.


%% @private Return an absolute file path based on the static file root.
-spec abs_path(Dir::[binary()], Path::[binary()]) -> [binary()].
abs_path(Dir, Path) ->
    Path0 = Dir ++ Path,
    abs_path_(Path0, []).

%% @private Normalize a path, removing all occurances of . and ..
-spec abs_path_(Path::[binary()], Stack::[binary()]) -> [binary()].
abs_path_([<<".">>|T], Stack) ->
    abs_path_(T, Stack);
abs_path_([<<"..">>|T], [_|Stack]) ->
    abs_path_(T, Stack);
abs_path_([<<"..">>|_], _Stack) ->
    invalid;
abs_path_([H|T], Stack) ->
    abs_path_(T, [H|Stack]);
abs_path_([], Stack) ->
    lists:reverse(Stack).


%% @private Strip a prefix from a path.
-spec strip_prefix(Prefix::[binary()], Path::[binary()]) -> [binary()].
strip_prefix([], Path) ->
    Path;
strip_prefix([H|Pref], [H|Path]) ->
    strip_prefix(Pref, Path).

%% @private Send the contents of a file to a client.
send_chunked_reply(FD, Size, Req, State) ->
    case file:read(FD, Size) of
        eof ->
            ok = file:close(FD),
            {ok, Req, State#state{fd=undefined}};
        {ok, Data} ->
            ok = cowboy_http_req:chunk(Data, Req),
            send_chunked_reply(FD, Size, Req, State)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

abs_path_test_() ->
    TestDir = [<<"tmp">>, <<"static">>],
    Tests = [
        %% Tests for ..
        {[<<"tmp">>, <<"static">>, <<"foo.css">>], [<<"foo.css">>]},
        {[<<"tmp">>, <<"foo.css">>], [<<"..">>, <<"foo.css">>]},
        {[<<"foo.css">>], [<<"..">>, <<"..">>, <<"foo.css">>]},
        {invalid, [<<"..">>, <<"..">>, <<"..">>, <<"foo.css">>]},
        %% Tests for .
        {[<<"tmp">>, <<"static">>, <<"foo.css">>], [<<".">>, <<"foo.css">>]}
    ],
    [?_assertEqual(Exp, abs_path(TestDir, Path)) || {Exp, Path} <- Tests].

-endif.
