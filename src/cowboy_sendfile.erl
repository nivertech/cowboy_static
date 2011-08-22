%% Copyright (c) 2011, Magnus Klaar <magnus.klaar@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cowboy_sendfile).
-behaviour(cowboy_http_handler).

%% include files
-include_lib("kernel/include/file.hrl").
-include_lib("cowboy/include/http.hrl").

%% cowboy callbacks
-export([init/3, handle/2, terminate/2]).

%% type aliases
-type uint() :: non_neg_integer().

%% handler config
-record(conf, {
    dir    :: [binary()],
    prefix :: [binary()],
    csize  :: pos_integer(),
    ranges :: boolean()}).

%% handler state
-record(state, {
    method :: 'GET' | 'HEAD',
    path   :: [binary()],
    finfo  :: #file_info{},
    ranges :: [{uint(), uint(), uint()}],
    fd     :: term()}).

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
    Ranges = case lists:keyfind(ranges, 1, Opts) of
        {_, IRanges} -> IRanges;
        false -> true
    end,
    {ok, Req, #conf{dir=Dir, csize=Size, prefix=Prefix, ranges=Ranges}}.

handle(Req, Conf) ->
    method_allowed(Req, Conf, #state{}).

terminate(_Req, _Conf) ->
    ok.

method_allowed(Req0, Conf, State) ->
    case cowboy_http_req:method(Req0) of
        {'GET', Req1} ->
            validate_path(Req1, Conf, State#state{method='GET'});
        {'HEAD', Req1} ->
            validate_path(Req1, Conf, State#state{method='HEAD'});
        {_, Req1} ->
            {ok, Req2} = cowboy_http_req:reply(405, [], <<>>, Req1),
            {ok, Req2, Conf}
    end.

validate_path(Req0, #conf{dir=Dir}=Conf, State) ->
    {Path0, Req1} = cowboy_http_req:path(Req0),
    case abs_path(Dir, Path0) of
        invalid ->
            {ok, Req2} = cowboy_http_reply:reply(403, [], <<>>, Req1),
            {ok, Req2, Conf};
        Path1 ->
            validate_path_allowed(Req1, Conf, State#state{path=Path1})
    end.

validate_path_allowed(Req0, #conf{dir=Dir}=Conf, #state{path=Path}=State) ->
    case lists:prefix(Dir, Path) of
        false ->
            {ok, Req1} = cowboy_http_req:reply(403, [], <<>>, Req0),
            {ok, Req1, Conf};
        true ->
            resource_exists(Req0, Conf, State#state{path=filename:join(Path)})
    end.

resource_exists(Req0, Conf, #state{path=Path}=State) ->
    case file:read_file_info(Path) of
        {ok, #file_info{}=FInfo} ->
            validate_resource_type(Req0, Conf, State#state{finfo=FInfo});
        {error, enoent} ->
            {ok, Req1} = cowboy_http_req:reply(404, [], <<>>, Req0),
            {ok, Req1, Conf}
    end.

validate_resource_type(Req0, Conf, #state{finfo=FInfo}=State) ->
    case FInfo of
        #file_info{type=regular} ->
            validate_resource_access(Req0, Conf, State);
        _Other ->
            {ok, Req1} = cowboy_http_req:reply(404, [], <<>>, Req0),
            {ok, Req1, Conf}
    end.

validate_resource_access(Req0, Conf, #state{finfo=FInfo}=State) ->
    case FInfo of
        #file_info{access=read} ->
            range_header_exists(Req0, Conf, State);
        #file_info{access=read_write} ->
            range_header_exists(Req0, Conf, State);
        _Other ->
            {ok, Req1} = cowboy_http_req:reply(403, [], <<>>, Req0),
            {ok, Req1, Conf}
    end.

range_header_exists(Req0, Conf, #state{finfo=FInfo}=State) when Conf#conf.ranges ->
    #file_info{size=ContentLength} = FInfo,
    case cowboy_http_req:header('Range', Req0) of
        {undefined, Req1} ->
            open_file_handle(Req1, Conf, State#state{ranges=none});
        {RangesBin, Req1} ->
            Ranges = cowboy_sendfile_range:parse_range(RangesBin, ContentLength),
            open_file_handle(Req1, Conf, State#state{ranges=Ranges})
    end;
range_header_exists(Req, Conf, State) ->
    open_file_handle(Req, Conf, State#state{ranges=none}).


open_file_handle(Req0, Conf, #state{path=Path}=State) ->
    case file:open(Path, [read,binary,raw]) of
        {ok, FD} ->
            init_send_reply(Req0, Conf, State#state{fd=FD});
        {error, eacces} ->
            {ok, Req1} = cowboy_http_req:reply(403, [], <<>>, Req0),
            {ok, Req1, Conf};
        {error, eisdir} ->
            {ok, Req1} = cowboy_http_req:reply(403, [], <<>>, Req0),
            {ok, Req1, Conf};
        {error, enoent} ->
            {ok, Req1} = cowboy_http_req:reply(404, [], <<>>, Req0),
            {ok, Req1, Conf};
        {error, Reason} ->
            Error = io_lib:format("Error opening file: ~p~n", [Reason]),
            {ok, Req1} = cowboy_http_req:reply(500, [], Error, Req0),
            {ok, Req1, Conf}
    end.


init_send_reply(Req, Conf, #state{ranges=[_]}=State) ->
    init_send_partial_response(Req, Conf, State);
init_send_reply(Req, Conf, #state{ranges=[_|_]}=State) ->
    %% init_send_multipart_response(Req, Conf, State);
    exit(multipart_response_not_implemented);
init_send_reply(Req, Conf, State) ->
    init_send_chunked_response(Req, Conf, State).


init_send_chunked_response(Req0, Conf, State) ->
    {ok, Req1} = cowboy_http_req:chunked_reply(200, [], Req0),
    case State#state.method of
        'GET' ->
            send_chunked_response_body(Req1, Conf, State);
        'HEAD' ->
            {ok, Req1, Conf}
    end.


send_chunked_response_body(Req, Conf, State) ->
    #conf{csize=ChSize} = Conf,
    #state{finfo=FInfo, fd=FD} = State,
    #file_info{size=CoLength} = FInfo,
    send_chunked_response_body(Req, Conf, State, FD, ChSize, CoLength).

send_chunked_response_body(Req, Conf, _State, FD, _ChSize, 0) ->
    file:close(FD),
    {ok, Req, Conf};
send_chunked_response_body(Req, Conf, State, FD, ChSize, N) ->
    NBytes = if N < ChSize -> N; true -> ChSize end,
    case file:read(FD, NBytes) of
        {ok, Data} when byte_size(Data) =:= NBytes ->
            ok = cowboy_http_req:chunk(Data, Req),
            send_chunked_response_body(Req, Conf, State, FD, ChSize, N-NBytes)
    end.


init_send_partial_response(Req0, Conf, State) ->
    #state{ranges=[{Start, End, Length}], finfo=FInfo, fd=FD} = State,
    #file_info{size=ContentLength} = FInfo,
    #conf{csize=ChunkSize} = Conf,
    StartStr   = integer_to_list(Start),
    EndStr     = integer_to_list(End),
    LengthStr  = integer_to_list(Length),
    ContLenStr = integer_to_list(ContentLength),
    Headers = [
        {<<"Content-Range">>,
            iolist_to_binary([<<"bytes ">>, StartStr, $-, EndStr, $/, ContLenStr])}],
    {ok, Req1} = cowboy_http_req:chunked_reply(206, Headers, Req0),
    {ok, Start} = file:position(FD, {bof, Start}),
    send_chunked_response_body(Req1, Conf, State, FD, ChunkSize, Length).


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
