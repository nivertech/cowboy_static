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

%% exported functions
-export([rule/1]).

%% cowboy callbacks
-export([init/3, handle/2, terminate/2]).

%% type aliases
-type uint() :: non_neg_integer().

%% handler config
-record(conf, {
    dir      :: [binary()],
    prefix   :: [binary()],
    csize    :: pos_integer(),
    ranges   :: boolean(),
    usesfile :: boolean()}).

%% handler state
-record(state, {
    method :: 'GET' | 'HEAD',
    path   :: [binary()],
    finfo  :: #file_info{},
    ranges :: [{uint(), uint(), uint()}],
    fd     :: term()}).

-type option()
   :: {prefix, [binary()]}
    | {chunk_size, pos_integer()}
    | {ranges, boolean()}
    | {sendfile, boolean()}.

%% @doc Return a cowboy dispatch rule.
%% @end
-spec rule([option()]) -> term().
rule(Opts) ->
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
    Sendfile = case lists:keyfind(sendfile, 1, Opts) of
        {_, ISendfile} -> ISendfile;
        false -> true
    end,
    Conf = #conf{
        dir=Dir,
        csize=Size,
        prefix=Prefix,
        ranges=Ranges,
        usesfile=Sendfile},
    Pattern = Prefix ++ ['...'],
    {Pattern, ?MODULE, Conf}.


init({tcp, http}, Req, Conf) when is_record(Conf, conf) ->
    {ok, Req, Conf};
init({tcp, http}, Req, Opts) when is_list(Opts) ->
    {_, _, Conf} = rule(Opts),
    {ok, Req, Conf}.

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
    {Path0, Req1} = cowboy_http_req:path_info(Req0),
    case abs_path(Dir, Path0) of
        invalid ->
            %% @todo Better response code?
            {ok, Req2} = cowboy_http_reply:reply(404, [], <<>>, Req1),
            {ok, Req2, Conf};
        Path1 ->
            validate_path_allowed(Req1, Conf, State#state{path=Path1})
    end.

validate_path_allowed(Req0, #conf{dir=Dir}=Conf, #state{path=Path}=State) ->
    case lists:prefix(Dir, Path) of
        false ->
            {ok, Req1} = cowboy_http_req:reply(404, [], <<>>, Req0),
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
    {RawPath, Req1} = cowboy_http_req:raw_path(Req0),
    LastChar = binary:last(RawPath),
    case FInfo of
        #file_info{type=regular} ->
            validate_resource_access(Req1, Conf, State);
        #file_info{type=directory} when LastChar =:= $/ ->
            {ok, Req2} = cowboy_http_req:reply(404, [], <<>>, Req1),
            {ok, Req2, Conf};
        #file_info{type=directory} when LastChar =/= $/ ->
            #http_req{transport=Transport} = Req1,
            Protocol = Transport:name(),
            %% @todo If we are running behind a proxy this is not a
            %% reliable mechanism to determine if the client connects
            %% using an http or https connection. likely downgrade.
            Scheme = case Protocol of
                tcp -> <<"http://">>;
                ssl -> <<"https://">>
            end,
            {Port, Req2} = cowboy_http_req:port(Req1),
            PortStr = case Scheme of
                <<"http://">> when Port =:= 80 ->
                    <<>>;
                <<"http://">> ->
                    [$:|integer_to_list(Port)];
                <<"https://">> when Port =:= 443 ->
                    <<>>;
                <<"https://">> ->
                    [$:|integer_to_list(Port)]
            end,
            {RawHost, Req3} = cowboy_http_req:raw_host(Req2),
            {RawQS, Req4} = cowboy_http_req:raw_qs(Req3),
            QueryString = case RawQS of
                <<>> -> <<>>;
                _ -> [$?|RawQS]
            end,
            RedirectURL = iolist_to_binary([
                Scheme,
                RawHost,
                PortStr,
                RawPath, $/,
                QueryString]),
            Headers = [{<<"Location">>, RedirectURL}],
            {ok, Req5} = cowboy_http_req:reply(301, Headers, <<>>, Req1),
            {ok, Req5, Conf};
        _Other ->
            {ok, Req2} = cowboy_http_req:reply(404, [], <<>>, Req1),
            {ok, Req2, Conf}
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



%% The sendfile module expects to be sent a filename and handles opening the
%% file itself. When using the regular file module we really want to aquire
%% the file handle as soon as possible in order to bail out early.
open_file_handle(Req, Conf, State) when Conf#conf.usesfile ->
    init_send_reply(Req, Conf, State);
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
    init_send_multipart_response(Req, Conf, State);
init_send_reply(Req, Conf, #state{ranges=error}=State) ->
    init_send_complete_response(Req, Conf, State);
init_send_reply(Req, Conf, State) ->
    init_send_complete_response(Req, Conf, State).


init_send_complete_response(Req0, Conf, State) ->
    #state{finfo=FInfo, path=Path} = State,
    #file_info{size=ContentLength} = FInfo,
    BinContentLength = list_to_binary(integer_to_list(ContentLength)),
    Headers = [
        {<<"Content-Length">>, BinContentLength}],
    {ok, Req1} = cowboy_http_req:reply(200, Headers, <<>>, Req0),
    %% The response to a HEAD Request is expected to be the same as a GET
    %% except for the lack of a Response body. Stop right before sending
    %% the response body and use the same code for everything else.
    case State#state.method of
        'HEAD' ->
            {ok, Req1, Conf};
        'GET' ->
            #http_req{socket=Socket} = Req1,
            init_send_file_contents(Req1, Conf, State, 0, ContentLength)
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

%% If a byte-range request only contains one byte range, the contents of
%% that range can be sent to the client using a normal response body.
init_send_partial_response(Req0, Conf, State) ->
    #state{ranges=[{Start, End, Length}], finfo=FInfo} = State,
    #file_info{size=ContentLength} = FInfo,
    LengthStr  = integer_to_list(Length),
    ContLenStr = integer_to_list(ContentLength),
    Headers = [
        {<<"Content-Length">>, list_to_binary(integer_to_list(Length))},
        cowboy_sendfile_range:make_range(Start, End, ContentLength)],
    {ok, Req1} = cowboy_http_req:reply(206, Headers, <<>>, Req0),
    init_send_file_contents(Req1, Conf, State, Start, Length).

%% If a byte-range request contains multiple ranges. The contents of
%% the ranges must be sent as parts of a multipart response.
init_send_multipart_response(Req0, Conf, State) ->
    #state{ranges=Ranges, finfo=FInfo} = State,
    #file_info{size=FileSize} = FInfo,
    Boundary = cowboy_sendfile_multipart:make_boundary(),
    Partial = cowboy_sendfile_multipart:partial(Ranges, Boundary, FileSize),
    ContentLength = cowboy_sendfile_multipart:content_length(Partial),
    ContentLengthStr = list_to_binary(integer_to_list(ContentLength)),
    ContentTypeStr = cowboy_sendfile_multipart:content_type(Boundary),
    Headers = [
        {<<"Content-Type">>, ContentTypeStr},
        {<<"Content-Length">>, ContentLengthStr}],
    {ok, Req1} = cowboy_http_req:reply(206, Headers, <<>>, Req0),
    #http_req{socket=Socket, transport=Transport} = Req1,
    send_multipart_response(Req1, Conf, State, Partial, Transport, Socket).


send_multipart_response(Req, Conf, _State, [], _Transport, _Socket) ->
    {ok, Req, Conf};

send_multipart_response(Req0, Conf, State, [{_,_,_}=H|T], Transport, Socket) ->
    {Start, _, Length} = H,
    {ok, Req1, _} = init_send_file_contents(Req0, Conf, State, Start, Length),
    send_multipart_response(Req1, Conf, State, T, Transport, Socket);

send_multipart_response(Req, Conf, State, [IOList|T], Transport, Socket) ->
    ok = Transport:send(Socket, IOList),
    send_multipart_response(Req, Conf, State, T, Transport, Socket).


%% Stream the contents of a file to a socket. This assumes that the response
%% headers has been sent and the Content-Length header was set to 'Length'.
init_send_file_contents(Req, Conf, State, Start, Length) when Conf#conf.usesfile ->
    #http_req{socket=Socket} = Req,
    #state{path=Path} = State,
    {ok, Length} = sendfile:send(Socket, Path, Start, Length),
    {ok, Req, Conf};
init_send_file_contents(Req, Conf, State, Start, Length) ->
    #http_req{socket=Socket, transport=Transport} = Req,
    #state{fd=FD} = State,
    #conf{csize=ChunkSize} = Conf,
    {ok, Start} = file:position(FD, {bof, Start}),
    send_file_contents(Req, Conf, State, Transport, Socket, FD, ChunkSize, Length).


send_file_contents(Req, Conf, State, Transport, Socket, FD, ChunkSize, 0) ->
    %% file:close(FD),
    {ok, Req, Conf};
send_file_contents(Req1, Conf, State, Transport, Socket, FD, ChunkSize, N) ->
    NBytes = if N < ChunkSize -> N; true -> ChunkSize end,
    case file:read(FD, NBytes) of
        {ok, Data} when byte_size(Data) =:= NBytes ->
            ok = Transport:send(Socket, Data),
            send_file_contents(Req1, Conf, State, Transport, Socket, FD, ChunkSize, N-NBytes)
    end.

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
