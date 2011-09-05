-module(cowboy_sendfile_multipart).
-export([make_boundary/0,
         partial/3,
         content_length/1]).

-type range() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

%% @doc Return a random string binary suitable for use as a boundary.
%% @end
-spec make_boundary() -> binary().
make_boundary() ->
    <<"fkj49sn38dcn3">>.


%% @doc Return a partial response.
%% The total size of the multipart response must be calculated before we begin
%% sending it. We can accomplish this by writing everything except the content
%% of each range to a temprary buffer. The content_lengt/1 function is used to
%% calculate the effective size based on this buffer. Callers are responsible
%% for replacing occurances of range() terms in the response with the actual
%% contents of the range.
%% @end
-spec partial([range()], binary(), non_neg_integer()) -> [range() | iolist()].
partial(Ranges, Boundary, FileSize) ->
    %% Insert CRLF before the first boundary.
    [<<"\r\n">>|partial_(Ranges, Boundary, FileSize)].

partial_([], Boundary, _FileSize) ->
    %% A boundary string starting and ending with -- terminates the last section.
    [[<<"--">>, Boundary, <<"--\r\n">>]];
partial_([{Start, End, _}=H|T], Boundary, FileSize) ->
    %% A boundary string starting with -- is written before each section.
    PreBoundary = [<<"--">>, Boundary, <<"\r\n">>],
    %% A Content-Range header defines the byte-range of this section.
    {_, RangeSpec} = cowboy_sendfile_range:make_range(Start, End, FileSize),
    ContentRangeHdr = [<<"Content-Range: bytes ">>, RangeSpec, <<"\r\n">>],
    %% Use a default type header before MIME type support is implemented.
    ContentTypeHdr = <<"Content-Type: application/octet-stream\r\n">>,
    %% A CRLF separates the headers of each section from the content.
    EndHeaders = <<"\r\n">>,
    %% A CRLF separates the content from the boundary value.
    EndContent = <<"\r\n">>,
    BeforeRange = [
        PreBoundary,
        ContentRangeHdr,
        ContentTypeHdr,
        EndHeaders],
    AfterRange = [
        EndContent],
    [BeforeRange,H,AfterRange|partial_(T, Boundary, FileSize)].


%% @doc Return the content length of a multipart response.
%% The input is expected to be a partial response. All range() terms are
%% expected to occur as an element of the top-level list of the response.
%% @end
-spec content_length([iolist() | range()]) -> non_neg_integer().
content_length(Partial) ->
    content_length(Partial, 0).

content_length([], Sum) ->
    Sum;
content_length([{_,_,Length}|T], Sum) ->
    content_length(T, Sum + Length);
content_length([H|T], Sum) ->
    content_length(T, Sum + iolist_size(H)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

content_length0_test() ->
    Partial = partial([{0,5,6},{5,10,6}], <<"fkj49sn38dcn3">>, 73),
    ?assertEqual(214, content_length(Partial)).

content_length1_test() ->
    Partial = partial([{0,5,6},{5,10,6},{10,11,2}], <<"fkj49sn38dcn3">>, 73),
    ?assertEqual(308, content_length(Partial)).


-endif.
