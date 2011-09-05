-module(cowboy_sendfile_multipart).
-export([make_boundary/0,
         content_length/3]).

-type range() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

%% @doc Return a random string binary suitable for use as a boundary.
%% @end
-spec make_boundary() -> binary().
make_boundary() ->
    <<"fkj49sn38dcn3">>.


%% @doc Return the content length of a multipart response.
%% The total size of the multipart response must be calculated before we begin
%% sending it. We can do this by outputting everything except the actual ranges
%% to a temporary buffer and adding the size of this buffer to the sum of the
%% byte-ranges that we will send in the response.
%% @end
-spec content_length([range()], binary(), non_neg_integer()) -> non_neg_integer().
content_length(Ranges, Boundary, FileSize) ->
    boundary_length(Ranges, Boundary) +
    content_range_length(Ranges, FileSize) +
    content_type_length(Ranges) +
    (length(Ranges) * 4) + %% CRLF after headers and content
    2. %% CRLF before first boundary

content_range_length(Ranges, FileSize) ->
    content_range_length(Ranges, FileSize, 0).

content_range_length([], _FileSize, Sum) ->
    Sum;
content_range_length([{Start, End, Length}|T], FileSize, Sum) ->
    {_, RangeSpec} = cowboy_sendfile_range:make_range(Start, End, FileSize),
    HeaderLen = 21 + byte_size(RangeSpec) + 2, %% "Content-Range: bytes " + ... + "\r\n"
    content_range_length(T, FileSize, Sum + HeaderLen + Length).

content_type_length(Ranges) ->
    NumRanges = length(Ranges),
    NumRanges * byte_size(<<"Content-Type: application/octet-stream\r\n">>).

boundary_length(Ranges, Boundary) ->
    NumRanges = length(Ranges),
    MidBoundaryLen = 2 + byte_size(Boundary) + 2, %% "--" + ... + "\r\n"
    EndBoundaryLen = 2 + byte_size(Boundary) + 4, %% "--" + ... + "--\r\n"
    (MidBoundaryLen * NumRanges) +  EndBoundaryLen.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

content_length0_test() ->
    Ranges = [{0,5,6},{5,10,6}],
    Boundary = <<"fkj49sn38dcn3">>,
    FileSize = 73,
    ?assertEqual(214, content_length(Ranges, Boundary, FileSize)).

content_length1_test() ->
    Ranges = [{0,5,6},{5,10,6},{10,11,2}],
    Boundary = <<"fkj49sn38dcn3">>,
    FileSize = 73,
    ?assertEqual(308, content_length(Ranges, Boundary, FileSize)).


-endif.
