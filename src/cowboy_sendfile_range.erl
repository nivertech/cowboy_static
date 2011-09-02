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

-module(cowboy_sendfile_range).
-export([get_range/1,
         parse_range/2,
         make_range/3,
         make_range/2]).

-type uint() :: non_neg_integer().


%% @doc Get the range requested by a client.
%% This function will return the raw value of the Range header. The parse_range
%% function must be used to convert this into a usable form.
%% @end
-spec get_range(T) -> {boolean(), T}.
get_range(Req) ->
    cowboy_http_req:header(<<"Range">>, Req).

%% @doc Return a usable form of a Range header value.
%% The range returned by this function is inclusive.
%% @end
-spec parse_range(BinRange::binary(), ContentLength::pos_integer()) ->
          {Start::pos_integer(), End::pos_integer()}.
parse_range(<<"bytes=", RangeSetBin/binary>>, ContentLength) ->
    RangeSetBins = binary:split(RangeSetBin, [<<",">>]),
    RangeSet = [parse_range_spec(E, ContentLength) || E <- RangeSetBins],
    %% @todo If an invalid range occurs in the Range header all ranges
    %% that are valid should be included in the response instead. Ensure
    %% that this is handled elsewhere and return the RangeSet as is.
    case lists:member(error, RangeSet) of
        true  -> error;
        false -> RangeSet
    end;
parse_range(_Other, _ContentLength) ->
    error.

%% @private Expect and parse a byte-range-spec.
-spec parse_range_spec(binary(), uint()) -> {Start::uint(), End::uint()}.
parse_range_spec(ElemBin, ContentLength) ->
    Range = case binary:split(ElemBin, [<<"-">>]) of
        [IStart, IEnd] -> {binary_to_integer(IStart), binary_to_integer(IEnd)};
        _ -> error
    end,
    case Range of
        %% "-N" (Last N bytes of content)
        {none, Length} when is_integer(Length) ->
            Start = ContentLength - Length,
            End = ContentLength - 1,
            valid_range_spec(Start, End, ContentLength);
        %% "N-" (From byte N to end of content)
        {Start, none} when is_integer(Start) ->
            End = ContentLength - 1,
            valid_range_spec(Start, End, ContentLength);
        %% "N-M" (From byte N to byte M)
        {Start, End} when is_integer(Start), is_integer(End) ->
            valid_range_spec(Start, End, ContentLength);
        _Other ->
            error
    end.

%% @private Contstuct a valid byte-range. Return error on invalid input.
-spec valid_range_spec(uint(), uint(), uint()) -> {uint(), uint()}.
valid_range_spec(Start, End, ContentLength)
when 0 =< Start, Start =< End, End < ContentLength ->
    {Start, End, (End-Start)+1};
valid_range_spec(_Start, _End, _ContentLength) ->
    error.

%% @doc Make a Content-Range header with a known Content-Length.
%% The content range header is included in 206 (Partial Content) responses
%% and indicates which byte range is sent in the response. The content range
%% header also includes the content length of the resource to aid clients
%% making subsequent requests.
%% @end
-spec make_range(uint(), uint(), uint()) -> {binary(), binary()}.
make_range(Start, End, ContentLength) ->
    SStr = integer_to_list(Start),
    EStr = integer_to_list(End),
    LStr = integer_to_list(ContentLength),
    HVal = iolist_to_binary([SStr, $-, EStr, $/, LStr]),
    {<<"Content-Range">>, HVal}.


%% @doc Make a Content-Range header with an unknown Content-Length.
%% This is equivalent to make_range/2. The total content range is replaced
%% with an asterix (*) to indicate that the total content is unknown.
%% @end
-spec make_range(uint(), uint()) -> {binary(), binary()}.
make_range(Start, End) ->
    SStr = integer_to_list(Start),
    EStr = integer_to_list(End),
    HVal = iolist_to_binary([SStr, $-, EStr, $/, $*]),
    {<<"Content-Range">>, HVal}.


%% @private Convert a binary to an integr. Return error on invalid input.
-spec binary_to_integer(binary()) -> non_neg_integer().
binary_to_integer(<<>>) ->
    none;
binary_to_integer(Bin) ->
    Str = binary_to_list(Bin),
    case string:to_integer(Str) of
        {error, _Reason} -> error;
        {Integer, ""} -> Integer;
        {_Integer, _} -> error
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rfc2615_examples_test_() ->
    P = fun(Bin) -> parse_range(Bin, 10000) end,
    [?_assertEqual([{0, 499, 500}], P(<<"bytes=0-499">>)),
     ?_assertEqual([{500, 999, 500}], P(<<"bytes=500-999">>)),
     ?_assertEqual([{9500,9999, 500}], P(<<"bytes=-500">>)),
     ?_assertEqual([{9500,9999, 500}], P(<<"bytes=9500-">>)),
     ?_assertEqual([{0, 0, 1}], P(<<"bytes=0-0">>)),
     ?_assertEqual([{9999,9999,1}], P(<<"bytes=-1">>)),
     ?_assertEqual([{0, 0, 1}, {9999, 9999, 1}], P(<<"bytes=0-0,-1">>)),
     ?_assertEqual(
        [{500, 600,101},{601,999,399}], P(<<"bytes=500-600,601-999">>)),
     ?_assertEqual(
        [{500,700,201},{601,999,399}], P(<<"bytes=500-700,601-999">>)),
     ?_assertEqual(error, P(<<"notbytes=1-2">>)),
     ?_assertEqual(error, P(<<"bytes=10000-">>)),
     ?_assertEqual(error, P(<<"bytes=-">>)),
     ?_assertEqual(error, P(<<"bytes=2-1">>)),
     ?_assertEqual(error, P(<<"bytes=1-b">>)),
     ?_assertEqual(error, P(<<"bytes=a-2">>))
    ].

range_header_test_() ->
    Name = <<"Content-Range">>,
    [?_assertEqual({Name, <<"0-0/2">>}, make_range(0, 0, 2)),
     ?_assertEqual({Name, <<"1-3/4">>}, make_range(1, 3, 4)),
     ?_assertEqual({Name, <<"0-4/*">>}, make_range(0, 4))
    ].

-endif.
