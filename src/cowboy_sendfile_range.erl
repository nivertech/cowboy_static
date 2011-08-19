-module(cowboy_sendfile_range).
-export([get_range/1,
         parse_range/2]).

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
    join_ranges(RangeSet);
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
    {Start, End};
valid_range_spec(_Start, _End, _ContentLength) ->
    error.

%% @private Join a set of byte-ranges.
-spec join_ranges([{uint(), uint()}]) -> [{uint(), uint(), uint()}].
join_ranges(Ranges) ->
    case lists:sort(Ranges) of
        [error|_] -> error;
        [First|Sorted] -> join_ranges(Sorted, [First])
    end.

join_ranges([], [{OStart, OEnd}|OT]) ->
    lists:reverse([{OStart, OEnd, (OEnd - OStart) + 1}|OT]);
join_ranges([{IStart, IEnd}|IT], [{OStart, OEnd}|OT]=Output) ->
    if  %% IStart and IEnd in output range
        IStart >= OStart, IStart =< OEnd, IEnd =< OEnd ->
            join_ranges(IT, Output);
        %% IStart in output range and IEnd beyond output range.
        IStart >= OStart, IStart =< OEnd ->
            join_ranges(IT, [{OStart, IEnd}|OT]);
        %% IStart adjacent to end of output range and IEnd beyond output range.
        (IStart - 1) =:= OEnd ->
            join_ranges(IT, [{OStart, IEnd}|OT]);
        %% IStart and IEnd beyond output range.
        IStart > OEnd ->
            ORange = {OStart, OEnd, (OEnd - OStart) + 1},
            join_ranges(IT, [{IStart, IEnd},ORange|OT])
    end.


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
     ?_assertEqual([{500, 999, 500}], P(<<"bytes=500-600,601-999">>)),
     ?_assertEqual([{500, 999, 500}], P(<<"bytes=500-700,601-999">>)),
     ?_assertEqual(error, P(<<"notbytes=1-2">>)),
     ?_assertEqual(error, P(<<"bytes=10000-">>)),
     ?_assertEqual(error, P(<<"bytes=-">>)),
     ?_assertEqual(error, P(<<"bytes=2-1">>)),
     ?_assertEqual(error, P(<<"bytes=1-b">>)),
     ?_assertEqual(error, P(<<"bytes=a-2">>))
    ].

-endif.
