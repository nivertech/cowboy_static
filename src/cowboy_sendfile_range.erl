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
parse_range(BinRange, ContentLength) ->
    <<"bytes=", RangeSetBin/binary>> = BinRange,
    hd([parse_range_spec(E, ContentLength) || E <- binary:split(RangeSetBin, [<<",">>])]).

%% @private Expect and parse a byte-range-spec.
-spec parse_range_spec(binary(), uint()) -> {Start::uint(), End::uint(), Length::uint()}.
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
            valid_range_spec(Start, End, Length, ContentLength);
        %% "N-" (From byte N to end of content)
        {Start, none} when is_integer(Start) ->
            End = ContentLength - 1,
            Length = (End - Start) + 1,
            valid_range_spec(Start, End, Length, ContentLength);
        %% "N-M" (From byte N to byte M)
        {Start, End} when is_integer(Start), is_integer(End) ->
            Length = (End - Start) + 1,
            valid_range_spec(Start, End, Length, ContentLength);
        _Other ->
            error
    end.

-spec valid_range_spec(uint(), uint(), uint(), uint()) ->
          {uint(), uint(), uint()}.
valid_range_spec(Start, End, Length, _Content) ->
    {Start, End, Length}.

-spec binary_to_integer(binary()) -> non_neg_integer().
binary_to_integer(<<>>) ->
    none;
binary_to_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

%% == BNF from RFC2616 section 14.35.1 ==
%% ranges-specifier = byte-ranges-specifier
%% byte-ranges-specifier = bytes-unit "=" byte-range-set
%% byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
%% byte-range-spec = first-byte-pos "-" [last-byte-pos]
%% first-byte-pos  = 1*DIGIT
%% last-byte-pos   = 1*DIGIT

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rfc2615_examples_test_() ->
    P = fun(Bin) -> parse_range(Bin, 10000) end,
    [?_assertEqual({0, 499, 500}, P(<<"bytes=0-499">>)),
     ?_assertEqual({500, 999, 500}, P(<<"bytes=500-999">>)),
     ?_assertEqual({9500,9999, 500}, P(<<"bytes=-500">>)),
     ?_assertEqual({9500,9999, 500}, P(<<"bytes=9500-">>))
    ].
        %% TODO
        %% - The first and last bytes only (bytes 0 and 9999):  bytes=0-0,-1
        %% - Several legal but not canonical specifications of the second 500
        %% bytes (byte offsets 500-999, inclusive):
        %%  bytes=500-600,601-999
        %%  bytes=500-700,601-999

-endif.