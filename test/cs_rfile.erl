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

-module(cs_rfile).
-export([make/2,
         read/2,
         write_file/2,
         read_file/1,
         delete/1]).

-record(rfile, {size, chars, rstate, real}).

%% @doc Create a new file generator.
%% The file generator will output 'Size' random characters from 'Chars'
-spec make(non_neg_integer(), binary()) -> #rfile{}.
make(Size, Chars) ->
    #rfile{size=Size, chars=Chars, rstate=now()}.

%% @doc Read N characters from the start of a file generator.
-spec read(non_neg_integer(), #rfile{}) -> {binary(), #rfile{}} | eof.
read(_Chars, #rfile{size=0}) ->
    eof;
read(N, #rfile{size=Size, chars=Chars, rstate=RState}=RFile) ->
    NChars = if N > Size -> Size; true -> N end,
    {Data, NewRState} = read_(NChars, Chars, RState, <<>>),
    {Data, RFile#rfile{size=Size-NChars, rstate=NewRState}}.

read_(0, _Chars, RState, Acc) ->
    {Acc, RState};
read_(N, Chars, RState, Acc) ->
    {Pos, NewRState} = random:uniform_s(byte_size(Chars), RState),
    Char = binary:at(Chars, Pos-1),
    read_(N - 1, Chars, NewRState, <<Acc/binary, Char>>).


%% @doc Write the contents of a file generator to a file.
%% A new file generator including the path of the file is returned.
-spec write_file(#rfile{}, string()) -> {ok, #rfile{}}.
write_file(RFile, File) ->
    {ok, FD} = file:open(File, [write,append,exclusive]),
    ok = write_file_(RFile, FD),
    {ok, RFile#rfile{real=File}}.

write_file_(RFile, FD) ->
    case read(4096, RFile) of
        eof ->
            file:close(FD);
        {Data, NewRFile} ->
            ok = file:write(FD, Data),
            write_file_(NewRFile, FD)
    end.


%% @doc Read all data from a file generator.
-spec read_file(#rfile{}) -> binary().
read_file(#rfile{size=Size}=RFile) ->
    {Data, _} = read(Size, RFile),
    Data.


%% @doc Delete the on-disk contents of a file generator.
%% This assumes that the generator has been returned from write_file.
delete(#rfile{real=File}) ->
    file:delete(File).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

onechar_test_() ->
    File = ?MODULE:make(1, <<"a">>),
    [?_assertMatch({<<"a">>, _}, ?MODULE:read(1, File)),
     ?_assertMatch({<<"a">>, _}, ?MODULE:read(2, File))].

eof_test() ->
    {<<"a">>, File} = ?MODULE:read(2, make(1, <<"a">>)),
    ?assertEqual(eof, ?MODULE:read(0, File)),
    ?assertEqual(eof, ?MODULE:read(1, File)).

twochar_test() ->
    O = fun({E, _}) -> timer:sleep(1), E end,
    Outputs = [O(?MODULE:read(2, make(2, <<"ab">>))) || _ <- lists:seq(1, 16)],
    ?assertEqual([<<A,B>> || A <- "ab", B <- "ab"], lists:usort(Outputs)).

write_file_test_() ->
    Dir = test_server:temp_name("/tmp/cowboy_sendfile."),
    File = Dir ++ "/file",
    {setup,local,
        _Setup=fun() ->
            ok = file:make_dir(Dir)
        end,
        _Teardown=fun(_) ->
            file:delete(File),
            ok = file:del_dir(Dir)
        end,
        ?_test(begin
            RFile0 = make(9999, <<"abcd1234">>),
            {ok, RFile1} = ?MODULE:write_file(RFile0, File),
            {ok, FData} = file:read_file(File),
            RData = read_file(RFile1),
            ?assertEqual(RData, FData),
            ok = cs_rfile:delete(RFile1),
            ?assertEqual({error, enoent}, file:read_file(File))
        end)}.

-endif.
