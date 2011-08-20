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

-module(cs_SUITE).
-include_lib("common_test/include/ct.hrl").

%% common test functions
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% static test functions
-export([empty_file/1,
         non_existing_file/1,
         below_static_root/1,
         below_static_root_esc/1,
         subdir_not_listed/1,
         subdir_file_access/1]).

%% content test functions
-export([ascii_one_chunk/1,
         ascii_two_chunks/1,
         ascii_hd_range/1]).

all() ->
    [{group, static}, {group, content}].

groups() ->
    [{static, [], [
        empty_file,
        non_existing_file,
        below_static_root,
        below_static_root_esc,
        subdir_not_listed,
        subdir_file_access
        ]}] ++
    [{content, [], [
        ascii_one_chunk,
        ascii_two_chunks,
        ascii_hd_range
        ]}].

init_per_suite(Config) ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(lhttpc),
    ok = application:start(cowboy),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(cowboy),
    ok = application:stop(lhttpc),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(crypto),
    ok.

init_per_group(static, Config) ->
    Port = 33081,
    Dir  = static_dir(data_dir, Config),
    Dispatch = [{
        [<<"localhost">>], [
            {['...'], cowboy_sendfile, [{dir, Dir}]}]}],
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]),
    [{scheme, "http"}, {port, Port}|Config];

init_per_group(content, Config) ->
    Port = 33081,
    Dir  = static_dir(priv_dir, Config),
    Dispatch = [{
        [<<"localhost">>], [
            {['...'], cowboy_sendfile,
                [{dir, Dir}, {chunk_size, 512}]}]}],
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]),
    [{scheme, "http"}, {port, Port}|Config].


end_per_group(static, _Config) ->
    cowboy:stop_listener(http),
    ok;

end_per_group(content, _Config) ->
    ok.

%% content test function fixtures

init_per_testcase(ascii_one_chunk=Name, Config) ->
    init_test_file(Name, cs_rfile:make(512, <<"abcd">>), Config);

init_per_testcase(ascii_two_chunks=Name, Config) ->
    init_test_file(Name, cs_rfile:make(1024, <<"abcd">>), Config);

init_per_testcase(ascii_hd_range=Name, Config) ->
    init_test_file(Name, cs_rfile:make(1024, <<"efg">>), Config);

init_per_testcase(_Name, Config) ->
    Config.


end_per_testcase(ascii_one_chunk, Config) ->
    end_test_file(ascii_one_chunk, Config);

end_per_testcase(ascii_two_chunks, Config) ->
    end_test_file(ascii_two_chunks, Config);

end_per_testcase(ascii_hd_range, Config) ->
    end_test_file(ascii_hd_range, Config);

end_per_testcase(_Name, _Config) ->
    ok.

init_test_file(Name, RFile0, Config) ->
    File = filename:join(?config(priv_dir, Config), atom_to_list(Name)),
    {ok, RFile1} = cs_rfile:write_file(RFile0, File), 
    [{reference_file, RFile1}|Config].

end_test_file(_Name, Config) ->
    ok = cs_rfile:delete(?config(reference_file, Config)).

%% static test functions

empty_file(Config) ->
    ?line({ok, {{200, "OK"}, _, <<"">>}} =
        make_get("/empty_file", [], Config)),
    ?line({ok, {{200, "OK"}, _, <<"">>}} =
        make_head("/empty_file", [], Config)).

non_existing_file(Config) ->
    ?line({ok, {{404, "Not Found"}, _Hdrs, _Body}} =
        make_get("/non_existing", [], Config)),
    ?line({ok, {{404, "Not Found"}, _, <<"">>}} =
        make_head("/non_existing", [], Config)).

below_static_root(Config) ->
    ?line({ok, {{403, "Forbidden"}, _Hdrs, _Body}} =
        make_get("/../cs_SUITE.erl", [], Config)),
    ?line({ok, {{403, "Forbidden"}, _, <<"">>}} =
        make_head("/../cs_SUITE.erl", [], Config)).

below_static_root_esc(Config) ->
    ?line({ok, {{403, "Forbidden"}, _Hdrs, _Body}} =
        make_get("/%2e%2e%2fcs_SUITE.erl", [], Config)),
    ?line({ok, {{403, "Forbidden"}, _, <<"">>}} =
        make_head("/%2e%2e%2fcs_SUITE.erl", [], Config)).

subdir_not_listed(Config) ->
    ?line({ok, {{404, "Not Found"}, _Hdrs, _Body}} =
        make_get("/subdir", [], Config)),
    ?line({ok, {{404, "Not Found"}, _Hdrs, <<"">>}}=
        make_head("/subdir", [], Config)).

subdir_file_access(Config) ->
    ?line({ok, {{200, "OK"}, _Hdrs, <<"subfile-contents\n">>}} =
        make_get("/subdir/subfile", [], Config)),
    ?line({ok, {{200, "OK"}, _, <<"">>}} =
        make_head("/subdir/subfile", [], Config)).

%% content test functions

ascii_one_chunk(Config) ->
    ?line({ok, {{200, "OK"}, _Hdrs, Body}} =
        make_get("/ascii_one_chunk", [], Config)),
    ?line(Body = cs_rfile:read_file(?config(reference_file, Config))).

ascii_two_chunks(Config) ->
    ?line({ok, {{200, "OK"}, _Hdrs, Body}} =
        make_get("/ascii_two_chunks", [], Config)),
    ?line(Body = cs_rfile:read_file(?config(reference_file, Config))).

ascii_hd_range(Config) ->
    ?line({ok, {{200, "OK"}, _Hdrs, Body}} =
        make_get("/ascii_hd_range", [{"Range", "bytes=0-499"}], Config)),
    ?line(500 = byte_size(Body)).

%% util functions

static_dir(Which, Config) when Which =:= data_dir; Which =:= priv_dir ->
    Dir = ?config(Which, Config),
    [list_to_binary(I) || I <- filename:split(Dir)].

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    Scheme ++ "://localhost:" ++ integer_to_list(Port) ++ Path.

make_get(Path, Headers, Config) ->
    URL = build_url(Path, Config),
    lhttpc:request(URL, 'GET', Headers, infinity).

make_head(Path, Headers, Config) ->
    URL = build_url(Path, Config),
    lhttpc:request(URL, 'HEAD', Headers, infinity).
