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
         init_per_group/2, end_per_group/2]).

%% test functions
-export([empty_file/1,
         non_existing_file/1,
         below_static_root/1,
         subdir_not_listed/1,
         subdir_file_access/1]).

all() ->
    [{group, static}].

groups() ->
    [{static, [], [
        empty_file,
        non_existing_file,
        below_static_root,
        subdir_not_listed,
        subdir_file_access
    ]}].

init_per_suite(Config) ->
    application:start(inets),
    application:start(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
    application:stop(inets),
    ok.

init_per_group(static, Config) ->
    Port = 33081,
    Dir = static_dir(Config),
    Dispatch = [{
        [<<"localhost">>], [
            {['...'], cowboy_sendfile, [{dir, Dir}]}]}],
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]),
    [{scheme, "http"}, {port, Port}|Config].

end_per_group(static, _Config) ->
    cowboy:stop_listener(http),
    ok.

empty_file(Config) ->
    ?line(URL = build_url("/empty_file", Config)),
    ?line({ok, {Status, _Hdrs, Body}} = httpc:request(URL)),
    ?line({"HTTP/1.1", 200, "OK"} = Status),
    ?line("" = Body).

non_existing_file(Config) ->
    ?line(URL = build_url("/non_existing", Config)),
    ?line({ok, {Status, _Hdrs, Body}} = httpc:request(URL)),
    ?line({"HTTP/1.1", 404, "Not Found"} = Status).

below_static_root(Config) ->
    ?line(URL = build_url("/../cs_SUITE.erl", Config)),
    ?line({ok, {Status, _Hdrs, Body}} = httpc:request(URL)),
    ?line({"HTTP/1.1", 403, "Forbidden"} = Status).

subdir_not_listed(Config) ->
    ?line(URL = build_url("/subdir", Config)),
    ?line({ok, {Status, _Hdrs, Body}} = httpc:request(URL)),
    ?line({"HTTP/1.1", 403, "Forbidden"} = Status).

subdir_file_access(Config) ->
    ?line(URL = build_url("/subdir/subfile", Config)),
    ?line({ok, {Status, _Hdrs, Body}} = httpc:request(URL)),
    ?line({"HTTP/1.1", 200, "OK"} = Status),
    %% TODO: It appears as if either cowboy_sendfile or httpc appends \n
    ?line("subfile-contents\n" = Body).

static_dir(Config) ->
    Datadir = ?config(data_dir, Config),
    [list_to_binary(I) || I <- filename:split(Datadir)].

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    Scheme ++ "://localhost:" ++ integer_to_list(Port) ++ Path.
