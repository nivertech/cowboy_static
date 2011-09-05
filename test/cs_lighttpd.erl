-module(cs_lighttpd).
-include_lib("common_test/include/ct.hrl").
%% This module implements a thin wrapper around the lighttpd web server.
%% It is used to verify that the assertions made by the test suite also
%% holds for a commonly used web server.
-export([init_server/1,
         end_server/1]).


init_server(Config) ->
    Port = ?config(port, Config),
    Dir = ?config(priv_dir, Config),
    ConfigFile = filename:join(Dir, "lighttpd.conf"),
    ?line(ok = file:write_file(ConfigFile, make_config(Port, Dir))),
    CmdLine = "cat | /usr/sbin/lighttpd -D -f " ++ ConfigFile ++ " ; kill 0",
    Runner = self(),
    Pid = spawn(fun() ->
        CmdPort = open_port({spawn, CmdLine}, [binary, stream, eof]),
        receive
            close ->
                port_close(CmdPort)
        end
    end),
    [{lighttpd_pid,Pid}|Config].

end_server(Config) ->
    Pid = ?config(lighttpd_pid, Config),
    PidFile = filename:join([?config(priv_dir, Config), "lighttpd.pid"]),
    {ok, BinOSPid} = file:read_file(PidFile),
    os:cmd("kill " ++ binary_to_list(BinOSPid)).

-define(fmt(Fmt, Args), io_lib:format(Fmt, Args)).
make_config(Port, Dir) ->
    PidFile = filename:join([Dir, "lighttpd.pid"]),
    IOConfigLines = [
        ?fmt("server.bind = \"localhost\"", []),
        ?fmt("server.port = ~.10B", [Port]),
        ?fmt("server.document-root = \"~s\"", [Dir]),
        ?fmt("server.pid-file = \"~s\"", [PidFile])
    ],
    BinConfigLines = [iolist_to_binary(Line) || Line <- IOConfigLines],
    StrConfigLines = [binary_to_list(Line) || Line <- BinConfigLines],
    string:join(StrConfigLines, "\n").
