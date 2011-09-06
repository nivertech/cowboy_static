-module(cowboy_sendfile_mime).
-export([filename/1]).

%% @doc Return the content type of a file.
%% @end
-spec filename(binary()) -> binary().
filename(Filename) ->
    case binary:split(Filename, [<<".">>], [global]) of
        [_] ->
            <<"application/octet-stream">>;
        [_|_]=Parts ->
            Ext = lists:last(Parts),
            case mimetypes:extension(Ext) of
                undefined -> <<"application/octet-stream">>;
                MimeType -> MimeType
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

mime_test_() ->
    Oct = <<"application/octet-stream">>,
    {setup,
        fun() -> application:start(mimetypes) end,
        fun(_) -> application:stop(mimetypes) end,
        [?_assertEqual(Oct, filename(<<"Makefile">>)),
         ?_assertEqual(Oct, filename(<<"djs.dse">>)),
         ?_assertEqual(<<"application/pdf">>, filename(<<"a.pdf">>))
        ]}.

-endif.
