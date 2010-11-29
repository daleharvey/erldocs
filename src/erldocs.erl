-module(erldocs).
-export([main/1]).

%% @doc Called automatically by escript
-spec main(list()) -> ok.
main(Args) ->
    parse(Args, []).

parse([], []) ->
    Dest = filename:absname("docs/erldocs"),
    parse([Dest], []);

parse([Dest], []) ->
    Apps = [cwd()],
    parse([Dest], Apps);

parse([Dest], Apps) ->
    Conf = [{apps, Apps}, {dest, filename:absname(Dest)}],
    run(Conf);

parse([App|Rest], Apps) ->
    parse(Rest, [filename:absname(App)|Apps]).

run(Conf) ->
    try erldocs_core:dispatch(Conf)
    catch Type:Error ->
            log("Error running script:~n~p~n~p~n",
                [erlang:get_stacktrace(), {Type, Error}])
    end.

-spec log(string(), [_]) -> ok.
log(Str, Args) ->
    io:format(Str, Args).

cwd() ->
    element(2, file:get_cwd()).
