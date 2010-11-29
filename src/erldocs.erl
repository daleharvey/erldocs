-module(erldocs).
-export([main/1]).

%% @doc Called automatically by escript
-spec main(list()) -> ok.
main([]) ->
    main(["docs/erldocs"]);

main([Dest]) ->
    {ok, CWD} = file:get_cwd(),
    Conf = [{apps, [CWD]}, {dest, Dest}],
    run(Conf);

main(Args) ->
    main(Args, []).

main([Dest], Apps) ->
    Conf = [{apps, Apps}, {dest, Dest}],
    run(Conf);

main([App|Rest], Apps) ->
    main(Rest, [App|Apps]).

run(Conf) ->
    try erldocs_core:dispatch(Conf)
    catch Type:Error ->
            log("Error running script:~n~p~n~p~n",
                [erlang:get_stacktrace(), {Type, Error}])
    end.

-spec log(string(), [_]) -> ok.
log(Str, Args) ->
    io:format(Str, Args).
