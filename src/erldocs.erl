-module(erldocs).
-export([main/1]).

-record(conf, { dirs = []
              , destination = cwd() ++ "/docs/erldocs"
              }).

%% @doc Called automatically by escript
-spec main(list()) -> ok.
main (Args) ->
    parse(Args, #conf{}).

parse ([], #conf{destination = Destination} = Conf) ->
    Dirs = case Conf#conf.dirs of
      [] -> [cwd()];
      Else -> Else
    end,
    run([{apps, Dirs}, {dest, filename:absname(Destination)}]);

parse (["-o", Dest | Rest], Conf) ->
    parse(Rest, Conf#conf{destination=Dest});

parse ([Dir | Rest], #conf{dirs = Dirs} = Conf) ->
    parse(Rest, Conf#conf{dirs = [Dir | Dirs]}).


run (Conf) ->
    try erldocs_core:dispatch(Conf)
    catch Type:Error ->
            log("Error running script:~n~p~n~p~n",
                [erlang:get_stacktrace(), {Type, Error}])
    end.

-spec log(string(), [_]) -> ok.
log (Str, Args) ->
    io:format(Str, Args).

cwd () ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.
