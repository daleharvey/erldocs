%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs).

%% erldocs: escript rendering edoc docs.

-export([ main/1 ]).

-record(conf, { dirs = []
              , destination = cwd() ++ "/docs/erldocs"
              }).

%% API

main (Args) ->
    parse(Args, #conf{}).

%% Internals

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

log (Str, Args) ->
    io:format(Str, Args).

cwd () ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

%% End of Module.
