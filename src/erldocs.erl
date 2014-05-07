%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs).

%% erldocs: escript rendering edoc docs.

-export([ main/1 ]).

-record(conf, { dirs = []
              , destination = cwd() ++ "/docs/erldocs"
              , includes = []
              }).

%% API

main (Args) ->
    parse(Args, #conf{}).

%% Internals

parse ([], Conf) ->
    Destination    = Conf#conf.destination,
    Includes       = Conf#conf.includes,
    case Conf#conf.dirs of
      []   -> Dirs = [cwd()];
      Else -> Dirs = Else
    end,
    run([ {apps, Dirs}
        , {dest, absp(Destination)}
        , {incs, Includes} ]);

parse (["-o", Dest | Rest], Conf) ->
    parse(Rest, Conf#conf{destination = Dest});

parse (["-I", Include | Rest], #conf{includes = Includes} = Conf) ->
    parse(Rest, Conf#conf{includes = [absp(Include)|Includes]});

parse ([Dir | Rest], #conf{dirs = Dirs} = Conf) ->
    parse(Rest, Conf#conf{dirs = [absp(Dir)|Dirs]}).


run (Conf) ->
    try erldocs_core:dispatch(Conf)
    catch Type:Error ->
            log("Error running script:\n~p\n~p\n",
                [erlang:get_stacktrace(), {Type, Error}])
    end.

absp (Filename) ->
    filename:absname(Filename).

log (Str, Args) ->
    io:format(Str, Args).

cwd () ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

%% End of Module.
