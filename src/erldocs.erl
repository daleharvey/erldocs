%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs).

%% erldocs: escript rendering edoc docs.

-export([ main/1 ]).

-record(conf, { dirs = []
              , destination = cwd() ++ "/docs/erldocs"
              , base = "./"
              , ga = "UA-44246018-1"
              }).

%% API

main ([]) ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: \n\t~s  "
              "[-o ‹output dir›]  ‹source path›⁺\n",
              [Arg0]),
    halt(1);
main (Args) ->
    parse(Args, #conf{}).

%% Internals

parse ([], Conf) ->
    case Conf#conf.dirs of
      []   -> Dirs = [cwd()];
      Else -> Dirs = Else
    end,
    PropList = [ {apps, Dirs}
               , {dest, aname(Conf#conf.destination)}
               , {base, Conf#conf.base}
               , {ga,   Conf#conf.ga} ],
    run(PropList);

parse (["-o", Dest | Rest], Conf) ->
    parse(Rest, Conf#conf{destination = Dest});

parse (["-I", _Include | Rest], Conf) ->
    io:format("Option -I is automatically filled, ignoring.\n"),
    parse(Rest, Conf);

parse (["--base", Base | Rest], Conf) ->
    parse(Rest, Conf#conf{base = Base});

parse (["--ga", GA | Rest], Conf) ->
    parse(Rest, Conf#conf{ga = GA});

parse ([Dir0 | Rest], #conf{dirs = Dirs} = Conf) ->
    case Dir0 of
        "." -> Dir = cwd();
        _   -> Dir = Dir0
    end,
    parse(Rest, Conf#conf{dirs = [aname(Dir)|Dirs]}).


run (Conf) ->
    try erldocs_core:dispatch(Conf)
    catch Type:Error ->
            log("Error running script:\n~p\n~p\n",
                [erlang:get_stacktrace(), {Type, Error}])
    end.

aname (Filename) ->
    filename:absname(Filename).

log (Str, Args) ->
    io:format(Str, Args).

cwd () ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

%% End of Module.
