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

%% @doc Returns true if some documentation was built;
%%  false if none was generated.
-spec main (_) -> boolean().
main (Args=[_|_]) ->
    PropList = parse(Args, #conf{}),
    erldocs_core:dispatch(PropList);
main (_) ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: \n\t~s  "
              "[-o ‹output dir›]  ‹source path›⁺\n",
              [Arg0]),
    halt(1).

%% Internals

parse ([], Conf) ->
    case Conf#conf.dirs of
      []   -> Dirs = [cwd()];
      Else -> Dirs = Else
    end,
    [ {apps, Dirs}
    , {dest, aname(Conf#conf.destination)}
    , {base, Conf#conf.base}
    , {ga,   Conf#conf.ga}
    ];

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


aname (Filename) ->
    filename:absname(Filename).

cwd () ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

%% End of Module.
