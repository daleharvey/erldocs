-module(erldocs).
-export([main/1]).

%% @doc Called automatically by escript
-spec main(list()) -> ok.
main(Args) ->
    Conf = create_conf(Args),
    try erldocs_core:dispatch(Conf)
    catch Type:Error ->
            log("Error running script:~n~p~n~p~n",
                [erlang:get_stacktrace(), {Type, Error}])
    end.

-spec parse_options([string()]) -> [{atom(), _}].
parse_options(Args) ->
    lists:flatmap(fun (Arg) -> parse_option(Arg) end, Args).

-spec parse_option(string()) -> [{atom(), _}].
parse_option("--sys-conf=" ++ Path) ->
    [{sys_conf, Path}];
parse_option("--name=" ++ Name) ->
    [{name, Name}];
parse_option(Unknown) ->
    throw({unknown_option, Unknown}).

-spec getopt(atom(), [{atom(), _}], _) -> _.
getopt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        false ->
            Default;
        {_, V} ->
            V
    end.

-spec create_conf([string()]) -> [{atom(), _}].
create_conf(Args) ->
    Opts = parse_options(Args),
    Path = getopt(sys_conf, Opts, "priv/sys.conf"),
    {ok, [Config]}  = file:consult(Path),
    {erldocs, Conf} = lists:keyfind(erldocs, 1, Config),
    {ok, Root}      = file:get_cwd(),
    Opts ++ [{root, Root} | Conf].

-spec log(string(), [_]) -> ok.
log(Str, Args) ->
    io:format(Str, Args).
