-module(erldocs_core).
-export([copy_static_files/1, build/1, dispatch/1]).
-export([mapreduce/4, pmapreduce/4, pmapreduce/5]).
-include_lib("kernel/include/file.hrl").

-define(LOG(Str, Args), io:format(Str, Args)).
-define(LOG(Str), io:format(Str)).

%% @doc Copy static files
-spec copy_static_files(list()) -> ok.
copy_static_files (Conf) ->
    Dest = dest(Conf),
    {ok, ErlDocsCSS} = erldocs_css_dtl:render([]),
    edoc_lib:write_file(ErlDocsCSS, Dest, "erldocs.css"),
    {ok, ErlDocsJS } =  erldocs_js_dtl:render([]),
    edoc_lib:write_file(ErlDocsJS,  Dest, "erldocs.js"),
    {ok, Jquery    } =   jquery_js_dtl:render([]),
    edoc_lib:write_file(Jquery,     Dest, "jquery.js"),
    ok.

%% @doc Parses arguments passed to script and calls
%% appropriate function.
-spec dispatch(list()) -> ok.
dispatch (Conf) ->
    Start = erlang:now(),
    build(Conf),
    Diff = timer:now_diff(erlang:now(), Start),
    Mins = trunc(Diff * 1.667e-8),
    log("Woot, finished in ~p Minutes ~p Seconds~n",
        [Mins, trunc((Diff * 1.0e-6) - (Mins * 60))]).


%% @doc Build everything
-spec build(list()) -> ok.
build (Conf) ->
    filelib:ensure_dir(dest(Conf)),

    Fun   = fun (X, Y) -> build_apps(Conf, X, Y) end,
    Index = strip_cos(lists:foldl(Fun, [], app_dirs(Conf))),

    ok = module_index(Conf, Index),
    ok = javascript_index(Conf, Index),
    ok = copy_static_files(Conf).

build_apps (Conf, App, Index) ->
    AppName = bname(App),
    log("Building ~s~n", [AppName]),
    Files   = ensure_docsrc(App, Conf),
    Map = fun (F) -> build_file_map(Conf, AppName, F) end,
    [["app", AppName, AppName, "[application]"] |
     pmapreduce(Map, fun lists:append/2, [], Files) ++ Index].

build_file_map (Conf, AppName, File) ->
    log("Generating HTML - ~s ~p\n", [bname(File, ".xml"), File]),
    {Type, _Attr, Content} = read_xml(Conf, File),

    TypeSpecsFile = filename:join([dest(Conf), ".xml", "specs_" ++ bname(File)]),
    TypeSpecs = case read_xml(Conf, TypeSpecsFile) of
        {error, _, _} -> [];
        {module, _, Specs} -> strip_whitespace(Specs)
    end,

    case lists:member(Type, buildable()) of
        false -> [];
        true  ->

            Module = bname(File, ".xml"),
            Xml = strip_whitespace(Content),

            Sum2 = case Type of
                       erlref ->
                           {modulesummary, [], Sum}
                               = lists:keyfind(modulesummary,1, Xml),
                            unicode:characters_to_list(
                                lists:filter(fun (X) -> not is_tuple(X) end, Sum));
                       cref ->
                           {libsummary, [], Sum}
                               = lists:keyfind(libsummary, 1, Xml),
                           Sum
                  end,

            Sum1 = lists:flatten(Sum2),

            % strip silly shy characters
            Funs = get_funs(AppName, Module, lists:keyfind(funcs, 1, Xml)),

            ok = render(Type, AppName, Module, Content, TypeSpecs, Conf),

            case lists:member({AppName, Module}, ignore()) of
                true -> [];
                false -> [ ["mod", AppName, Module, Sum1] |  Funs]
            end
    end.

%% @doc strip out the cos* files from the index, who the hell needs them
%% anyway
-spec strip_cos(list()) -> list().
strip_cos (Index) ->
    [X || X = [_, App |_] <- Index, nomatch == re:run(App, "^cos") ].

ensure_docsrc (AppDir, Conf) ->

    % List any doc/src/*.xml files that exist in the source files
    XMLFiles = filelib:wildcard(filename:join([AppDir, "doc", "src", "*.xml"])),
    HandWritten = [bname(File, ".xml") || File <- XMLFiles],

    ErlFiles = filelib:wildcard(filename:join([AppDir, "*.erl"])) ++
        filelib:wildcard(filename:join([AppDir, "src", "*.erl"])),

    % Generate any missing module XML
    SrcFiles = [filename:absname(File) ||
                   File <- ErlFiles,
                   not lists:member(bname(File, ".erl"), HandWritten)],

    % Output XML files to destination folder
    % This prevents polluting the source files
    XMLDir = filename:join([dest(Conf), ".xml", bname(AppDir)]),
    filelib:ensure_dir(XMLDir ++ "/"),

    SpecsDest = filename:join([dest(Conf), ".xml"]),

    SpecsGenEscript =
        case erlang:system_info(otp_release) of
            Old when Old < "R15" -> "./priv/bin/specs_gen__below_R15.escript     ";
            _ ->                    "./priv/bin/specs_gen__R15_and_above.escript "
        end,
    [ begin
          log("Generating Type Specs - ~p\n", [File]),
          Args = "-I" ++ AppDir ++ "/include -o" ++ SpecsDest ++ " " ++ File,
          os:cmd(SpecsGenEscript ++ Args)
      end || File <- ErlFiles],

    %% Return the complete list of XML files
    XMLFiles ++ tmp_cd(XMLDir, fun () -> gen_docsrc(AppDir, SrcFiles, XMLDir) end).


gen_docsrc (AppDir, SrcFiles, Dest) ->
    AppDirInclude = AppDir ++ "/include", %% Those ones usually don't work!
    AppDirIncludes = [AppDirInclude | filelib:wildcard(AppDirInclude)],
    Opts = [ {sort_functions, false}
           , {layout, docgen_edoc_xml_cb}
           , {file_suffix, ".xml"}
           , {preprocess, true}
           ],

    lists:foldl(
      fun (File, Acc) ->
              Basename = bname(File, ".erl"),
              DestFile = filename:join([Dest,Basename]) ++ ".xml",
              log("Generating XML - ~s ~p -> ~p\n", [Basename,File,DestFile]),
              AbsInclude = filename:dirname(File) ++ "/../include",
              Options = [ {includes, [AbsInclude | AppDirIncludes]}
                        , {dir, filename:dirname(DestFile)}
                        | Opts],
              case (catch edoc:file(File, Options)) of
                  ok ->
                      [DestFile | Acc];
                  Error ->
                      log("Error generating XML (~p): ~p~n", [File, Error]),
                      Acc
              end
      end, [], SrcFiles).

%% @doc run a function with the cwd set, ensuring the cwd is reset once
%% finished (some dumb functions require to be ran from a particular dir)
-spec tmp_cd(list(), fun()) -> term().
tmp_cd (Dir, Fun) ->

    {ok, OldDir} = file:get_cwd(),

    ok = filelib:ensure_dir(Dir),
    ok = file:set_cwd(Dir),

    try
        Result = Fun(),
        ok = file:set_cwd(OldDir),
        Result
    catch
        Type:Err ->
            ok = file:set_cwd(OldDir),
            throw({Type, Err})
    end.


module_index (Conf, Index) ->

    log("Creating index.html ...~n"),

    Html = "<h1>Module Index</h1><hr/><br/><div>"
        ++ xml_to_str(emit_index(Index))
        ++ "</div>",

    Args = [{base,    "./"},
            {title,   "Module Index"},
            {content, Html},
            {funs,    ""}],

    {ok, Data} = erldocs_dtl:render(Args),
    ok = file:write_file([dest(Conf), "/index.html"], Data).

emit_index (L) ->
    lists:flatmap(
      fun index_html/1,
      lists:sort(fun sort_index/2, L)).

index_html (["app", App, _, _Sum]) ->
    [{a, [{name, App}]}, {h4, [], [App]}];
index_html (["mod", App, Mod, Sum]) ->
    Url = App ++ "/" ++ Mod ++ ".html",
    [{p,[], [{a, [{href, Url}], [Mod]}, {br,[],[]}, Sum]}];
index_html (_) ->
    [].

type_ordering ("app") -> 1;
type_ordering ("mod") -> 2;
type_ordering ("fun") -> 3.

index_ordering ([Type, App, Mod, _Sum]) ->
    [string:to_lower(App),
     type_ordering(Type),
     string:to_lower(Mod)].

sort_index (A, B) ->
    index_ordering(A) =< index_ordering(B).

html_encode (Str) ->
    re:replace(Str, "'", "", [{return, list}, global]).

javascript_index (Conf, FIndex) ->

    log("Creating erldocs_index.js ...~n"),

    F = fun ([Else, App, NMod, Sum]) ->
                [Else, App, NMod, fmt("~ts", [string:substr(Sum, 1, 50)])]
        end,

    Index =
        lists:map(
              fun ([A,B,C,[]]) ->
                      fmt("['~s','~s','~s',[]]",
                        [html_encode(A),html_encode(B),html_encode(C)]);
                  ([A,B,C,D]) ->
                      fmt("['~s','~s','~s','~s']",
                        [html_encode(A),html_encode(B),html_encode(C),html_encode(D)])
              end,
              lists:sort(fun sort_index/2, lists:map(F, FIndex))),

    Js = re:replace(fmt("var index = [~s];", [string:join(Index, ",")]),
                    "\\n|\\r", "", [{return,list}, global]),

    ok = file:write_file([dest(Conf), "/erldocs_index.js"], Js).

render (cref, App, Mod, Xml, Types, Conf) ->
    render(erlref, App, Mod, Xml, Types, Conf);

render (erlref, App, Mod, Xml, Types, Conf) ->

    File = filename:join([dest(Conf), App, Mod ++ ".html"]),
    ok   = filelib:ensure_dir(filename:dirname(File) ++ "/"),

    Acc = [{ids,[]}, {list, ul}, {functions, []}, {types, Types}],

    {[_Id, _List, {functions, Funs}, {types, _}], NXml}
        = render(fun tr_erlref/2,  Xml, Acc),

    XmlFuns = [{li, [], [{a, [{href, "#"++X}], [X]}]}
                || X <- lists:reverse(Funs) ],

    Args = [{base,    "../"},
            {title,   Mod ++ " (" ++ App ++ ") - "},
            {content, xml_to_str(NXml)},
            {funs,    xml_to_str({ul, [{id, "funs"}], XmlFuns})}],

    {ok, Data} = erldocs_dtl:render(Args),
    ok = file:write_file(File, Data).

render (Fun, List, Acc) when is_list(List) ->
    case io_lib:char_list(List) of
        true  ->
            {Acc, List};
        false ->
            F = fun (X, {Ac, L}) ->
                        {NAcc, NEl} = render(Fun, X, Ac),
                        {NAcc, [NEl | L]}
                end,

            {Ac, L} = lists:foldl(F, {Acc, []}, List),
            {Ac, lists:reverse(L)}
    end;

render (Fun, Element, Acc) ->

    % this is nasty
    F = fun (ignore, NAcc) ->
                {NAcc, ""};
            ({NEl, NAttr, NChild}, NAcc) ->
                {NNAcc, NNChild} = render(Fun, NChild, NAcc),
                {NNAcc, {NEl, NAttr, NNChild}};
            (Else, NAcc) ->
                {NAcc, Else}
        end,

    case Fun(Element, Acc) of
        {El, NAcc} -> F(El, NAcc);
        El         -> F(El, Acc)
    end.

get_funs (_App, _Mod, false) ->
    [];
get_funs (App, Mod, {funcs, [], Funs}) ->
    lists:foldl(
      fun (X, Acc) -> fun_stuff(App, Mod, X) ++ Acc end,
      [], Funs).

fun_stuff (App, Mod, {func, [], Child}) ->

    {fsummary, [], Xml} = lists:keyfind(fsummary, 1, Child),
    Summary = string:substr(xml_to_str(Xml), 1, 50),

    F = fun ({name, [], Name}, Acc) ->
                case make_name(Name) of
                    ignore -> Acc;
                    NName  -> [ ["fun", App, Mod++":"++NName, Summary] | Acc ]
                end;
            ({name, [{name, Name}, {arity, Arity}], []}, Acc) ->
                [ ["fun", App, Mod++":"++Name++"/"++Arity, Summary] | Acc ];
            (_Else, Acc) -> Acc
        end,

    lists:foldl(F, [], Child);
fun_stuff (_App, _Mod, _Funs) ->
    [].

make_name (Name) ->
    Tmp = lists:flatten(Name),
    case string:chr(Tmp, 40) of
        0 ->
            ignore;
        Pos ->
            {Name2, Rest2} = lists:split(Pos-1, Tmp),
            Name3          = lists:last(string:tokens(Name2, ":")),
            Args           = string:substr(Rest2, 2, string:chr(Rest2, 41)-2),
            NArgs          = length(string:tokens(Args, ",")),
            Name3 ++ "/" ++ integer_to_list(NArgs)
    end.

app_dirs (Conf) ->
    Fun = fun (Path, Acc) ->
                  Acc ++ [X || X <- filelib:wildcard(Path), filelib:is_dir(X)]
          end,
    lists:foldl(Fun, [], kf(apps, Conf)).

add_html ("#"++Rest) ->
    "#"++Rest;
add_html (Link) ->
    case string:tokens(Link, "#") of
        [Tmp]    -> Tmp++".html";
        [N1, N2] -> lists:flatten([N1, ".html#", N2])
    end.

%% Transforms erlang xml format to html
tr_erlref ({type_desc, [{variable, Name}], [Desc]}, _Acc) ->
    {'div', [{class, "type_desc"}], [{code, [], [Name, " = ",Desc]}]};
tr_erlref ({header,[],_Child}, _Acc) ->
    ignore;
tr_erlref ({marker, [{id, Marker}], []}, _Acc) ->
    {span, [{id, Marker}], [" "]};
tr_erlref ({term,[{id, Term}], _Child}, _Acc) ->
    Term;
tr_erlref ({lib,[],Lib}, _Acc) ->
    {h1, [], [lists:flatten(Lib)]};
tr_erlref ({module,[],Module}, _Acc) ->
    {h1, [], [lists:flatten(Module)]};
tr_erlref ({modulesummary, [], Child}, _Acc) ->
    {h2, [{class, "modsummary"}], Child};
tr_erlref ({c, [], Child}, _Acc) ->
    {code, [], Child};
tr_erlref ({section, [], Child}, _Acc) ->
    {'div', [{class, "section"}], Child};
tr_erlref ({title, [], Child}, _Acc) ->
    {h4, [], [Child]};
tr_erlref ({type, [], Child}, _Acc) ->
    {ul, [{class, "type"}], Child};
tr_erlref ({v, [], []}, _Acc) ->
    {li, [], [" "]};
tr_erlref ({v, [], Child}, _Acc) ->
    {li, [], [{code, [], Child}]};
tr_erlref ({seealso, [{marker, Marker}], Child}, _Acc) ->
    N = case string:tokens(Marker, ":") of
            [] -> add_html(lists:flatten(Child));
	    [Tmp]     -> add_html(Tmp);
	    [Ap | Md] ->  "../"++Ap++"/" ++ add_html(lists:flatten(Md))
	end,
    {a, [{href, N}, {class, "seealso"}], Child};
tr_erlref ({desc, [], Child}, _Acc) ->
    {'div', [{class, "description"}], Child};
tr_erlref ({description, [], Child}, _Acc) ->
    {'div', [{class, "description"}], Child};
tr_erlref ({funcs, [], Child}, _Acc) ->
    {'div', [{class,"functions"}], [{h4, [], ["Functions"]},
                                    {hr, [], []} | Child]};
tr_erlref ({func, [], Child}, _Acc) ->
    {'div', [{class,"function"}], Child};
tr_erlref ({tag, [], Child}, _Acc) ->
    {dt, [], Child};
tr_erlref ({taglist, [], Child}, [Ids, _List, Funs]) ->
    { {dl, [], Child}, [Ids, {list, dl}, Funs] };
tr_erlref ({input, [], Child}, _Acc) ->
    {code, [], Child};
tr_erlref ({item, [], Child}, [_Ids, {list, dl}, _Funs]) ->
    {dd, [], Child};
tr_erlref ({item, [], Child}, [_Ids, {list, ul}, _Funs]) ->
    {li, [], Child};
tr_erlref ({list, _Type, Child}, [Ids, _List, Funs]) ->
    { {ul, [], Child}, [Ids, {list, ul}, Funs] };
tr_erlref ({code, [{type, "none"}], Child}, _Acc) ->
    {pre, [{class, "sh_erlang"}], Child};
tr_erlref ({pre, [], Child}, _Acc) ->
    {pre, [{class, "sh_erlang"}], Child};
tr_erlref ({note, [], Child}, _Acc) ->
    {'div', [{class, "note"}], [{h2, [], ["Note!"]} | Child]};
tr_erlref ({warning, [], Child}, _Acc) ->
    {'div', [{class, "warning"}], [{h2, [], ["Warning!"]} | Child]};
tr_erlref ({name, [], [{ret,[],[Ret]}, {nametext,[],[Desc]}]}, _Acc) ->
    {pre, [], [Ret ++ " " ++ Desc]};
tr_erlref ({name, [{name, Name}, {arity, N}], []}, Acc) ->
    [{ids, Ids}, List, {functions, Funs}, {types, Types}] = Acc,
    PName = case find_spec(Name, N, Types) of
        {ok, Tmp} -> Tmp;
        _ -> Name ++ "/" ++ N
    end,
    NName = inc_name(Name, Ids, 0),
    { {h3, [{id, Name ++ "/" ++ N}], [PName]},
      [{ids, [NName | Ids]}, List, {functions, [NName|Funs]}, {types, Types}]};
tr_erlref ({name, [], Child}, [{ids, Ids}, List, {functions, Funs}, {types, Types}]) ->
    case make_name(Child) of
        ignore -> ignore;
        Name   ->
            NName = inc_name(Name, Ids, 0),
            { {h3, [{id, NName}], [Child]},
              [{ids, [NName | Ids]}, List, {functions, [NName|Funs]}, {types, Types}]}
    end;
tr_erlref ({fsummary, [], _Child}, _Acc) ->
    ignore;
tr_erlref (Else, _Acc) ->
    Else.

find_spec (_Name, _Arity, []) ->
    spec_not_found;

find_spec (Name, Arity, [{spec, [], Specs} | Rest]) ->
    {name, _, [SpecName]} = lists:keyfind(name, 1, Specs),
    {arity, _, [ArityName]} = lists:keyfind(arity, 1, Specs),
    case SpecName =:= Name andalso ArityName =:= Arity of
        true ->
            {contract, _, Contracts} = lists:keyfind(contract, 1, Specs),
            {clause, _, Clause} = lists:keyfind(clause, 1, Contracts),
            {head, _, Head} = lists:keyfind(head, 1, Clause),
            case io_lib:deep_char_list(Head) of
                true -> {ok, lists:flatten(Head)};
                false -> invalid_name
            end;
        false ->
            find_spec(Name, Arity, Rest)
    end;
find_spec (Name, Arity, [_ | Rest]) ->
    find_spec(Name, Arity, Rest).


nname (Name, 0)   -> Name;
nname (Name, Acc) -> Name ++ "-" ++ integer_to_list(Acc).

inc_name (Name, List, Acc) ->
    case lists:member(nname(Name, Acc), List) of
        true   -> inc_name(Name, List, Acc+1);
        false  -> nname(Name, Acc)
    end.

%% Strips xml children that are entirely whitespace (space, tabs, newlines)
strip_whitespace (List) when is_list(List) ->
    [ strip_whitespace(X) || X <- List, is_whitespace(X) ];
strip_whitespace ({El,Attr,Children}) ->
    {El, Attr, strip_whitespace(Children)};
strip_whitespace (Else) ->
    Else.

is_whitespace (X) when is_tuple(X); is_number(X) ->
    true;
is_whitespace (X) ->
    nomatch == re:run(X, "^[ \n\t]*$", [unicode]). %"

%% rather basic xml to string converter, takes xml of the form
%% {tag, [{listof, "attributes"}], ["list of children"]}
%% into <tag listof="attributes">list of children</tag>
xml_to_str (Xml) ->
    xml_to_html (Xml).

xml_to_html ({Tag, Attr}) ->
    %% primarily for cases such as <a name="">
    fmt("<~ts ~ts>", [Tag, atos(Attr)]);
xml_to_html ({Tag, Attr, []}) ->
    fmt("<~ts ~ts />", [Tag, atos(Attr)]);
xml_to_html ({Tag, [], []}) ->
    fmt("<~ts />", [Tag]);
xml_to_html ({Tag, [], Child}) ->
    fmt("<~ts>~ts</~ts>", [Tag, xml_to_html(Child), Tag]);
xml_to_html ({Tag, Attr, Child}) ->
    fmt("<~ts ~ts>~ts</~ts>", [Tag, atos(Attr), xml_to_html(Child), Tag]);
xml_to_html (List) when is_list(List) ->
    case io_lib:char_list(List) of
        true  -> htmlchars(List);
        false -> lists:flatten([ xml_to_html(X) || X <- List])
    end.

atos ([])                      -> "";
atos (List) when is_list(List) -> string:join([ atos(X) || X <- List ], " ");
atos ({Name, Val})             -> atom_to_list(Name) ++ "=\""++Val++"\"".

%% convert ascii into html characters
htmlchars (List) ->
    htmlchars(List, []).

htmlchars ([], Acc) ->
    lists:flatten(lists:reverse(Acc));

htmlchars ([$<   | Rest], Acc) -> htmlchars(Rest, ["&lt;" | Acc]);
htmlchars ([$>   | Rest], Acc) -> htmlchars(Rest, ["&gt;" | Acc]);
htmlchars ([160  | Rest], Acc) -> htmlchars(Rest, ["&nbsp;" | Acc]);
htmlchars ([Else | Rest], Acc) -> htmlchars(Rest, [Else | Acc]).

%% @doc parse xml file against otp's dtd, need to cd into the
%% source directory because files are addressed relative to it
-spec read_xml(list(), list()) -> tuple().
read_xml (_Conf, XmlFile) ->
    %Opts  = [{fetch_path, [code:lib_dir(docbuilder, dtd)]},
    %         {encoding, "latin1"}],
    log("Reading xml for ~p\n", [XmlFile]),
    DocgenDir = code:priv_dir(erl_docgen),
    Opts = [ {fetch_path, [ filename:join(DocgenDir, "dtd")
                          , filename:join(DocgenDir, "dtd_html_entities")
                          ]}
           , {encoding, "latin1"}
           ],
    {Xml, _}  = xmerl_scan:file(XmlFile, Opts),
    xmerl_lib:simplify_element(Xml).

%% lazy shorthand
fmt (Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

log (Str) ->
    ?LOG(Str).
log (Str, Args) ->
    ?LOG(Str, Args).

%% @doc shorthand for lists:keyfind
-spec kf(term(), list()) -> term().
kf (Key, Conf) ->
    {Key, Val} = lists:keyfind(Key, 1, Conf),
    Val.

%% @doc path to the destination folder
-spec dest(list()) -> list().
dest (Conf) ->
    [kf(dest, Conf)].

bname (Name) ->
    filename:basename(Name).
bname (Name, Ext) ->
    filename:basename(Name, Ext).

% List of the type of xml files erldocs can build
buildable () ->
    [erlref, cref].

ignore () ->
    [{"kernel", "init"},
     {"kernel", "zlib"},
     {"kernel", "erlang"},
     {"kernel", "erl_prim_loader"}].

-type map_fun(D, R) :: fun((D) -> R).
-type reduce_fun(T) :: fun((T, _) -> _).

-spec pmapreduce(map_fun(T, R), reduce_fun(R), R, [T]) -> [R].
pmapreduce (Map, Reduce, Acc0, L) ->
    pmapreduce(Map, Reduce, Acc0, L, 4). % erlang:system_info(schedulers_online) !

-spec pmapreduce(map_fun(T, R), reduce_fun(R), R, [T], pos_integer()) -> [R].
pmapreduce (Map, Reduce, Acc0, L, N) ->
    Keys = [rpc:async_call(node(), ?MODULE, mapreduce,
                           [Map, Reduce, Acc0, Segment])
            || Segment <- segment(L, N)],
    mapreduce(fun rpc:yield/1, Reduce, Acc0, Keys).

-spec mapreduce(map_fun(T, R), reduce_fun(R), R, [T]) -> [R].
mapreduce (Map, Reduce, Acc0, L) ->
    F = fun (Elem, Acc) ->
                Reduce(Map(Elem), Acc)
        end,
    lists:foldl(F, Acc0, lists:reverse(L)).

-spec segment([T], pos_integer()) -> [[T]].
segment (List, Segments) ->
    segment(List, length(List) div Segments, Segments).

-spec segment([T], non_neg_integer(), pos_integer()) -> [[T]].
segment (List, _N, 1) ->
    [List];
segment (List, N, Segments) ->
    {Front, Back} = lists:split(N, List),
    [Front | segment(Back, N, Segments - 1)].
