%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_core).

%% erldocs_core: utilities for module erldocs.

-export([ copy_static_files/1
        , build/1
        , dispatch/1

        , mapreduce/4
        , pmapreduce/4
        , pmapreduce/5 ]).

-include_lib("kernel/include/file.hrl").

-define(LOG(Str, Args), io:format(Str, Args)).
-define(LOG(Str),       io:format(Str)).

%% API

%% @doc Copy static files
-spec copy_static_files (list()) -> ok.
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
-spec dispatch (list()) -> ok.
dispatch (Conf) ->
    Start = erlang:now(),
    build(Conf),
    Diff = timer:now_diff(erlang:now(), Start),
    Mins = trunc(Diff * 1.667e-8),
    Secs = trunc(Diff * 1.000e-6 - Mins * 60),
    log("Woot, finished in ~p Minutes ~p Seconds~n",
        [Mins, Secs]).


%% @doc Build everything
-spec build (list()) -> ok.
build (Conf) ->
    filelib:ensure_dir(dest(Conf)),

    Fun   = fun (X, Y) -> build_apps(Conf, X, Y) end,
    Index = lists:foldl(Fun, [], app_dirs(Conf)),

    case length(kf(apps,Conf)) == length(Index) of
        true  ->
            %% Only the "[application]" (inserted by build_apps/3) are present
            %% in Index, thus:
            log("No documentation was generated!\n");
        false ->
            ok = module_index(Conf, Index),
            ok = javascript_index(Conf, Index),
            ok = copy_static_files(Conf)
    end.

build_apps (Conf, App, Index) ->
    AppName = bname(App),
    log("Building ~s~n", [AppName]),
    Files   = ensure_docsrc(Conf, App),
    Map = fun (F) -> build_file_map(Conf, AppName, F) end,
    [["app", AppName, AppName, "[application]"] |
     pmapreduce(Map, fun lists:append/2, [], Files) ++ Index].

build_file_map (Conf, AppName, File) ->
    log("Generating HTML - ~s ~p\n", [bname(File, ".xml"), File]),
    {Type, _Attr, Content} = read_xml(Conf, File),

    TypeSpecsFile = filename:join([dest(Conf), ".xml", "specs_" ++ bname(File)]),
    case filelib:is_file(TypeSpecsFile) of
        false ->                      TypeSpecs = [];
        true ->
            case read_xml(Conf, TypeSpecsFile) of
                {error, _, _} ->      TypeSpecs = [];
                {module, _, Specs} -> TypeSpecs = strip_whitespace(Specs)
            end
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

            %% strip silly shy characters
            Funs = get_funs(AppName, Module, lists:keyfind(funcs, 1, Xml)),

            ok = render(Type, AppName, Module, Content, TypeSpecs, Conf),

            case lists:member({AppName, Module}, ignore()) of
                true -> [];
                false -> [ ["mod", AppName, Module, Sum1] |  Funs]
            end
    end.

ensure_docsrc (Conf, AppDir) ->
    %% List any doc/src/*.xml files that exist in the source files
    XMLFiles = filelib:wildcard(filename:join([AppDir, "doc", "src", "*.xml"])),
    HandWritten = [bname(File, ".xml") || File <- XMLFiles],

    ErlFiles = filelib:wildcard(filename:join([AppDir,        "*.erl"]))
        ++     filelib:wildcard(filename:join([AppDir, "src", "*.erl"])),

    %% Generate any missing module XML
    SrcFiles = [filename:absname(File) ||
                   File <- ErlFiles,
                   not lists:member(bname(File, ".erl"), HandWritten)],

    %% Output XML files to destination folder
    %% This prevents from polluting the source files
    XMLDir = filename:join([dest(Conf), ".xml", bname(AppDir)]),
    filelib:ensure_dir(XMLDir ++ "/"),

    case erlang:system_info(otp_release) of
        "R"++Old when Old < "15" -> SpecsGenModule = specs_gen__below_R15;
        _ ->                        SpecsGenModule = specs_gen__R15_and_above
    end,
    SpecsDest = filename:join([dest(Conf), ".xml"]),
    IncFiles = includes(Conf, AppDir),
    SpecsIncludes = ["-I"++Inc || Inc <- IncFiles],
    [ begin
          log("Generating Type Specs - ~p\n", [File]),
          Args = ["-o"++SpecsDest] ++ SpecsIncludes ++ [File],
          try SpecsGenModule:main(Args)
          catch _:_SpecsGenError ->
                  log("Error generating type specs for ~p\n", [File])
          end
      end || File <- ErlFiles],

    %% Return the complete list of XML files
    XMLFiles ++ tmp_cd(XMLDir, fun () ->
                                       gen_docsrc(SrcFiles, IncFiles, XMLDir)
                               end).


includes (Conf, AppDir) ->
    Above_AppDir = filename:dirname(AppDir),
    keep_existings([ Above_AppDir
                     %% Those 3 look suspicious (FIXME)
                   , filename:join(Above_AppDir, "include")
                   , filename:join(Above_AppDir, "deps")
                   , filename:join(Above_AppDir, "lib")
                   , filename:join(AppDir, "include")
                   , filename:join(AppDir, "deps")
                   , filename:join(AppDir, "lib")
                     %% These 2 in case of a non-regular architecture:
                   , AppDir%%
                   , filename:join(Above_AppDir, "src")%%
                     | kf(incs,Conf) ]).


gen_docsrc (SrcFiles, IncFiles, Dest) ->
    Opts = [ {includes, IncFiles}
           , {sort_functions, false}
           , {layout, docgen_edoc_xml_cb}
           , {file_suffix, ".xml"}
           , {preprocess, true} ],
    lists:foldl(
      fun (File, Acc) ->
              Basename = bname(File, ".erl"),
              DestFile = filename:join([Dest, Basename++".xml"]),
              log("Generating XML - ~s ~p -> ~p\n", [Basename,File,DestFile]),
              Options = [ {dir, filename:dirname(DestFile)} | Opts],
              case (catch edoc:file(File, Options)) of
                  ok ->
                      [DestFile | Acc];
                  Error ->
                      log("Error generating XML (~p): ~p~n", [File, Error]),
                      Acc
              end
      end, [], SrcFiles).

keep_existings (AppDirIncludes) ->
    [Include || Include <- AppDirIncludes, filelib:is_dir(Include)].

%% @doc run a function with the cwd set, ensuring the cwd is reset once
%% finished (some dumb functions require to be ran from a particular dir)
-spec tmp_cd (list(), fun()) -> term().
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
        ++      xml_to_str(emit_index(Index))
        ++ "</div>",
    Args = [ {base,    kf(base,Conf)}
           , {search_base, "./"}
           , {title,   "Module Index"}
           , {content, Html}
           , {funs,    ""}
           , {ga,      kf(ga,Conf)} ],

    {ok, Data} = erldocs_dtl:render(Args),
    ok = file:write_file([dest(Conf), "/index.html"], Data).

emit_index (L) ->
    lists:flatmap(
      fun (["app", App, _,  _Sum]) ->
              [{a, [{name, App}]}, {h4, [], [App]}];
          (["mod", App, Mod, Sum]) ->
              Url = App ++ "/" ++ Mod ++ ".html",
              [{p,[], [{a, [{href, Url}], [Mod]}, {br,[],[]}, Sum]}];
          (_) ->
              []
      end,
      lists:sort(fun sort_index/2, L)).

type_ordering ("app") -> 1;
type_ordering ("mod") -> 2;
type_ordering ("fun") -> 3.

index_ordering ([Type, App, Mod, _Sum]) ->
    [ string:to_lower(App)
    , type_ordering(Type)
    , string:to_lower(Mod) ].

sort_index (A, B) ->
    index_ordering(A) =< index_ordering(B).

html_encode (Str)  ->
    [C || C <- Str, C /= $'].

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

    Js = fmt("var index = [~s];",
             [string:join([[C || C <- I, C /= $\n, C /= $\r] || I <- Index], ",")]),

    ok = file:write_file([dest(Conf), "/erldocs_index.js"], Js).

render (cref, App, Mod, Xml, Types, Conf) ->
    render(erlref, App, Mod, Xml, Types, Conf);

render (erlref, App, Mod, Xml, Types, Conf) ->

    File = filename:join([dest(Conf), App, Mod ++ ".html"]),
    ok   = filelib:ensure_dir(filename:dirname(File) ++ "/"),

    Acc = [{ids,[]}, {list,ul}, {functions,[]}, {types,Types}],

    {[_Id, _List, {functions,_Funs}, {types,_Types}], NXml}
        = render(fun tr_erlref/2,  Xml, Acc),

%%  XmlFuns = [{li, [], [{a, [{href,"#"++X}], [X]}]}
%%              || X <- lists:reverse(Funs) ],

    case kf(base,Conf) of %this is awkward
        "./" ->  %% Default value. In the default case
            Base = "../";  %% … files are up one leve.
        Other -> Base = Other
    end,
    Args = [ {base,    Base}
           , {search_base, "../"}
           , {title,   Mod ++ " (" ++ App ++ ") - "}
           , {content, xml_to_str(NXml)}
%%         , {funs,    xml_to_str({ul, [{id,"funs"}], XmlFuns})}
           , {ga,      kf(ga,Conf)} ],

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
    case lists:keyfind(fsummary, 1, Child) of
        {fsummary, [], Xml} ->
            Summary = string:substr(xml_to_str(Xml), 1, 50);
        false ->
            Summary = ""
            %% Things like 'ose_erl_driver.xml' (C drivers) don't have fsummary
            %%  but nametext instead. In such cases fsummary is ignored anyway.
    end,

    F = fun ({name, [], Name}, Acc) ->
                case make_name(Name) of
                    ignore -> Acc;
                    NName  -> [ ["fun", App, Mod++":"++NName, Summary] | Acc ]
                end;
            ({name, [{name,Name}, {arity,Arity}], []}, Acc) ->
                [ ["fun", App, Mod++":"++Name++"/"++Arity, Summary] | Acc ];
            ({name, [{name,Name}, {arity,Arity}, {clause_i,"1"}], []}, Acc) ->
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

'add .html' ("#"++Rest) ->
    "#"++ separate_f_from_a(Rest);
'add .html' (Link) ->
    case string:tokens(Link, "#") of
        [Tmp]    -> Tmp++".html";
        [N1, N2] -> lists:flatten([N1, ".html#", separate_f_from_a(N2)])
    end.

tr__marker (FdashA) ->
    %% When Marker denotes "function-arity", replace its - with a /.
    Mark = separate_f_from_a(FdashA),
    case FdashA =:= Mark of
        true  -> {span, [{id,Mark}], [" "]};
        false -> ignore
    end.

separate_f_from_a (FdashA) ->
    separate_f_from_a(lists:reverse(FdashA), [], []).
separate_f_from_a ([], Arity, AccF0) ->
    AccF = lists:reverse(AccF0),
    case Arity of
        [] -> AccF;
        _  -> AccF ++"/"++ Arity
    end;
separate_f_from_a ([C|Rest], AccA, []) ->
    case C of
        _  when $0 =< C andalso C =< $9 ->
            separate_f_from_a(Rest, [C|AccA], []);
        $- ->
            separate_f_from_a([],      AccA,  Rest);
        _  ->
            separate_f_from_a([],      AccA,  [C|Rest])
end.


%% Transforms erlang xml format to html
tr_erlref (Element) ->
    tr_erlref(Element, ignore_acc).
tr_erlref ({header,[],_Child}, _Acc) ->
    ignore;
tr_erlref ({marker, [{id,Marker}], []}, _Acc) ->
    tr__marker(Marker);
tr_erlref ({term,[{id, Term}], _Child}, _Acc) ->
    Term;
tr_erlref ({lib,[],Lib}, _Acc) ->
    {h1, [], [lists:flatten(Lib)]};
tr_erlref ({module,[],Module}, _Acc) ->
    {h1, [], [lists:flatten(Module)]};
tr_erlref ({modulesummary, [], Child}, _Acc) ->
    {h2, [{class,"modsummary"}], Child};
tr_erlref ({c, [], Child}, _Acc) ->
    {code, [], Child};
tr_erlref ({title, [], Child}, _Acc) ->
    {h4, [], [Child]};
tr_erlref ({v, [], []}, _Acc) ->
    {li, [], [" "]};
tr_erlref ({v, [], Child}, _Acc) ->
    {li, [], [{code, [], Child}]};
tr_erlref ({seealso, [{marker, Marker}], Child}, _Acc) ->
    case string:tokens(Marker, ":") of
        []        -> Url = 'add .html'(lists:flatten(Child));
        [Tmp]     -> Url = 'add .html'(Tmp);
        [Ap | Md] -> Url = "../"++ Ap ++"/"++ 'add .html'(lists:flatten(Md))
    end,
    {a, [{href,Url},{class,"seealso"}], Child};

tr_erlref ({desc, [], Child}, _Acc) ->
    {'div', [{class, "description"}], Child};
tr_erlref ({description, [], Child}, _Acc) ->
    {'div', [{class, "description"}], Child};

tr_erlref ({funcs, [], Child}, _Acc) ->
    tr__category("Functions", "functions", Child);
tr_erlref ({func, [], Child}, _Acc) ->
    {'div', [{class,"function"}], Child};

tr_erlref ({datatypes, [], Child}, _Acc) ->
    tr__category("Types", "types", Child);
tr_erlref ({datatype, [], Child}, _Acc) ->
    {'div', [{class,"type"}], Child};
tr_erlref ({name, [], [{marker,[{id,ID="type-"++_}],Child}|_]}, _Acc) ->
    %% Documented exported opaque types
    %% Note: opaque types' contents are not described.
    tr__type_name(ID, Child);
tr_erlref ({name, [{name,TName}], []}, Acc) ->
    tr__type_name(TName, "0", Acc);
tr_erlref ({name, [{name,TName},{n_vars,NVars}], []}, Acc) ->
    tr__type_name(TName, NVars, Acc);
tr_erlref ({name, [{name,TName},{n_vars,_,[NVars]}], []}, Acc) ->
    tr__type_name(TName, NVars, Acc);

tr_erlref ({section, [], [{title,[],["DATA TYPES"]}|Child]}, Acc) ->
    {taglist, _, Tags} = lists:keyfind(taglist, 1, Child),
    DTypes = [ begin
                   CompressedName = TName ++ "/0",
                   case tr__type_name(TName, "0", Acc) of
                       {h3, [{id,"type-"++TName}], [CompressedName]} = NotFound ->
                           %% Did not find type, will use taglist's definition
                           Defs = [X || {tag,_,[{c,_,[X]}]} <- Tags,
                                        lists:prefix(TName++"(", X)],
                           case Defs of
                               [] -> DType = NotFound;
                               _  -> DType = {h3, [{id,"type-"++TName}], [hd(Defs)]}
                           end;
                       Found ->
                           DType = Found
                   end,
                   [ "\n    "
                   , {'div', [{class,"type"}], [DType]} ]
               end || {item,_,[{marker,[{id,"type-"++TName}|_],_}|_]} <- Tags ],
    tr__category("Types", "types", DTypes);
tr_erlref ({section, [], Child}, _Acc) ->
    {'div', [{class,"section"}], Child};

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

tr_erlref ({type, [{variable,_VarName}|_], []}, _Acc) ->
    ignore;
tr_erlref ({type, [], Child}, _Acc) ->
    {ul, [{class, "type"}], Child};
tr_erlref (E={type, [{name,TName}], []}, Acc) ->
    {_, Types} = lists:keyfind(types, 1, Acc),
    case find_type(TName, "0", Types) of
        ignore -> E;
        {_ID, Child} -> {ul
                        , [{class, "type"}]
                        , {li, [], {code, [], [Child]}} }
    end;

tr_erlref ({name, [{name,Name}, {arity,N}, {clause_i,ClauseI}], []}, Acc)
  when ClauseI =:= "1" ->
    tr_erlref({name, [{name,Name}, {arity,N}], []}, Acc);
tr_erlref ({name, [{name,____}, {arity,_}, {clause_i,ClauseI}], []}, ___)
  when ClauseI  >  "1" ->
    ignore;
tr_erlref ({name, [{name,Name}, {arity,N}], []}, Acc) ->
    [{ids,Ids}, List, {functions,Funs}, {types,Types}] = Acc,
    NName = inc_name(Name, Ids, 0),
    ID = Name ++ "/" ++ N,
    Found = find_spec(Name, N, Types),
    {SpecsFound, Names} = lists:unzip(Found),
    Specs = [ {li, [], [{code, [], [Spec]}]}
              || Spec <- merge_specs(SpecsFound), Spec /= [] ],
    NSpecs = case Specs of
                 [] -> [];
                 _  -> ["\n      ", {ul, [{class,"type_desc"}], Specs}]
             end,
    Tags = case Names of
               []             ->
                   [{h3, [{id,ID}], [ID]}];
               [PName|PNames] ->
                   [{h3, [{id,ID}], [PName]}]
                       ++ [ {h3, [], [PNameK]} || PNameK <- PNames ]
           end,
    { Tags ++ NSpecs
    , [{ids,[NName|Ids]}, List, {functions,[NName|Funs]}, {types,Types}] };
tr_erlref ({name, [], Child}, Acc) ->
    [{ids,Ids}, List, {functions,Funs}, {types,Types}] = Acc,
    case make_name(Child) of
        ignore -> ignore;
        Name   ->
            NName = inc_name(Name, Ids, 0),
            { {h3, [{id, NName}], [Child]}
            , [{ids,[NName|Ids]}, List, {functions,[NName|Funs]}, {types,Types}] }
    end;

tr_erlref ({type_desc, [{variable, Name}], [Desc]}, _Acc) ->
    {'div', [{class, "type_desc"}], [{code, [], [Name, " = ",Desc]}]};
tr_erlref ({fsummary, [], _Child}, _Acc) ->
    ignore;
tr_erlref (Else, _Acc) ->
    Else.


merge_specs (Specs) ->
    case Specs of
        []    -> [];
        [H|T] -> merge_specs(T, lists:reverse(H))
    end.
merge_specs ([], Acc) -> lists:reverse(Acc);
merge_specs ([[]|Rest], Acc) ->
    merge_specs(Rest, Acc);
merge_specs ([[Spec|Specs]|Rest], Acc) ->
    case lists:member(Spec, Acc) of
        true  -> merge_specs([Specs|Rest],       Acc );
        false -> merge_specs([Specs|Rest], [Spec|Acc])
    end.

find_spec (_Name, _Arity, []) -> [];
find_spec (Name, Arity, [{spec, [], Specs} |Rest]) ->
    {_, _, [SpecName]}  = lists:keyfind(name, 1, Specs),
    {_, _, [ArityName]} = lists:keyfind(arity, 1, Specs),
    case (SpecName =:= Name) and (ArityName =:= Arity) of
        false ->
            find_spec(Name, Arity, Rest);
        true  ->
            {_, _, Contracts} = lists:keyfind(contract, 1, Specs),
            {_, _, Clause}    = lists:keyfind(clause, 1, Contracts),
            {_, _, Head}      = lists:keyfind(head, 1, Clause),
            TheName = lists:map(fun tr_erlref/1, Head),
            case lists:keyfind(guard, 1, Clause) of
                false ->
                    TheSpec = [];
                {_, _, Subtypes} ->
                    TheSpec = [ lists:map(fun tr_erlref/1, S)
                                || {subtype,[]
                                   , [ {typename,[],_}
                                     , {string,[],S} ] } <- Subtypes]
            end,
            [ {TheSpec,TheName}  %% Continue searching for other clauses
              | find_spec(Name, Arity, Rest) ]
    end;
find_spec (Name, Arity, [_ | Rest]) ->
    find_spec(Name, Arity, Rest).

find_type (Name0, NVars0, [{type,[],Type} |Rest]) ->
    %%don't print insides when type is -opaque
    {_, _, [Name]}   = lists:keyfind(name, 1, Type),
    {_, _, [NVars]}  = lists:keyfind(n_vars, 1, Type),
    case (Name =:= Name0) and (NVars =:= NVars0) of
        true ->
            {_, _, TypeDecl} = lists:keyfind(typedecl, 1, Type),
            {_, _, TypeHead} = lists:keyfind(typehead, 1, TypeDecl),
            [{marker,[{id,ID}|_],[NName]} |Child] = TypeHead, %%refactor with tr_erlref
            {ID, lists:flatten([NName|Child])};
        false ->
            find_type(Name0, NVars0, Rest)
    end;
find_type (Name, NVars, [_|Rest]) ->
    find_type(Name, NVars, Rest);
find_type (_Name, _NVars, []) ->
    ignore.


tr__type_name (ID, Child) ->
    NChild = [case E of
                  Br when element(1,E) =:= br ->
                      %%Hack to align display of `#types.type h3`. TODO
                      %% [Br, string:copies("&nbsp;", 8)];
                      [Br | lists:duplicate(8, {nbsp,[],[]})];
                  _ -> E
              end || E <- Child],
    {h3, [{id,ID}], [NChild]}.
tr__type_name (TName, NVars, Acc) ->
    {_, Types} = lists:keyfind(types, 1, Acc),
    case find_type(TName, NVars, Types) of
        {ID, Child} ->
            tr__type_name(ID, Child);
        ignore ->
            tr__type_name("type-"++TName, TName++"/"++NVars)
    end.


tr__category (Name, ID, Child) ->
    {'div', [{id,ID}, {class,"category"}]
    , [ {h4, [], [{a, [{href,"#"++ID}], [Name]}]}
      , {hr, [], []}
      | Child ]}.


nname (Name, 0)   -> Name;
nname (Name, Acc) -> Name ++ "-" ++ integer_to_list(Acc).

inc_name (Name, List, Acc) ->
    case lists:member(nname(Name, Acc), List) of
        true   -> inc_name(Name, List, Acc+1);
        false  -> nname(Name, Acc)
    end.

%% Strips xml children that are entirely whitespace (space, tabs, newlines)
strip_whitespace (List) when is_list(List) ->
    [strip_whitespace(X) || X <- List, 'keeper?'(X)];
strip_whitespace ({El,Attr,Children}) ->
    {El, Attr, strip_whitespace(Children)};
strip_whitespace (Else) ->
    Else.

'keeper?' (X) when is_tuple(X); is_number(X) ->
    true;
'keeper?' (X) ->
    not lists:all(fun (C) -> C == $  orelse C == $\n orelse C == $\t end, X).

%% rather basic xml to string converter, takes xml of the form
%% {tag, [{listof, "attributes"}], ["list of children"]}
%% into <tag listof="attributes">list of children</tag>
xml_to_str (Xml) ->
    xml_to_html (Xml).

xml_to_html (Nbsp)
  when element(1, Nbsp) =:= nbsp ->
    "&nbsp;";
xml_to_html ({nbsp, [], Child}) ->
    "&nbsp;" ++ xml_to_html(Child);

xml_to_html ({Tag, Attr}) ->
    %% primarily for cases such as <a name="">
    fmt("<~ts ~ts>", [Tag, atos(Attr)]);
xml_to_html ({Tag, Attr, []}) ->
    fmt("<~ts ~ts/>", [Tag, atos(Attr)]);
xml_to_html ({Tag, [], []}) ->
    fmt("<~ts/>", [Tag]);
xml_to_html ({Tag, [], Child}) ->
    fmt("<~ts>~ts</~ts>", [Tag, xml_to_html(Child), Tag]);
xml_to_html ({Tag, Attr, Child}) ->
    fmt("<~ts ~ts>~ts</~ts>", [Tag, atos(Attr), xml_to_html(Child), Tag]);
xml_to_html (List) when is_list(List) ->
    case io_lib:char_list(List) of
        true  -> htmlchars(List);
        false -> lists:flatten([xml_to_html(X) || X <- List])
    end;
xml_to_html (Else) ->
    Else.


atos ([])                      -> "";
atos (List) when is_list(List) -> string:join([ atos(X) || X <- List ], " ");
atos ({Name, Val})             -> atom_to_list(Name) ++ "=\""++Val++"\"".

%% convert ascii into html characters
htmlchars (List) -> htmlchars(List, []).
htmlchars ([], Acc) -> lists:flatten(lists:reverse(Acc));
htmlchars ([$<  |Rest], Acc) -> htmlchars(Rest, ["&lt;"  |Acc]);
htmlchars ([$>  |Rest], Acc) -> htmlchars(Rest, ["&gt;"  |Acc]);
htmlchars ([$   |Rest], Acc) -> htmlchars(Rest, ["&nbsp;"|Acc]);
htmlchars ([Else|Rest], Acc) -> htmlchars(Rest, [Else    |Acc]).

%% @doc parse xml file against otp's dtd, need to cd into the
%% source directory because files are addressed relative to it
-spec read_xml (list(), list()) -> tuple().
read_xml (_Conf, XmlFile) ->
    log("Reading XML for ~p\n", [XmlFile]),
    DocgenDir = code:priv_dir(erl_docgen),
    Opts = [ {fetch_path, [ filename:join(DocgenDir, "dtd")
                          , filename:join(DocgenDir, "dtd_html_entities") ]}
           , {encoding, "latin1"} ],
    case catch xmerl_scan:file(XmlFile, Opts) of
        {Xml, _Rest} ->
            xmerl_lib:simplify_element(Xml);
        Error ->
            log("Error in read_xml File ~p Erro ~p\n", [XmlFile, Error]),
            throw({error_in_read_xml, XmlFile, Error})
    end.

%% lazy shorthand
fmt (Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

log (Str) ->       ?LOG(Str).
log (Str, Args) -> ?LOG(Str, Args).

%% @doc shorthand for lists:keyfind
-spec kf (term(), list()) -> term().
kf (Key, Conf) ->
    {Key, Val} = lists:keyfind(Key, 1, Conf),
    Val.

%% @doc path to the destination folder
-spec dest (list()) -> list().
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
    [ {"kernel", "init"}
    , {"kernel", "zlib"}
    , {"kernel", "erlang"}
    , {"kernel", "erl_prim_loader"} ].

-type map_fun(D, R) :: fun((D) -> R).
-type reduce_fun(T) :: fun((T, _) -> _).

-spec pmapreduce (map_fun(T, R), reduce_fun(R), R, [T]) -> [R].
pmapreduce (Map, Reduce, Acc0, L) ->
    pmapreduce(Map, Reduce, Acc0, L, erlang:system_info(schedulers_online)).

-spec pmapreduce (map_fun(T, R), reduce_fun(R), R, [T], pos_integer()) -> [R].
pmapreduce (Map, Reduce, Acc0, L, N) ->
    Keys = [rpc:async_call(node(), ?MODULE, mapreduce,
                           [Map, Reduce, Acc0, Segment])
            || Segment <- segment(L, N)],
    mapreduce(fun rpc:yield/1, Reduce, Acc0, Keys).

-spec mapreduce (map_fun(T, R), reduce_fun(R), R, [T]) -> [R].
mapreduce (Map, Reduce, Acc0, L) ->
    lists:foldl(fun (Elem, Acc) ->
                        Reduce(Map(Elem), Acc)
                end,
                Acc0, lists:reverse(L)).

-spec segment ([T], pos_integer()) -> [[T]].
segment (List, Segments) ->
    segment(List, length(List) div Segments, Segments).

-spec segment ([T], non_neg_integer(), pos_integer()) -> [[T]].
segment (List, _N, 1) ->
    [List];
segment (List, N, Segments) ->
    {Front, Back} = lists:split(N, List),
    [Front | segment(Back, N, Segments - 1)].

%% End of Module.
