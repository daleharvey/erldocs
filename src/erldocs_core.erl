%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_core).

%% erldocs_core: utilities for module erldocs.

-export([ copy_static_files/1
        , build/1
        , dispatch/1

        , mapreduce/4
        , pmapreduce/4
        , pmapreduce/5

        , maybe_delete_xmerl_table/0
        ]).

-ifdef(TEST).
- export([includes/1]).
- export([filename__remove_prefix/2]).
-endif.

-include("erldocs.hrl").

-define(log(Str, Args), io:format(Str++"\n", Args)).
-define(log(Str),       io:format(Str++"\n")).

%% API

%% @doc Copy static files
-spec copy_static_files (list()) -> ok.
copy_static_files (Conf) ->
    Dest = kf(dest, Conf),
    {ok, ErlDocsCSS} = erldocs_css_dtl:render([]),
    edoc_lib:write_file(ErlDocsCSS, Dest, "erldocs.css"),
    {ok, ErlDocsJS } =  erldocs_js_dtl:render([]),
    edoc_lib:write_file(ErlDocsJS,  Dest, "erldocs.js"),
    {ok, Jquery    } =   jquery_js_dtl:render([]),
    edoc_lib:write_file(Jquery,     Dest, "jquery.js"),
    ok.

%% @doc Parses arguments passed to script and calls
%% appropriate function.
-spec dispatch (list()) -> boolean().
dispatch (Conf) ->
    maybe_create_xmerl_table(),
    DidBuild = build([ {building_otp,is_building_otp(kf(apps,Conf))} | Conf]),
    ?log("Woot, finished"),
    DidBuild.

is_building_otp ([]) -> false;
is_building_otp ([AppDir|Rest]) ->
    case find_erlang_module(AppDir) of
        false -> is_building_otp(Rest);
        Path -> {true, Path}
    end.

find_erlang_module (AppDir) ->
    ExitFast = fun (Fn, _Acc) -> throw({found, Fn}) end,
    try filelib:fold_files(AppDir, "^erlang\\.erl$", true, ExitFast, not_found) of
        not_found -> false
    catch
        {found,Fn} ->
            ?log("building_otp: true"),
            Fn
    end.


%% @doc Build everything
-spec build (list()) -> boolean().
build (Conf) ->
    mkdir_p(kf(dest, Conf)),
    AppDirs = [Path || Path <- kf(apps,Conf), filelib:is_dir(Path)],
    IncludePaths = lists:usort(lists:flatmap(fun includes/1, AppDirs)),

    BuildApps = fun (AppDir) -> build_apps(Conf, IncludePaths, bname(AppDir), AppDir) end,
    Index = [I || I <- lists:flatmap(BuildApps, AppDirs), I /= ignore],

    case length(kf(apps,Conf)) == length(Index) of
        true ->
            %% Only the "[application]" (inserted by build_apps/5) are present
            %% in Index, thus:
            ?log("No documentation was generated!"),
            false;
        false ->
            SortedIndex = sort_index(Index),
            ok = module_index(Conf, SortedIndex),
            ok = javascript_index(Conf, SortedIndex),
            ok = copy_static_files(Conf),
            true
    end.

build_apps (Conf, IncludePaths, AppName, AppDir) ->
    ?log("Building ~s", [AppName]),
    Files = ensure_docsrc(Conf, IncludePaths, AppName, AppDir),
    Map = fun (File) -> build_file_map(Conf, AppName, bname(File,".xml"), File) end,
    [ {"app", AppName, AppName, "[application]"}
      | pmapreduce(Map, fun lists:append/2, [], Files)
    ].

build_file_map (Conf, AppName, Module, File) ->
    case is_ignored(AppName, Module) of
        true ->
            ?log("HTML generation for ~s skipped - ~p", [Module, File]),
            [];
        false ->
            {Type, _Attr, Content} = read_xml(File),
            case is_buildable(Type) of
                false ->
                    %% ?log("HTML generation for ~s impossible - ~p", [Module, File]),
                    [];
                true ->
                    ?log("Generating HTML - ~s ~p", [Module, File]),
                    Xml = strip_whitespace(Content),
                    Funs = get_funs(AppName, Module, Xml),
                    TypeSpecs = read_xml_specs(Conf, Module),
                    ok = render(AppName, Module, Content, TypeSpecs, Conf),
                    Sum = lists:flatten(module_summary(Type, Xml)),
                    [ {"mod", AppName, Module, Sum} | Funs]
            end
    end.

module_summary (erlref, Xml) ->
    {_, [], Sum} = lists:keyfind(modulesummary, 1, Xml),
    unicode:characters_to_list([X || X <- Sum, not is_tuple(X)]);
module_summary (cref, Xml) ->
    {_, [], Sum} = lists:keyfind(libsummary, 1, Xml),
    Sum.

function_summary (FuncXml) ->
    case lists:keyfind(fsummary, 1, FuncXml) of
        {fsummary, [], Xml} ->
            shorten(xml_to_html(Xml));
        false -> ""
                 %% Things like 'ose_erl_driver.xml' (C drivers) don't have fsummary
                 %%  but nametext instead. In such cases fsummary is ignored anyway.
    end.

shorten (Str) ->
    Lines = string:tokens(lists:flatten(Str), "\r\n"),
    Clean = string:join(lists:map(fun string:strip/1, Lines), " "),
    string:substr(Clean, 1, 50).

ensure_docsrc (Conf, IncludePaths, AppName, AppDir) ->
    %% Output XML files to destination folder
    %% This prevents from polluting the source files
    TmpRoot = jname(kf(dest,Conf), ?ERLDOCS_SPECS_TMP),
    XMLDir  = jname(TmpRoot, AppName),
    mkdir_p(XMLDir ++ "/"),

    %% List all *.erl under AppDir/src/
    Erls = filelib:fold_files(jname(AppDir,"src"), "\\.erl$", true, fun cons/2, []),
    XMLFiles = filelib:wildcard(jname([AppDir, "doc", "src", "*.xml"])),

    case kf(building_otp, Conf) of
        false ->
            ErlFiles = filelib:wildcard(jname(AppDir, "*.erl")) ++ Erls,
            HandWritten = [bname(File,".xml") || File <- XMLFiles],
            NoXMLs = [File || File <- ErlFiles,
                              not lists:member(bname(File,".erl"), HandWritten)],
            %% Generate any missing module XML
            F = fun () -> gen_docsrc(IncludePaths, AppName, AppDir, XMLDir, NoXMLs) end,
            MissingXMLFiles = tmp_cd(XMLDir, F);

        {true,ErlangErl} ->
            ErlFiles = maybe_add_otp_preloaded(AppName, ErlangErl) ++ Erls,
            MissingXMLFiles = []
    end,

    SpecsGenF = fun (File) -> gen_type_specs(IncludePaths, TmpRoot, File) end,
    lists:foreach(SpecsGenF, ErlFiles),

    %% Return the complete list of XML files
    XMLFiles ++ MissingXMLFiles.

maybe_add_otp_preloaded ("erts", ErlangErl) ->
    ErlangErlAppDir = dname(dname(ErlangErl)),
    filelib:wildcard(jname([ErlangErlAppDir, "src", "*.erl"]));
maybe_add_otp_preloaded (_AppName, _) -> [].

gen_type_specs (IncludePaths, SpecsDest, ErlFile) ->
    case erlang:system_info(otp_release) of
        [$R,$1,Digit|_] when Digit < $5 -> SpecsGenModule = specs_gen__below_R15;
        "R"++_ ->                          SpecsGenModule = specs_gen__R15_to_17;
        Vsn when Vsn < "18" ->             SpecsGenModule = specs_gen__R15_to_17;
        _Otherwise ->                      SpecsGenModule = specs_gen__18_and_above
    end,
    ?log("Generating Type Specs - ~p", [ErlFile]),
    Args = ["-o"++SpecsDest] ++ ["-I"++Inc || Inc <- IncludePaths] ++ [ErlFile],
    try SpecsGenModule:main(Args)
    catch _:_SpecsGenError -> false
    end.

%% @doc
%% Read Erlang type specs from an XML file
read_xml_specs (Conf, Module) ->
    Fn = "specs_" ++ Module ++ ".xml",
    File = jname([kf(dest,Conf), ?ERLDOCS_SPECS_TMP, Fn]),
    case filelib:is_file(File) of
        false -> [];
        true ->
            case read_xml(File) of
                {error, _, _} -> [];
                {module, _, Specs} ->
                    ?log("Read XML Specs for ~s - '~s'", [Fn, File]),
                    strip_whitespace(Specs)
            end
    end.

includes (AppDir) ->
    Hrls = filelib:fold_files(AppDir, "\\.hrl$", true, fun cons/2, []),
    F = fun (Hrl) -> includes_parents(AppDir, Hrl) end,
    lists:usort([AppDir, dname(AppDir)] ++
                    lists:flatmap(F, Hrls)).

includes_parents (AppDir, Hrl) ->
    RelativeHrl = filename__remove_prefix(AppDir, Hrl),
    includes_parents(AppDir, dname(RelativeHrl), []).
includes_parents (_, ".", Acc) -> Acc;
includes_parents (AppDir, Parent, Acc) ->
    case lists:member("test", filename:split(Parent)) of
        true  -> NewAcc = Acc;
        false -> NewAcc = [jname(AppDir,Parent) | Acc]
    end,
    includes_parents(AppDir, dname(Parent), NewAcc).

filename__remove_prefix (Prefix, Path) ->
    case lists__remove_prefix(filename:split(Prefix), filename:split(Path)) of
        "" -> "";
        Exploded -> filename:join(Exploded)
    end.

lists__remove_prefix ([A|Prefix], [A|Rest]) ->
    lists__remove_prefix(Prefix, Rest);
lists__remove_prefix (_, Rest) ->
    Rest.

gen_docsrc (IncludePaths, AppName, AppDir, Dest, SrcFiles) ->
    App = list_to_atom(AppName),
    Opts = [ {includes, IncludePaths}
           , {sort_functions, false}
           , {file_suffix, ".xml"}
           , {preprocess, true}
           , {dir, Dest}
           , {packages, false}  %% Find modules in subfolders of src/
           , {layout, docgen_edoc_xml_cb}
           , {application, App}
           ],

    ?log("Generating XML for application ~s ~p -> ~p", [AppName,AppDir,Dest]),
    case catch (edoc:application(App, AppDir, Opts)) of
        ok ->
            XmlFiles = filelib:wildcard(jname(Dest, "*.xml")),
            ?log("Generated ~s XMLs: ~p", [AppName, XmlFiles]),
            XmlFiles;

        _Error ->
            ?log("Error generating ~s XMLs. Using fallback ...", [AppName]),
            lists:foldl(
              fun (File, Acc) ->
                      Module = bname(File, ".erl"),
                      DestFile = jname(Dest, Module++".xml"),
                      ?log("Generating XML ~s - ~s ~p -> ~p", [AppName,Module,File,DestFile]),
                      case catch (edoc:file(File, Opts)) of
                          ok ->
                              [DestFile | Acc];
                          Error ->
                              ?log("Error generating XML (~p): ~p", [File,Error]),
                              Acc
                      end
              end, [], SrcFiles)
    end.


%% @doc run a function with the cwd set, ensuring the cwd is reset once
%% finished (some dumb functions require to be ran from a particular dir)
-spec tmp_cd (file:name(), fun()) -> _.
tmp_cd (Dir, Fun) ->
    {ok, OldDir} = file:get_cwd(),
    mkdir_p(Dir),
    ok = file:set_cwd(Dir),
    try
        Result = Fun(),
        ok = file:set_cwd(OldDir),
        Result
    catch
        Type:Err ->
            ok = file:set_cwd(OldDir),
            throw({Type, Err, erlang:get_stacktrace()})
    end.


module_index (Conf, Index) ->
    ?log("Creating index.html ..."),
    Html = [ "<h1>Module Index</h1><hr/><br>\n<div>"
           ,   xml_to_html(index_to_xml(Index))
           , "</div>"
           ],
    Args = [ {base,    kf(base,Conf)}
           , {search_base, "./"}
           , {title,   "Module Index"}
           , {content, Html}
           %% , {funs,    ""}
           , {ga,      kf(ga,Conf)}
           ],
    {ok, Data} = erldocs_dtl:render(Args),
    Path = jname(kf(dest,Conf), "index.html"),
    ok = file:write_file(Path, Data).

index_to_xml (SortedIndex) ->
    [ case I of
          {"app", App, _,  _Sum} ->
              {h4, [{id, "app-"++App}], [App]};
          {"mod", App, Mod, Sum} ->
              Url = jname(App, Mod++".html"),
              {p, [], [ {a, [{href, Url}], [Mod]}
                      , {br, [], []}
                      , Sum
                      ]};
          {"fun", _App, _MFA, _Sum} ->
              <<>>
      end || I <- SortedIndex ].


sort_index (Index) ->
    lists:sort(fun compare_index_items/2, Index).

compare_index_items (Lhs, Rhs) ->
    index_ordering(Lhs) =< index_ordering(Rhs).

index_ordering ({Type, App, Mod, _Sum}) ->
    [ string:to_lower(App)
    , index_item(Type)
    , string:to_lower(Mod)
    ].

index_item ("app") -> 1;
index_item ("mod") -> 2;
index_item ("fun") -> 3.


js_strip (Str) ->
    [C || C <- Str, C /= $\n, C /= $\r, C /= $'].

javascript_index (Conf, SortedIndex) ->
    ?log("Creating erldocs_index.js ..."),
    IndexStr = iolists_tl(  %% Removes leading $,
                 [ [ ",['", js_strip(A)
                   , "','", js_strip(B)
                   , "','", js_strip(C)
                   , "','", js_strip(D), "']"
                   ]
                   || {A,B,C,D} <- SortedIndex ]
                ),
    %% io:format("bin ~s\n", [iolists_flatten(IndexStr)]),
    Data = ["var index = [", IndexStr, "];"],
    Path = jname(kf(dest,Conf), "erldocs_index.js"),
    ok = file:write_file(Path, Data).

%% Note: handles both erlref and cref types
render (App, Mod, Xml, Types, Conf) ->
    File = jname([kf(dest,Conf), App, Mod++".html"]),
    mkdir_p(dname(File) ++ "/"),

    Acc = [{ids,[]}, {list,ul}, {functions,[]}, {types,Types}],

    {[_Id, _List, {functions,_Funs}, {types,_Types}], NXml}
        = render(fun tr_erlref/2, Xml, Acc),

%%  XmlFuns = [{li, [], [{a, [{href,"#"++X}], [X]}]}
%%              || X <- lists:reverse(Funs) ],

    case kf(base,Conf) of %this is awkward
        "./" ->  %% Default value. In the default case
            Base = "../";  %% … files are up one level
        Other ->
            Base = Other
    end,

    Args = [ {base,    Base}
           , {search_base, "../"}
           , {title,   Mod ++ " (" ++ App ++ ") - "}
           , {content, xml_to_html(NXml)}
%%         , {funs,    xml_to_html({ul, [{id,"funs"}], XmlFuns})}
           , {ga,      kf(ga,Conf)}
           ],

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

get_funs (App, Mod, Xml) ->
    case lists:keyfind(funcs, 1, Xml) of
        false -> [];
        {funcs, [], Funs} ->
            F = fun (X) -> fun_stuff(App, Mod, X) end,
            lists:flatmap(F, Funs)
    end.

fun_stuff (App, Mod, {func, [], Children}) ->
    Summary = function_summary(Children),
    [ case Child of
          {name, [], Name} ->
              case make_name(Name) of
                  ignore -> ignore;
                  NName ->
                      {"fun", App, Mod++":"++NName, Summary}
              end;
          {name, [{name,Name}, {arity,Arity}], []} ->
              {"fun", App, Mod++":"++Name++"/"++Arity, Summary};
          {name, [{name,Name}, {arity,Arity}, {clause_i,"1"}], []} ->
              {"fun", App, Mod++":"++Name++"/"++Arity, Summary};
          _Else ->
              ignore
      end || Child <- Children ];
fun_stuff (_App, _Mod, _Else) ->
    [].

make_name (Name) ->
    Tmp = lists:flatten(Name),
    case string:chr(Tmp, $() of
        0 ->
            ignore;
        Pos ->
            {Name2, Rest2} = lists:split(Pos-1, Tmp),
            FName          = lists:last(string:tokens(Name2, ":")),
            Args           = string:substr(Rest2, 2, string:chr(Rest2, $))-2),
            NArgs          = length(string:tokens(Args, ",")),
            FName ++ "/" ++ integer_to_list(NArgs)
    end.

'add .html' ("#" ++ Rest) ->
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
    case re:run(FdashA, "^(.+)[/-]([0-9])+$", [{capture,all_but_first,list}]) of
        {match, [F,A]} -> F ++ "/" ++ A;
        nomatch -> FdashA
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
                        , {li, [], {code, [], [Child]}}
                        }
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
    case (SpecName == Name) and (ArityName == Arity) of
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
    { 'div'
    , [{id,ID}, {class,"category"}]
    , [ {h4, [], [{a, [{href,"#"++ID}], [Name]}]}
      , {hr, [], []}
      | Child
      ]
    }.


nname (Name, 0)   -> Name;
nname (Name, Acc) -> Name ++ "-" ++ integer_to_list(Acc).

inc_name (Name, List, Acc) ->
    case lists:member(nname(Name, Acc), List) of
        true  -> inc_name(Name, List, Acc+1);
        false -> nname(Name, Acc)
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
'keeper?' (X) when is_list(X) ->
    not lists:all(fun is_whitespace/1, X).

is_whitespace ($\s) -> true;
is_whitespace ($\n) -> true;
is_whitespace ($\t) -> true;
is_whitespace (_) -> false.

%% @doc
%% Rather basic xml to string converter, takes xml of the form
%% {tag, [{listof, "attributes"}], ["list of children"]}
%% into <tag listof="attributes">list of children</tag>
xml_to_html ({nbsp, [], Child}) ->
    ["&nbsp;", xml_to_html(Child)];
xml_to_html (Nbsp)
  when element(1, Nbsp) =:= nbsp ->
    "&nbsp;";
xml_to_html ({br, [], []}) ->
    "<br>\n";

xml_to_html ({Tag, Attr}) ->
    io_lib:format("<~ts ~ts>",
                  [atom_to_list(Tag), atos(Attr)]);
xml_to_html ({Tag, [], []}) ->
    io_lib:format("<~ts/>",
                  [atom_to_list(Tag)]);
xml_to_html ({Tag, Attr, []}) ->
    io_lib:format("<~ts ~ts/>",
                  [atom_to_list(Tag), atos(Attr)]);
xml_to_html ({Tag, [], Child}) ->
    io_lib:format("<~ts>~ts</~ts>",
                  [atom_to_list(Tag), xml_to_html(Child), atom_to_list(Tag)]);
xml_to_html ({Tag, Attr, Child}) ->
    io_lib:format("<~ts ~ts>~ts</~ts>",
                  [atom_to_list(Tag), atos(Attr), xml_to_html(Child), atom_to_list(Tag)]);
xml_to_html ([H | T]) ->
    [xml_to_html(H) | xml_to_html(T)];
xml_to_html (Else) ->
    htmlchar(Else).

atos ([])                      -> "";
atos (List) when is_list(List) -> iolists_join($\s, [atos(X) || X <- List]);
atos ({Name, Val})             -> [atom_to_list(Name), "=\"", Val, "\""].

htmlchar ($<) -> "&lt;";
htmlchar ($>) -> "&gt;";
htmlchar (Else) -> Else.


%% @doc
%% Parse XML file against OTP's DTD
-spec read_xml (file:name()) -> {atom(), _, _}.
read_xml (XmlFile) ->
    ?log("Reading XML for '~s'", [XmlFile]),
    DocgenDir = code:priv_dir(erl_docgen),
    Opts = [ {fetch_path, [ jname(DocgenDir, "dtd")
                          , jname(DocgenDir, "dtd_html_entities") ]}
           , {encoding, "latin1"}
           , {rules, ?ERLDOCS_XMERL_ETS_TABLE}
           ],
    case catch xmerl_scan:file(XmlFile, Opts) of
        {Xml, _Rest} ->
            xmerl_lib:simplify_element(Xml);
        Error ->
            ?log("Error in read_xml File ~p Erro ~p", [XmlFile,Error]),
            throw({error_in_read_xml, XmlFile, Error})
    end.

-spec kf (_, list()) -> _.
kf (Key, Conf) ->
    {Key, Val} = lists:keyfind(Key, 1, Conf),
    Val.

bname (Name) ->
    filename:basename(Name).
bname (Name, Ext) ->
    filename:basename(Name, Ext).

dname (Name) ->
    filename:dirname(Name).

jname (Dir1, Dir2) ->
    filename:join(Dir1, Dir2).
jname (ExplodedPath) ->
    filename:join(ExplodedPath).

%% @doc Tells whether this XML doc can be built with erldocs
is_buildable (erlref) -> true;
is_buildable (cref) -> true;
is_buildable (_Type) -> false.

%% @doc A black list for OTP
is_ignored ("kernel", "init") -> true;
is_ignored ("kernel", "zlib") -> true;
is_ignored ("kernel", "erlang") -> true;
is_ignored ("kernel", "erl_prim_loader") -> true;
is_ignored (_AppName, _Module) -> false.

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

mkdir_p (Path) ->
    case filelib:ensure_dir(Path) of
        ok -> ok;
        {error, eexist} -> ok
    end.

maybe_create_xmerl_table () ->
    case is_xmerl_table_created() of
        true -> ok;
        false ->
            Opts = [named_table, ordered_set, public],
            ?ERLDOCS_XMERL_ETS_TABLE = ets:new(?ERLDOCS_XMERL_ETS_TABLE, Opts)
    end.

is_xmerl_table_created () ->
    undefined /= ets:info(?ERLDOCS_XMERL_ETS_TABLE, compressed).

%% @doc
%% Ensure the table xmerl uses is deleted
maybe_delete_xmerl_table () ->
    case is_xmerl_table_created() of
        false -> ok;
        true -> ets:delete(?ERLDOCS_XMERL_ETS_TABLE)
    end.

cons (H, T) -> [H | T].


iolists_join (_, []) -> [];
iolists_join (_, [H]) -> [H];
iolists_join (Sep, [H|T]) ->
    [H] ++ [[Sep,E] || E <- T].

iolists_tl ([H|T])
  when is_list(H) ->
    [iolists_tl(H)|T];
iolists_tl ([_|T]) ->
    T.

%% iolists_flatten ([]) -> [];
%% iolists_flatten ([H|T]) ->
%%     iolists_flatten(H) ++ iolists_flatten(T);
%% iolists_flatten (NotAList) ->
%%     [NotAList].

%% End of Module.
