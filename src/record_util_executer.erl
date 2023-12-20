-module(record_util_executer).

-export([exec/2]).

-export([
    pre_compile/2,
    pre_clean/2,
    post_compile/2,
    post_clean/2
]).

-define(DEFAULT_HRL_DIRS, ["include"]).         %% 默认头文件读取路径，如果没有配置，则默认为该路径
-define(DEFAULT_DEST_DIR, "src/").              %% 默认生成文件的路径，如果没有配置，则生成到该路径
-define(DEFAULT_MODULE_NAME, "record_helper").  %% 默认模块名，如果没有配置，则生成到该模块中

-define(CHECK_VALUES(Values),
    case check_values(Values) of
        true -> skip;
        _ -> exit({error, "Invalid record_util config args"})
    end
    ).

-define(CHECK_VALUE(Value),
    case check_value(Value) of
        true -> skip;
        _ -> exit({error, "Invalid record_util config args"})
    end
    ).

%% ===================================================================
%% Public API
%% ===================================================================
-type provider_hook() :: pre_compile | pre_clean | post_compile | post_clean.

-spec exec(provider_hook(), rebar_state:t()) -> ok.
exec(ProviderHook, State) ->
    Apps = case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        AppInfo -> [AppInfo]
    end,
    lists:foreach(
        fun(App) ->
            ?MODULE:ProviderHook(App, State)
        end,
        Apps
    ),
    ok.

-spec pre_compile(rebar_app_info:t(), rebar_state:t()) -> ok.
pre_compile(AppInfo, _State) ->
    % %% 获取项目目录
    % AppDir = rebar_app_info:dir(AppInfo),
    % rebar_api:info("AppDir:~p", [AppDir]),
    % %% 获取依赖库的目录
    % DepsDir = rebar_dir:deps_dir(State),
    % rebar_api:info("DepsDir:~p", [DepsDir]),
    % %% 获取项目代码编译后的beam目录
    % AppOutDir = rebar_app_info:out_dir(AppInfo),
    % rebar_api:info("AppOutDir:~p", [AppOutDir]),

    %% 获取项目rebar.config的配置
    Opts = rebar_app_info:opts(AppInfo),
    {ok, RecordUtilOpts} = dict:find(record_util_opts, Opts),
    HrlDirs = proplists:get_value(hrl_dirs, RecordUtilOpts, ?DEFAULT_HRL_DIRS),
    ?CHECK_VALUES(HrlDirs),
    DestDir = proplists:get_value(dest_dir, RecordUtilOpts, ?DEFAULT_DEST_DIR),
    ?CHECK_VALUE(DestDir),
    ModuleName = proplists:get_value(module_name, RecordUtilOpts, ?DEFAULT_MODULE_NAME),
    ?CHECK_VALUE(ModuleName),
    HrlFiles = get_hrl_files(HrlDirs),
    RecordInfos = get_record_infos(HrlFiles),
    AllRecordInfos = get_record_fields(RecordInfos),
    gen_file(DestDir, ModuleName, HrlFiles, AllRecordInfos).

-spec pre_clean(rebar_app_info:t(), rebar_state:t()) -> ok.
pre_clean(AppInfo, _State) ->
    %% 获取项目rebar.config的配置
    Opts = rebar_app_info:opts(AppInfo),
    {ok, RecordUtilOpts} = dict:find(record_util_opts, Opts),
    DestDir = proplists:get_value(dest_dir, RecordUtilOpts, ?DEFAULT_DEST_DIR),
    ?CHECK_VALUE(DestDir),
    ModuleName = proplists:get_value(module_name, RecordUtilOpts, ?DEFAULT_MODULE_NAME),
    ?CHECK_VALUE(ModuleName),
    del_file(DestDir, ModuleName).
    
-spec post_compile(rebar_app_info:t(), rebar_state:t()) -> ok.
post_compile(_AppInfo, _State) ->
    rebar_api:info("record_util post_compile, but nothing to do...", []),
    ok.

-spec post_clean(rebar_app_info:t(), rebar_state:t()) -> ok.
post_clean(_AppInfo, _State) ->
    rebar_api:info("record_util post_clean, but nothing to do...", []),
    ok.

%% ===================================================================
%% Private API
%% ===================================================================
%% 检测配置参数合法性
check_values(Values) ->
    lists:all(fun(Value) -> check_value(Value) end, Values).

check_value(Value) when is_atom(Value) orelse is_list(Value) -> true;
check_value(_) -> false.

%% 获取制定目录内的所有头文件
get_hrl_files(HrlDirs) ->
    HrlFiles = lists:foldl(
        fun(Dir, TempFiles) ->
            case file:list_dir(Dir) of
                {ok, Files} ->
                    TempFiles ++ [filename:join([Dir, File]) || File <- Files, is_hrl_file(filename:join([Dir, File]))];
                _ -> TempFiles
            end
        end,
        [],
        HrlDirs
    ),
    lists:sort(HrlFiles).

%% 判断是否为头文件
is_hrl_file(FileName) when is_atom(FileName) ->
    NewFileName = atom_to_list(FileName),
    is_hrl_file(NewFileName);
is_hrl_file(FileName) when is_list(FileName) ->
    lists:suffix(".hrl", FileName);
% is_hrl_file(FileName) when is_list(FileName) ->
%     filename:extension(FileName) == ".hrl";
is_hrl_file(_) -> false.

%% 获取指定头文件的record信息
get_record_infos(HrlFiles) ->
    RecordInfos = lists:flatten([get_file_record_infos(HrlFile) || HrlFile <- HrlFiles]),
    lists:foldl(
        fun(RecordInfo, TempInfos) ->
            {RecordName, _} = RecordInfo,
            case lists:keyfind(RecordName, 1, TempInfos) of
                {RecordName, _} -> TempInfos;
                _ -> [RecordInfo | TempInfos]
            end
        end,
        [],
        RecordInfos
    ).

%% 获取当个文件内的所有record信息
get_file_record_infos(File) ->
    case epp:parse_file(File, [], []) of
        {ok, FileInfo} ->
            lists:foldl(
                fun(FieldInfos, TempRecords) ->
                    case FieldInfos of
                        {attribute, _, record, Record} -> [Record | TempRecords];
                        _ -> TempRecords
                    end
                end,
                [],
                FileInfo
            );
        _ -> []
    end.

%% 获取record的字段信息
get_record_fields(RecordInfos) ->
    lists:sort([
        begin
            {RecordName, FieldInfos} = Record,
            Fields = [filter_record_field(tuple_to_list(FieldInfo)) || FieldInfo <- FieldInfos],
            {RecordName, Fields}
        end || Record <- RecordInfos
    ]).

%% 过滤信息，只留下record字段
filter_record_field([record_field, _, {_, _, Field} | _]) -> Field;
filter_record_field([Info | RetInfos]) ->
    case is_tuple(Info) of
        true ->
            case tuple_to_list(Info) of
                [record_field, _, {_, _, Field} | _] -> Field;
                _ -> filter_record_field(RetInfos)
            end;
        _ -> filter_record_field(RetInfos)
    end.

%% 生成文件内容
gen_file(DestDir, ModuleName, HrlFiles, AllRecordInfos) ->
    NewName = case is_atom(ModuleName) of
        true -> atom_to_list(ModuleName);
        _ -> ModuleName
    end,
    Data =
    "%% -*- coding: utf-8 -*-\n" ++
    "%% Automatically generated, do not edit\n" ++
    "%% Generated by record_util\n" ++
    "\n" ++
    "-module(" ++ NewName ++ ").\n" ++
    "\n" ++
    ["-include(\"" ++ HrlFile ++ "\").\n" || HrlFile <- HrlFiles] ++
    "\n" ++
    "-export([fields_info/1, is_record/1, get_record/1, map_2_record/2, record_2_map/1]).\n" ++
    "\n" ++
    "%% get all fields name of records\n" ++
    [
        "fields_info(" ++ atom_to_list(RecordName) ++ ") -> " ++ io_lib:format("~p", [RecordFields]) ++ ";\n"
        || {RecordName, RecordFields} <- AllRecordInfos
    ] ++
    "fields_info(_Other) -> exit({error, \"Invalid Record Name\"}).\n" ++
    "\n" ++
    ["is_record(" ++ atom_to_list(RecordName) ++ ") -> true;\n" || {RecordName, _} <- AllRecordInfos] ++
    "is_record(_Other) -> false.\n" ++
    "\n" ++
    [
        "get_record(" ++ atom_to_list(RecordName) ++ ") -> #" ++ atom_to_list(RecordName) ++ "{};\n"
        || {RecordName, _} <- AllRecordInfos
    ] ++
    "get_record(_Other) -> undefined.\n" ++
    "\n" ++
    "map_2_record(Map, RecordName) when is_map(Map) ->\n" ++
    "    RecordFields = fields_info(RecordName),\n" ++
    "    ValueList = [maps:get(Field, Map) || Field <- RecordFields],\n" ++
    "    list_to_tuple([RecordName | ValueList]);\n" ++
    "map_2_record(_, _) -> undefined.\n" ++
    "\n" ++
    [
        begin
            "record_2_map(Record) when is_record(Record, " ++ atom_to_list(RecordName) ++ ") ->\n" ++
            "    #{\n" ++
            [
                 case Field =/= lists:last(RecordFields) of
                     true ->
                         "        " ++ atom_to_list(Field) ++ " => Record#" ++ atom_to_list(RecordName) ++ "." ++ atom_to_list(Field) ++ ",\n";
                     _ ->
                         "        " ++ atom_to_list(Field) ++ " => Record#" ++ atom_to_list(RecordName) ++ "." ++ atom_to_list(Field) ++ "\n"
                 end || Field <- RecordFields
            ] ++
            "    };\n"
        end || {RecordName, RecordFields} <- AllRecordInfos
    ] ++
    "record_2_map(_) -> undefined.",
    file:make_dir(DestDir),
    ok = file:write_file(filename:join([DestDir, ModuleName]) ++ ".erl", list_to_binary(Data)).

del_file(DestDir, ModuleName) ->
    FileName = filename:join([DestDir, ModuleName]) ++ ".erl",
    file:delete(FileName).
