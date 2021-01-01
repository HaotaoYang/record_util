-module(record_util_executer).

-export([exec/2]).

-export([
    pre_compile/2,
    pre_clean/2,
    post_compile/2,
    post_clean/2
]).

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
pre_compile(_AppInfo, _State) -> ok.

-spec pre_clean(rebar_app_info:t(), rebar_state:t()) -> ok.
pre_clean(_AppInfo, _State) -> ok.
    
-spec post_compile(rebar_app_info:t(), rebar_state:t()) -> ok.
post_compile(_AppInfo, _State) ->
    io:format("===> post_compile, but nothing~n"),
    ok.

-spec post_clean(rebar_app_info:t(), rebar_state:t()) -> ok.
post_clean(_AppInfo, _State) ->
    io:format("===> post_clean, but nothing~n"),
    ok.

%% ===================================================================
%% Private API
%% ===================================================================
