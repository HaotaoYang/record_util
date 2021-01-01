-module(record_util).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = record_util_pre_compile:init(State),
    {ok, State2} = record_util_post_compile:init(State1),
    {ok, State3} = record_util_pre_clean:init(State2),
    {ok, State4} = record_util_post_clean:init(State3),
    {ok, State4}.
