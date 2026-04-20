%% @doc Minimal aggregate used by the `pair_dispatch_via_evoq' slice.
%%
%% Identical shape to the `bench_agg' in `evoq/benchmarks' — kept here
%% so the ecosystem bench can run without adding a non-bench dependency.
-module(pair_bench_agg).

-behaviour(evoq_aggregate).

-compile({no_auto_import, [apply/2]}).

-export([state_module/0, init/1, execute/2, apply/2]).

state_module() -> ?MODULE.

init(_AggregateId) ->
    {ok, #{count => 0}}.

execute(_State, Payload) when is_map(Payload) ->
    {ok, [#{event_type => <<"bench_appended_v1">>, data => Payload}]};
execute(_State, _Payload) ->
    {ok, [#{event_type => <<"bench_appended_v1">>, data => #{}}]}.

apply(#{count := C} = State, _Event) ->
    State#{count => C + 1}.
