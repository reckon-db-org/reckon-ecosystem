%% @doc Paired slice — full-stack evoq dispatch side.
%%
%% Mirrors `pair_storage_bare' and `pair_storage_via_gater' in event
%% shape and scenario params so paired runs produce apples-to-apples
%% overhead deltas against either.
-module(pair_dispatch_via_evoq).

-behaviour(reckon_bench_slice).

-export([describe/0, setup/1, run/2, teardown/2]).

-include_lib("evoq/include/evoq.hrl").

-define(AGG_PREFIX, <<"bench.pair_dispatch.">>).

describe() ->
    #{
        question => <<
            "Full-stack evoq dispatch path: dispatcher -> aggregate -> "
            "adapter -> gater -> reckon-db. Paired with pair_storage_*."
        >>,
        units   => #{},
        metrics => []
    }.

setup(Scenario) ->
    Size    = maps:get(event_size_bytes, Scenario, 256),
    _Store  = maps:get(store_id,         Scenario, bench_store),
    AggId   = fresh_aggregate_id(),
    Payload = binary:copy(<<$x>>, Size),
    #{
        aggregate_type => pair_bench_agg,
        aggregate_id   => AggId,
        data_bytes     => Payload,
        next_seq       => 0
    }.

run(#{aggregate_type := AggType,
      aggregate_id   := AggId,
      data_bytes     := Payload,
      next_seq       := Seq} = State, _Scenario) ->
    CmdPayload = #{seq => Seq, payload => Payload},
    Command = evoq_command:new(bench_append, AggType, AggId, CmdPayload),
    Opts = #{
        store_id         => bench_store,
        expected_version => -2
    },
    {ok, _Version, _Events} = evoq_dispatcher:dispatch(Command, Opts),
    {ok, State#{next_seq => Seq + 1}}.

teardown(_State, _Scenario) ->
    ok.

fresh_aggregate_id() ->
    <<?AGG_PREFIX/binary,
      (integer_to_binary(erlang:unique_integer([positive])))/binary>>.
