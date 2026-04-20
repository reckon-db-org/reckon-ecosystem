%% @doc Paired slice — direct storage side.
%%
%% One of two slices run back-to-back on the same VM / same store so
%% layer-overhead deltas are directly comparable. Must match the
%% `pair_storage_via_gater' slice in event shape and scenario params
%% for the join step to work.
-module(pair_storage_bare).

-behaviour(reckon_bench_slice).

-export([describe/0, setup/1, run/2, teardown/2]).

-define(STREAM_PREFIX, <<"bench.pair_bare.">>).
-define(EVENT_TYPE,    <<"bench.appended_v1">>).
-define(ANY_VERSION, -2).

describe() ->
    #{
        question => <<
            "Bare-storage append path: reckon_db_streams:append/4. "
            "Paired with pair_storage_via_gater for the gater-overhead delta."
        >>,
        units   => #{},
        metrics => []
    }.

setup(Scenario) ->
    Size     = maps:get(event_size_bytes, Scenario, 256),
    StoreId  = maps:get(store_id,         Scenario, bench_store),
    StreamId = fresh_stream_id(),
    Payload  = binary:copy(<<$x>>, Size),
    #{store_id => StoreId,
      stream_id => StreamId,
      data_bytes => Payload,
      next_seq => 0}.

run(#{store_id := Store, stream_id := Stream, data_bytes := Payload,
      next_seq := Seq} = State, _Scenario) ->
    Event = #{event_type => ?EVENT_TYPE,
              data => #{seq => Seq, payload => Payload}},
    {ok, _V} = reckon_db_streams:append(Store, Stream, ?ANY_VERSION, [Event]),
    {ok, State#{next_seq => Seq + 1}}.

teardown(#{store_id := Store, stream_id := Stream}, _Scenario) ->
    _ = reckon_db_streams:delete(Store, Stream),
    ok.

fresh_stream_id() ->
    <<?STREAM_PREFIX/binary,
      (integer_to_binary(erlang:unique_integer([positive])))/binary>>.
