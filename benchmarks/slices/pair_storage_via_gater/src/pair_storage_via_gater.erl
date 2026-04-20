%% @doc Paired slice — gater-API side.
%%
%% Mirrors `pair_storage_bare' in shape and scenario params so the
%% join step can compute a clean layer-overhead delta.
-module(pair_storage_via_gater).

-behaviour(reckon_bench_slice).

-export([describe/0, setup/1, run/2, teardown/2]).

-define(STREAM_PREFIX, <<"bench.pair_gater.">>).
-define(EVENT_TYPE,    <<"bench.appended_v1">>).

describe() ->
    #{
        question => <<
            "Gater-API append path: reckon_gater_api:append_events/3. "
            "Paired with pair_storage_bare for the gater-overhead delta."
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
    {ok, _V} = reckon_gater_api:append_events(Store, Stream, [Event]),
    {ok, State#{next_seq => Seq + 1}}.

teardown(#{store_id := Store, stream_id := Stream}, _Scenario) ->
    _ = reckon_gater_api:delete_stream(Store, Stream),
    ok.

fresh_stream_id() ->
    <<?STREAM_PREFIX/binary,
      (integer_to_binary(erlang:unique_integer([positive])))/binary>>.
