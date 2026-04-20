%% @doc Join two paired slice results into a layer-overhead report.
%%
%% Invoked after two slices of a pair have each produced their JSON.
%% Produces a compact markdown + JSON summary of the delta.
-module(paired_join).

-export([join/3]).

-spec join(BasePath, ComparePath, OutPath) -> ok when
    BasePath    :: file:filename_all(),
    ComparePath :: file:filename_all(),
    OutPath     :: file:filename_all().
join(BasePath, ComparePath, OutPath) ->
    {ok, BaseJson} = file:read_file(BasePath),
    {ok, CmpJson}  = file:read_file(ComparePath),
    Base = jsone:decode(BaseJson, [{object_format, map}]),
    Cmp  = jsone:decode(CmpJson,  [{object_format, map}]),

    BaseThr = maps:get(<<"throughput_ops_sec">>, Base),
    CmpThr  = maps:get(<<"throughput_ops_sec">>, Cmp),
    BaseLat = maps:get(<<"latency_ns">>, Base),
    CmpLat  = maps:get(<<"latency_ns">>, Cmp),

    ThrDelta = case BaseThr of
                   +0.0 -> 0.0;
                   0    -> 0.0;
                   _    -> (CmpThr - BaseThr) / BaseThr * 100
               end,
    PctDelta = fun(Key) ->
        B = maps:get(Key, BaseLat, 0),
        C = maps:get(Key, CmpLat,  0),
        case B of
            0 -> 0.0;
            _ -> (C - B) / B * 100
        end
    end,

    Report = #{
        base          => Base,
        compare       => Cmp,
        deltas => #{
            throughput_pct => ThrDelta,
            p50_pct        => PctDelta(<<"p50">>),
            p99_pct        => PctDelta(<<"p99">>),
            p99_9_pct      => PctDelta(<<"p99_9">>)
        }
    },
    ok = file:write_file(OutPath,
                         jsone:encode(Report,
                                      [native_utf8, {indent, 2}, {space, 1}])),
    io:format("~n==================== PAIRED RESULT ====================~n"),
    io:format("base   : ~s / ~s  →  ~.2f ops/s  p99 ~.3f ms~n",
              [maps:get(<<"slice">>,    Base),
               maps:get(<<"scenario">>, Base),
               BaseThr,
               maps:get(<<"p99">>, BaseLat, 0) / 1_000_000]),
    io:format("compare: ~s / ~s  →  ~.2f ops/s  p99 ~.3f ms~n",
              [maps:get(<<"slice">>,    Cmp),
               maps:get(<<"scenario">>, Cmp),
               CmpThr,
               maps:get(<<"p99">>, CmpLat, 0) / 1_000_000]),
    ThrSign = if ThrDelta >= 0 -> "+"; true -> "" end,
    P99Delta = PctDelta(<<"p99">>),
    P99Sign  = if P99Delta >= 0 -> "+"; true -> "" end,
    io:format("delta  : throughput ~s~.1f%   p99 ~s~.1f%~n",
              [ThrSign, ThrDelta, P99Sign, P99Delta]),
    io:format("========================================================~n~n"),
    ok.
