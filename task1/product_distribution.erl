-module(product_distribution).
-export([start/1]).

start(NumConveyors) ->
    spawn(fun() -> start_system(NumConveyors) end).

start_system(NumConveyors) ->
    %create trucks
    TruckIds = lists:map(fun(TruckNum) -> spawn(fun() -> truck(TruckNum) end) end, lists:seq(1, NumConveyors)),
    
    %create belt-truck pairs
    lists:foreach(fun({BeltNum, TruckPid}) ->
        spawn(fun() -> conveyor_belt(BeltNum, TruckPid) end)
    end, lists:zip(lists:seq(1, NumConveyors), TruckIds)),

    io:format("System started with ~p conveyors and trucks.~n", [NumConveyors]).


conveyor_belt(BeltId, TruckPid) ->
    io:format("Belt ~p starting to send packages to Truck. ~p~n", [self(), TruckPid]),
    send_packages(BeltId, TruckPid, 20),
    io:format("Belt ~p finished.~n", [self()]),
    exit(normal).%terminate after

send_packages(_, _, 0) ->
    ok;

send_packages(BeltId, TruckPid, PackageCount) ->
    PackageSize = 1,
    io:format("Belt ~p sending package to Truck ~p~n", [self(), TruckPid]),
    TruckPid ! {receive_package, PackageSize},
    timer:sleep(1000),%wait
    send_packages(BeltId, TruckPid, PackageCount - 1).

truck(TruckId) ->
    truck_loop(TruckId, 0, 10).

truck_loop(TruckId, CurrentLoad, Capacity) ->
    receive
        {receive_package, PackageSize} ->
            NewLoad = CurrentLoad + PackageSize,
            io:format("Truck ~p received package. Current load: ~p.~n", [self(), NewLoad]),
            if
                NewLoad >= Capacity ->
                    io:format("Truck ~p replaced!~n", [self()]),
                    truck_loop(TruckId, 0, Capacity);%just reset
                true ->
                    truck_loop(TruckId, NewLoad, Capacity)
            end
    end.
