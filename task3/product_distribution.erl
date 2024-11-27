-module(product_distribution).
-export([start/1]).

start(NumConveyors) ->
    spawn(fun() -> start_system(NumConveyors) end).

start_system(NumConveyors) ->
    ConveyorTruckPairs = lists:map(fun(TruckNum) ->
        TruckPid = spawn(fun() -> truck(TruckNum, self()) end),
        {TruckPid, spawn(fun() -> conveyor_belt(TruckNum, TruckPid) end)}
    end, lists:seq(1, NumConveyors)),

    lists:foreach(fun({TruckPid, ConveyorPid}) ->
        TruckPid ! {set_conveyor, ConveyorPid}
    end, ConveyorTruckPairs),

    io:format("System started with ~p conveyors and trucks.~n", [NumConveyors]).

conveyor_belt(BeltId, TruckPid) ->
    io:format("Belt ~p starting to send packages to Truck ~p~n", [self(), TruckPid]),
    send_packages(BeltId, TruckPid, 20),
    io:format("Belt ~p finished.~n", [BeltId]),
    exit(normal).

send_packages(_, _, 0) ->
    ok;

send_packages(BeltId, TruckPid, PackageCount) ->
    receive
        {truck_replacing, TruckPid} -> 
            io:format("Belt ~p paused. Truck ~p is being replaced.~n", [self(), TruckPid]),
            wait_for_replacement(TruckPid),
            io:format("Belt ~p resuming operation.~n", [self()])
    after 100 ->%keep going, no message
        ok
    end,

    send_packages(BeltId, TruckPid, PackageCount - 1),
    PackageSize = rand:uniform(4),
    io:format("Belt ~p sending package of size ~p to Truck ~p~n", [self(), PackageSize, TruckPid]),
    TruckPid ! {receive_package, PackageSize, self()},
    timer:sleep(1000).

wait_for_replacement(TruckPid) ->
    receive
        {replacement_done, TruckPid} -> 
            io:format("Truck ~p replacement done.~n", [TruckPid]);
        _ ->
            wait_for_replacement(TruckPid)
    end.

truck(TruckId, ConveyorPid) ->
    truck_loop(TruckId, ConveyorPid, 0, 10).

truck_loop(TruckId, ConveyorPid, CurrentLoad, Capacity) ->
    receive
        {receive_package, PackageSize, ConvPid} ->
            NewLoad = CurrentLoad + PackageSize,
            if
                NewLoad == Capacity -> %full
                    io:format("Truck ~p received package of size ~p. Truck is FULL!.~n", [self(), PackageSize]),
                    io:format("Truck ~p replacing itself.~n", [self()]),
                    %notify conveyor
                    ConvPid ! {truck_replacing, self()},
                    timer:sleep(3000),
                    %notify conveyor
                    ConvPid ! {replacement_done, self()},
                    truck_loop(TruckId, ConveyorPid, 0, Capacity);
                NewLoad > Capacity -> %overloaded
                    io:format("Truck ~p received package of size ~p. Truck is FULL!.~n", [self(), PackageSize]),
                    io:format("Truck ~p replacing itself.~n", [self()]),
                    %notify conveyor
                    ConvPid ! {truck_replacing, self()},
                    timer:sleep(3000),
                    %notify conveyor
                    ConvPid ! {replacement_done, self()},
                    truck_loop(TruckId, ConveyorPid, PackageSize, Capacity);%reset
                true -> %still good
                    io:format("Truck ~p received package of size ~p. Current load: ~p.~n", [self(), PackageSize, NewLoad]),
                    truck_loop(TruckId, ConveyorPid, NewLoad, Capacity)
            end
    end.
