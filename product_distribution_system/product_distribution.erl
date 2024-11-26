-module(product_distribution).
-export([start/1, loop/5]).

start(NumConveyors) ->
    TotalPackages = 100,
    Conveyors = spawn_conveyors(NumConveyors),
    PackageGenerator = spawn(package_generator, start, [Conveyors, TotalPackages]),
    Trucks = spawn_trucks(NumConveyors),

    %Main Loop
    loop(PackageGenerator, Conveyors, Trucks, 0, TotalPackages).



spawn_conveyors(NumConveyors) ->
    lists:map(fun(Id) -> spawn(conveyor_belt, start, [Id, self()]) end, lists:seq(1, NumConveyors)).

spawn_trucks(NumConveyors) ->
    lists:map(fun(Id) -> spawn(truck, start, [Id, 5, self()]) end, lists:seq(1, NumConveyors)).

loop(PkgGen, Conveyors, Trucks, ProcessedCount, TotalPackages) ->
    receive
        {conveyor_done, _ConveyorId, _Package} ->
            NewProcessedCount = ProcessedCount + 1,
            io:format("Processed packages: ~p/~p~n", [NewProcessedCount, TotalPackages]),
            
            if
                NewProcessedCount >= TotalPackages ->
                    io:format("All packages processed. Shutting down...~n"),
                    % Send message to stop to all processes
                    PkgGen ! stop,
                    lists:foreach(fun(Conveyor) -> Conveyor ! stop end, Conveyors),
                    lists:foreach(fun(Truck) -> Truck ! stop end, Trucks),
                    ok;
                true ->
                    loop(PkgGen, Conveyors, Trucks, NewProcessedCount, TotalPackages)
            end;

        {truck_full, TruckId} ->
            io:format("Truck ~p is full and replaced.~n", [TruckId]),
            NewTruck = spawn(truck, start, [TruckId, 5, self()]),
            % Replace full truck by key
            UpdatedTrucks = lists:keyreplace(TruckId, 1, Trucks, {TruckId, NewTruck}),
            loop(PkgGen, Conveyors, UpdatedTrucks, ProcessedCount, TotalPackages);

        stop ->
            io:format("System terminated.~n"),
            ok;

        _ ->
            % Default
            loop(PkgGen, Conveyors, Trucks, ProcessedCount, TotalPackages)
    end.
