-module(truck).
-export([start/3]).

start(Id, Capacity, Coordinator) ->
    truck_loop(Id, Capacity, [], Coordinator).

truck_loop(Id, 0, Packages, Coordinator) ->
    io:format("Truck ~p is full with packages: ~p.~n", [Id, Packages]),
    Coordinator ! {truck_full, Id},
    truck_loop(Id, 5, [], Coordinator); % Reset truck capacity (simulate the replacement)
truck_loop(Id, Capacity, Packages, Coordinator) ->
    receive
        stop ->
            io:format("Truck ~p stopped.~n", [Id]),
            ok;
        {load_package, PkgId} ->
            io:format("Truck ~p loaded package ~p.~n", [Id, PkgId]),
            truck_loop(Id, Capacity - 1, [PkgId | Packages], Coordinator)
    end.
