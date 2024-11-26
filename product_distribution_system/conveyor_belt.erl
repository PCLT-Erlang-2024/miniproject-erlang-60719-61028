-module(conveyor_belt).
-export([start/2]).

start(Id, Coordinator) ->
    conveyor_loop(Id, Coordinator).

conveyor_loop(Id, Coordinator) ->
    receive
        stop ->
            io:format("Conveyor ~p stopped.~n", [Id]),
            ok;
        {new_package, PkgId} ->
            io:format("Conveyor ~p received package ~p.~n", [Id, PkgId]),
            Coordinator ! {conveyor_done, Id, PkgId},
            conveyor_loop(Id, Coordinator)
    end.
