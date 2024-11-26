-module(package_generator).
-export([start/2]).

start(Conveyors, TotalPackages) ->
    generate_packages(1, Conveyors, TotalPackages).

generate_packages(PkgId, _Conveyors, TotalPackages) when PkgId > TotalPackages ->
    io:format("Package generator completed ~p packages.~n", [TotalPackages]),
    ok;
generate_packages(PkgId, Conveyors, TotalPackages) ->
    % Nao sei se deixamos round-robin
    [Conveyor | Rest] = Conveyors,
    Conveyor ! {new_package, PkgId},
    timer:sleep(100), % Simular delay
    generate_packages(PkgId + 1, Rest++ [Conveyor], TotalPackages).
