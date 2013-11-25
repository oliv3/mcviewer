-module(mcviewer).

-export([main/1]).


main(["file" | File]) ->
    application:start(mcviewer),

    timer:sleep(500),
    erlubi:start(),
    erlubi:vertex(),
    cio:on(),
    Errors = mcviewer_xml:file(File),
    cio:ok("Errors: ~p~n", [length(Errors)]),
    %% mcviewer_graph:clear(),
    mcviewer_graph:errors(Errors),
    timer:sleep(infinity),
    halt(0);
main(Args) ->
    cio:on(),
    cio:ok("Args: ~p~n", [Args]),
    halt(0).
