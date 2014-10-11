-module(mcviewer).
-define(VSN, '1.0').
-vsn(?VSN).

-export([main/1]).

-export([recv/0]).
-export([test/0]).

%% Internal export
-export([stdio/2]).


-define(TEST_FILE, "lebiniou-vg-dbg.xml").


split(File) ->
    {ok, Bin} = file:read_file(File),
    split_random(binary_to_list(Bin)).

-define(CHUNK_SIZE, 256).

split_random(L) ->
    random:seed(now()),
    split_random(L, []).
split_random(L, Acc) when length(L) > ?CHUNK_SIZE ->
    Rnd = random:uniform(?CHUNK_SIZE),
    {Chunk, Rest} = lists:split(Rnd, L),
    split_random(Rest, [Chunk | Acc]);
split_random(L, Acc) ->
    [list_to_binary(C) || C <- lists:reverse([L | Acc])].



wait() ->
    timer:sleep(1000).


valgrind(Command) ->
    "valgrind"
    %% ++ " -v"
	++ " --xml=yes"
	++ " --xml-fd=2"
	++ " --error-exitcode=66"
    %% ++ " --log-fd=2"
    %% ++ " --xml-socket=127.0.0.1:6666"
	++ " --tool=memcheck"
	++ " --leak-check=full"
	++ " --xml-user-comment=\"Powered by mcviewer v" ++ atom_to_list(?VSN) ++ "\""
    %% ++ " --trace-children=yes"
	++ " " ++ Command.


stream(XML, []) ->
    cio:dbg("Parsing done~n", []),
    XML;
stream(XML, [Chunk | Chunks]) ->
    NewXML = xml_stream:parse(XML, Chunk),
    %% cio:info("XML: ~p~n", [NewXML]),
    stream(NewXML, Chunks).

recv() ->
    receive
	Any ->
	    cio:info("got ~p~n", [Any]),
	    recv()

    after 1000 ->
	    ok
    end.


main(["document" | File]) ->
    application:start(mcviewer),

    wait(),
    erlubi:start(),
    erlubi:vertex(),
    cio:on(),
    Errors = mcviewer_xml:file(File),
    cio:ok("Errors: ~p~n", [length(Errors)]),
    %% mcviewer_graph:clear(),
    mcviewer_graph:errors(Errors),
    timer:sleep(infinity),
    halt(0);
main(["stream"]) ->
    main(["stream", ?TEST_FILE]);
main(["stream" | File]) ->
    cio:on(),
    %% cio:debug(true),
    application:start(mcviewer),
    xml_stream:start_link(), %% when ok => in sup tree
    mcviewer_leaks:start_link(), %% ditto

    wait(),
    erlubi:start(),
    erlubi:vertex(),

    {ok, Pid} = mcviewer_xml_stream:start_link(),
    XML = xml_stream:new(Pid),

    Chunks = split(File),
    stream(XML, Chunks),
    %% mcviewer_xml_stream:sync(),

    %% recv(),

    cio:ok("All:~n~s~n~p~n~s~n", [sep(), mcviewer_leaks:info(), sep()]),
    cio:ok("Leaks: ~p~n", [mcviewer_leaks:info(count)]),
    cio:ok("Blocks: ~p~n", [mcviewer_leaks:info(blocks)]),
    cio:ok("Definitively lost: ~p~n", [mcviewer_leaks:info(definitively_lost)]),
    cio:ok("Possibly lost: ~p~n", [mcviewer_leaks:info(possibly_lost)]),

    %% timer:sleep(infinity),
    halt(0);
main(["run" | Args]) ->
    cio:on(),

    %% application:start(mcviewer),
    application:start(exec),
    %% exec:start([debug, verbose]),

    erlubi:start_link(),
    mcviewer_graph:start_link(),
    xml_stream:start_link(),
    mcviewer_leaks:start_link(),

    %% wait(),
    cio:dbg("Args: ~p~n", [Args]),

    %% xml_stream:start_link(), %% when ok => in sup tree
    %% mcviewer_leaks:start_link(), %% ditto

    wait(),
    %% erlubi:start(),
    %% erlubi:vertex(),

    Command = string:join(Args, " "),
    cio:info("Command: ~p~n", [Command]),

    Command1 = valgrind(Command),
    cio:ok("Running: ~s~n", [Command1]),

    {ok, Pid} = mcviewer_xml_stream:start_link(self(), Command),
    XML = xml_stream:new(Pid),

    StdErr = spawn(?MODULE, stdio, [stderr, XML]), %% TODO spawn_link ?
    {ok, ExecPid, OsPid} = Res = exec:run(Command1, [{stderr, StdErr}, monitor]),
    cio:ok("Started wrapper: ~p~n", [Res]),
    StdErr ! {pid, OsPid},

    receive
	{finished, Pid, _Errors} ->
	    cio:ok("All:~n~s~n~p~n~s~n", [sep(), mcviewer_leaks:info(), sep()]),
	    cio:ok("Leaks: ~p~n", [mcviewer_leaks:info(count)]),
	    cio:ok("Blocks: ~p~n", [mcviewer_leaks:info(blocks)]),
	    cio:ok("Definitely lost: ~p~n", [mcviewer_leaks:info(definitely_lost)]),
	    cio:ok("Possibly lost: ~p~n", [mcviewer_leaks:info(possibly_lost)]),
	    %% cio:dbg("Errors: ~p~n", [Errors]),
	    %% mcviewer_graph:errors(Errors)
	    ok

	%% Other1 ->
	%%     cio:warn("OMG Other(1)= ~p~n", [Other1])
    end,

    exec:stop(OsPid),

    receive
	{'DOWN', _Ref, process, ExecPid, {exit_status, 9 = Status}} ->
	    cio:fail("Wrapper exited with status: ~p (SIGKILL)~n", [Status]),
	    halt(1);

	{'DOWN', _Ref, process, ExecPid, {exit_status, 16896 = Status}} ->
	    cio:ok("Wrapper exited with status: ~p which means that memcheck exited with errors, you get the point.~n", [Status]),
	    cio:ok("Since we used --error-exitcode=66, this is correct, period.~n", []);

	{'DOWN', _Ref, process, ExecPid, {exit_status, Status}} ->
	    cio:info("Wrapper exited with status: ~p~n", [Status]);

	{'DOWN', _Ref, process, ExecPid, normal} ->
	    cio:ok("Wrapper exited~n", []);

	Other2 ->
	    cio:warn("OMG Other= ~p~n", [Other2])
    end,

    %% recv(),

    %% cio:ok("Sleeping for eternity...~n", []),
    %% timer:sleep(infinity),
    halt(0);
main(["server"]) ->
    application:start(mcviewer),
    timer:sleep(infinity);
main([]) ->
    usage(escript:script_name()),
    halt(1);
main(Args) ->
    cio:on(),
    cio:ok("Args: ~p~n", [Args]),
    halt(0).


usage(ProgName) ->
    io:format("usage: ~s [command] [arguments]~n", [ProgName]),
    io:format("~n\tcommands:~n"),
    io:format("\t\trun <command>: Run the specified command through valgrind~n").


test() ->
    main(["run", "gnome-mines"]).


sep() ->
    lists:duplicate(80, [$=]).



stdio(Name, XML) ->
    cio:on(),
    cio:ok("~s: ~w starting as ~w~n", [?MODULE, Name, self()]),
    if
	Name =:= stdio ->
	    cio:debug(true);
	true -> ok
    end,

    receive
	{pid, Pid} ->
	    stdio(Name, XML, Pid)
    end.


stdio(Name, XML, OsPid) ->
    receive
	{Name, OsPid, Bin} ->
	    cio:dbg("got streamed ~w: ~p~n", [Bin, Name]),
	    NewXML = xml_stream:parse(XML, Bin),
	    stdio(Name, NewXML, OsPid);

	Any ->
	    cio:warn("~s: channel ~w (~w) received ~p~n", [?MODULE, Name, self(), Any]),
	    stdio(Name, XML, OsPid)
    end.
