-module(cio).
-compile(native).
-compile([export_all]).

-export([on/0, off/0, enable/1]).
-export([debug/1]).
-export([ok/2, warn/2, info/2, fail/2, dbg/2, conf/2]).
-export([elapsed/1]).

-define(DEBUG, '__cio_debug').

-define(SEABLUE, [27, "[0;34m"]).
-define(WHITE,   [27, "[0;37m"]).
-define(BWHITE,  [27, "[1;37m"]).
-define(BGREEN,  [27, "[1;32m"]).
-define(BRED,    [27, "[1;31m"]).
-define(BYELLOW, [27, "[1;33m"]).
-define(BPURPLE, [27, "[1;35m"]). %% FIXME blink (5) does not seem to work
-define(NONE,    [27, "[0m"]).
-define(INVERSE, "").


%% pebkac(Text) ->
%%     pebkac(Text, get(clr)).
%% pebkac(Text, false) ->
%%     io:format("~s", [Text]);
%% pebkac(Text, undefined) ->
%%     io:format("[o,\\ ] ~s", [Text]);
%% pebkac(Text, true) ->
%%     io:format("\r[~so,\\ ~s] ~s", [?BPURPLE, ?NONE, Text]);
%% pebkac(Fmt, Args) ->
%%     pebkac(io_lib:format(Fmt, Args)).


%% dump(Text) ->
%%     dump(Text, get(clr)).
%% dump(Text, false) ->
%%     io:format("~s", [Text]);
%% dump(Text, undefined) ->
%%     io:format("[dump] ~s", [Text]);
%% dump(Text, true) ->
%%     io:format("[~sdump~s] ~s", [?BPURPLE, ?NONE, Text]).

debug(true) ->
    put(?DEBUG, true);
debug(false) ->
    put(?DEBUG, false).


on() ->
    put(clr, true).
off() ->
    put(clr, false).

enable(true) ->
    on();
enable(false) ->
    off().


get_clr() ->
    case get(clr) of
	undefined ->
	    false;
	E ->
	    E
    end.


ok(Fmt, Args) ->
    ok(Fmt, Args, get_clr()).
ok(Fmt, Args, false) ->
    io:format("[ ok ] " ++ Fmt, Args);
ok(Fmt, Args, true) ->
    io:format("[~s ok ~s] " ++ Fmt, [?BGREEN, ?NONE] ++ Args).


warn(Fmt, Args) ->
    warn(Fmt, Args, get_clr()).
warn(Fmt, Args, false) ->
    io:format("[warn] " ++ Fmt, Args);
warn(Fmt, Args, true) ->
    io:format("[~swarn~s] " ++ Fmt, [?BYELLOW, ?NONE] ++ Args).


info(Fmt, Args) ->
    info(Fmt, Args, get_clr()).
info(Fmt, Args, false) ->
    io:format("[info] " ++ Fmt, Args);
info(Fmt, Args, true) ->
    io:format("[~sinfo~s] " ++ Fmt, [?SEABLUE, ?NONE] ++ Args).


fail(Fmt, Args) ->
    fail(Fmt, Args, get_clr()).
fail(Fmt, Args, false) ->
    io:format("[FAIL] " ++ Fmt, Args);
fail(Fmt, Args, true) ->
    io:format("[~sFAIL~s] " ++ Fmt, [?BRED, ?NONE] ++ Args).


dbg(Fmt, Args) ->
    dbg(Fmt, Args, get_clr(), get(?DEBUG)).
dbg(Fmt, Args, Clr, undefined) ->
    dbg(Fmt, Args, Clr, false);
dbg(_Fmt, _Args, _Clr, false) ->
    ok;
dbg(Fmt, Args, false, true) ->
    io:format("[++++] " ++ Fmt, Args);
dbg(Fmt, Args, true, true) ->
    io:format("[~s++++~s] " ++ Fmt, [?BWHITE, ?NONE] ++ Args);
dbg(Fmt, Args, Clr, Debug) ->
    fail("bad dbg: ~p ~p ~p ~p~n", [Fmt, Args, Clr, Debug]).



conf(Fmt, Args) ->
    conf(Fmt, Args, get_clr()).
conf(Fmt, Args, false) ->
    io:format("[ ** ] " ++ Fmt, Args);
conf(Fmt, Args, true) ->
    io:format("\r[~s ** ~s] " ++ Fmt, [?WHITE, ?NONE] ++ Args).


elapsed(StartTime) ->
    E = timer:now_diff(now(), StartTime),
    elapsed(E, get_clr()).

elapsed(Time, true) ->
    info("...in ~w µs~n", [Time]);
elapsed(Time, false) ->
    info("...in ~w micro-seconds~n", [Time]).
