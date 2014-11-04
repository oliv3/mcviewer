-module(mcviewer_xml).

-include_lib("xmerl/include/xmerl.hrl").
-include("mcviewer.hrl").

-export([file/1]).


file(File) ->
    load_atoms(),
    {El, _Rest} = xmerl_scan:file(File),
    parse(El).


load_atoms() ->
    cio:dbg("~p~n", [?MC_ERR_UNV]),
    cio:dbg("~p~n", [?MC_ERR_UNC]),
    cio:dbg("~p~n", [?MC_ERR_IR]),
    cio:dbg("~p~n", [?MC_ERR_IW]),
    cio:dbg("~p~n", [?MC_LEAK_DEFINITIVELY]),
    cio:dbg("~p~n", [?MC_LEAK_POSSIBLY]).



%% {xmlElement,valgrindoutput,valgrindoutput,[],
%%              {xmlNamespace,[],[]},
%%              [],1,[],
%%              [{xmlText,[{valgrindoutput,1}],1,[],"\n\n",text},
%%               {xmlElement,protocolversion,protocolversion,[],
%%                {xmlNamespace,[],[]},
%%                [{valgrindoutput,1}],
%%                2,[],
%%                [{xmlText,
%%                  [{protocolversion,2},{valgrindoutput,1}],
%%                  1,[],"4",text}],
%%                [],".",undeclared},
%%               {xmlText,[{valgrindoutput,1}],3,[],"\n",text},
%%               {xmlElement,protocoltool,protocoltool,[],
%%                {xmlNamespace,[],[]},
%%                [{valgrindoutput,1}],
%%                4,[],
%%                [{xmlText,
%%                  [{protocoltool,4},{valgrindoutput,1}],
%%                  1,[],"memcheck",text}],
%%                [],undefined,undeclared},
%%               {xmlText,[{valgrindoutput,1}],5,[],"\n\n",text},
%%               {xmlElement,preamble,preamble,[],
%%                {xmlNamespace,[],[]},
%%                [{valgrindoutput,1}],
%%                6,[],
%%                [{xmlText,[{preamble,6},{valgrindoutput,1}],1,[],"\n  ",text},
%%                 {xmlElement,line,line,[],
%%                  {xmlNamespace,[],[]},
%%                  [{preamble,6},{valgrindoutput,1}],
%%                  2,[],
%%                  [{xmlText,
%%                    [{line,2},{preamble,6},{valgrindoutput,1}],
%%                    1,[],"Memcheck, a memory error detector",text}],
%%                  [],undefined,undeclared},
%%                 {xmlText,[{preamble,6},{valgrindoutput,1}],3,[],"\n  ",text},
%%                 {xmlElement,line,line,[],
%%                  {xmlNamespace,[],[]},
%%                  [{preamble,6},{valgrindoutput,1}],
%%                  4,[],

parse(#xmlElement{name = valgrindoutput, content = C}) ->
    %% cio:info("valgrindoutput: ~p~n", [C]),
    C1 = trim_whites(C),
    valgrindoutput(C1).

trim_whites(L) ->
    lists:filter(fun do_trim_whites/1, L).

do_trim_whites(#xmlText{value = V}) ->
    case string:tokens(V, [$ , $\r, $\n]) of
	[] ->
	    false;
	_ ->
	    true
    end;
do_trim_whites(_) ->
    true.

valgrindoutput(L) ->
    valgrindoutput(L, []).

valgrindoutput([], Acc) ->
    cio:ok("Done parsing~n", []),
    Res = lists:reverse(Acc),
    %% cio:ok("Result: ~p~n", [Res]),
    Res;
valgrindoutput([#xmlElement{name = protocolversion, content = [#xmlText{value = V}]} | Elems], Acc) ->
    cio:info("Protocol version: ~s~n", [V]),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = protocoltool, content = [#xmlText{value = V}]} | Elems], Acc) ->
    cio:info("Protocol tool: ~s~n", [V]),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = preamble} | Elems], Acc) ->
    cio:dbg("Skipping preamble~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = pid} | Elems], Acc) ->
    cio:dbg("Skipping pid~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = ppid} | Elems], Acc) ->
    cio:dbg("Skipping ppid~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = tool} | Elems], Acc) ->
    cio:info("Skipping tool~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = args} | Elems], Acc) ->
    cio:info("Skipping args~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = status} | Elems], Acc) ->
    cio:info("Skipping status~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlText{value = V} | Elems], Acc) ->
    cio:warn("xmlText: ~s~n", [V]),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = error, content = C} | Elems], Acc) ->
    C1 = trim_whites(C),
    cio:dbg("Found one error !!!~n", []),
    %% cio:fail("Found one error: ", []),
    Err = mc_error(C1),
    %% io:format("~p~n", [Err]),
    valgrindoutput(Elems, [Err | Acc]);
valgrindoutput([#xmlElement{name = errorcounts} | Elems], Acc) ->
    cio:info("Skipping error counts~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([#xmlElement{name = suppcounts} | Elems], Acc) ->
    cio:info("Skipping suppression counts~n", []),
    valgrindoutput(Elems, Acc);
valgrindoutput([Elem | Elems], Acc) ->
    cio:warn("==> ~p~n", [Elem]),
    valgrindoutput(Elems, Acc).


%% <frame>
%% <ip>0x40FA68</ip>
%% <obj>/home/olivier/git/biniou/biniou/src/lebiniou</obj>
%% <fn>export_RGBA_buffer</fn>
%% <dir>/home/olivier/git/biniou/biniou/src</dir>
%% <file>context_export.c</file>
%% <line>150</line>
%% </frame>

mc_error([#xmlElement{name = unique, content = [#xmlText{value = U0}]},
	  #xmlElement{name = tid, content = [#xmlText{value = Tid0}]},
	  #xmlElement{name = kind, content = [#xmlText{value = Kind0}]},
	  #xmlElement{name = what, content = [#xmlText{value = What}]},
	  #xmlElement{name = stack, content = Stack0}]) ->
    U = list_to_binary(U0),
    Tid = list_to_integer(Tid0),
    Kind = list_to_existing_atom(Kind0),
    Stack = trim_whites(Stack0),
    Frames = [mc_frame(F) || F <- Stack],
    #{
       unique => U,
       tid => Tid,
       kind => Kind,
       what => What,
       stack => Frames
     };
mc_error([#xmlElement{name = unique, content = [#xmlText{value = U0}]},
	  #xmlElement{name = tid, content = [#xmlText{value = Tid0}]},
	  #xmlElement{name = kind, content = [#xmlText{value = Kind0}]},
	  #xmlElement{name = what, content = [#xmlText{value = What}]},
	  #xmlElement{name = stack, content = Stack0},
	  #xmlElement{name = auxwhat, content = [#xmlText{value = AuxWhat0}]}]) ->
    U = list_to_binary(U0),
    Tid = list_to_integer(Tid0),
    Kind = list_to_existing_atom(Kind0),
    Stack = trim_whites(Stack0),
    Frames = [mc_frame(F) || F <- Stack],
    AuxWhat = list_to_binary(AuxWhat0),
    #{
       unique => U,
       tid => Tid,
       kind => Kind,
       what => What,
       stack => Frames,
       auxwhat => AuxWhat
     };
mc_error([#xmlElement{name = unique, content = [#xmlText{value = U0}]},
	  #xmlElement{name = tid, content = [#xmlText{value = Tid0}]},
	  #xmlElement{name = kind, content = [#xmlText{value = Kind0}]},
	  #xmlElement{name = xwhat, content = XWhat0},
	  #xmlElement{name = stack, content = Stack0}]) ->
    U = list_to_binary(U0),
    Tid = list_to_integer(Tid0),
    Kind = list_to_existing_atom(Kind0),
    XWhat1 = trim_whites(XWhat0),
    XWhat2 = mc_leak(XWhat1),
    Stack = trim_whites(Stack0),
    %% cio:info("Unique: ~p~n", [U]),
    %% cio:info("Tid: ~p~n", [Tid]),
    %% cio:info("Kind: ~p~n", [Kind]),
    %% cio:info("XWhat: ~p~n", [XWhat]),
    %% cio:info("Stack: ~p~n", [Stack]),
    Frames = [mc_frame(F) || F <- Stack],
    %% cio:info("Frames: ~p~n", [Frames]),
    #{
       unique => U,
       tid => Tid,
       kind => Kind,
       xwhat => XWhat2,
       stack => Frames
     };
%% TODO here handle memleaks etc
mc_error(El) ->
    Recs = [element(1, T) || T <- El],
    cio:info("Recs: ~p~n", [Recs]),
    Names = [element(2, T) || T <- El],
    cio:info("Names: ~p~n", [Names]),
    cio:warn("unhandled mc_error(~p)~n", [El]),
    undefined.


mc_frame(#xmlElement{name = frame, content = F0}) ->
    F = trim_whites(F0),
    %% cio:info("mc_frame: ~p~n", [F]),
    mc_frame(F);
mc_frame([
	  #xmlElement{name = ip, content = [#xmlText{value = IP0}]},
	  #xmlElement{name = obj, content = [#xmlText{value = Obj0}]},
	  #xmlElement{name = fn, content = [#xmlText{value = Fn0}]},
	  #xmlElement{name = dir, content = [#xmlText{value = Dir0}]},
	  #xmlElement{name = file, content = [#xmlText{value = File0}]},
	  #xmlElement{name = line, content = [#xmlText{value = Line0}]}
	 ]) ->
    %% cio:info("IP0: ~p~n", [IP0]),
    %% cio:info("Obj0: ~p~n", [Obj0]),
    %% cio:info("Fn0: ~p~n", [Fn0]),
    %% cio:info("Dir0: ~p~n", [Dir0]),
    %% cio:info("File0: ~p~n", [File0]),
    %% cio:info("Line0: ~p~n", [Line0]),
    #mc_frame{
       ip = IP0,
       obj = Obj0,
       fn = Fn0,
       dir = Dir0,
       file = File0,
       line = list_to_integer(Line0)
      };
mc_frame([
	  #xmlElement{name = ip, content = [#xmlText{value = IP0}]},
	  #xmlElement{name = obj, content = [#xmlText{value = Obj0}]},
	  #xmlElement{name = fn, content = [#xmlText{value = Fn0}]}
	 ]) ->
    %% cio:info("IP0: ~p~n", [IP0]),
    %% cio:info("Obj0: ~p~n", [Obj0]),
    %% cio:info("Fn0: ~p~n", [Fn0]),
    #mc_frame{
       ip = IP0,
       obj = Obj0,
       fn = Fn0
      };
mc_frame([
	  #xmlElement{name = ip, content = [#xmlText{value = IP0}]},
	  #xmlElement{name = obj, content = [#xmlText{value = Obj0}]}
	 ]) ->
    %% cio:info("IP0: ~p~n", [IP0]),
    %% cio:info("Obj0: ~p~n", [Obj0]),
    #mc_frame{
       ip = IP0,
       obj = Obj0
      };
mc_frame([
	  #xmlElement{name = ip, content = [#xmlText{value = IP0}]}
	 ]) ->
    %% cio:info("IP0: ~p~n", [IP0]),
    #mc_frame{
       ip = IP0
      };
mc_frame(X) ->
    cio:fail("mc_frame(~p)~n", [X]),
    undefined.


mc_leak([
	 #xmlElement{name = text, content = [#xmlText{value = Text0}]},
	 #xmlElement{name = leakedbytes, content = [#xmlText{value = LeakedBytes0}]},
	 #xmlElement{name = leakedblocks, content = [#xmlText{value = LeakedBlocks0}]}
	]) ->
    Text = list_to_binary(Text0),
    LeakedBytes = list_to_integer(LeakedBytes0),
    LeakedBlocks = list_to_integer(LeakedBlocks0),
    %% A leak
    #{
       text => Text,
       leakedbytes => LeakedBytes,
       leakedblocks => LeakedBlocks
     }.
