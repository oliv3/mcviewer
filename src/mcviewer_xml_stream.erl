-module(mcviewer_xml_stream).

-behaviour(gen_fsm).
-include_lib("p1_xml/include/xml.hrl").
-include("mcviewer_xml_stream.hrl").
-include("mcviewer.hrl").

%% API
-export([start/2, start_link/2]).
%% -export([sync/0]). %% TOTALLY USELESS, TO BE REMOVED
%% -export([errors/0]).

%% FSM
-export([waiting/2, valgrindoutput/2, starting/2, running/2, exiting/2]).
%% -export([waiting/3]).

%% Valgrind error callbacks
-export([mc_error/1]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {parent, tool, pid, ppid, errors = [], error_cb, user_comment, command}).
-record(leak, {bytes = 0, blocks = 0}). %% Let us be optimilistic :)

%%%===================================================================
%%% API
%%%===================================================================
%% sync() ->
%%     gen_fsm:sync_send_event(?SERVER, sync).

%% errors() ->
%%     gen_fsm:sync_send_all_state_event(?SERVER, errors).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Parent, Command) ->
    gen_fsm:start({local, ?SERVER}, ?MODULE, [Parent, Command], []).

start_link(Parent, Command) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Parent, Command], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Parent, Command]) ->
    cio:on(),
    cio:debug(true),

    %% Populate atoms table
    %% Listed in the order they appear in memcheck/mc_errors.c
    %% Do not comment or remove those lines !
    cio:dbg("Atom: ~p~n", ['CoreMemError']),
    cio:dbg("Atom: ~p~n", ['UninitValue']),
    cio:dbg("Atom: ~p~n", ['UninitCondition']),
    cio:dbg("Atom: ~p~n", ['SyscallParam']),
    cio:dbg("Atom: ~p~n", ['ClientCheck']),
    cio:dbg("Atom: ~p~n", ['InvalidFree']),
    cio:dbg("Atom: ~p~n", ['MismatchedFree']),
    cio:dbg("Atom: ~p~n", ['InvalidRead']),
    cio:dbg("Atom: ~p~n", ['InvalidWrite']),
    cio:dbg("Atom: ~p~n", ['InvalidJump']),
    cio:dbg("Atom: ~p~n", ['Overlap']),
    cio:dbg("Atom: ~p~n", ['InvalidMemPool']),

    {ok, waiting, #state{parent = Parent, command = Command}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
waiting({xmlstreamerror, _Error}, #state{command = Command} = State) ->
    cio:fail("XML stream error: ~p~n", [_Error]),
    cio:info("Relax, maybe the command ~p just failed, please re-check :)~n", [Command]),
    timer:sleep(3000), halt(1),
    {stop, xmlstreamerror, State};

waiting({xmlstreamstart, ?VALGRIND_OUTPUT, []}, State) ->
    cio:ok("~s tag detected~n", [?VALGRIND_OUTPUT]),
    {next_state, valgrindoutput, State}.

valgrindoutput({xmlstreamelement, #xmlel{name = ?VALGRIND_PROTOCOL_VERSION,
					 children = [{xmlcdata, BinVsn}]}}, %% <<"4">>
	       State) ->
    Vsn = list_to_integer(binary_to_list(BinVsn)),
    case Vsn of
	4 ->
	    cio:ok("Protocol version 4~n", []),
	    {next_state, valgrindoutput, State};

	Vsn ->
	    cio:fail("Unhandled protocol version ~p~n", [Vsn]),
	    {stop, unknown_protocol, State}
    end;	    
valgrindoutput({xmlstreamelement, #xmlel{name = ?VALGRIND_PROTOCOL_TOOL,
					 children = [{xmlcdata, BinTool}]}}, State) ->
    case BinTool of
	<<"memcheck">> ->
	    cio:ok("Protocol tool: memcheck~n", []),
	    {next_state, valgrindoutput, State#state{tool = memcheck}};
	BinTool ->
	    cio:fail("Unhandled tool (~s)~n", [BinTool]),
	    {stop, unknown_tool, State}
    end;
valgrindoutput({xmlstreamelement, #xmlel{name = ?VALGRIND_PREAMBLE, children = C}}, State) ->
    preamble(C),
    {next_state, valgrindoutput, State};
valgrindoutput({xmlstreamelement, #xmlel{name = ?VALGRIND_PID, children = [{xmlcdata, BinPid}]}}, State) ->
    Pid = binary_to_integer(BinPid),
    cio:dbg("Pid:  ~w~n", [Pid]),
    {next_state, valgrindoutput, State#state{pid = Pid}};
valgrindoutput({xmlstreamelement, #xmlel{name = ?VALGRIND_PPID, children = [{xmlcdata, BinPPid}]}}, State) ->
    PPid = binary_to_integer(BinPPid),
    cio:dbg("PPid: ~w~n", [PPid]),
    {next_state, valgrindoutput, State#state{ppid = PPid}};
valgrindoutput({xmlstreamelement, #xmlel{name = ?VALGRIND_TOOL, children = [{xmlcdata, ?VALGRIND_TOOL_MEMCHECK}]}}, State) ->
    cio:ok("Error callback: mc_error/1~n", []),
    {next_state, starting, State#state{error_cb = mc_error}};
valgrindoutput(_Event, State) ->
    cio:warn("valgrindoutput/2( ~p )~n", [_Event]),
    {next_state, valgrindoutput, State}.

starting({xmlstreamelement, #xmlel{name = ?VALGRIND_ARGS, children = C0}}, State) ->
    %% cio:warn("starting/2(~p)~n", [_Event]),
    C1 = remove_whitespaces(C0),
    %% cio:dbg("TBD: valgrind args ~p~n", [C1]),
    parse_args(C1),
    {next_state, starting, State};
starting({xmlstreamelement, #xmlel{name = ?VALGRIND_STATUS,
				   children = C0
				   %%    [{xmlcdata,<<"\n  ">>},
				   %% {xmlel,<<"state">>,[],
				   %%  [{xmlcdata,<<"RUNNING">>}]},
                                %% {xmlcdata,<<"\n  ">>},
                                %% {xmlel,<<"time">>,[],
                                %%  [{xmlcdata,<<"00:00:00:00.065 ">>}]},
                                %% {xmlcdata,<<"\n">>}]}
				  }} = _Event, State) ->
    C1 = remove_whitespaces(C0),
    parse_status(C1),
    %% cio:dbg("TBD: parse status ~p~n", [C1]),
    cio:info("Got status, assuming we're ok...~n", []),
    {next_state, running, State};
starting({xmlstreamelement, #xmlel{name = ?VALGRIND_USER_COMMENT, children = [{xmlcdata, UserComment}]}}, State) ->
    cio:info("User comment: ~s~n", [UserComment]),
    {next_state, starting, State#state{user_comment = UserComment}};
starting(_Event, State) ->
    cio:warn("starting/2(~p)~n", [_Event]),
    {next_state, starting, State}.


running({xmlstreamelement, #xmlel{name = ?VALGRIND_ERROR, children = C0}}, #state{errors = Errors, error_cb = CB} = State) ->
    %% [{xmlcdata,<<"\n  ">>},
    %%  {xmlel,<<"unique">>,[],[{xmlcdata,<<"0x2">>}]},
    %%  {xmlcdata,<<"\n  ">>},
    %%  {xmlel,<<"tid">>,[],[{xmlcdata,<<"1">>}]},
    %%  {xmlcdata,<<"\n  ">>},
    %%  {xmlel,<<"kind">>,[],
    %%   [{xmlcdata,<<"UninitValue">>}]},
    C1 = remove_whitespaces(C0),
    %% cio:dbg("running/2: error(~n~p~n)~n", [C1]),
    Error = ?MODULE:CB(C1),
    %% cio:dbg("Error: ~p~n", [Error]),
    cio:warn("#memcheck Access ERROR (~w)~n", [Error#mc_error.kind]),
    mcviewer_graph:error(Error),
    {next_state, running, State#state{errors = [Error | Errors]}};
running({xmlstreamelement, #xmlel{name = ?VALGRIND_STATUS,
				  children = C0
				  %%    [{xmlcdata,<<"\n  ">>},
				  %% {xmlel,<<"state">>,[],
				  %%  [{xmlcdata,<<"RUNNING">>}]},
				  %% {xmlcdata,<<"\n  ">>},
				  %% {xmlel,<<"time">>,[],
				  %%  [{xmlcdata,<<"00:00:00:00.065 ">>}]},
				  %% {xmlcdata,<<"\n">>}]}
				 }} = _Event, State) ->
    C1 = remove_whitespaces(C0),
    parse_status(C1),
    %% cio:dbg("TBD: parse status ~p~n", [C1]),
    cio:info("Got status, assuming we're [EXITING] ok...~n", []),
    {next_state, exiting, State};
running(_Event, State) ->
    cio:warn("running/2(~p)~n", [_Event]),
    {next_state, running, State}.


exiting({xmlstreamelement, #xmlel{name = ?VALGRIND_ERROR, children = C0}}, #state{errors = Errors, error_cb = CB} = State) ->
    C1 = remove_whitespaces(C0),
    %% cio:dbg("exiting/2: CB= ~p error(~n~p~n)~n", [CB, C1]),
    #mc_error{kind = _Kind} = Error = ?MODULE:CB(C1),
    %% cio:ok("Error: ~p~n", [Error]),
    cio:warn("#memcheck Leak ERROR (~w)~n", [_Kind]),
    mcviewer_graph:error(Error),
    {next_state, exiting, State#state{errors = [Error | Errors]}};

%% exiting/2({xmlstreamelement,
%%                              {xmlel,<<"errorcounts">>,[],
%%                               [{xmlcdata,<<"\n  ">>},
%%                                {xmlel,<<"pair">>,[],
%%                                 [{xmlcdata,<<"\n    ">>},
%%                                  {xmlel,<<"count">>,[],
%%                                   [{xmlcdata,<<"45429669">>}]},
%%                                  {xmlcdata,<<"\n    ">>},
%%                                  {xmlel,<<"unique">>,[],
%%                                   [{xmlcdata,<<"0x2">>}]},
%%                                  {xmlcdata,<<"\n  ">>}]},
%%                                {xmlcdata,<<"\n">>}]}})

exiting({xmlstreamelement, #xmlel{name = ?VALGRIND_ERRORS_COUNT,
				  children = C0}}, State) ->
    C1 = remove_whitespaces(C0),
    case C1 of
	[] ->
	    ok;

	C1 ->
	    cio:warn("Errors:~n"),
	    parse_errors(lists:reverse(C1))
    end,
    {next_state, exiting, State};
exiting({xmlstreamelement, #xmlel{name = ?VALGRIND_SUPPRESSIONS_COUNT,
				  children = C0}}, State) ->
    C1 = remove_whitespaces(C0),
    case C1 of
	[] ->
	    ok;

	C1 ->
	    cio:info("Suppressions:~n"),
	    parse_suppressions(C1)
    end,
    {next_state, exiting, State};
exiting({xmlstreamend, ?VALGRIND_OUTPUT}, #state{parent = P, errors = Errors} = State) ->
    cio:ok("All done, FSM exiting~n", []),
    dump_state(State),
    P ! {finished, self(), lists:reverse(Errors)}, %% return the errors in order
    {stop, normal, State};

exiting(_Event, State) ->
    cio:warn("exiting/2(~p)~n", [_Event]),
    {next_state, exiting, State}.

state_name(_Event, State) ->
    cio:warn("state_name/2(~p)~n", [_Event]),
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
%% waiting(sync = _Event, _From, State) ->
%%     cio:ok("~s: synced(~p)~n", [?MODULE, _Event]),
%%     Reply = ok,
%%     {reply, Reply, waiting, State}.


%% waiting(_Event, _From, State) ->
%%     cio:ok("~s: waiting/3(~p)~n", [?MODULE, _Event]),
%%     Reply = ok,
%%     {reply, Reply, waiting, State}.

state_name(_Event, _From, State) ->
    cio:warn("state_name/3(~p)~n", [_Event]),
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event({xmlstreamcdata, <<"\n">> = _Event}, StateName, State) ->
    %% cio:warn("Empty line ~p~n", [_Event]),
    {next_state, StateName, State};

handle_event({xmlstreamcdata, StdErr}, running, State) ->
    cio:warn("(stderr) ~s~n", [StdErr]),
    {next_state, running, State};

handle_event(_Event, StateName, State) ->
    cio:warn("event(~p)~n", [_Event]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    cio:warn("sync_event(~p)~n", [_Event]),
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    cio:warn("info(~p)~n", [_Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
binary_to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin)).
binary_to_existing_atom(Bin) when is_binary(Bin) ->
    list_to_existing_atom(binary_to_list(Bin)).


preamble([]) ->
    ok;
preamble([#xmlel{name = <<"line">>, children = [{xmlcdata, L}]} | Rest]) ->
    cio:info("~s~n", [L]),
    preamble(Rest);
preamble([_ | Rest]) ->
    preamble(Rest).


remove_whitespaces(List) ->
    lists:filter(fun({xmlcdata, <<$\n, _/binary>>}) ->
			 false;
		    (_) ->
			 true
		 end, List).


mc_error([#xmlel{name = <<"unique">>,
		 children = [{xmlcdata, UniqueBin}]}, %% <<"0x2">>
	  #xmlel{name = <<"tid">>,
		 children = [{xmlcdata, TidBin}]}, %% <<"1">>
	  #xmlel{name = <<"kind">>,
		 children = [{xmlcdata, KindBin}]}, %% <<"UninitValue">>
	  #xmlel{name = <<"what">>,
		 children = [{xmlcdata, WhatBin}]}, %% <<"Use of uninitialised value of size 8">>
	  #xmlel{name = <<"stack">>,
		 children = Stack0}]) ->
    Stack = stack(remove_whitespaces(Stack0)),
    #mc_error{unique = UniqueBin,
	      tid = binary_to_integer(TidBin),
	      kind = binary_to_existing_atom(KindBin),
	      what = WhatBin,
	      stack = Stack};
mc_error([#xmlel{name = <<"unique">>,
		 children = [{xmlcdata, UniqueBin}]}, %% <<"0x2">>
	  #xmlel{name = <<"tid">>,
		 children = [{xmlcdata, TidBin}]}, %% <<"1">>
	  #xmlel{name = <<"kind">>,
		 children = [{xmlcdata, KindBin}]}, %% <<"UninitValue">>
	  #xmlel{name = <<"what">>,
		 children = [{xmlcdata, WhatBin}]}, %% <<"Use of uninitialised value of size 8">>
	  #xmlel{name = <<"stack">>,
		 children = Stack0},
	  #xmlel{name = <<"auxwhat">>,
		 children = [{xmlcdata, AuxWhatBin}]}, %% <<"Use of uninitialised value of size 8">>
	  #xmlel{name = <<"stack">>,
		 children = AuxWhatStack0}]) ->
    Stack = stack(remove_whitespaces(Stack0)),
    AuxWhatStack = stack(remove_whitespaces(AuxWhatStack0)),
    #mc_error{unique = UniqueBin,
	      tid = binary_to_integer(TidBin),
	      kind = binary_to_existing_atom(KindBin),
	      what = WhatBin,
	      stack = Stack,
	      auxwhat = AuxWhatBin,
	      auxstack = AuxWhatStack};
mc_error([#xmlel{name = <<"unique">>,
		 children = [{xmlcdata, UniqueBin}]}, %% <<"0x2">>
	  #xmlel{name = <<"tid">>,
		 children = [{xmlcdata, TidBin}]}, %% <<"1">>
	  #xmlel{name = <<"kind">>,
		 children = [{xmlcdata, KindBin}]}, %% <<"UninitValue">>
	  #xmlel{name = <<"what">>,
		 children = [{xmlcdata, WhatBin}]}, %% <<"Use of uninitialised value of size 8">>
	  #xmlel{name = <<"stack">>,
		 children = Stack0}
	  | Rest]) ->
    Stack = stack(remove_whitespaces(Stack0)),
    Error = #mc_error{unique = UniqueBin,
		      tid = binary_to_integer(TidBin),
		      kind = binary_to_existing_atom(KindBin),
		      what = WhatBin,
		      stack = Stack},
    update_error(Error, Rest);
mc_error([#xmlel{name = <<"unique">>,
		 children = [{xmlcdata, UniqueBin}]},
	  #xmlel{name = <<"tid">>,
		 children = [{xmlcdata, TidBin}]},
	  #xmlel{name = <<"kind">>,
		 children = [{xmlcdata, KindBin}]},
	  #xmlel{name = <<"xwhat">>,
		 children = XWhat0},
	  #xmlel{name = <<"stack">>,
		 children = Stack0}
	  | Rest]) ->
    Kind = binary_to_atom(KindBin),
    XWhat = xwhat(remove_whitespaces(XWhat0)),
    Stack = stack(remove_whitespaces(Stack0)),
    mcviewer_leaks:add_leak(Kind, XWhat#leak.bytes, XWhat#leak.blocks),
    Error = #mc_error{unique = UniqueBin,
		      tid = binary_to_integer(TidBin),
		      kind = binary_to_existing_atom(KindBin),
		      xwhat = XWhat,
		      auxstack = Stack},
    %% update_error_aux(Error, Rest); %% Or use existing update_error/2 ?
    Error;
mc_error(Other) ->
    cio:warn("Unimplemented error: ~p~n", [Other]),
    undefined.


update_error(#mc_error{auxwhat = undefined} = Error, [#xmlel{name = <<"auxwhat">>,
							     children = [{xmlcdata, AuxWhatBin0}]},
						      #xmlel{name = <<"auxwhat">>,
							     children = [{xmlcdata, AuxWhatBin1}]}]) ->
    AuxWhat = iolist_to_binary([AuxWhatBin0, <<", ">>, AuxWhatBin1]),
    Error#mc_error{auxwhat = AuxWhat};
update_error(Error, Rest) ->
    cio:dbg("NEW parser: Rest= ~p~n", [Rest]),
    Error.


stack(Frames) ->
    stack(Frames, []).

stack([], Acc) ->
    lists:reverse(Acc);
stack([#xmlel{name = <<"frame">>,
	      children = C0} | Tail], Acc) ->
    Frame = frame(remove_whitespaces(C0)),
    stack(Tail, [Frame | Acc]).

frame([#xmlel{name = <<"ip">>,
	      children = [{xmlcdata, IPBin}]}, %% <<"0x40FA68">>
       #xmlel{name = <<"obj">>,
	      children = [{xmlcdata, ObjBin}]}, %% <<"/home/olivier/git/biniou/biniou/src/lebiniou">>
       #xmlel{name = <<"fn">>,
	      children = [{xmlcdata, FnBin}]}, %% <<"export_RGBA_buffer">>
       #xmlel{name = <<"dir">>,
	      children = [{xmlcdata, DirBin}]}, %% <<"/home/olivier/git/biniou/biniou/src">>
       #xmlel{name = <<"file">>,
	      children = [{xmlcdata, FileBin}]}, %% <<"context_export.c">>
       #xmlel{name = <<"line">>,
	      children = [{xmlcdata, LineBin}]} %% <<"150">>
      ]) ->
    #mc_frame{ip = IPBin,
	      obj = ObjBin,
	      fn = binary_to_atom(FnBin),
	      dir = DirBin,
	      file = FileBin,
	      line = binary_to_integer(LineBin)};
frame([#xmlel{name = <<"ip">>,
	      children = [{xmlcdata, IPBin}]},
       #xmlel{name = <<"obj">>,
	      children = [{xmlcdata, ObjBin}]},
       #xmlel{name = <<"fn">>,
	      children = [{xmlcdata, FnBin}]}
      ]) ->
    #mc_frame{ip = IPBin,
	      obj = ObjBin,
	      fn = binary_to_atom(FnBin)};
frame([#xmlel{name = <<"ip">>,
	      children = [{xmlcdata, IPBin}]},
       #xmlel{name = <<"obj">>,
	      children = [{xmlcdata, ObjBin}]}
      ]) ->
    #mc_frame{ip = IPBin,
	      obj = ObjBin};
frame([#xmlel{name = <<"ip">>,
	      children = [{xmlcdata, IPBin}]}]) ->
    #mc_frame{ip = IPBin};
frame(_Other) ->
    cio:fail("TODO: frame(~p)~n", [_Other]),
    halt(1),
    frame_tbd.


xwhat([#xmlel{name = <<"text">>,
	      children = [{xmlcdata, TextBin}]},
       #xmlel{name = <<"leakedbytes">>,
	      children = [{xmlcdata, LeakedBytesBin}]},
       #xmlel{name = <<"leakedblocks">>,
	      children = [{xmlcdata, LeakedBlocksBin}]}
      ]) ->
    LeakedBytes = binary_to_integer(LeakedBytesBin),
    LeakedBlocks = binary_to_integer(LeakedBlocksBin),
    cio:dbg("INSULT: ~s~n", [TextBin]),
    %% cio:fail("~w mother fucking *BYTES* lost in ~w fucking *BLOCKS*, man !!!~n", [LeakedBytes, LeakedBlocks]),
    #leak{bytes = LeakedBytes, blocks = LeakedBlocks}.


dump_state(#state{tool = Tool, pid = Pid, ppid = PPid, errors = Errors}) ->
    cio:ok("===[ ~s INFO REPORT on ~w ]===~n", [Tool, now()]),
    cio:info("Run PID/PPID: ~w/~w~n", [Pid, PPid]),
    NE = length(Errors),
    if
	NE > 0 ->
	    cio:fail("~w errors found~n", [NE]);
	true ->
	    cio:ok("No errors found~n", [])
    end,
    ok.


parse_args([#xmlel{name = <<"vargv">>, children = VArgv0},
	    #xmlel{name = <<"argv">>, children = Argv0}]) ->
    VArgv = remove_whitespaces(VArgv0),
    Argv = remove_whitespaces(Argv0),
    %% cio:dbg("VArgv: ~p~n", [VArgv]),
    %% cio:dbg("Argv;  ~p~n", [Argv]),
    parse_cmdline("Valgrind:", VArgv),
    parse_cmdline("Program: ", Argv).


parse_cmdline(Label, [#xmlel{name = <<"exe">>, children = [{xmlcdata, Exe}]} | Args]) ->
    cio:info("~s ~s~n", [Label, Exe]),
    [fmt_arg(A) || A <- Args].


fmt_arg(#xmlel{name = <<"arg">>, children = [{xmlcdata, Arg}]}) ->
    io:format("                   ~s~n", [Arg]).


parse_status([#xmlel{name = <<"state">>, children = [{xmlcdata, State}]},
	      #xmlel{name = <<"time">>, children = [{xmlcdata, Time}]}]) ->
    cio:info("State: ~s, time: ~s~n", [State, Time]).


parse_suppressions([]) ->
    ok;
%% [{xmlel,<<"pair">>,[],
%%                                                  [{xmlcdata,<<"\n    ">>},
%%                                                   {xmlel,<<"count">>,[],
%%                                                    [{xmlcdata,<<"168">>}]},
%%                                                   {xmlcdata,<<"\n    ">>},
%%                                                   {xmlel,<<"name">>,[],
%%                                                    [{xmlcdata,
%%                                                      <<"X on SUSE11 writev uninit padding">>}]},
%%                                                   {xmlcdata,<<"\n  ">>}]}]
parse_suppressions([#xmlel{name = <<"pair">>,
			   children = C0} | Tail]) ->
    C1 = remove_whitespaces(C0),
    parse_suppression(C1),
    %% cio:dbg("C1: ~p~n", [C1]),
    parse_suppressions(Tail).


parse_suppression([#xmlel{name = <<"count">>, children = [{xmlcdata, Count}]},
		   #xmlel{name = <<"name">>, children = [{xmlcdata, Name}]}]) ->
    io:format("         ~s (~s)~n", [Name, Count]).


parse_errors([]) ->
    ok;
%% [++++] exiting/2: got ERRORS: [{xmlel,<<"pair">>,[],
%%                                            [{xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"count">>,[],
%%                                              [{xmlcdata,<<"1755">>}]},
%%                                             {xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"unique">>,[],
%%                                              [{xmlcdata,<<"0x2">>}]},
%%                                             {xmlcdata,<<"\n  ">>}]},
%%                                           {xmlel,<<"pair">>,[],
%%                                            [{xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"count">>,[],
%%                                              [{xmlcdata,<<"1755">>}]},
%%                                             {xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"unique">>,[],
%%                                              [{xmlcdata,<<"0x1">>}]},
%%                                             {xmlcdata,<<"\n  ">>}]},
%%                                           {xmlel,<<"pair">>,[],
%%                                            [{xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"count">>,[],
%%                                              [{xmlcdata,<<"16">>}]},
%%                                             {xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"unique">>,[],
%%                                              [{xmlcdata,<<"0xce6">>}]},
%%                                             {xmlcdata,<<"\n  ">>}]},
%%                                           {xmlel,<<"pair">>,[],
%%                                            [{xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"count">>,[],
%%                                              [{xmlcdata,<<"2">>}]},
%%                                             {xmlcdata,<<"\n    ">>},
%%                                             {xmlel,<<"unique">>,[],
%%                                              [{xmlcdata,<<"0xce5">>}]},
%%                                             {xmlcdata,<<"\n  ">>}]}]
parse_errors([#xmlel{name = <<"pair">>,
		     children = C0} | Tail]) ->
    C1 = remove_whitespaces(C0),
    parse_error(C1),
    %% cio:dbg("C1: ~p~n", [C1]),
    parse_errors(Tail).


parse_error([#xmlel{name = <<"count">>, children = [{xmlcdata, Count}]},
	     #xmlel{name = <<"unique">>, children = [{xmlcdata, Unique}]}]) ->
    io:format("         ~s (~s)~n", [Unique, Count]).
