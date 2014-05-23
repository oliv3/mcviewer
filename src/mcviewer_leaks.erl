-module(mcviewer_leaks).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_leak/3]).
-export([info/0, info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  leak_count = 0,
	  leak_blocks = 0,
	  leak_definitely_lost = 0,
	  leak_possibly_lost = 0
	 }).

%%%===================================================================
%%% API
%%%===================================================================
add_leak(Kind, Bytes, Blocks) ->
    gen_server:call(?SERVER, {add, Kind, Bytes, Blocks}).

info() ->
    gen_server:call(?SERVER, info).

info(What) ->
    gen_server:call(?SERVER, {info, What}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    cio:on(),
    cio:ok("~s started~n", [?SERVER]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add, 'Leak_DefinitelyLost', Bytes, Blocks}, _From, #state{leak_count = LC, leak_blocks = LB, leak_definitely_lost = LDL} = State) ->
    NewState = State#state{
		 leak_count = LC + 1,
		 leak_definitely_lost = LDL + Bytes,
		 leak_blocks = LB + Blocks
		},
    {reply, ok, NewState};

handle_call({add, 'Leak_PossiblyLost', Bytes, Blocks}, _From, #state{leak_count = LC, leak_blocks = LB, leak_possibly_lost = LPL} = State) ->
    NewState = State#state{
		 leak_count = LC + 1,
		 leak_possibly_lost = LPL + Bytes,
		 leak_blocks = LB + Blocks
		},
    {reply, ok, NewState};

handle_call(info, _From, State) ->
    Reply = [
	     {leaks, State#state.leak_count},
	     {blocks, State#state.leak_blocks},
	     {definitely_lost, State#state.leak_definitely_lost},
	     {possibly_lost, State#state.leak_possibly_lost}
	    ],
    {reply, Reply, State};

handle_call({info, count}, _From, State) ->
    Reply = State#state.leak_count,
    {reply, Reply, State};

handle_call({info, blocks}, _From, State) ->
    Reply = State#state.leak_blocks,
    {reply, Reply, State};

handle_call({info, definitely_lost}, _From, State) ->
    Reply = State#state.leak_definitely_lost,
    {reply, Reply, State};

handle_call({info, possibly_lost}, _From, State) ->
    Reply = State#state.leak_possibly_lost,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    cio:warn("~s: call(~p)~n", [?MODULE, _Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    cio:warn("~s: cast(~p)~n", [?MODULE, _Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    cio:warn("~s: info(~p)~n", [?MODULE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
