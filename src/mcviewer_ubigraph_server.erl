%%%-------------------------------------------------------------------
%%% @author Olivier Girondel <olivier@biniou.info>
%%% @copyright (C) 2013, Olivier Girondel
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2013 by Olivier Girondel <olivier@biniou.info>
%%%-------------------------------------------------------------------
-module(mcviewer_ubigraph_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([main/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pid}).

%%%===================================================================
%%% API
%%%===================================================================

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
    Pid = spawn_link(?MODULE, main, []),
    %% process_flag(trap_exit, true),
    cio:info("Started server ~p~n", [Pid]),
    {ok, #state{pid = Pid}}.

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
handle_call(_Request, _From, State) ->
    cio:warn("~s: call ~p~n", [?MODULE, _Request]),
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
    cio:warn("~s: cast ~p~n", [?MODULE, _Msg]),
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
%% handle_info({'EXIT', Pid, normal}, #state{pid = Pid} = State) ->
%%     cio:ok("~s: ~p shutting down~n", [?MODULE, Pid]),
%%     {stop, normal, State};

%% handle_info({'EXIT', Pid, error}, #state{pid = Pid} = State) ->
%%     cio:fail("~s: ~p shutting down on error~n", [?MODULE, Pid]),
%%     {stop, error, State};

%% handle_info(stop, #state{pid = Pid} = State) ->
%%     cio:ok("~s: ~p shutting down~n", [?MODULE, Pid]),
%%     {stop, normal, State};

handle_info(_Info, State) ->
    cio:info("~s: info ~p~n", [?MODULE, _Info]),
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
    cio:warn("~s: terminate ~p~n", [?MODULE, _Reason]),
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
main() ->
    cio:on(),
    Res = exec:run("ubigraph_server", [sync]),
    %% cio:info("~s: main returned ~p~n", [?MODULE, Res]),
    case Res of
	{ok, []} ->
	    cio:ok("~s shutting down~n", [?MODULE]),
	    %% ?SERVER ! stop,
	    exit(normal);

	{error, Reason} ->
	    cio:fail("~s: error ~p~n", [?MODULE, Reason]),
	    exit(error)
    end.
