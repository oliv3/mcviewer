-module(mcviewer_graph).
-include("mcviewer.hrl").

-behaviour(gen_server).

%% Public API
%% error/1 for live display
%% errors/1 for use when all errors are collected
-export([error/1, errors/1]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {last_error}).

%%%===================================================================
%%% API
%%%===================================================================
error(Error) ->
    gen_server:cast(?SERVER, {error, Error}).

errors(Errors) ->
    gen_server:call(?SERVER, {errors, Errors}).

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
    ets:new(?SERVER, [named_table, public]),
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
handle_call({errors, Errors}, _From, #state{last_error = LE} = State) ->
    NLE = errors_cb(Errors, LE),
    {reply, ok, State#state{last_error = NLE}};

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
handle_cast({error, Error}, #state{last_error = LE} = State) ->
    NLE = errors_cb([Error], LE),
    {noreply, State#state{last_error = NLE}};

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
vertex(Id) ->
    case ets:lookup(?SERVER, Id) of
	[] ->
	    {ok, V} = erlubi:vertex(),
	    %% cio:dbg("Added new vertex ~p: ~w~n", [Id, V]),
	    ets:insert(?SERVER, {Id, V}),
	    V;

	[{Id, V}] ->
	    %% cio:dbg("Reused vertex ~p: ~w~n", [Id, V]),
	    V
    end.

error_vertex(Id) ->
    vertex({error, Id}).
frame_vertex(Id) ->
    vertex({frame, Id}).


errors_cb([], LastErrorV) ->
    LastErrorV;
errors_cb([#{ kind := 'Leak_PossiblyLost' } | Errors], LastErrorV) ->
    errors_cb(Errors, LastErrorV);
errors_cb([#{ unique := Unique,
	      tid := Tid,
	      kind := Kind,
	      what := What,
	      stack := [#{ ip := IP } = At | By]
	    } | Errors], LastErrorV) ->
    cio:dbg("~p (~s) in thread ~w~n", [Kind, Unique, Tid]),
    %% cio:warn("At ~p~n", [At]),
    %% cio:warn("By ~p~n", [By]),

    Color = color(Kind),
    Shape = shape(Kind),
    ErrorV = error_vertex(Unique),
    ErrorV:color(Color),
    ErrorV:shape(Shape),
    %% Label = lists:flatten(io_lib:format("[~w] ~s ~s", [Tid, Unique, What])),
    %% ErrorV:label(Label),

    %% Add some label if we have one
    case What of
	undefined ->
	    ok;
	What ->
	    ErrorV:label(binary_to_list(What))
    end,

    AtV = frame_vertex(IP),
    %% {AtColor, _AtLabel} = label(At),
    AtColor = frame_color(At),
    _AtLabel = label(At),
    %% AtV:label(AtLabel),
    AtV:color(AtColor),

    {ok, E1} = erlubi:edge(AtV, ErrorV),
    E1:arrow(true),

    graph_by(AtV, By),

    case LastErrorV of
	undefined ->
	    ok;

	LastErrorV ->
	    {ok, E2} = erlubi:edge(ErrorV, LastErrorV),
	    %% io:format("E2: ~p~n", [E2]),
	    E2:arrow(true),
	    E2:stroke(dashed)
    end,

    errors_cb(Errors, ErrorV);
errors_cb([#{ unique := Unique,
	      tid := _Tid,
	      kind := _Kind,
	      what := _What %% ,
	      %% stack := Stack,
	      %% auxstack := AuxStack
	    } | Errors], LastErrorV) ->
  %% when Stack =:= undefined andalso AuxStack =:= undefined ->
    cio:warn("No stack found for error ~p~n", [Unique]),
    errors_cb(Errors, LastErrorV);
errors_cb([_Other | Errors], LastErrorV) ->
    cio:dbg("~s: unhandled: ~p~n", [?MODULE, _Other]),
    errors_cb(Errors, LastErrorV).


color('UninitCondition') ->
    blue;
color('UninitValue') ->
    green;
color('Leak_PossiblyLost') ->
    yellow;
color('Leak_DefinitelyLost') ->
    red;
color('InvalidRead') ->
    orange;
color('InvalidWrite') ->
    red;
color(Other) ->
    cio:warn("No color set for ~p~n", [Other]),
    white.

shape('UninitCondition') ->
    cube;
shape('UninitValue') ->
    sphere;
shape('Leak_PossiblyLost') ->
    octahedron;
shape('Leak_DefinitelyLost') ->
    icosahedron;
shape('InvalidRead') ->
    cube;
shape('InvalidWrite') ->
    cube;
shape(Other) ->
    cio:warn("No shape set for ~p~n", [Other]),
    cube.


graph_by(_LastV, []) ->
    ok;
graph_by(LastV, [#{ ip := IP } = Frame | Frames]) ->
    %% {Color, _Label} = label(Frame),
    V = frame_vertex(IP),

    Color = frame_color(Frame),
    V:color(Color),

    _Label = label(Frame),
    %% V:label(Label),

    {ok, E} = erlubi:edge(V, LastV),
    E:arrow(true),
    E:oriented(true),

    graph_by(V, Frames).


%% frame_color(#{
%% 	       fn := undefined,
%% 	       dir = undefined,
%% 	       file = undefined,
%% 	       line = undefined
%% 	     }) ->
%%     red;
frame_color(#{
	       ip := _IP,
	       file := _File,
	       line := _Line
	     }) ->
    white;
frame_color(#{ ip := _IP }) ->
    orange.


%% label(#mc_frame{ip = Ip, obj = _Obj, fn = undefined, dir = undefined, file = undefined, line = undefined}) ->
%%     %% lists:flatten(io_lib:format("~s (~s)", [Ip, Obj]));
%%     lists:flatten(io_lib:format("~s", [Ip]));
%% label(#mc_frame{ip = Ip, obj = _Obj, fn = Fn, dir = undefined, file = undefined, line = undefined}) ->
%%     lists:flatten(io_lib:format("~s (~s)", [Ip, Fn]));
%% label(#mc_frame{ip = Ip, obj = _Obj, fn = Fn, dir = _Dir, file = _File, line = _Line}) ->
%%     lists:flatten(io_lib:format("~s (~s)", [Ip, Fn])).

%% label(#mc_frame{ip = Ip, obj = _Obj, fn = undefined, dir = undefined, file = undefined, line = undefined}) ->
%%     %% lists:flatten(io_lib:format("~s (~s)", [Ip, Obj]));
%%     lists:flatten(io_lib:format("~s", [Ip]));
%% label(#mc_frame{ip = Ip, obj = _Obj, fn = Fn, dir = undefined, file = undefined, line = undefined}) ->
%%     lists:flatten(io_lib:format("~s (~s)", [Ip, Fn]));
label(#{
	 ip := Ip,
	 fn := Fn
       }) ->
    lists:flatten(io_lib:format("~s (~s)", [Ip, Fn])).
