-module(mcviewer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% init([]) ->
%%     Ubigraph = ?CHILD(mcviewer_ubigraph_server, worker),
%%     Grapher = ?CHILD(mcviewer_graph, worker),
%%     Erlubi = ?CHILD(erlubi, worker),
%%     XML = ?CHILD(xml_stream, worker),
%%     Leaks = ?CHILD(mcviewer_leaks, worker),
%%     {ok, {{one_for_one, 5, 10}, [Ubigraph, Grapher, Erlubi, XML, Leaks]}}.

init([]) ->
    Ubigraph = ?CHILD(mcviewer_ubigraph_server, worker),
    {ok, {{one_for_one, 5, 10}, [Ubigraph]}}.
