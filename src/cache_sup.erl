%% @private
-module(cache_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    % Server = {tr_server, {tr_server, start_link, []},
    %           permanent, 2000, worker, [tr_server]},
    % Children = [Server],
    {ok, { {one_for_one, 5, 10}, []} }.
    % {ok, {{simple_one_for_one, 10, 10},
    %       [{cache_db, {cache_db, start_link, []},
    %         transient, brutal_kill, worker, [cache_db]}]}}.
