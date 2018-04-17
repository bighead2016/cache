%% cache 内存分析监控模块
%% laojiajie@gmail.com
%% 2014-11-26


-module(cache_analyse).

-include("cache.hrl").
-include("log.hrl").

-export([start_link/1]).

-export([init/1, handle_info/2,terminate/2,handle_cast/2]).

-export([log_update/1,log_insert/1,log_select/1,log_delete/1]).




start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
	erlang:process_flag(trap_exit, true),
	% erlang:process_flag(priority, high), 

	erlang:send_after(?CACHE_ANALYSE_GAP, self(), analyse),

    {ok, []}.


log_update(Tab) ->
	gen_server:cast(?MODULE,{log_update,Tab,util:unixtime()}).

log_insert(Tab) ->
	gen_server:cast(?MODULE,{log_insert,Tab,util:unixtime()}).

log_select(Tab) ->
	gen_server:cast(?MODULE,{log_select,Tab,util:unixtime()}).

log_delete(Tab) ->
	gen_server:cast(?MODULE,{log_delete,Tab,util:unixtime()}).


handle_cast({log_update,Tab,Time},State) ->
	Info = get_log_info(Tab,Time),
	update_info(Tab,Info#tab_log_info{update_times = Info#tab_log_info.update_times + 1}),
	{noreply,State};

handle_cast({log_insert,Tab,Time},State) ->
	Info = get_log_info(Tab,Time),
	update_info(Tab,Info#tab_log_info{insert_times = Info#tab_log_info.insert_times + 1}),
	{noreply,State};

handle_cast({log_select,Tab,Time},State) ->
	Info = get_log_info(Tab,Time),
	update_info(Tab,Info#tab_log_info{select_times = Info#tab_log_info.select_times + 1}),
	{noreply,State};

handle_cast({log_delete,Tab,Time},State) ->
	Info = get_log_info(Tab,Time),
	update_info(Tab,Info#tab_log_info{delete_times = Info#tab_log_info.delete_times + 1}),
	{noreply,State}.

handle_info(analyse,State) ->
	do_analyse(),
	erlang:send_after(?CACHE_ANALYSE_GAP, self(), analyse),
	{noreply,State}.

terminate(_Reason,_State) ->
	?ERR(cache_analyse,"terminate,start analyse cache!!"),
	cache:analyse_times(1,true),
	cache:analyse(),
	?ERR(cache_analyse,"terminate,finish analyse cache!!"),
	ok.


do_analyse() ->
	% cache:analyse_times(1,false),
	case erlang:memory(total) > ?CACHE_ANALYSE_MEN*1024*1024 of
		true ->
			cache:analyse();
		false ->
			void
	end.

get_log_info(Tab,Time) ->
	LogEtsRef = cache_util:make_log_ref(Tab),
	case ets:lookup(LogEtsRef,Time) of
		[] ->
			Info = #tab_log_info{
					time = Time,
					insert_times = 0,
					update_times = 0,
					select_times = 0
			},
			ets:insert(LogEtsRef,Info),
			Info;
		[Info] ->
			Info
	end.

update_info(Tab,Info) ->
	LogEtsRef = cache_util:make_log_ref(Tab),
	ets:insert(LogEtsRef,Info).




