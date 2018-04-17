%% cache 内存回收模块
%% laojiajie@gmail.com
%% 2014-11-10

%%-----------------------------设计思路-----------------------------
% 1.游戏系统本身内存占用不多，是随着玩家上线占用内存来储存数据而导致内存不断增加；
% 2.cache设计其实就是根据玩家行为来设计的，一个玩家一个进程操作自己的数据；
% 3.对于玩家来说，登陆时查询过数据库一次后，内存里面的数据是完整的，查询无需数据库操作，响应快速。
% 4.而插入和更新操作交给底层的进程来做就可以了，所以基本不会出现数据等待的情况；
% 5.问题出现在：第一次查询db过后的数据没有释放掉，所以玩家下线后还会占用内存；
% 6.而其他计数回收，标记回收等，cpu占用很高，而且忽略了玩家数据整体性，回收不干净而且导致玩家频繁查询数据库
% 7.所以只需要以玩家为单位回收玩家产生的内存就可以了

% 具体做法：
% 1.起一个全局gc进程
% 2.玩家下线后往gc进程的gc列表里记入玩家id
% 3.gc进程定时清空gc列表里面的玩家的相关数据，同时更新数据库
% 4.额外检查：玩家是否在线
% 5.额外操作：清空佣兵缓存数据
% 6.gc保护列表：把排行榜玩家id过滤，不进行回收，保障活跃玩家的体验。

%%----------------------------------------------------------------

-module(cache_gc).

-include("cache.hrl").
-include("log.hrl").

-export([start_link/1]).

-export([init/1, handle_cast/2, handle_info/2,terminate/2,handle_call/3]).

-export([
		 set_flag/1,		%% 设置回收标志
		 unset_flag/1,		%% 抹除回收标志
		 set_ungc_list/1, 	%% 设置不被回收的列表（排行榜前200名不被移除等）

		 auto_gc/1 			%% 处理查看其他人资料后引起的内存残留
		]).

-record(state,{gc_list = [],un_gc_list = []}).

start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_flag(FirstKey) ->
	gen_server:cast(?MODULE,{set_flag,FirstKey}).

unset_flag(FirstKey) ->
	gen_server:call(?MODULE,{unset_flag,FirstKey}).

set_ungc_list(List) ->
	gen_server:cast(?MODULE,{set_ungc_list,List}).

auto_gc(Id) ->
	case cache_gc_util:gc_check(Id) of
        true ->
            cache_gc:set_flag(Id);
        false ->
            void
    end.

init([]) ->
	erlang:process_flag(trap_exit, true),
	erlang:process_flag(priority, high), 

	erlang:send_after(?CACHE_GC_GAP, self(), gc),

    {ok, #state{}}.

handle_cast({set_flag,FirstKey},State) ->
	Now = cache_util:unixtime(),
	case lists:keyfind(FirstKey,1,State#state.gc_list) of
		false ->
			NewState = State#state{gc_list = State#state.gc_list++[{FirstKey,Now}]};
		{FirstKey,Time} ->
			NewList = lists:keyreplace(FirstKey,1,State#state.gc_list,{FirstKey,Now}),
			NewState = State#state{gc_list = NewList}
	end,
	?INF(cache_gc,"set_flag,add_id = ~w,gc_list = ~p",[FirstKey,NewState#state.gc_list]),
    {noreply, NewState};


handle_cast({set_ungc_list,List},State) ->
	{noreply, State#state{un_gc_list = List}}.


handle_call({unset_flag,FirstKey},_From,State) ->
	Reply = 
	case lists:keyfind(FirstKey,1,State#state.gc_list) of
		false ->
			NewGcList = State#state.gc_list,
			false;
		{FirstKey,_Time} ->
			NewGcList = lists:keydelete(FirstKey, 1, State#state.gc_list),
			true
	end,
	?INF(cache_gc,"unset_flag,gc_list = ~p",[NewGcList]),
	{reply, Reply, State#state{gc_list = NewGcList}}.



handle_info(gc,State) ->
	Now = cache_util:unixtime(),
	Fun = fun({Key,Time},UnGcList) ->
		case Now - Time > ?DB_UPDATE_INTERVAL/1000 + 20*60 andalso cache_gc_util:gc_check(Key) == true of
			true ->
				cache_gc_util:do_gc(Key),
				UnGcList;
			false ->
				UnGcList++[{Key,Time}]
		end
	end,
	Fun2 = fun({Key,Time}) ->
		not lists:member(Key,State#state.un_gc_list)
	end,
	NeedGcList = lists:filter(Fun2,State#state.gc_list),
	UnGcList = lists:foldl(Fun,[],NeedGcList),
	erlang:send_after(?CACHE_GC_GAP, self(), gc),
	% ?ERR(gc,"Now = ~w,UnGcList = ~w",[Now,UnGcList]),
	{noreply,State#state{gc_list = (State#state.gc_list -- NeedGcList)++UnGcList}}.



terminate(_Reason,_State) ->
	ok.

