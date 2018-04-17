%% 2013-6-13
%% mail:laojiajie@gmail.com

%%************************************************基本模块*********************************************

%%------------------------------cache-------------------------------
%% 1.数据存取接口，不经进程排序，避免玩家之间的数据依赖，每个玩家的操作操作独立
%% 2.每张表用一个进程维护，负责cache与数据库之间的同步，update数据通过进程用ets保存即可
%%------------------------------cache_db----------------------------
%% 1.数据库的初始化，数据库数据存取
%% 2.cache与db之间的接口
%%------------------------------sql_operate--------------------------
%% 1.sql操作,语句封装，其他工具


%%************************************************使用方法**********************************************
%% 1.自己定义需要作数据储存的record；
%% 2.map_data中的tabs函数中添加record名，相应的map数据，预定义规则cache_opt
%% 3.restart your server and welcome to use cache!!


%%************************************************注意事项**********************************************
%% 1.增删改查没经统一的通道控制，相当于ets操作。
%% 2.必须一个进程控制一份数据，不然会出现竞争现象
%% 3.对于交互性的数据，可以利用全局进程来控制增删改，其他进程可以查
%% 4.多使用call阻塞方法，这样全局进程就可以控制每个玩家的数据，阻塞时间内玩家不会操作这份数据，避免竞争

-module(cache).

-include("cache.hrl").
-include("log.hrl").

-compile([export_all]).
%% 用户接口
-export([lookup/2,          %% 查找
         gc_lookup/2,       %% 查找(之后根据条件判定执行gc,例如查看别人的佣兵信息)
         insert/1,          %% 插入
         update/1,          %% 更新
         update_ets/1,      %% 更新数据ets
         delete/1,          %% 删除
         delete/2,          %% 删除2
         delete_all/1,      %% 删除整个表的数据
         update_all/0,      %% 马上更新数据库
         tab2list/1,        %% 获取表里所有数据
         gc/1               %% 内存回收接口

         ]).


%% 性能测试接口
-export([analyse/0,info/0,info/1]).


-export([purge_lru/5]).


gc_lookup(Tab, Key) ->
    Result = lookup(Tab,Key),
    FirstKey = 
    case is_tuple(Key) of
        true ->
            element(1,Key);
        false ->
            Key
    end,
    % Index = cache_util:do_key_hash(FirstKey),
    case cache_gc_util:gc_check(FirstKey) of
        true ->
            cache_gc:set_flag(FirstKey);
        false ->
            void
    end,
    Result.



lookup(Tab, Key) ->
    % ?INF(cache,"lookup,tab=~p,key = ~p",[Tab,Key]),
	Map = map_data:map(Tab),
	Index = cache_util:do_key_hash(Key),
	EtsRef = cache_util:make_hash_ref(Tab,Index),
	DBNullEtsRef = cache_util:make_hash_dbnull_ref(Tab,Index),
    CacheOpt = map_data:cache_opt(Tab),

	case length(Map#map.key_fields) of
		1 ->
			case ets:lookup(EtsRef,Key) of
				[] ->
					case ets:lookup(DBNullEtsRef,Key) of
						[] ->
							case sql_operate:sql_select(Tab,[Key]) of
								not_found ->
									ets:insert(DBNullEtsRef,#db_null_index{key = Key}),
									[];
								Records ->
									Fun2 = fun(Record) ->
										ets:insert(EtsRef,Record)
									end,
									lists:map(Fun2,Records),
                                    access_lru(EtsRef, Key, CacheOpt),
									Records
							end;
						_DBNullIndex ->
							[]
					end;
				Records ->
                    access_lru(EtsRef, Key, CacheOpt),
					Records
			end;

		_ ->
			EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
			case is_tuple(Key) of
				true ->
					[Id | _] = erlang:tuple_to_list(Key);
				false ->
					Id = Key
			end,

            SearchIndex = case Id =:= Key of
                false ->    % key是tuple，先找一下ets表
                    case ets:lookup(EtsRef, Key) of
                        [] ->
                            search_index;
                        SpecRecs ->
                            {hit, SpecRecs}
                    end;
                true ->
                    search_index
            end,

            case SearchIndex of
                {hit, SpecRecords} ->
                    access_lru(EtsRef, Key, CacheOpt),
                    SpecRecords;
                search_index ->
                    LookupInedx = ets:lookup(EtsIndexRef,Id),
                    % ?ERR(cache,"tab = ~w,key = ~w,Index = ~w",[Tab,Id,LookupInedx]),
                    case LookupInedx of	
                        [{Id,KeyTupleList,1}] ->
                            FilterKeyTuples = cache_util:filterKeyTuples(Id,Key,KeyTupleList,Map#map.key_fields),
                            Fun1 = fun(KeyTuple,Records) ->
                                case ets:lookup(EtsRef,KeyTuple) of
                                    [] ->
                                        Records;
                                    [Record] ->
                                        access_lru(EtsRef, KeyTuple, CacheOpt),
                                        [Record|Records]
                                end
                            end, 
                            lists:foldl(Fun1,[],FilterKeyTuples);
                        _ ->
                            case LookupInedx of
                                [] ->
                                    EtsKeyTuples = [];
                                [{Id,EtsKeyTuples,0}] ->
                                    void
                            end,
                            case ets:lookup(DBNullEtsRef,Id) of
                                [] ->
                                    case sql_operate:sql_select(Tab,[Id]) of
                                        not_found ->
                                            % ?ERR(sql_operate,"not_found Tab=~w,Id=~w",[Tab,Id]),
                                            ets:insert(DBNullEtsRef,#db_null_index{key = Id}),
                                            ets:insert(EtsIndexRef,{Id,EtsKeyTuples,1}),
                                            [];
                                        Records ->
                                            % ?ERR(sql_operate,"Record = ~w",[Records]),

                                            Fun = fun(Record) ->
                                                [Tab,KeyTuple|_Res] = tuple_to_list(Record),
                                                case lists:member(KeyTuple,EtsKeyTuples) of
                                                    true ->
                                                        [];
                                                    false ->
                                                        access_lru(EtsRef, KeyTuple, CacheOpt),
                                                        ets:insert(EtsRef,Record),
                                                        KeyTuple
                                                end
                                            end,
                                            KeyTupleList = lists:flatten(lists:map(Fun,Records)),
                                            ets:insert(EtsIndexRef,{Id,KeyTupleList++EtsKeyTuples,1}),

                                            case Id == Key of
                                                true ->
                                                    Records;
                                                false ->
                                                    case length(tuple_to_list(Key)) == length(Map#map.key_fields) of
                                                        true ->
                                                            case lists:keyfind(Key,2,Records) of
                                                                false ->
                                                                    [];
                                                                Records1 ->
                                                                    [Records1]
                                                            end;
                                                        false ->
                                                            {Id1,Id2} = Key,
                                                            Fun1 = fun(Record) ->
                                                                [Tab,{Key1,Key2,_Key3}|_Res] = tuple_to_list(Record),
                                                                Id1 == Key1 andalso Id2 == Key2
                                                            end,
                                                            lists:filter(Fun1,Records)
                                                    end
                                            end
                                    end;
                                _DBNullIndex ->
                                    []
                            end
                    end
            end
	end.


insert(Record) ->
	[Tab,Key|_Res] = tuple_to_list(Record),
	Map = map_data:map(Tab),
    CacheOpt = map_data:cache_opt(Tab),

	Index = cache_util:do_key_hash(Key),
	EtsRef = cache_util:make_hash_ref(Tab,Index),
	DBRef = cache_util:make_db_hash_ref(Tab,Index),

    case lookup(Tab, Key) of
        [] ->
        	ets:insert(EtsRef,Record),
            access_lru(EtsRef, Key, CacheOpt),
        	cache_db:insert_to_db(DBRef,Record),
        	case length(Map#map.key_fields) of
        		1 ->
        			void;
        		_ ->
        			EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
        			[Id | _] = erlang:tuple_to_list(Key),
        			case ets:lookup(EtsIndexRef,Id) of
        				[] ->
        					ets:insert(EtsIndexRef,{Id,[Key],0});
        				[{Id,KeyTuples,IsSearchDB}] ->
        					ets:insert(EtsIndexRef,{Id,lists:usort([Key|KeyTuples]),IsSearchDB})
        			end
        	end;
        _ ->
            duplicate_key
    end.

update(Record) ->
	[Tab,Key|_Res] = tuple_to_list(Record),
	Index = cache_util:do_key_hash(Key),
	EtsRef = cache_util:make_hash_ref(Tab,Index),
	DBRef = cache_util:make_db_hash_ref(Tab,Index),
    CacheOpt = map_data:cache_opt(Tab),
	case ets:lookup(EtsRef,Key) of
		[] ->
            OldRecord = none,
			void;
		[OldRecord] ->
			ets:insert(EtsRef,Record),
            access_lru(EtsRef, Key, CacheOpt)
	end,  
	cache_db:cache_update_notify(DBRef,Key,OldRecord,Record).


delete(Record) ->
	[Tab,Keys|_Res] = tuple_to_list(Record),
	Index = cache_util:do_key_hash(Keys),
	EtsRef = cache_util:make_hash_ref(Tab,Index),
    CacheOpt = map_data:cache_opt(Tab),
    delete_lru(EtsRef, Keys, CacheOpt),
	delete(Tab,Keys).

delete(Tab,Keys) ->
	Map = map_data:map(Tab),
	Index = cache_util:do_key_hash(Keys),
	EtsRef = cache_util:make_hash_ref(Tab,Index),
	DBRef = cache_util:make_db_hash_ref(Tab,Index),
    CacheOpt = map_data:cache_opt(Tab),
	case length(Map#map.key_fields) of
		1 ->
            delete_lru(EtsRef, Keys, CacheOpt),
			ets:delete(EtsRef,Keys);
		_ ->
			EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
			case is_integer(Keys) of
				true ->
					Id = Keys;
				false ->
					[Id | _] = erlang:tuple_to_list(Keys)
			end,
			case ets:lookup(EtsIndexRef,Id) of
				[] ->
					?ERR(cache,"cant find index to delete,Tab = ~w,Key = ~w",[Tab,Keys]);
				[{Id,KeyTuples,IsSearchDB}] ->
					FilterKeyTuples = cache_util:filterKeyTuples(Id,Keys,KeyTuples,Map#map.key_fields),
					ets:insert(EtsIndexRef,{Id,KeyTuples--FilterKeyTuples,IsSearchDB}),
					Fun = fun(KeyTuple) ->
                        delete_lru(EtsRef, KeyTuple, CacheOpt),
						ets:delete(EtsRef,KeyTuple)
					end,
					lists:map(Fun,FilterKeyTuples)
			end
	end,
	DBKeys = 
	case is_integer(Keys) of
		true ->
			[Keys];
		false ->
			tuple_to_list(Keys)
	end,
	cache_db:delete_in_db(DBRef,DBKeys).

%% 删除某个表的所有数据
delete_all(Tab) ->
    Fun = fun(Index) ->
        Map = map_data:map(Tab),
        EtsRef = cache_util:make_hash_ref(Tab,Index),
        UpdateEtsRef = cache_util:make_hash_update_ref(Tab,Index),
        % CacheOpt = map_data:cache_opt(Tab),
        DBNullEtsRef = cache_util:make_hash_dbnull_ref(Tab,Index),
        ets:delete_all_objects(EtsRef),
        case length(Map#map.key_fields) of
            1 ->
                void;
            _ ->
                EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
                ets:delete_all_objects(EtsIndexRef)
        end,
        case Tab of
            arena_rank ->
                ets:delete_all_objects(DBNullEtsRef);
            _ ->
                ok
        end,
        ets:delete_all_objects(UpdateEtsRef)
    end,
    lists:map(Fun,lists:seq(1,?DB_HASH_NUM)),
    SqlTabName = sql_operate:make_sql_tab(Tab),
    Sql = io_lib:format("TRUNCATE TABLE ~w ", [SqlTabName]),
    sql_operate:do_execute(Sql),
    ok.
    

update_all() ->
    Tabs = map_data:tabs(),
    Fun = fun({Tab, Index}) -> 
        DBRef = cache_util:make_db_hash_ref(Tab,Index),
        cache_db:update_all_to_db(DBRef)
    end,
    List = [{Tab, Index} || Index <- lists:seq(1,?DB_HASH_NUM),Tab <- Tabs],
    lists:foreach(Fun,List).

%% 仅仅更新ETS 主要用于更新非DB的附加数据
update_ets(Record) ->
    [Tab,Key|_Res] = tuple_to_list(Record),
    Index = cache_util:do_key_hash(Key),
    EtsRef = cache_util:make_hash_ref(Tab,Index),
    CacheOpt = map_data:cache_opt(Tab),
    case ets:lookup(EtsRef,Key) of
        [] ->
            void;
        _ ->
            ets:insert(EtsRef,Record),
            access_lru(EtsRef, Key, CacheOpt)
    end.

remove_from_cache(Tab, Keys, EtsRef, UpdateEtsRef) ->
	Map = map_data:map(Tab),
	Index = cache_util:do_key_hash(Keys),
    CacheOpt = map_data:cache_opt(Tab),
	case length(Map#map.key_fields) of
		1 ->
            ets:delete(UpdateEtsRef, Keys),
            case ets:lookup(EtsRef, Keys) of
                [] -> void;
                [Record] -> sql_operate:sql_update(Record)
            end,

            delete_lru(EtsRef, Keys, CacheOpt),
			ets:delete(EtsRef,Keys);

		_ ->
			EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
			case is_integer(Keys) of
				true ->
					Id = Keys;
				false ->
					[Id | _] = erlang:tuple_to_list(Keys)
			end,
			case ets:lookup(EtsIndexRef,Id) of
				[] ->
					ok;
				[{Id,KeyTuples,_IsSearchDB}] ->
					FilterKeyTuples = cache_util:filterKeyTuples(Id,Keys,KeyTuples,Map#map.key_fields),
					ets:insert(EtsIndexRef,{Id,KeyTuples--FilterKeyTuples,0}),
					Fun = fun(KeyTuple) ->
                        ets:delete(UpdateEtsRef, KeyTuple),
                        case ets:lookup(EtsRef, KeyTuple) of
                            [] -> void;
                            [Record] -> sql_operate:sql_update(Record)
                        end,

                        delete_lru(EtsRef, KeyTuple, CacheOpt),
						ets:delete(EtsRef,KeyTuple)
					end,
					lists:foreach(Fun,FilterKeyTuples)
			end
	end.

tab2list(Tab) ->
	IndexList = lists:seq(1,?DB_HASH_NUM),
	lists:flatten([ets:tab2list(cache_util:make_hash_ref(Tab,Index))||Index <- IndexList]).

size(Tab) ->
    IndexList = lists:seq(1,?DB_HASH_NUM),
    lists:sum([ets:info(cache_util:make_hash_ref(Tab,Index), size) || Index <- IndexList]).

gc(Key) ->
    Tabs = map_data:tabs(),
    Index = cache_util:do_key_hash(Key),
    [gc_tab(Tab,Index,Key) || Tab <- Tabs],
    ok.

gc_tab(Tab,Index,Keys) ->
    Map = map_data:map(Tab),
    EtsRef = cache_util:make_hash_ref(Tab,Index),
    DBRef = cache_util:make_db_hash_ref(Tab,Index),
    DBNullEtsRef = cache_util:make_hash_dbnull_ref(Tab,Index),
    CacheOpt = map_data:cache_opt(Tab),
    case CacheOpt#cache_opt.pre_load == false andalso CacheOpt#cache_opt.need_gc == true of
        true ->
            case length(Map#map.key_fields) of
                1 ->
                    cache_db:gc_update(DBRef,Keys),
                    delete_lru(EtsRef, Keys, CacheOpt),
                    ets:delete(EtsRef,Keys);
                _ ->
                    EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
                    case is_integer(Keys) of
                        true ->
                            Id = Keys;
                        false ->
                            [Id | _] = erlang:tuple_to_list(Keys)
                    end,
                    case ets:lookup(EtsIndexRef,Id) of
                        [] ->
                            % ?ERR(cache,"cant find index to delete,Tab = ~w,Key = ~w",[Tab,Keys]);
                            void;
                        [{Id,KeyTuples,_IsSearchDB}] ->
                            FilterKeyTuples = cache_util:filterKeyTuples(Id,Keys,KeyTuples,Map#map.key_fields),

                            Fun = fun(KeyTuple) ->
                                cache_db:gc_update(DBRef,KeyTuple),
                                delete_lru(EtsRef, KeyTuple, CacheOpt),
                                ets:delete(EtsRef,KeyTuple)
                            end,
                            lists:map(Fun,FilterKeyTuples),
                            ets:delete(EtsIndexRef,Id)
                    end
            end,
            ets:delete(DBNullEtsRef, Keys);
        false ->
            void
    end.















%%-----------------------analyse_tool-----------------------------------------


analyse() ->
    % ?ERR(io_lib:format("\e[32m~s\e[0m\n",["hello"]),io_lib:format("\e[32m~s\e[0m\n",["hello"])),
    Tabs = map_data:tabs(),
    Fun = fun(Tab,Infos) ->
        [gen_tab_info(Tab)|Infos]
    end,
    Infos = lists:foldl(Fun,[],Tabs),
    FunSort = fun(Info1,Info2) ->
        Info1#tab_info.name < Info2#tab_info.name
    end,
    TotInfo = cal_tot_info(Infos),
    SortInfos = lists:sort(FunSort,Infos),
    FunMake = fun(Info,Str) ->
        Str++make_info_str(Info)
    end,
    {{Y,M,D},{H,Mi,S}} = calendar:local_time(),
    Date = integer_to_list(Y)++"_"++integer_to_list(M)++"_"++trans_time(D),
    Time = trans_time(H)++":"++trans_time(Mi)++":"++trans_time(S),
    Content = lists:foldl(FunMake,"time: "++Date++" "++Time++"\n"++make_other_info()++make_title(),SortInfos),
    Dir = "./analyse/cache/"++Date,
    file:make_dir(Dir),
    file:write_file(Dir++"/"++Time++"-"++trans_men(erlang:memory(total)),Content++draw_line()++make_info_str(TotInfo)++"\n"++show_sys_memory()).

info() ->
    Tabs = map_data:tabs(),
    Fun = fun(Tab,Infos) ->
        [gen_tab_info(Tab)|Infos]
    end,
    Infos = lists:foldl(Fun,[],Tabs),
    FunSort = fun(Info1,Info2) ->
        Info1#tab_info.name < Info2#tab_info.name
    end,
    TotInfo = cal_tot_info(Infos),
    SortInfos = lists:sort(FunSort,Infos),
    {TopInfos,_} = lists:split(5,SortInfos),
    [io_tabinfo(Info)|| Info<-[TotInfo]++TopInfos],
    ok.



info(Tab) ->
    TabInfo = gen_tab_info(Tab),
    io_tabinfo(TabInfo).

io_tabinfo(TabInfo) ->
    io:format("tab =           ~p~n"++
              "memory =        ~ts~n"++
              "size =          ~p~n"++
              "index_memeory = ~ts~n"++
              "index_size =    ~p~n"++
              "update_memory = ~ts~n"++
              "update_size =   ~p~n~n"
            ,[TabInfo#tab_info.name,trans_men(TabInfo#tab_info.memory),TabInfo#tab_info.size,
              trans_men(TabInfo#tab_info.index_memeory),TabInfo#tab_info.index_size,
              trans_men(TabInfo#tab_info.up_memory),TabInfo#tab_info.up_size]).

gen_tab_info(Tab) ->
    Fun = fun(Index,{Mem,Size,IndexMem,IndexSize,UpMem,UpSize}) ->
        {Mem1,Size1} = cal_mem_size(cache_util:make_hash_ref(Tab,Index)),
        {IndexMem1,IndexSize1} = cal_mem_size(cache_util:get_key_index_ets(Tab,Index)),
        {UpMem1,UpSize1} = cal_mem_size(cache_util:make_hash_update_ref(Tab,Index)),
        {Mem+Mem1,Size+Size1,IndexMem+IndexMem1,IndexSize+IndexSize1,UpMem+UpMem1,UpSize+UpSize1}
    end,
    {Mem,Size,IndexMem,IndexSize,UpMem,UpSize} = lists:foldl(Fun,{0,0,0,0,0,0},lists:seq(1,?DB_HASH_NUM)),
    #tab_info{
                name    = Tab,
                memory  = Mem,
                size    = Size,
                index_memeory = IndexMem,
                index_size = IndexSize,
                up_memory = UpMem,
                up_size    = UpSize
            }.

make_info_str(Info) ->
    Name = atom_to_list(Info#tab_info.name),
    Mem  = trans_men(Info#tab_info.memory),
    Size = trans_size(Info#tab_info.size),
    IndexMem = trans_men(Info#tab_info.index_memeory),
    IndexSize = trans_size(Info#tab_info.index_size),
    UpMem = trans_men(Info#tab_info.up_memory),
    UpSize = trans_size(Info#tab_info.up_size),

    Name++lists:flatten(lists:duplicate(max(1,(50-length(Name)))," "))++
    Mem++lists:flatten(lists:duplicate(20-length(Mem)," "))++
    Size++lists:flatten(lists:duplicate(20-length(Size)," "))++
    IndexMem++lists:flatten(lists:duplicate(20-length(IndexMem)," "))++
    IndexSize++lists:flatten(lists:duplicate(20-length(IndexSize)," "))++
    UpMem++lists:flatten(lists:duplicate(20-length(UpMem)," "))++
    UpSize++"\n".


make_title() ->
    Name = "table_name",
    Mem  = "memory",
    Size = "size",
    IndexMem = "index_memeory",
    IndexSize = "index_size",
    UpMem = "update_memory",
    UpSize = "update_size",

    Name++lists:flatten(lists:duplicate(50-length(Name)," "))++
    Mem++lists:flatten(lists:duplicate(20-length(Mem)," "))++
    Size++lists:flatten(lists:duplicate(20-length(Size)," "))++
    IndexMem++lists:flatten(lists:duplicate(20-length(IndexMem)," "))++
    IndexSize++lists:flatten(lists:duplicate(20-length(IndexSize)," "))++
    UpMem++lists:flatten(lists:duplicate(20-length(UpMem)," "))++
    UpSize++"\n"++draw_line().
    
make_other_info() ->
    case ets:info(ets_online,size) of
        undefined ->
            "";
        _ ->
            "online num: "++integer_to_list(ets:info(ets_online,size))++"\n"++
            "temp_role num: "++integer_to_list(ets:info(ets_role_temp,size))++"\n"++
            "temp_role memory: "++trans_men(ets:info(ets_role_temp,memory))++"\n"++
            draw_line()
    end.




draw_line() ->
    "---------------------------------------------------------------------"++
    "---------------------------------------------------------------------\n".

draw_line_2() ->
    "---------------------------------------------------------------------"++
    "---------------------------------------------------------------------"++
    "---------------------------------------------------------------------\n".

show_sys_memory() ->
    Fun = fun({Type,Value}) ->
        {Type,Value,trans_men(Value)}
    end,
    Infos = lists:map(Fun,erlang:memory()),
    io_lib:format("~p",[Infos]).


trans_men(Mem) ->
    % WordSize = erlang:system_info(wordsize),
    Num = Mem/1024,
    case Num > 0 of
        true ->
            case Num > 1024 of
                true ->
                    hd(io_lib:format("~.1f", [Num/1024]))++"m";
                false ->
                    hd(io_lib:format("~.1f", [Num]))++"k"
            end;
        false ->
            " "
    end.

trans_size(Size) ->
    case Size > 0 of
        true ->
            integer_to_list(Size);
        false ->
            " "
    end.

trans_time(Num) ->
    case Num >=10 of
        true ->
             integer_to_list(Num);
        false ->
            "0"++integer_to_list(Num)
    end.

cal_mem_size(EtsRef) ->
    case ets:info(EtsRef) of
        undefined ->
            {0,0};
        EtsInfos ->
            {_,Mem} = lists:keyfind(memory,1,EtsInfos),
            {_,Size} = lists:keyfind(size,1,EtsInfos),
            {Mem,Size}
    end.

cal_tot_info(Infos) ->
    Fun = fun(Info,{Mem,Size,IndexMem,IndexSize,UpMem,UpSize}) ->
        {Mem+Info#tab_info.memory,Size+Info#tab_info.size,IndexMem+Info#tab_info.index_memeory,
        IndexSize+Info#tab_info.index_size,UpMem+Info#tab_info.up_memory,UpSize+Info#tab_info.up_size}
    end,
    {Mem,Size,IndexMem,IndexSize,UpMem,UpSize} = lists:foldl(Fun,{0,0,0,0,0,0},Infos),
    #tab_info{
                name    = total,
                memory  = Mem,
                size    = Size,
                index_memeory = IndexMem,
                index_size = IndexSize,
                up_memory = UpMem,
                up_size    = UpSize
            }.


%%-----------------------analyse_tool_2-----------------------------------------
analyse_times(LimitTimes) ->
    analyse_times(LimitTimes,true).

analyse_times(LimitTimes,NeedDetail) ->
    ets:delete_all_objects(ets_log_times_temp),
    Tabs = map_data:tabs(),
    Fun = fun(Tab) ->
        analyse_tab_times(Tab,LimitTimes,NeedDetail)
    end,
    lists:map(Fun,Tabs),
    analyse_tab_times_tot(LimitTimes,NeedDetail).


analyse_tab_times(Tab,LimitTimes,NeedDetail) ->
    LogEtsRef = cache_util:make_log_ref(Tab),
    Fun = fun(Info,Content) ->
        calInfo(Tab,Info),
        case check_times_limit(Info,LimitTimes) of
            true ->
                Content ++ make_times_info_str(Info);
            false ->
                Content
        end
    end,
    FunSort = fun(Info1,Info2) ->
        Info1#tab_log_info.time < Info2#tab_log_info.time
    end,
    case NeedDetail of
        true ->
            Content = lists:foldl(Fun,make_title_2(),lists:sort(FunSort,ets:tab2list(LogEtsRef))),
            Dir = "./analyse/cache_times",
            file:make_dir(Dir),
            file:write_file(Dir++"/"++atom_to_list(Tab),Content);
        false ->
            void
    end.


analyse_tab_times_tot(LimitTimes,NeedDetail) ->
    LogEtsRef = ets_log_times_temp,
    Fun = fun(Info,Content) ->
        case check_times_limit(Info,LimitTimes) of
            true ->
                Content ++ make_times_info_str_tot(Info);
            false ->
                Content
        end
    end,
    FunSort = fun(Info1,Info2) ->
        Info1#tab_log_info.time < Info2#tab_log_info.time
    end,
    Content = lists:foldl(Fun,make_title_3(),lists:sort(FunSort,ets:tab2list(LogEtsRef))),
    Dir = "./analyse/cache_times",
    file:make_dir(Dir),
     {{Y,M,D},{H,Mi,S}} = calendar:local_time(),
     Date = integer_to_list(Y)++"_"++integer_to_list(M)++"_"++trans_time(D),
    Time = trans_time(H)++":"++trans_time(Mi)++":"++trans_time(S),
    file:write_file(Dir++"/"++Date++"_"++Time++".erl",Content).


calInfo(Tab,Info) ->
    case ets:lookup(ets_log_times_temp,Info#tab_log_info.time) of
        [] ->
            NewInfo = Info#tab_log_info{
                        max_insert = select_max([],Tab,Info#tab_log_info.insert_times),
                        max_update = select_max([],Tab,Info#tab_log_info.update_times),
                        max_select = select_max([],Tab,Info#tab_log_info.select_times),
                        max_delete = select_max([],Tab,Info#tab_log_info.delete_times)
            },
            ets:insert(ets_log_times_temp,NewInfo);
        [OldInfo] ->
            NewInfo = #tab_log_info{time = OldInfo#tab_log_info.time,
                update_times = OldInfo#tab_log_info.update_times + Info#tab_log_info.update_times,
                select_times = OldInfo#tab_log_info.select_times + Info#tab_log_info.select_times,
                insert_times = OldInfo#tab_log_info.insert_times + Info#tab_log_info.insert_times,
                delete_times = OldInfo#tab_log_info.delete_times + Info#tab_log_info.delete_times,
                max_insert = select_max(OldInfo#tab_log_info.max_insert,Tab,Info#tab_log_info.insert_times),
                max_update = select_max(OldInfo#tab_log_info.max_update,Tab,Info#tab_log_info.update_times),
                max_select = select_max(OldInfo#tab_log_info.max_select,Tab,Info#tab_log_info.select_times),
                max_delete = select_max(OldInfo#tab_log_info.max_delete,Tab,Info#tab_log_info.delete_times)
            },
            ets:insert(ets_log_times_temp,NewInfo)
    end.

select_max(List,Tab,Num) ->
    case Num > 0 of
        true ->
            FunSort = fun({T1,N1},{T2,N2}) ->
                N1 > N2
            end,
            List1 = lists:sort(FunSort,List++[{Tab,Num}]),
            case length(List1) > 3 of
                true ->
                    {NewList,_} = lists:split(3,List1),
                    NewList;
                false ->
                    List1
            end;
        false ->
            List
    end.


check_times_limit(Info,LimitTimes) ->
    Info#tab_log_info.update_times +
    Info#tab_log_info.select_times +
    Info#tab_log_info.insert_times +
    Info#tab_log_info.delete_times >= LimitTimes.

make_times_info_str(Info) ->
    {{_,_,_},{H,M,S}} = calendar:gregorian_seconds_to_datetime(Info#tab_log_info.time),
    Time = trans_time(fix_hour(H))++":"++trans_time(M)++":"++trans_time(S),
    Update_times = integer_to_list(Info#tab_log_info.update_times),
    Select_tiems = integer_to_list(Info#tab_log_info.select_times),
    Insert_times = integer_to_list(Info#tab_log_info.insert_times),
    Delete_times = integer_to_list(Info#tab_log_info.delete_times),

    Time++lists:flatten(lists:duplicate(25-length(Time)," "))++
    Update_times++lists:flatten(lists:duplicate(20-length(Update_times)," "))++
    Select_tiems++lists:flatten(lists:duplicate(20-length(Select_tiems)," "))++
    Insert_times++lists:flatten(lists:duplicate(20-length(Insert_times)," "))++
    Delete_times++lists:flatten(lists:duplicate(20-length(Delete_times)," "))++
    "\n".

make_times_info_str_tot(Info) ->
     {{_,_,_},{H,M,S}} = calendar:gregorian_seconds_to_datetime(Info#tab_log_info.time),
    Time = trans_time(fix_hour(H))++":"++trans_time(M)++":"++trans_time(S),
    Update_times = integer_to_list(Info#tab_log_info.update_times)++" ("++format_t_list(Info#tab_log_info.max_update)++")",
    Select_tiems = integer_to_list(Info#tab_log_info.select_times)++" ("++format_t_list(Info#tab_log_info.max_select)++")",
    Insert_times = integer_to_list(Info#tab_log_info.insert_times)++" ("++format_t_list(Info#tab_log_info.max_insert)++")",
    Delete_times = integer_to_list(Info#tab_log_info.delete_times)++" ("++format_t_list(Info#tab_log_info.max_delete)++")",

    Time++lists:flatten(lists:duplicate(25-length(Time)," "))++
    Update_times++lists:flatten(format_times(Update_times))++
    Select_tiems++lists:flatten(format_times(Select_tiems))++
    Insert_times++lists:flatten(format_times(Insert_times))++
    Delete_times++lists:flatten(format_times(Delete_times))++
    "\n".


format_times(Times) ->
    case length(Times) < 50 of
        true ->
            lists:duplicate(50-length(Times)," ");
        false ->
            " "
    end.



format_t_list(List) ->
    Fun = fun({Tab,Num},Content) ->
        Content++atom_to_list(Tab)++":"++integer_to_list(Num)++" "
    end,
    lists:foldl(Fun,"",List).


fix_hour(H) ->
    case H+8 >= 24 of
        true ->
            H + 8 - 24;
        false ->
            H + 8
    end.


make_title_2() ->
    Time = "time",
    Update_times  = "update",
    Select_tiems = "select",
    Insert_times = "insert",
    Delete_times = "delete",

    Time++lists:flatten(lists:duplicate(25-length(Time)," "))++
    Update_times++lists:flatten(lists:duplicate(20-length(Update_times)," "))++
    Select_tiems++lists:flatten(lists:duplicate(20-length(Select_tiems)," "))++
    Insert_times++lists:flatten(lists:duplicate(20-length(Insert_times)," "))++
    Delete_times++lists:flatten(lists:duplicate(20-length(Delete_times)," "))++
    "\n"++draw_line().

make_title_3() ->
    Time = "time",
    Update_times  = "update",
    Select_tiems = "select",
    Insert_times = "insert",
    Delete_times = "delete",

    Time++lists:flatten(lists:duplicate(25-length(Time)," "))++
    Update_times++lists:flatten(lists:duplicate(50-length(Update_times)," "))++
    Select_tiems++lists:flatten(lists:duplicate(50-length(Select_tiems)," "))++
    Insert_times++lists:flatten(lists:duplicate(50-length(Insert_times)," "))++
    Delete_times++lists:flatten(lists:duplicate(50-length(Delete_times)," "))++
    "\n"++draw_line_2().

purge_lru(_Tab, _EtsRef, _UpdateEtsRef, _MaxSize, RemTimes) when RemTimes =< 0 ->
    ok;
%% MaxSize =< 0 说明LRU被禁用了……
purge_lru(_Tab, _EtsRef, _UpdateEtsRef, MaxSize, _RemTimes) when MaxSize =< 0 ->
    ok;
purge_lru(Tab, EtsRef, UpdateEtsRef, MaxSize, RemTimes) ->
    case ets:info(EtsRef, size) > MaxSize of
        true ->
            case lru:get_lru(EtsRef) of
                undefined ->
                    ok;
                {Key, _AccessTime} ->
                    remove_from_cache(Tab, Key, EtsRef, UpdateEtsRef),
                    purge_lru(Tab, EtsRef, UpdateEtsRef, MaxSize, RemTimes - 1)
            end;
        false ->
            ok
    end.

access_lru(EtsRef, Key, CacheOpt) ->
    case CacheOpt#cache_opt.max_objects > 0 of
        true ->
            lru:access(EtsRef, Key);
        false ->
            void
    end.

delete_lru(EtsRef, Key, CacheOpt) ->
    case CacheOpt#cache_opt.max_objects > 0 of
        true ->
            lru:delete(EtsRef, Key);
        false ->
            void
    end.



