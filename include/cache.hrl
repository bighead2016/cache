-ifndef(__CACHE_HRL__).
-define(__CACHE_HRL__, true).

-define(__CACHE_LOG_DEBUG__, true).

-define(DB_HASH_NUM,  32).
-define(DB_UPDATE_INTERVAL, 30*60*1000).
-define(CACHE_GC_GAP,		6*1000).
-define(CACHE_ANALYSE_GAP,	30*60*1000).
-define(CACHE_ANALYSE_MEN,  300).			%% 内存大于200M进行cache分析

-define(GC_CHECK_LIST,	[{mod_account,is_offline}]).			%% gc检查
-define(GC_OPERATE,		[{mod_role,gc},{mod_town_combo,gc},{cache,gc}]).		%% gc操作（注：cache:gc(Key)要放最后

-ifdef(__CACHE_LOG_DEBUG__).

-define(C_LOG_UPDATE(Tag),       	cache_analyse:log_update(Tag)).
-define(C_LOG_INSERT(Tag), 			cache_analyse:log_insert(Tag)).
-define(C_LOG_SELECT(Tag), 			cache_analyse:log_select(Tag)).
-define(C_LOG_DELETE(Tag), 			cache_analyse:log_delete(Tag)).

-endif.

-ifndef(__CACHE_LOG_DEBUG__).

-define(C_LOG_UPDATE(Tag),       	void).
-define(C_LOG_INSERT(Tag), 			void).
-define(C_LOG_SELECT(Tag), 			void).
-define(C_LOG_DELETE(Tag),  		void).

-endif.


% -record(map, {ets_tab = none,	%% 对应的ets表
% 			  sql_tab = "",		%% 对应的sql
% 			  key_classic = 1,	%% 表关键字的类别
% 			  key_fields = [],	%% slq关键字的名称列表
% 			  fields = [],		%% ets表中的所有字段名
% 			  fields_spec,		%% 该map中的记录所对应的xxx_types记录
% 			  ignored_fields = [] %% fields中该忽略掉的字段
% 			  }).
-record(map, {
				key_fields     = [],
				term_fields    = [],
				string_fields  = [],
				fields = [],
				ignored_fields = [],
				fields_length  = []}).

-record(update_index,{key = none,
					  is_update_all = false,
					  update_info = []
					 }).

-record(db_null_index,{key = 0,
					   time = 0}).

-record(cache_opt, {
		pre_load = false,				%% 是否要预加载数据
        compressed = false,             %% 是否压缩ETS表
        max_objects = 0,                %% ETS中保存的最大数据量
        need_gc = true 					%% 是否gc 	
	}).

-record(db_state,{tab = none,
				  index = none,
				  ref = none,
				  ets = none,
				  update_ets = none,
				  update_timer = none,
				  update_interval = none}).

-record(tab_info,{
			name  	= none,
			memory 	= 0,
			size 	= 0,
			index_memeory = 0,
			index_size = 0,
			up_memory = 0,
			up_size    = 0
		}).

-record(tab_log_info,{
			time = 0,
			insert_times = 0,
			update_times = 0,
			select_times = 0,
			delete_times = 0,
			max_insert = [],
			max_update = [],
			max_select = [],
			max_delete = []
	}).


-endif.
