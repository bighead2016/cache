-module(cache_app).

-include("cache.hrl").
-include("log.hrl").

-behaviour(application).

-export([
     start/2,
     stop/1
     ]).

start(_,_) ->
    case cache_sup:start_link() of  %% 启动服务器根监督者cache_sup
      {ok, Pid} ->
          start_cache(),        %% 根监督者cache_sup下开启工作者
          do_preload(),
          {ok, Pid};
      Other ->
          {error, Other}
    end.

stop(_State) ->
    ok.


%%-------------------local fun--------------------------------------
start_cache() ->
  make_gc_util(),
  sql_operate:add_pool(),  %% creat connecting pool
  sql_operate:init_tabs(), %% creat db tables
  Tabs = map_data:tabs(),
    ets:new(ets_tmp_cache_for_pre, [named_table,{keypos, 1},public]),
    ets:new(ets_log_times_temp,[named_table,public,set,{keypos,#tab_log_info.time}]),
    ets:new(ets_cache_sys_info,[named_table,{keypos, 1},public]),
  Map_data_num = length(Tabs)*?DB_HASH_NUM,
  TotUpdateTime = ?DB_UPDATE_INTERVAL,
  Interval = TotUpdateTime div Map_data_num,
  sql_operate:prepare_create(),
  Fun = fun({Tab, Index},Num) -> 
    case Index == 1 of
      true ->
        sql_operate:prepare(Tab);  %% sql prepare
      false ->
        void
    end,
    {EtsRef,UpdateEtsRef} = init_ets(Tab, Index),

    DBState = #db_state{tab = Tab,
              ref = cache_util:make_db_hash_ref(Tab,Index),
              index = Index,
              ets = EtsRef,
              update_ets = UpdateEtsRef,
              update_interval = ?DB_UPDATE_INTERVAL},


    % {ok, _APid} = supervisor:start_child(cache_sup, [{DBState,Interval*Num}]),

    supervisor:start_child(cache_sup,
        {DBState#db_state.ref, {cache_db, start_link, [{DBState,Interval*Num}]},
         transient, 50000, worker, [cache_db]}),

    Num+1
  end,
  List = [{Tab, Index} || Index <- lists:seq(1,?DB_HASH_NUM),Tab <- Tabs],
  lists:foldl(Fun,1,List),
  supervisor:start_child(cache_sup,
  {cache_gc, {cache_gc, start_link, [[]]},
   transient, 5000, worker, [cache_gc]}),
  supervisor:start_child(cache_sup,
  {cache_analyse, {cache_analyse, start_link, [[]]},
   transient, 5000, worker, [cache_analyse]}).

do_preload() ->
  Tabs = map_data:tabs(),
  Fun = fun(Tab) ->
      Opt = map_data:cache_opt(Tab),
      case Opt#cache_opt.pre_load of
        true ->
          {ok,Records} = sql_operate:sql_select_all(Tab),
          [insert_to_hash_ets(Record)||Record <- Records];
        false ->
          void
      end
  end,
  lists:map(Fun,Tabs).

insert_to_hash_ets(Record) ->
  [Tab,Key|_Res] = tuple_to_list(Record),
  Map = map_data:map(Tab),
  Index = cache_util:do_key_hash(Key),
  EtsRef = cache_util:make_hash_ref(Tab,Index),
  DBRef = cache_util:make_db_hash_ref(Tab,Index),
  ets:insert(EtsRef,Record),
  case length(Map#map.key_fields) of
    1 ->
      void;
    _ ->
      EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
      [Id | _] = erlang:tuple_to_list(Key),
      case ets:lookup(EtsIndexRef,Id) of
        [] ->
          ets:insert(EtsIndexRef,{Id,[Key],1});
        [{Id,KeyTuples,IsSearchDB}] ->
          ets:insert(EtsIndexRef,{Id,lists:usort([Key|KeyTuples]),1})
      end
  end.

%% init: ets,ets_index,update_ets,db_null_ets
init_ets(Tab, Index) ->
    UpdateEtsRef = cache_util:make_hash_update_ref(Tab,Index),
    EtsRef = cache_util:make_hash_ref(Tab,Index),
    DBNullEtsRef = cache_util:make_hash_dbnull_ref(Tab,Index),
    Map = map_data:map(Tab),
    case length(Map#map.key_fields) of
        1 -> void;
        _ ->
            EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
            ets:new(EtsIndexRef, [named_table, public, set, {keypos,1}])
    end,
    Opt = map_data:cache_opt(Tab),
    case Opt#cache_opt.compressed of
        false ->
            ets:new(EtsRef, [named_table, public, set, {keypos,2}]);
        true ->
            ets:new(EtsRef, [named_table, public, set, {keypos,2}, compressed])
    end,
    ets:new(UpdateEtsRef, [named_table, public, set, {keypos, #update_index.key}]),
    ets:new(DBNullEtsRef,[named_table,public,set,{keypos,#db_null_index.key}]),
    case Index == 1 of
      true ->
        LogEtsRef = cache_util:make_log_ref(Tab),
        ets:new(LogEtsRef,[named_table,public,set,{keypos,#tab_log_info.time}]);
      false ->
        void
    end,
    case Opt#cache_opt.max_objects > 0 of
        true  -> lru:init(EtsRef);
        false -> void
    end,
    {EtsRef,UpdateEtsRef}.


make_gc_util() ->
    {Mod, Code} = dynamic_compile:from_string(make_src()),
    code:load_binary(Mod, "cache_gc_util.erl", Code).

make_src() ->
  "-module(cache_gc_util).
    -export([gc_check/1,do_gc/1]).
    gc_check(Key) -> " ++ make_check() ++ "
    do_gc(Key) ->"++  make_do_gc()  ++ "
    ".


make_check() ->
  Fun = fun({Mod,Fun},Src) ->
    Src++atom_to_list(Mod)++":"++atom_to_list(Fun)++"(Key) andalso"
  end,
  Src = lists:foldl(Fun,"",?GC_CHECK_LIST),
  Src ++ " true .".

make_do_gc() ->
  Fun = fun({Mod,Fun},Src) ->
    Src++atom_to_list(Mod)++":"++atom_to_list(Fun)++"(Key),
    "
  end,
  Src = lists:foldl(Fun,"",?GC_OPERATE),
  Src ++ "ok.".




 
