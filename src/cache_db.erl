%% 2013-6-15
%% mail:laojiajie@gmail.com
-module(cache_db).

-include("log.hrl").
-include("cache.hrl").

-export([start_link/1]).

-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

-define(GC_BASE_INTER_TIME, 60 * 1000).     % 单位毫秒
-define(GC_RAND_INTER_TIME, 60 * 9 * 1000). % 单位毫秒

-compile(export_all).

 % #db_state{tab = Tab,
	% 		index = Index,
	% 		ets = Ets_ref,
	% 		update_ets = Update_ets_ref,
	%		update_timer = none},

start_link({DBState,Time_start_update}) ->
	gen_server:start_link({local, DBState#db_state.ref}, ?MODULE, [DBState,Time_start_update], []).

init([DBState,Time_start_update]) ->
	erlang:process_flag(trap_exit, true),
	erlang:process_flag(priority, high), 

	{ok,TRef} = timer:apply_after(Time_start_update, ?MODULE, 
					  update_to_db, [DBState#db_state.ref]),

	GCTime = rand_gc_time(DBState),
	erlang:send_after(GCTime, self(), gc_timer),

    {ok, DBState#db_state{update_timer = TRef}}.


update_to_db(Ref) ->
	gen_server:cast(Ref,update_to_db).

update_all_to_db(Ref) ->
	gen_server:cast(Ref,update_all_to_db).

%% no use interface
gc_update(Ref,Key) ->
	gen_server:call(Ref,{gc_update,Key}).

insert_to_db(Ref,Record) ->
	gen_server:cast(Ref,{insert_to_db,Record}).

delete_in_db(Ref,Key) ->
	gen_server:cast(Ref,{delete_in_db,Key}).

cache_update_notify(Ref,Key,OldRecord,Record) ->
	gen_server:cast(Ref,{cache_update_notify,Key,OldRecord,Record}).


handle_call({gc_update,Key},_From,State) ->
	case ets:lookup(State#db_state.update_ets,Key) of
		[] ->
			true;
		[#update_index{key = Key,is_update_all = UpdateAll,update_info = UpdateInfo}] ->
			case ets:lookup(State#db_state.ets,Key) of
				[] ->
					ets:delete(State#db_state.update_ets,Key);
				[Record] ->
					case UpdateAll of
						true ->
							sql_operate:sql_update(Record);
						false ->
							case UpdateInfo of
								[] ->
									void;
								_ ->
									?C_LOG_UPDATE(State#db_state.tab),
									case sql_operate:do_execute(make_update_sql(Record,UpdateInfo)) of
										{ok,_} ->
											void;
										ERR ->
											% ?ERR(cache_db,"update single err:~p",[ERR]),
											sql_operate:sql_update(Record)
									end
							end
					end,
					ets:delete(State#db_state.update_ets,Key)	
			end
	end,
	{reply,true,State}.


handle_cast({cache_update_notify,Key,OldRecord,Record},State) ->
	case ets:lookup(State#db_state.update_ets,Key) of
		[] ->
			OldUpdateIndex = none;
		[OldUpdateIndex] ->
			void
	end,
	NewUpdateIndex = merge_update_info(OldUpdateIndex,OldRecord,Record),
	ets:insert(State#db_state.update_ets,NewUpdateIndex),
	% ?ERR(cache_db,"OldUpdateIndex = ~w,NewUpdateIndex = ~w",[OldUpdateIndex,NewUpdateIndex]),
	{noreply,State};

handle_cast(update_all_to_db,State) ->
	UpdateList = ets:tab2list(State#db_state.update_ets),
	gen_server:cast(self(),{do_update, UpdateList}),
	{noreply,State};


handle_cast(update_to_db,State) ->
	%%?DBG(db,"start update to db:~w",[State#db_state.ets]),
	UpdateList = ets:tab2list(State#db_state.update_ets),
	% ?ERR(cache_db,"update_to_db:~w size:~p",[State#db_state.ets, erlang:length(UpdateList)]),
	gen_server:cast(self(),{do_update, UpdateList}),
	timer:cancel(State#db_state.update_timer),
	{ok,TRef} = timer:apply_after(State#db_state.update_interval, ?MODULE, 
					  update_to_db, [State#db_state.ref]),
    {noreply, State#db_state{update_timer = TRef}};

handle_cast({do_update, UpdateList},State) ->
	case UpdateList of
		[] ->
			?DBG(db,"finish update to db:~w",[State#db_state.ets]),
			void;
		[#update_index{key = Key,is_update_all = UpdateAll,update_info = UpdateInfo}|Res] ->
			case ets:lookup(State#db_state.ets,Key) of
				[] ->
					ets:delete(State#db_state.update_ets,Key);
				[Record] ->
					case UpdateAll of
						true ->
							sql_operate:sql_update(Record);
						false ->
							case UpdateInfo of
								[] ->
									void;
								_ ->
									?C_LOG_UPDATE(State#db_state.tab),
									case sql_operate:do_execute(make_update_sql(Record,UpdateInfo)) of
										{ok,_} ->
											void;
										ERR ->
											?ERR(cache_db,"update single err:~p",[ERR]),
											sql_operate:sql_update(Record)
									end
							end
					end,
					ets:delete(State#db_state.update_ets,Key)	
			end,
			gen_server:cast(self(),{do_update, Res})
	end,
	{noreply,State};

handle_cast({insert_to_db,Record},State) ->
	sql_operate:sql_insert(Record),
	{noreply,State};

handle_cast({delete_in_db,Key},State) ->
	case Key of
		[Key1] ->
			ets:delete(State#db_state.update_ets,Key1);
		_ ->
			ets:delete(State#db_state.update_ets,list_to_tuple(Key))
	end,
	sql_operate:sql_delete(State#db_state.tab,Key),
	{noreply,State}.

handle_info(gc_timer,State) ->
	GCTime = rand_gc_time(State),
	erlang:garbage_collect(self()),
	erlang:send_after(GCTime, self(), gc_timer),
    CacheOpt = map_data:cache_opt(State#db_state.tab),
    case CacheOpt#cache_opt.pre_load of
        false ->
            cache:purge_lru(State#db_state.tab, State#db_state.ets, 
                            State#db_state.update_ets,
                            CacheOpt#cache_opt.max_objects, 1000);
        true ->
            void
    end,
	{noreply,State}.

terminate(_Reason,State) ->
	% ?INF(cache,"terminate reason:~w, update to db:~w",[Reason,State#db_state.update_ets]),
	UpdateList = ets:tab2list(State#db_state.update_ets),
	Fun = fun(UpdateIndex) ->
		Key = UpdateIndex#update_index.key,
		case ets:lookup(State#db_state.ets,Key) of
			[] ->
				void;
			[Record] ->
				sql_operate:sql_update(Record)
		end
	end,
	lists:map(Fun,UpdateList),
	ok.

rand_gc_time(State) ->
	NowTime = util:unixtime(),
	Seed = {erlang:phash2(erlang:phash2(State#db_state.tab)+NowTime), erlang:phash2(erlang:phash2(State#db_state.ets)+NowTime), 
			erlang:phash2(erlang:phash2(State#db_state.update_ets)+NowTime)},
	random:seed(Seed),
	RandTime = random:uniform(?GC_RAND_INTER_TIME),
	?DBG(cache, "NowTime = ~p, RandTime=~p, Tab=~p, Seed = ~p", [util:unixtime(), RandTime, State#db_state.ets, Seed]),
	RandTime + ?GC_BASE_INTER_TIME.

merge_update_info(_,none,Record) ->
	[Tab,Key|Res] = tuple_to_list(Record),
	#update_index{key = Key,
				  is_update_all = true,
				  update_info = []
				 };

merge_update_info(none,OldRecord,Record) ->
	[Tab,Key|Res] = tuple_to_list(Record),
	[Tab,Key|OldRes] = tuple_to_list(OldRecord),
	Map = map_data:map(Tab),
	[_KeyField|ResFields] = Map#map.fields,
	NewInfo = merge_update_info_helper(Res,OldRes,ResFields,[],Map#map.ignored_fields),
	case length(NewInfo) == length(Res) of
		true ->
			#update_index{key = Key,
				  is_update_all = true,
				  update_info = []
				 };
		false ->
			#update_index{key = Key,
				  is_update_all = false,
				  update_info = NewInfo
				 }
	end;

merge_update_info(OldUpdateIndex,OldRecord,Record) when OldUpdateIndex#update_index.is_update_all == true ->
	OldUpdateIndex;

merge_update_info(OldUpdateIndex,OldRecord,Record) ->
	[Tab,Key|Res] = tuple_to_list(Record),
	[Tab,Key|OldRes] = tuple_to_list(OldRecord),
	Map = map_data:map(Tab),
	[_KeyField|ResFields] = Map#map.fields,
	NewInfo = merge_update_info_helper(Res,OldRes,ResFields,OldUpdateIndex#update_index.update_info,Map#map.ignored_fields),
	case length(NewInfo) == length(Res) - length(Map#map.ignored_fields) of
		true ->
			#update_index{key = Key,
				  is_update_all = true,
				  update_info = []
				 };
		false ->
			#update_index{key = Key,
				  is_update_all = false,
				  update_info = NewInfo
				 }
	end.


merge_update_info_helper([],[],[],Info,_) ->
	Info;

merge_update_info_helper([Value|Res],[OldValue|OldRes],[Field|ResFields],Info,IgnoreFields) ->
	case Value == OldValue of
		true ->
			merge_update_info_helper(Res,OldRes,ResFields,Info,IgnoreFields);
		false ->
			case lists:member(Field,IgnoreFields) of
				false ->
					merge_update_info_helper(Res,OldRes,ResFields,merge_info(Info,Field),IgnoreFields);
				true ->
					merge_update_info_helper(Res,OldRes,ResFields,Info,IgnoreFields)
			end
	end.

merge_info(Info,Field) ->
	case lists:member(Field,Info) of
		false ->
			[Field|Info];
		true ->
			Info
	end.


make_update_sql(Record,UpdateInfo) ->
	[Tab,Key|ResValue] = tuple_to_list(Record),
	SqlTabName = sql_operate:make_sql_tab(Tab),
	Map = map_data:map(Tab),
	KeysList =
	case is_tuple(Key) of
		true -> 
			tuple_to_list(Key);
		false ->
			[Key]
	end,
	[_KeyField|ResFields] = Map#map.fields,
	Where = make_where(Map#map.key_fields,KeysList,""),
	Set = make_set(UpdateInfo,Map,ResFields,ResValue,""),
	Sql = io_lib:format("UPDATE ~w SET ~ts WHERE ~ts", [SqlTabName,Set,Where]),
	% ?ERR(cache_db,"update sql = ~p",[Sql]),
	Sql.



make_where([],[],S) ->
	S;
make_where([Field|Res],[Key|ResKey],"") ->
	make_where(Res,ResKey,atom_to_list(Field)++"="++integer_to_list(Key)++" ");
make_where([Field|Res],[Key|ResKey],S) ->
	make_where(Res,ResKey,S++"AND "++atom_to_list(Field)++"="++integer_to_list(Key)++" ").

make_set([],_,_,_,S) ->
	S;
make_set([Field|Res],Map,ResFields,ResValue,"") ->
	Value = get_record_field_value(ResFields,ResValue,Field),
	Value1 =
	case lists:member(Field,Map#map.term_fields) of
		true ->
			"'"++cache_util:term_to_string(Value)++"'"; %% term
		false ->
			case lists:member(Field,Map#map.string_fields) of
				true ->
					?DBG(cache,"Value = ~w",[Value]),
					"'"++utf8_to_unicode(to_list(Value))++"'"; %% string
				false ->
					integer_to_list(Value) %% integer
			end
	end,
	make_set(Res,Map,ResFields,ResValue,atom_to_list(Field)++"="++Value1);

make_set([Field|Res],Map,ResFields,ResValue,S) ->
	Value = get_record_field_value(ResFields,ResValue,Field),
	Value1 =
	case lists:member(Field,Map#map.term_fields) of
		true ->
			"'"++cache_util:term_to_string(Value)++"'"; %% term
		false ->
			case lists:member(Field,Map#map.string_fields) of
				true ->
					?DBG(cache,"Value = ~w",[Value]),
					"'"++utf8_to_unicode(to_list(Value))++"'"; %% string
				false ->
					integer_to_list(Value) %% integer
			end
	end,
	make_set(Res,Map,ResFields,ResValue,S++", "++atom_to_list(Field)++"="++Value1).

to_list(Value) when is_binary(Value) ->
	binary_to_list(Value);
to_list(Value) when is_list(Value) ->
	Value;
to_list(Value) when is_atom(Value) ->
	atom_to_list(Value);
to_list(Value) when is_integer(Value) ->
	integer_to_list(Value).


utf8_to_unicode(List) ->
	xmerl_ucs:from_utf8(List).
	% utf8_to_unicode(List,[]).

% utf8_to_unicode([],OutPut) ->
% 	OutPut;


% utf8_to_unicode([A|[B|[C|Res]]] ,OutPut) when A >= 224  ->
% 	<<U/utf8>> = <<A,B,C>>,
% 	utf8_to_unicode(Res,OutPut++[U]);

% utf8_to_unicode([A|[B|Res]] ,OutPut) when A >= 192  ->
% 	<<U/utf8>> = <<A,B>>,
% 	utf8_to_unicode(Res,OutPut++[U]);

% utf8_to_unicode([A|Res] ,OutPut) when A < 128  ->
% 	<<U/utf8>> = <<A>>,
% 	utf8_to_unicode(Res,OutPut++[U]).




get_record_field_value([Field1|_ResFields],[Value1|_ResValues],Field)  when Field == Field1 ->
	Value1;
get_record_field_value([Field1|ResFields],[Value1|ResValues],Field)  ->
	get_record_field_value(ResFields,ResValues,Field).


