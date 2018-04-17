%% mail:laojiajie@gmail.com
%% 2013-6-13
-module(sql_operate).

-include("cache.hrl").
-include("log.hrl").

-export([prepare/1,prepare_create/0,init_create/1,init_get/1]). %% sql prepare

-export([sql_select/2,sql_delete/2,sql_insert/1,sql_update/1,sql_select_all/1]). %% sql operation

-export([add_pool/0,init_tabs/0,do_execute/1]).

-export([make_sql_tab/1, make_fields/2,test_init/1,test_get/1,test/1]).


test_init(Num) ->
	Index = (util:unixtime() rem 10000) * 1000,
	Fun = fun(Id) ->
		erlang:spawn(?MODULE,init_create,[Id])
	end,
	lists:map(Fun,lists:seq(Index,Index+Num-1)).


test_get(Num) ->
	Index = (util:unixtime() rem 10000) * 1000,
	Fun = fun(Id) ->
		erlang:spawn(?MODULE,init_get,[Id])
	end,
	lists:map(Fun,lists:seq(Index,Index+Num-1)).

test(Num) ->
	Index = (util:unixtime() rem 10000) * 1000,
	Fun = fun(Id) ->
		erlang:spawn(?MODULE,init_create,[Id])
	end,
	Fun2 = fun(Id) ->
		erlang:spawn(?MODULE,init_get,[Id])
	end,
	lists:map(Fun,lists:seq(Index,Index+Num-1)),
	timer:sleep(5000),
	lists:map(Fun2,lists:seq(Index,Index+Num-1)).


add_pool() ->
	{ok, User} = application:get_env(cache, db_user),
	{ok, Password} = application:get_env(cache, db_pass),
	{ok, Host} = application:get_env(cache, db_host),
	{ok, Port} = application:get_env(cache, db_port),
	{ok, Database} = application:get_env(cache, db_name),
	{ok, Encoding} = application:get_env(cache, db_encode),
    {ok, PoolSize} = application:get_env(cache, db_pool_size),
    ?INF(cache,"cache start: ~p",[{User,Password,Host,Port,Database,Encoding,PoolSize}]),
	emysql:add_pool(mgserver_db_pool, PoolSize, User, Password, Host, Port, Database, Encoding).
	% emysql:add_pool(mgserver_db_pool, 100,
	% 	"root", "123456", "192.168.24.159", 3306,
	% 	"zs_server", utf8).

do_execute(Sql) ->
	?C_LOG_UPDATE(account),
	case emysql:execute(mgserver_db_pool,Sql) of
		{result_packet,_,_,Data,_} ->
			{ok,Data};
		{ok_packet,_,EffectRows,_,_,_,_} ->
			{ok, [[EffectRows]]};
		{error_packet,_,_,_,Msg} ->
			?ERR(sql,"sql error when execute:~s,Info:~s",[Sql,Msg]);
		Res ->
			Res
	end.


init_tabs() ->
	Tabs = map_data:tabs(),
	Fun = fun(Tab) ->
		Map = map_data:map(Tab),
		SqlTabName = make_sql_tab(Tab),
		Sql = erlang:list_to_bitstring(io_lib:format("SELECT * from ~w Limit 1", [SqlTabName])),
		case emysql:execute(mgserver_db_pool,Sql) of
			{error_packet,_,_,<<"42S02">>,_Msg} ->
				creat_db_table(Tab,Map);
			_ ->
				void
		end
	end,
	lists:map(Fun,Tabs),
	OtherSql = map_data:init_sql(),
	case OtherSql of
		"" ->
			void;
		_ ->
			do_execute(OtherSql)
	end.


prepare(Tab) ->
	Map = map_data:map(Tab),
	SqlTabName = make_sql_tab(Tab),
	do_insert_prepare(Tab,Map,SqlTabName,Map#map.key_fields,Map#map.ignored_fields),
	do_update_prepare(Tab,Map,SqlTabName,Map#map.key_fields,Map#map.ignored_fields),
	case Map#map.key_fields of
		[ID1] ->
			do_prepare(Tab,SqlTabName,ID1);
		[ID1,ID2] ->
			do_prepare(Tab,SqlTabName,ID1),
			do_prepare(Tab,SqlTabName,ID1,ID2);
		[ID1,ID2,ID3] ->
			do_prepare(Tab,SqlTabName,ID1),
			do_prepare(Tab,SqlTabName,ID1,ID2),
			do_prepare(Tab,SqlTabName,ID1,ID2,ID3)
	end.


init_get(AccountID) ->
	Tabs =  map_data:tabs(),
	Fun = fun(Tab,{STabs,Content}) ->
		Map = map_data:map(Tab),
		case Map#map.key_fields of
			[account_id|_] ->
				SqlTabName = make_sql_tab(Tab),
				{STabs++[Tab],Content++io_lib:format("SELECT * from ~w WHERE ~w = ~w;", [SqlTabName,account_id,AccountID])};
			_ ->
				{STabs,Content}
		end
	end,
	{STabs,Content} = lists:foldl(Fun,{[],""},Tabs),
	Now1 = util:unixtime(),
	Results = emysql:execute(mgserver_db_pool,Content),
	Now2 = util:unixtime(),
	?ERR(sql_operate,"init get sql time = ~w",[Now2 - Now1]),
	init_get_helper(AccountID,Results,STabs).
	
init_get_helper(_AccountID,[],_) ->
	void;
init_get_helper(AccountID,[Result|Results],[Tab|Tabs]) ->
	case Result of
		{result_packet,_,_,Data,_} ->
			Map = map_data:map(Tab),
			[_KeyField|Fields] = Map#map.fields,
			Records = fill_record(Tab,Data,Map,Fields,[]),
			Index = cache_util:do_key_hash(AccountID),
			EtsRef = cache_util:make_hash_ref(Tab,Index),
			DBNullEtsRef = cache_util:make_hash_dbnull_ref(Tab,Index),
			Fun = fun(Record) ->
				ets:insert(EtsRef,Record)
			end,
			lists:map(Fun,Records),
			case Records of
				[] ->
					ets:insert(DBNullEtsRef,#db_null_index{key = AccountID});
				_ ->
					void
			end,
			case length(Map#map.key_fields) of
				1 ->
					void;
				_ ->
					EtsIndexRef = cache_util:get_key_index_ets(Tab,Index),
					Fun2 = fun(Record) ->
                        [Tab,KeyTuple|_Res] = tuple_to_list(Record),
                        KeyTuple
                    end,
                    KeyTupleList = lists:map(Fun2,Records),
					ets:insert(EtsIndexRef,{AccountID,KeyTupleList,1})
			end;
		_ ->
			void
	end,
	init_get_helper(AccountID,Results,Tabs).
		

init_create(AccountID) ->
	Records = mgserver_event:create_role_records(AccountID),
	init_create(AccountID,Records).

init_create(AccountID,Records) ->
	Tabs =  map_data:tabs(),
	NotInitTabs = map_data:not_init_tab(),
	Fun = fun(Tab,Content) ->
		Map = map_data:map(Tab),
		case Map#map.key_fields of
			[Key] ->
				case Key == account_id andalso not lists:member(Tab,NotInitTabs) of
					true ->
						SqlTabName = make_sql_tab(Tab),
						Content++io_lib:format("INSERT INTO ~w (~s) VALUES (~w);", [SqlTabName,Key,AccountID]);
					false ->
						Content
				end;
			_ ->
				Content
		end
	end,
	Content1 = lists:foldl(Fun,"",Tabs),
	Fun2 = fun(Record,Content) ->
		Content++init_create_sql(Record)
	end,
	Content2 = lists:foldl(Fun2,"",Records),
	Content = Content1++Content2,
	% ?ERR(sql_operate,Content2),
	Now1 = util:unixtime(),
	case emysql:execute(mgserver_db_pool,Content) of
		ResultList when is_list(ResultList) ->
			Now2 = util:unixtime(),
			FunE = fun(Result) ->
				case Result of
					{error_packet,N,_,_,Msg} ->
						?ERR(sql_operate,"init sql err2:~w,Sql = ~w",[Msg,lists:nth(N,split_sql(Content))]);
					_ ->
						void
				end
			end,
			lists:map(FunE,ResultList);
		Res ->
			Now2 = util:unixtime(),
			?ERR(sql_operate,"init sql err:~w",[Res])
	end,
	?ERR(sql_operate,"init create sql time = ~w",[Now2 - Now1]).
	% emysql:prepare(prepare_create,erlang:list_to_bitstring(Content)),
	% ets:insert(ets_cache_sys_info,{prepare_create_num,Num}).

init_create_sql(Record) ->
	[Tab,Keys1|Res] = tuple_to_list(Record),
	SqlTabName = make_sql_tab(Tab),
	Map = map_data:map(Tab),
	Keys2 =
	case is_tuple(Keys1) of
		true -> 
			tuple_to_list(Keys1);
		false ->
			[Keys1]
	end,
	Keys = lists:map(fun(Key) -> integer_to_list(Key) end,Keys2),
	[_KeyField|ResFields] = Map#map.fields,
	FieldValues = get_field_values(Res,ResFields,Map#map.ignored_fields,Map#map.term_fields,Map#map.string_fields,[]),

	FieldsList = make_field_list(Map,Map#map.key_fields,Map#map.ignored_fields), %% put key on
	Fields = make_fields(FieldsList,""),

	[ValuesFormat] = io_lib:format("~10000p",[trans_binary(Keys++FieldValues)]),
	[_|ValuesFormat1] = ValuesFormat,
	{ValuesFormat2,_} = lists:split(length(ValuesFormat1)-1,ValuesFormat1),
	io_lib:format("INSERT INTO ~w (~s) VALUES (~s);", [SqlTabName,Fields,ValuesFormat2]).


split_sql(L) ->
    L1=lists:reverse(L),
    split_sql_a([],L1,[]).
split_sql_a([],[],Result) -> Result;
split_sql_a(R1,[],Result) -> [R1|Result];
split_sql_a(R1,[H|T],Result) ->
    if
        H==59 ->
            if
                R1==[] -> split_sql_a([],T,Result);
                true   -> split_sql_a([],T,[R1|Result])
            end;
        true -> split_sql_a([H|R1],T, Result)
    end.

trans_binary(List) ->
	Fun = fun(A) ->
		case is_binary(A) of
			true ->
				binary_to_list(A);
			false ->	
				A
		end
	end,
	lists:map(Fun,List).


prepare_create() ->
	void.


sql_select(Tab,Args) ->
	?C_LOG_SELECT(Tab),
	{result_packet,_,_,Data,_} = 
	case length(Args) of
		1 ->
			emysql:execute(mgserver_db_pool, make_select_stmt_1(Tab), Args);
		2 ->
			emysql:execute(mgserver_db_pool, make_select_stmt_2(Tab), Args);
		3 ->
			emysql:execute(mgserver_db_pool, make_select_stmt_3(Tab), Args)
	end,
		case Data of
		[] ->
			not_found;
		_ ->
			Map = map_data:map(Tab),
			[_KeyField|Fields] = Map#map.fields,
			fill_record(Tab,Data,Map,Fields,[])
	end.

sql_select_all(Tab) ->
	SqlTabName = make_sql_tab(Tab),
	Sql = erlang:list_to_bitstring(io_lib:format("SELECT * from ~w WHERE 1 = 1", [SqlTabName])),
	?C_LOG_SELECT(Tab),
	case emysql:execute(mgserver_db_pool,Sql) of
		{result_packet,_,_,Data,_} ->
			Map = map_data:map(Tab),
			[_KeyField|Fields] = Map#map.fields,
			Records = fill_record(Tab,Data,Map,Fields,[]),
			{ok,Records};
		Else ->
			?ERR(sql,"sql error when execute:~w,Info:~w",[Sql,Else])
	end.



sql_delete(Tab,Args) ->
	?C_LOG_DELETE(Tab),
	case length(Args) of
		1 ->
			emysql:execute(mgserver_db_pool, make_delete_stmt_1(Tab), Args);
		2 ->
			emysql:execute(mgserver_db_pool, make_delete_stmt_2(Tab), Args);
		3 ->
			emysql:execute(mgserver_db_pool, make_delete_stmt_3(Tab), Args)
	end.

sql_insert(Record) ->
	[Tab,Keys1|Res] = tuple_to_list(Record),
	?C_LOG_INSERT(Tab),
	Map = map_data:map(Tab),
	Keys2 =
	case is_tuple(Keys1) of
		true -> 
			tuple_to_list(Keys1);
		false ->
			[Keys1]
	end,
	Keys = lists:map(fun(Key) -> integer_to_list(Key) end,Keys2),
	[_KeyField|ResFields] = Map#map.fields,
	FieldValues = get_field_values(Res,ResFields,Map#map.ignored_fields,Map#map.term_fields,Map#map.string_fields,[]),
	?DBG(cache,"FieldValues = ~w",[FieldValues]),
	emysql:execute(mgserver_db_pool, make_insert_stmt(Tab), Keys++FieldValues,10000).


sql_update(Record) ->
	[Tab,Keys1|Res] = tuple_to_list(Record),
	?C_LOG_UPDATE(Tab),
	Map = map_data:map(Tab),
	Keys2 =
	case is_tuple(Keys1) of
		true -> 
			tuple_to_list(Keys1);
		false ->
			[Keys1]
	end,
	Keys = lists:map(fun(Key) -> integer_to_list(Key) end,Keys2),
	[_KeyField|ResFields] = Map#map.fields,
	FieldValues = get_field_values(Res,ResFields,Map#map.ignored_fields,Map#map.term_fields,Map#map.string_fields,[]),
	case emysql:execute(mgserver_db_pool, make_update_stmt(Tab), FieldValues++Keys,10000) of
		{ok_packet,_,_,_,_,_,Msg} ->
			case lists:nth(3,string:tokens(Msg," ")) of
				"0" ->   %% if no rows match,insert record
					?ERR(cache, "no rows match, Record = ~p", [Record]),
					sql_insert(Record),
					void;
				_ ->
					void
			end;
		_ ->
			void
	end.

%%------------------------------------------local fun-------------------------------------------------

list_to_atom_helper(List) ->
	% try erlang:list_to_existing_atom(List)
	% catch _:_ -> erlang:list_to_atom(List)
	% end.
	erlang:list_to_atom(List). %% qq说不会重复堆原子

make_sql_tab(Tab) ->
	list_to_atom_helper("gd_"++ erlang:atom_to_list(Tab)).

make_select_stmt_1(Tab) ->
	list_to_atom_helper("select_"++ erlang:atom_to_list(Tab)++"_1").

make_select_stmt_2(Tab) ->
	list_to_atom_helper("select_"++ erlang:atom_to_list(Tab)++"_2").

make_select_stmt_3(Tab) ->
	list_to_atom_helper("select_"++ erlang:atom_to_list(Tab)++"_3").

make_delete_stmt_1(Tab) ->
	list_to_atom_helper("delete_"++ erlang:atom_to_list(Tab)++"_1").

make_delete_stmt_2(Tab) ->
	list_to_atom_helper("delete_"++ erlang:atom_to_list(Tab)++"_2").

make_delete_stmt_3(Tab) ->
	list_to_atom_helper("delete_"++ erlang:atom_to_list(Tab)++"_3").

make_insert_stmt(Tab) ->
	list_to_atom_helper("insert_"++ erlang:atom_to_list(Tab)).

make_update_stmt(Tab) ->
	list_to_atom_helper("update_"++ erlang:atom_to_list(Tab)).

make_field_list(Map,KeyFields,IgnoreFields) ->
	[_KeyField|ResFields] = Map#map.fields,
	KeyFields++ResFields--IgnoreFields.

make_fields([],S) ->
	S;
make_fields([Field|Res],"") ->
	make_fields(Res,atom_to_list(Field));
make_fields([Field|Res],S) ->
	make_fields(Res,S++", "++atom_to_list(Field)).

make_set([],S) ->
	S;
make_set([Field|Res],"") ->
	make_set(Res,atom_to_list(Field)++"=?");
make_set([Field|Res],S) ->
	make_set(Res,S++", "++atom_to_list(Field)++"=?").

make_where([],S) ->
	S;
make_where([Field|Res],"") ->
	make_where(Res,atom_to_list(Field)++"=? ");
make_where([Field|Res],S) ->
	make_where(Res,S++"AND "++atom_to_list(Field)++"=? ").

get_field_values([],[],_IgnoreFields,_TermFields,_StringFields,NewValus) ->
	NewValus;
get_field_values([Value1|Res],[Field|ResFields],IgnoreFields,TermFields,StringFields,NewValus) ->
	case lists:member(Field,IgnoreFields) of
		true ->
			get_field_values(Res,ResFields,IgnoreFields,TermFields,StringFields,NewValus);
		false ->
			Value =
			case lists:member(Field,TermFields) of
				true ->
					cache_util:term_to_string(Value1); %% term
				false ->
					case lists:member(Field,StringFields) of
						true ->
							?DBG(cache,"Value1 = ~w",[Value1]),
							Value1; %% string
						false ->
							integer_to_list(Value1) %% integer
					end
			end,
			get_field_values(Res,ResFields,IgnoreFields,TermFields,StringFields,NewValus++[Value])
	end.

fill_record(_Tab,[],_Map,_Fields,Records) ->
	Records;
fill_record(Tab,[Data|ResData],Map,Fields,Records) ->
	KeyNum = length(Map#map.key_fields),
	{Keys,ResData1} = lists:split(KeyNum,Data),
	case KeyNum of
		1 ->
			[Key] = Keys;
		_ ->
			Key = list_to_tuple(Keys)
	end,
	Rec = value_to_rec(ResData1,Fields,Map#map.ignored_fields,Map#map.term_fields,Map#map.string_fields,[Tab]++[Key]),
	fill_record(Tab,ResData,Map,Fields,[Rec|Records]). 

value_to_rec([],[],_IgnoreFields,_TermFields,_StringFields,NewValus) ->
	list_to_tuple(NewValus);
value_to_rec(_,[],_IgnoreFields,_TermFields,_StringFields,NewValus) ->
	?ERR(cache,"db unmatch table! tab = ~w",[lists:nth(1,NewValus)]);
value_to_rec(Value,[Field|ResFields],IgnoreFields,TermFields,StringFields,NewValus) ->
	case lists:member(Field,IgnoreFields) of
		true ->
			case lists:member(Field,TermFields) of
				true ->
					value_to_rec(Value,ResFields,IgnoreFields,TermFields,StringFields,NewValus++[[]]);
				false ->
					case lists:member(Field,TermFields) of
						true ->
                            value_to_rec(Value,ResFields,IgnoreFields,TermFields,StringFields,NewValus++[<<"">>]);
						false ->
							value_to_rec(Value,ResFields,IgnoreFields,TermFields,StringFields,NewValus++[0])
					end
			end;
		false ->
			case Value of
				[Value1|Res] ->
					Val = 
					case lists:member(Field,TermFields) of
						true ->
							 cache_util:bitstring_to_term(Value1); %% term
						false ->
							case lists:member(Field,StringFields) of
								true ->
									Value1; %% string
								false ->
									Value1 %% integer
							end
					end,
					value_to_rec(Res,ResFields,IgnoreFields,TermFields,StringFields,NewValus++[Val]);
				_ ->
					?ERR(cache,"table unmatch db! tab = ~w",[lists:nth(1,NewValus)])
			end
	end.


do_prepare(Tab,SqlTabName,ID1) ->
	SelectStatement = erlang:list_to_bitstring(io_lib:format("SELECT * from ~w WHERE ~w = ?", [SqlTabName,ID1])),
	DeleteStatement = erlang:list_to_bitstring(io_lib:format("DELETE from ~w WHERE ~w = ?", [SqlTabName,ID1])),
	emysql:prepare(make_select_stmt_1(Tab),SelectStatement),
	emysql:prepare(make_delete_stmt_1(Tab),DeleteStatement).
do_prepare(Tab,SqlTabName,ID1,ID2) ->
	SelectStatement = erlang:list_to_bitstring(io_lib:format("SELECT * from ~w WHERE ~w = ? and ~w = ?", [SqlTabName,ID1,ID2])),
	DeleteStatement = erlang:list_to_bitstring(io_lib:format("DELETE from ~w WHERE ~w = ? and ~w = ?", [SqlTabName,ID1,ID2])),
	emysql:prepare(make_select_stmt_2(Tab),SelectStatement),
	emysql:prepare(make_delete_stmt_2(Tab),DeleteStatement).
do_prepare(Tab,SqlTabName,ID1,ID2,ID3) ->
	SelectStatement = erlang:list_to_bitstring(io_lib:format("SELECT * from ~w WHERE ~w = ? and ~w = ? and ~w = ?", [SqlTabName,ID1,ID2,ID3])),
	DeleteStatement = erlang:list_to_bitstring(io_lib:format("DELETE from ~w WHERE ~w = ? and ~w = ? and ~w = ?", [SqlTabName,ID1,ID2,ID3])),
	emysql:prepare(make_select_stmt_3(Tab),SelectStatement),
	emysql:prepare(make_delete_stmt_3(Tab),DeleteStatement).

do_insert_prepare(Tab,Map,SqlTabName,KeyFields,IgnoreFields) ->
	FieldsList = make_field_list(Map,KeyFields,IgnoreFields), %% put key on
	Fields = make_fields(FieldsList,""),
	ValuesFormat = make_fields(lists:map(fun(_Any) -> '?' end,FieldsList),""),
	InsertStatement = erlang:list_to_bitstring(
		io_lib:format("INSERT INTO ~w (~s) VALUES (~s)", [SqlTabName,Fields,ValuesFormat])
		),
	emysql:prepare(make_insert_stmt(Tab),InsertStatement).

do_update_prepare(Tab,Map,SqlTabName,KeyFields,IgnoreFields) ->
	FieldsList = make_field_list(Map,[],IgnoreFields),  %% no key
	Set = make_set(FieldsList,""),
	Where = make_where(KeyFields,""),
	InsertStatement = erlang:list_to_bitstring(
		io_lib:format("UPDATE ~w SET ~s WHERE ~s", [SqlTabName,Set,Where])
		),
	emysql:prepare(make_update_stmt(Tab),InsertStatement).

creat_db_table(Tab,Map) ->
	SqlTabName = make_sql_tab(Tab),
	[_KeyField|Fields] = Map#map.fields,
	FieldInfo = make_sql_key_field_info(Map#map.key_fields,"")++"PRIMARY KEY ("++make_fields(Map#map.key_fields,"")++")"++make_sql_field_info(Fields,Map#map.ignored_fields,Map#map.term_fields,Map#map.string_fields,Map#map.fields_length,""),
	Sql = erlang:list_to_bitstring(io_lib:format("CREATE TABLE ~w (~s)", [SqlTabName,FieldInfo])),
	case emysql:execute(mgserver_db_pool,Sql) of
		{ok_packet,_,_,_,_,_,_} ->
			?INF(sql,"Creat table successful! New Tab = ~w, If you want "
                 "to change the default config, please open your "
                 "sql management tool",[SqlTabName]);
		Msg ->
			Fun = fun(Arg) ->
				case is_list(Arg) of
					true ->
						list_to_atom(Arg);
					false ->
						Arg
				end
			end,
			Msg1 = lists:map(Fun,tuple_to_list(Msg)),
			?ERR(sql,"Creat table fail! Tab = ~w,Message is:~w SQL:~s",[SqlTabName,Msg1,Sql])
	end.

make_sql_key_field_info([],FieldInfo) ->
	FieldInfo;
make_sql_key_field_info([KeyField|Res],FieldInfo) ->
	make_sql_key_field_info(Res,FieldInfo++atom_to_list(KeyField)++" INT,").

make_sql_field_info([],_IgnoreFields,_TermFields,_StringFields,_FieldsLengths,S) ->
	case S of
		[] ->
			S;
		_ ->
			","++S
	end;
make_sql_field_info([Field|Res],IgnoreFields,TermFields,StringFields,FieldsLengths,"") ->
	case lists:member(Field,IgnoreFields) of
		true ->
			make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,"");
		false ->
			case lists:member(Field,TermFields) of
				true ->
					Length = 
					case lists:keyfind(Field,1,FieldsLengths) of
						false ->
							256;
						{_,L} ->
							L
					end,
					make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,atom_to_list(Field)++" VARCHAR("++integer_to_list(Length)++") NOT NULL DEFAULT '[]'"); %% term
				false ->
					case lists:member(Field,StringFields) of
						true ->
							Length = 
							case lists:keyfind(Field,1,FieldsLengths) of
								false ->
									512;
								{_,L} ->
									L
							end,
							make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,atom_to_list(Field)++" VARCHAR("++integer_to_list(Length)++") NOT NULL DEFAULT '""'"); %% string
						false ->
							make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,atom_to_list(Field)++" INT NOT NULL DEFAULT 0") %% integer
					end
			end
	end;
make_sql_field_info([Field|Res],IgnoreFields,TermFields,StringFields,FieldsLengths,S) ->
	case lists:member(Field,IgnoreFields) of
		true ->
			make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,S);
		false ->
			case lists:member(Field,TermFields) of
				true ->
					Length = 
					case lists:keyfind(Field,1,FieldsLengths) of
						false ->
							256;
						{_,L} ->
							L
					end,
					make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,S++","++atom_to_list(Field)++" VARCHAR("++integer_to_list(Length)++") NOT NULL DEFAULT '[]'"); %% term
				false ->
					case lists:member(Field,StringFields) of
						true ->
							Length = 
							case lists:keyfind(Field,1,FieldsLengths) of
								false ->
									512;
								{_,L} ->
									L
							end,
							make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,S++","++atom_to_list(Field)++" VARCHAR("++integer_to_list(Length)++") NOT NULL DEFAULT '""'"); %% string
						false ->
							make_sql_field_info(Res,IgnoreFields,TermFields,StringFields,FieldsLengths,S++","++atom_to_list(Field)++" INT NOT NULL DEFAULT 0") %% integer
					end
			end
	end.
