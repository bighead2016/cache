%% 2013-6-13
%% mail:laojiajie@gmail.com
-module(cache_util).

-include("cache.hrl").

-export([make_hash_ref/2,make_hash_update_ref/2]).

-compile(export_all).

%%--------------------ets-------------------------------
make_hash_ref(Tab,Index) -> %% item1,item2
	list_to_atom_helper(atom_to_list(Tab)++integer_to_list(Index)).

get_key_index_ets(Tab,Index) -> %% index_item1,index_item2
	list_to_atom_helper("index_"++atom_to_list(Tab)++integer_to_list(Index)).

make_hash_update_ref(Tab,Index) -> %% update_item1
	list_to_atom_helper("update_"++atom_to_list(Tab)++integer_to_list(Index)).

make_hash_dbnull_ref(Tab,Index) -> %% update_item1
	list_to_atom_helper("dbnull_"++atom_to_list(Tab)++integer_to_list(Index)).

%%--------------------pid-------------------------------
make_db_hash_ref(Tab,Index) -> %% db_item1,db_item2
	list_to_atom_helper("db_"++atom_to_list(Tab)++integer_to_list(Index)).

make_cache_hash_ref(Tab,Index) -> %% cache_item1,cache_item2
	list_to_atom_helper("cache_"++atom_to_list(Tab)++integer_to_list(Index)).

make_log_ref(Tab) ->
	list_to_atom_helper("cache_log_"++ erlang:atom_to_list(Tab)).


do_key_hash(Key) when is_tuple(Key) ->
	erlang:phash(element(1,Key),?DB_HASH_NUM);
do_key_hash(Key) ->
	erlang:phash(Key,?DB_HASH_NUM).


list_to_atom_helper(List) -> %% consider the atom is not reclaimed
	% try erlang:list_to_existing_atom(List)
	% catch _:_ -> erlang:list_to_atom(List)
	% end.
	erlang:list_to_atom(List). 

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
	lists:flatten(io_lib:format("~w", [Term])).

%%-------------------

filterKeyTuples(Id,Key,KeyTupleList,KeyFields) ->
	case Key == Id of
		true ->
			KeyTupleList;
		false ->
			case length(tuple_to_list(Key)) == length(KeyFields) of
				true ->
					case lists:member(Key,KeyTupleList) of
						true ->
							[Key];
						false ->
							[]
					end;
				false ->
					{Id1,Id2} = Key,
					Fun = fun(KeyTuple) ->
						{Key1,Key2,_Key3} = KeyTuple,
						Id1 == Key1 andalso Id2 == Key2
					end,
					lists:filter(Fun,KeyTupleList)
			end
	end.

unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.
    