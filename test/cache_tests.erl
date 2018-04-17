-module(cache_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

cache_test_() ->
    [
         {"Cache performance",
         {setup, fun perf_test_setup/0, fun perf_test_clear/1,
          fun(_SetupData) ->
            [{timeout, 60, fun run/0}]
          end}}
    ].


perf_test_setup() ->
	application:start(emysql),
	cache_app:start(1,2).

perf_test_clear(_Arg) ->
	void.

run() ->
	{Time0,_Arg0} = timer:tc(?MODULE,do_insert,[]),
	error_logger:info_report(
                        [{module, ?MODULE}, {line, ?LINE}, {info, list_to_atom(lists:flatten(io_lib:format("insert Time = ~w",[Time0/1000000])))}]),
	timer:sleep(1000),
	{Time1,_Arg1} = timer:tc(?MODULE,do_lookup,[]),
	error_logger:info_report(
                        [{module, ?MODULE}, {line, ?LINE}, {info, list_to_atom(lists:flatten(io_lib:format("lookup Time = ~w",[Time1/1000000])))}]),
	{Time3,_Arg3} = timer:tc(?MODULE,do_lookup,[]),
	error_logger:info_report(
                        [{module, ?MODULE}, {line, ?LINE}, {info, list_to_atom(lists:flatten(io_lib:format("lookup2 Time = ~w",[Time3/1000000])))}]),
	{Time2,_Arg2} = timer:tc(?MODULE,do_update,[]),
	error_logger:info_report(
                        [{module, ?MODULE}, {line, ?LINE}, {info, list_to_atom(lists:flatten(io_lib:format("update Time = ~w",[Time2/1000000])))}]),
	{Time4,_Arg4} = timer:tc(?MODULE,do_delete,[]),
	error_logger:info_report(
                        [{module, ?MODULE}, {line, ?LINE}, {info, list_to_atom(lists:flatten(io_lib:format("delete Time = ~w",[Time4/1000000])))}]),
	{Time5,_Arg5} = timer:tc(?MODULE,do_lookup,[]),
	error_logger:info_report(
                        [{module, ?MODULE}, {line, ?LINE}, {info, list_to_atom(lists:flatten(io_lib:format("lookup3 Time = ~w",[Time5/1000000])))}]).


run2(Num,Behaviour) ->
	Fun = fun(Id) ->
		erlang:spawn(?MODULE,Behaviour,[Id,self()])
	end,
	lists:map(Fun,lists:seq(1,Num)),
	loop(Num).

loop(Num) when Num =< 1 ->
	ok;
loop(Num) ->
	receive
		_ ->
			loop(Num -1)
	end.



do_insert() ->
	Fun = fun({ID1,ID2}) ->
		cache:insert({bag,{ID1,ID2},9999})
	end,
	lists:map(Fun,[{ID1,ID2}|| ID1 <- lists:seq(1,300),ID2 <- lists:seq(1,300)]).

do_lookup() ->
	Fun = fun({ID1,ID2}) ->
		cache:lookup(bag,{ID1,ID2})
	end,
	lists:map(Fun,[{ID1,ID2}|| ID1 <- lists:seq(1,300),ID2 <- lists:seq(1,300)]).

do_update() ->
	Fun = fun({ID1,ID2}) ->
		cache:update({bag,{ID1,ID2},888})
	end,
	lists:map(Fun,[{ID1,ID2}|| ID1 <- lists:seq(1,300),ID2 <- lists:seq(1,300)]).

do_delete() ->
	Fun = fun(ID1) ->
		cache:delete(bag,ID1)
	end,
	lists:map(Fun,[ID1|| ID1 <- lists:seq(1,300)]).


insert(ID1,Pid) ->
	Fun = fun(ID2) ->
		cache:insert({bag,{ID1,ID2},9999})
	end,
	lists:map(Fun,[ID2|| ID2 <- lists:seq(1,300)]),
	Pid ! {ok}.

lookup(ID1,Pid) ->
	Fun = fun(ID2) ->
		cache:lookup(bag,{ID1,ID2})
	end,
	[Arg|_REs] = lists:map(Fun,[ID2|| ID2 <- lists:seq(1,300)]),
	% ?INF(cache,"Args = ~w",[Arg]),
	Pid ! {ok}.

update(ID1,Pid) ->
	Fun = fun(ID2) ->
		cache:update({bag,{ID1,ID2},ID1})
	end,
	lists:map(Fun,[ID2|| ID2 <- lists:seq(1,300)]),
	Pid ! {ok}.

delete(ID1,Pid) ->
	Fun = fun(ID2) ->
		cache:delete(bag,{ID1,ID2})
	end,
	lists:map(Fun,[ID2|| ID2 <- lists:seq(1,300)]),
	Pid ! {ok}.

check() ->
	{Time,_Arg3} = timer:tc(?MODULE,do_check,[]),
	error_logger:info_report(
                        [{module, ?MODULE}, {line, ?LINE}, {info, list_to_atom(lists:flatten(io_lib:format("Time = ~w",[Time/1000000])))}]).

do_check() ->
	Fun = fun() ->
		cache:lookup(item,12)
	end,
	lists:foreach(Fun,lists:seq(1,10000)).