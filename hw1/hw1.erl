%!/usr/bin/env erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% CS521/hw1 %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Ehsan MohyedinKermani

-module(hw1).
-export([nthtail/2, prefix/2, search/2]). % for question 1
-export([time_it/1, measure_subtract/1, subtract/2, 
		subtract_helper/2, measure_fast_subtract/1]). % for question 2
%% 1.a)
% nthtail(N, List) -> Tail
% @return the N application of tl to List
nthtail(0, List) when is_list(List) -> 
	List;
nthtail(N, [_|Tail]) when is_integer(N), N > 0, N =< 1 + length(Tail) -> 
	nthtail(N - 1, Tail);
nthtail(_, _) ->  
	error(function_clause).

%% 1.b)
% prefix(List1, List2) -> boolean()
% @return true iff List1 is a prefix of List2 and fails 
% for otherwise and for other inputs.
prefix([], List) when is_list(List) -> 
	true;
prefix(List1, List2) when length(List1) > length(List2) ->
	false;
prefix([H1|T1], [H2|T2]) ->
	case H1 =:= H2 of
		true -> prefix(T1, T2);
		false -> false
	end;
prefix(A, B) ->
	error("no function clause matching", [{A, B}]).

%% 1.c)
% search(List1, List2) -> [integer()]
% @return a list of all integers, N, s.t. List1 is a prefix of List2
search(List1, List2) when is_list(List1), is_list(List2) -> 
	case length(List1) =< length(List2) of
		true -> [N || N <- lists:seq(1, length(List2)), 
				prefix(List1, lists:sublist(List2, N, length(List2)))];
		false -> []
	end; 
search(A, B) ->
	error("no function clause matching", [{A, B}]).

%% 2.a)
% measure the execution time
time_it(F) ->
	Start = now(),
	F(),
	End = now(),
	Diff = timer:now_diff(End, Start) / 1000,
	io:format("The operation took: ~p milliseconds~n", [Diff]).

measure_subtract(List)->
	case is_list(List) of 
		true -> [{N, hw1:time_it(fun() -> 
					lists:seq(1, N) -- lists:seq(N, 1, -1) end)} || N <- List];
		false -> error("input must be a list!", [{List}])
	end.
% List = lists:seq(1000, 10000, 1000) ++ lists:seq(10000, 50000, 10000). 
% measure_subtract(List).

%The operation took: 6.305 milliseconds
%The operation took: 20.561 milliseconds
%The operation took: 30.497 milliseconds
%The operation took: 34.553 milliseconds
%The operation took: 40.282 milliseconds
%The operation took: 54.131 milliseconds
%The operation took: 72.5 milliseconds
%The operation took: 94.614 milliseconds
%The operation took: 119.924 milliseconds
%The operation took: 147.419 milliseconds
%The operation took: 148.033 milliseconds
%The operation took: 587.956 milliseconds
%The operation took: 1315.765 milliseconds
%The operation took: 2341.231 milliseconds
%The operation took: 3647.631 milliseconds

%% 2.b) See hw1.pdf
%% 2.c)
% subtract(List1, List2) -> List3
% @return fast list difference
subtract(List, []) when is_list(List) -> List;
subtract([], List) when is_list(List) -> [];
subtract(List1, List2) when is_list(List1), is_list(List2) ->
	{Result,_} = lists:unzip(lists:sort(
							fun({_, B1}, {_, B2}) -> B1 =< B2 end,
								hw1:subtract_helper(
									lists:sort(lists:zip(
												List1, lists:seq(1, length(List1)))), 
									lists:sort(List2)))),
	Result;
subtract(A, B) ->
	error("no function clause matching", [{A, B}]).

subtract_helper(List, []) -> List;
subtract_helper([], _) -> [];
subtract_helper([{H1A, H1B}|T1], [H2|T2]) ->
	if H1A > H2 -> subtract_helper([{H1A, H1B}|T1], T2);
	   H1A < H2 -> [{H1A, H1B}|subtract_helper(T1, [H2|T2])];
	   H1A =:= H2 -> subtract_helper(T1, T2)
	end.
%% 2.d) See hw1.pdf
% List = lists:seq(1000, 10000, 1000) ++ lists:seq(10000, 50000, 10000).
measure_fast_subtract(List)->
	case is_list(List) of 
		true -> [{N, hw1:time_it(fun() -> 
					hw1:subtract(lists:seq(1, N),lists:seq(N, 1, -1)) end)} || N <- List];
		false -> error("input must be a list!", [{List}])
	end.
% hw1:measure_fast_subtract(List).

%The operation took: 0.112 milliseconds
%The operation took: 0.476 milliseconds
%The operation took: 0.481 milliseconds
%The operation took: 0.787 milliseconds
%The operation took: 0.72 milliseconds
%The operation took: 0.97 milliseconds
%The operation took: 1.331 milliseconds
%The operation took: 1.521 milliseconds
%The operation took: 1.725 milliseconds
%The operation took: 1.794 milliseconds
%The operation took: 1.461 milliseconds
%The operation took: 3.018 milliseconds
%The operation took: 5.563 milliseconds
%The operation took: 9.525 milliseconds
%The operation took: 10.133 milliseconds
	
	
