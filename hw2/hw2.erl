%!/usr/bin/env erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% CS521/hw2 %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Ehsan MohyedinKermani

-module(hw2).
-import(workers, [get/2, put/3, update/3]).
-import(wtree, [reduce/3, scan/5]).
-import(misc, [cut/2]).
-import(time_it, [t/1]).
-export [pi/0, degree_to_radian/1, move_pos/2, move_par/4].
-export [rle/1, rle/2].
-export [longest_run/2, longest_run_filter/2, longest_run/3, rlep/1, rlep/3].
-export [best_match/2, match_count/2, best_match_help/2, best_match_par/3, 
			best_match_help2/2, best_match_help3/2,best_match_par_help/2].
-export [move_par_test/1, move_par_test/0, longest_run_test/0, longest_run_test/1,
		 best_match_par_test/1, best_match_par_test/0].
-export [best_match_time/3, best_match_time/0, best_match_time_normalized/0,
		best_match_par_time/4].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Question 1 %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi() -> 3.1415926535897932384626433832795028841971693993751058.

degree_to_radian(Deg) -> Deg*pi()/180.0.

move_pos({X, Y, Dir}, {Turn, Dist}) ->
  NewDir = Dir+Turn,
  NewDirRad = degree_to_radian(NewDir),
  { X + Dist*math:cos(NewDirRad),
    Y + Dist*math:sin(NewDirRad),
    NewDir
  };
move_pos(Pos, []) -> Pos;
move_pos(Pos, [MoveH | MoveTail]) ->
  move_pos(move_pos(Pos, MoveH), MoveTail).

move_par(W, InitPos, MoveKey, PosKey) ->
	wtree:scan(W,
        fun(PS) -> move_pos(InitPos, workers:get(PS, MoveKey)) end,% Leaf1
        fun(PS, AccIn) -> 
        	workers:put(
        		PS,
        		PosKey,
            	element(1,
            		lists:mapfoldl(fun(Move, Acc) -> 
            			E = move_pos(Acc, Move), {E, E} end,
            		AccIn,
            		workers:get(PS, MoveKey)
            		)
            	)
          	)
        end, % Leaf2
        fun({L_X, L_Y, L_Dir}, {R_X, R_Y, R_Dir}) -> 
        		{L_X + R_X*math:cos(degree_to_radian(L_Dir)) - 
        			   R_Y*math:sin(degree_to_radian(L_Dir)),
        		 L_Y + R_X*math:sin(degree_to_radian(L_Dir)) +
        		 	   R_Y*math:cos(degree_to_radian(L_Dir)),
        		 L_Dir + R_Dir} 
        end, %Combine
        {0, 0, 0} % Acc0
      ).

move_par_test(Nworkers) ->
	InitPos = {0,0,0},
	L = [{90, 10}, {90, 10}, {2, 10}, {4, 20}, {50, -10}],
	Sequential = move_pos(InitPos, L),
	W = wtree:create(Nworkers),
	workers:update(W, data, misc:cut(L, Nworkers)),
	Par = move_par(W, InitPos, data, result),
	io:format("Sequential result is ~w~n", [Sequential]),
	io:format("Parallel result is ~w~n", [Par]),
	io:format("Intermediate results are ~w~n",
			   [lists:append(workers:retrieve(W, result))]).

move_par_test() -> move_par_test(4).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Question 2 %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
rle([]) -> [];
rle(List) when is_list(List) -> rle(List, 1).
rle([], _) -> [];
rle([H|[]], Acc) -> [{H, Acc}];
rle([H|T], Acc) ->
	case H =:= hd(T) of
		true -> rle(T, Acc + 1);
		false -> [{H, Acc} | rle(T, 1)]
	end;
rle(_, _) -> error(function_clause).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Question 3 %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% Sequential %%%%%%%%%%%%%%%% 

rlep([]) -> [];
rlep(List) when is_list(List) -> rlep(List, 1, 1).
rlep([], _, _) -> [];
rlep([H|[]], LenAcc, Pos) -> [{H, {LenAcc, Pos}}];
rlep([H|T], LenAcc, Pos) ->
	case H =:= hd(T) of
		true -> rlep(T, LenAcc + 1, Pos);
		false -> [{H, {LenAcc, Pos}} | rlep(T, 1, Pos + LenAcc)]
	end;
rlep(_, _, _) -> error(function_clause).

longest_run_filter(V, L) -> lists:filter(fun({X, _}) -> X =:= V end, rlep(L)).
longest_run(V, L) -> 
	{_, T} = lists:max(longest_run_filter(V, L)),
	T.

%%%%%%%%%%%%%%%%% Parallel %%%%%%%%%%%%%%%%

longest_run(W, V, Key) ->
	wtree:reduce(W,
	fun(PS) -> LocalList = workers:get(PS, Key),
				{[ Y || {_, Y} <- longest_run_filter(V, LocalList)],
				 hd(LocalList),
				 lists:last(LocalList),
				 length(LocalList)
				}
	end,
	fun({L_List, L_First, L_Last, L_Length}, {R_List, R_First, R_Last, R_Length}) ->
		case (L_Last =:= V) and (V =:= R_First) of
			true -> {L_Len, L_Pos} = lists:last(L_List),
					[{R_Len, _}|_] = R_List,
					New = [{L_Len + R_Len, L_Pos}],
					NewList = lists:sublist(L_List, 1, L_Length - 1) ++ New ++ 
							  [{Len, Pos + L_Length} || 
							  	{Len, Pos} <- lists:sublist(R_List, 2, R_Length)
							  ],
					{NewList, L_First, R_Last, L_Length + R_Length};
			false -> NewList2 = L_List ++ 
								[ {Len, Pos + L_Length} || {Len, Pos} <- R_List],
					{NewList2, L_First, R_Last, L_Length + R_Length}
		end
	end,
	fun({FinalList, _, _, _}) -> lists:max(FinalList)
	end
	).

longest_run_test(Nworkers) ->
	L = [a, b, b, b, a, c, c, a, a, a],
	V = a,
	Sequential = longest_run(V, L),
	W = wtree:create(Nworkers),
	workers:update(W, data, misc:cut(L, Nworkers)),
	Par = longest_run(W, V, data),
	io:format("Sequential result is ~w~n", [Sequential]),
	io:format("Parallel result is ~w~n", [Par]).

longest_run_test() -> longest_run_test(4).
		 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Question 4 %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% Part a) Sequential %%%%%%%

best_match_help([], []) -> [];
best_match_help([], L2) when is_list(L2) -> [];
best_match_help(L1, []) when is_list(L1) -> [];
best_match_help(L1, L2) when is_list(L1), is_list(L2) ->  
		if length(L1) < length(L2) -> 
				A = [ {match_count(L1, lists:sublist(L2, N, length(L1))), N - 1}
					   || N <- lists:seq(1, length(L2)) ],
					  
				B = [ {match_count(lists:sublist(L1, M + 1, length(L1) - M), 
										lists:sublist(L2, length(L1) - M)), -M }
						|| M <- lists:seq(1, length(L1) - 1) ],
					
				lists:filter(fun({X, _}) -> X /= 0 end, A ++ B);
		
		length(L1) > length(L2) -> 
				A = [ { match_count(L2, lists:sublist(L1, N, length(L2))), -N + 1 }
						|| N <- lists:seq(1, length(L1)) ],
						
				 B = [ { match_count(lists:sublist(L2, M + 1, length(L2) - M), 
										lists:sublist(L1, length(L2) - M)), M }
						|| M <- lists:seq(1, length(L2) - 1) ],
				
				lists:filter(fun({X,_}) -> X /= 0 end, A ++ B);
		
		length(L1) =:= length(L2) ->
				MC = match_count(L1, L2), 
				A = if MC /= 0 -> [{match_count(L1, L2), 0}];
					   MC =:= 0 -> []
				end,
				% right side
				B = [ {match_count(L1, lists:sublist(L2, N, Len)), N - 1}
						 || {N, Len} <- lists:zip(lists:seq(2, length(L2)),
						 							lists:seq(length(L2) - 1, 1, -1)
						 						 )
					],
				% left side
				C = [ {match_count(lists:sublist(L1, N, Len), L2), -N + 1}
						 || {N, Len} <- lists:zip(lists:seq(2, length(L1)),
						 							lists:seq(length(L1) - 1, 1, -1)
						 						 )
					],
				
				lists:filter(fun({X,_}) -> X /= 0 end, A ++ B ++ C)
				
		end.

best_match(L1, L2) -> 
	Result = best_match_help(L1, L2),
	MC = lists:max([X || {X, _} <- Result]),
	Align = lists:min([ Y || {X, Y} <- Result, X =:= MC]),
	{MC, Align}.

match_count([], _) -> 0;
match_count(_, []) -> 0;
match_count(L1, L2) when is_list(L1), is_list(L2) ->
	if length(L1) =:= length(L2) ->
			length([ true || {X, Y} <- lists:zip(L1, L2), X =:= Y ]);
	   length(L1) > length(L2) -> 
	   		match_count(lists:sublist(L1, length(L2)), L2);
	   	length(L1) < length(L2) ->
	   		match_count(L1, lists:sublist(L2, length(L1)))
	end;
match_count(_, _) -> error(function_clause).


% q4.b) Theoretically, it's O(N1 * N2) where N1 = length(L1) and N2 = length(L2)

best_match_time(N1, N2, Times) ->
	All = [t(fun() -> hw2:best_match(lists:seq(1, N1), lists:seq(1, N2)) end) 
			|| _ <- lists:seq(1,Times)],
	Mean = lists:sum( [Y||{mean, Y} <- [lists:keyfind(mean,1, X) || X <- All]]) / Times,
	Std = lists:sum( [Y||{std, Y} <- [lists:keyfind(std,1, X) || X <- All]]) / Times,
	{Mean, Std}.

best_match_time() ->
	L1 = [10*N || N <- lists:seq(1,10)],
	L2 = [100*N || N <- lists:seq(1,10)],
	M = lists:zip(L1, L2),
	[{best_match_time(N1, N2, 20), N1*N2} || {N1, N2} <- M].

% Running hw2:best_match_time() results in the following
%[{{9.814703706868305e-5,4.150782783372282e-6},1000},
%{{3.640094471610942e-4,9.224361622143229e-6},4000},
%{{7.968318592210628e-4,1.126886823838621e-5},9000},
%{{0.0014067530726250394,2.9824621922423392e-5},16000},
%{{0.002213208417452609,2.5117281054543852e-5},25000},
%{{0.003174147982522188,3.689862689237753e-5},36000},
%{{0.004325664567706082,4.071022877885073e-5},49000},
%{{0.006020692836833595,3.9087734126549186e-4},64000},
%{{0.007253403086142264,2.1429831104489383e-4},81000},
%{{0.008909343185398233,2.9767448294198143e-4},100000}]

best_match_time_normalized() ->
	Mean_stds = best_match_time(),
	[{X/N, Y} || {{X, Y}, N} <- Mean_stds].
	
% Running hw2:best_match_time_normalized() we get the following result, 
% showing the complexity is 
% O(N1 * N2) in practice as well, because of the approximately constant value 
% that we found from (Mean of 20 means) / N1 * N2 as showed bellow:

%%% {Mean of 20 means / N1 * N2, Mean of 20 stds} %%%

%[{9.977493303786413e-8,5.5974977060482895e-6},
% {9.163793226839973e-8,1.1702166684283937e-5},
% {9.02332360211502e-8,2.5154654648052252e-5},
% {9.74091952015841e-8,1.5387779858375993e-4},
% {9.301441613595567e-8,1.2669667363839207e-4},
% {9.312416881919859e-8,1.644899483964961e-4},
% {9.033891351962949e-8,1.1968270894667694e-4},
% {9.26038952626958e-8,2.9211241917920575e-4},
% {9.111402222956641e-8,1.905946288040128e-4},
% {9.37392423745002e-8,6.328061893310034e-4}]

%%%%%%%%%%%%%%%%% Part c) Parallel %%%%%%%%%%%%%%%%%%%%

best_match_par_help([], []) -> [];
best_match_par_help([], L2) when is_list(L2) -> [];
best_match_par_help(L1, []) when is_list(L1) -> [];
best_match_par_help(L1, L2) when is_list(L1), is_list(L2) ->  
		if length(L1) < length(L2) -> 
				A = [ {{match_count(L1, lists:sublist(L2, N, length(L1))), N - 1},
					  	 length(L2)% length L2
					  } || N <- lists:seq(1, length(L2)) ],
					  
				B = [ {{match_count(lists:sublist(L1, M + 1, length(L1) - M), 
										lists:sublist(L2, length(L1) - M)), -M },
						length(L2)
					  } || M <- lists:seq(1, length(L1) - 1) ],
				
				%lists:filter(fun({{X, _}, _}) -> X /= 0 end ,A ++ B);
				A ++ B;
		
		length(L1) > length(L2) -> 
				A = [ {{ match_count(L2, lists:sublist(L1, N, length(L2))), -N + 1 },
						  length(L2)
						}
						|| N <- lists:seq(1, length(L1)) ],
						
				 B = [ {{ match_count(lists:sublist(L2, M + 1, length(L2) - M), 
										lists:sublist(L1, length(L2) - M)), M },
						length(L2)
						}
						|| M <- lists:seq(1, length(L2) - 1) ],
						
				%lists:filter(fun({{X, _}, _}) -> X /= 0 end ,A ++ B);
				A ++ B;
		
		length(L1) =:= length(L2) ->
				MC = match_count(L1, L2), 
				A = if MC /= 0 -> [{{MC, 0}, length(L2)}];
					   MC =:= 0 -> [{{0,0}, length(L2)}]
				end,
				% right side
				B = [ {{match_count(L1, lists:sublist(L2, N, Len)), N - 1},
						length(L2)
						}
						 || {N, Len} <- lists:zip(lists:seq(2, length(L2)),
						 							lists:seq(length(L2) - 1, 1, -1)
						 						 )
					],
				% left side
				C = [ {{match_count(lists:sublist(L1, N, Len), L2), -N + 1},
						length(L2)
						}
						 || {N, Len} <- lists:zip(lists:seq(2, length(L1)),
						 							lists:seq(length(L1) - 1, 1, -1))
					],
				
				%lists:filter(fun({{X, _}, _}) -> X /= 0 end ,A ++ B ++ C)
				A ++ B ++ C
				
		end.

best_match_par(_, [], _) -> [];
best_match_par(W, L, Key) when is_list(L) ->
	wtree:reduce(W,
		fun(PS) -> LocalList = workers:get(PS, Key),
					best_match_par_help(L, LocalList)
		end,
		fun(Left, Right) -> best_match_help3(Left, Right)
		end,
		fun(FinalList) -> 
			MC = lists:max([ X || {{X, _}, _} <- FinalList]),
			Align = lists:min([ Y || {{X, Y}, _} <- FinalList, X =:= MC]),
			{MC, Align}
		end
		).

best_match_help2([], []) -> [];
best_match_help2([], R) -> R;
best_match_help2(L, []) -> L;
best_match_help2({{L_MC,L_Align}, L_Len}, {{R_MC, R_Align}, R_Len}) ->
					case L_Align =:= R_Align + L_Len of
			 			true -> [{{L_MC + R_MC, R_Align + L_Len }, L_Len + R_Len }];
			 			false -> [{{L_MC, L_Align}, L_Len + R_Len},
			 					  {{R_MC, R_Align + L_Len}, L_Len + R_Len}
			 					 ]
			 		end.
best_match_help3([], []) -> [];
best_match_help3([], R) when is_list(R) -> R;
best_match_help3(L, []) when is_list(L) -> L; 
best_match_help3(L, R) when is_list(L), is_list(R) ->
	lists:flatmap(fun(Z) -> Z end, [best_match_help2(X, Y) || X <- L, Y <- R]).

best_match_par_test(Nworkers) ->
	W = wtree:create(Nworkers),
	L = [1,2,3,3,4,5],
	L1 = [0,1,1,2,3,5,8],
	L2 = [1,2,3,5,1,5,2,3,4,5],
	L3 = [2,4,6,8,10],
	L4 = [a,b,a],
	workers:update(W, data, misc:cut(L, Nworkers)),
	workers:update(W, data1, misc:cut(L1, Nworkers)),
	workers:update(W, data2, misc:cut(L2, Nworkers)),
	workers:update(W, data3, misc:cut(L3, Nworkers)),
	workers:update(W, data4, misc:cut(L4, Nworkers)),
	All = lists:all( fun({X, Y}) -> X =:= Y end, 
			   [{hw2:best_match([3], L) , hw2:best_match_par(W, [3], data)},
			    {hw2:best_match([3,3], L), hw2:best_match_par(W, [3,3], data)},
			    {hw2:best_match([3,3,3], L), hw2:best_match_par(W, [3,3,3], data)},
			    {hw2:best_match([3,3,3,4], L), hw2:best_match_par(W, [3, 3, 3, 4], data)},
			    {hw2:best_match([3,3,3,4,5], L), hw2:best_match_par(W, [3, 3, 3, 4, 5], data)},
			    {hw2:best_match([1,2,3,4], L1), hw2:best_match_par(W, [1,2,3,4], data1)},
			    {hw2:best_match([1,2,3,4], L2), hw2:best_match_par(W, [1,2,3,4], data2)},
			    {hw2:best_match([1,2,4,8], L3), hw2:best_match_par(W, [1,2,4,8], data3)},
			    {hw2:best_match([a,b,a], L4), hw2:best_match_par(W, [a,b,a], data4)},
			    {hw2:best_match([a,b,a,b], L4), hw2:best_match_par(W, [a,b,a,b], data4)}
			   ]
			 ),
	if All =:= true -> io:format("best_match_par_tests have passed ~n" );
		All =:= false -> io:format("best_match_par_tests have failed! ~n")
	end.

best_match_par_test() -> best_match_par_test(4).

% q4.d)

best_match_par_time(Nworkers, N1, N2, Times) ->
	W = wtree:create(Nworkers),
	workers:update(W, data, misc:cut(lists:seq(1, N2), Nworkers)),
	All = [t(fun() -> hw2:best_match_par(W, lists:seq(1, N1), data) end) 
			|| _ <- lists:seq(1,Times)],
	Mean = lists:sum( [ Y || {mean, Y} <- [lists:keyfind(mean,1, X) || X <- All]]) / Times,
	%Std = lists:sum( [Y || {std, Y} <- [lists:keyfind(std,1, X) || X <- All]]) / Times,
	Mean.

% As discussed in class, when N2 >> N1, then we'd expect to get speed up by
% p * N2 / (N1 + p log p N2) for my implementation, otherwise,
% we shouldn't see any speed up for the case, for example, N1 approx equal to N2.
% But practically, the problem I got is that I think
% because I have used lists comprehension a lot, whenever I want to measure the time, so much
% of the memory will be allocated and the VM cannot garbage collect properly, so for the 
% above implementation, I cannot measure the speed up for 4 workers or more (on my laptop
% has 8 cores). For 2 cores, there's actually no speed up as the communication is the bottleneck. Finally, that has given me a good lesson that even garbage collection and
% memory allocation can be real bottlenecks! 



