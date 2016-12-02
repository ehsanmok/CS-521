-module(hw2_test).
-include_lib("eunit/include/eunit.hrl").

% It's not possible to use eunit:test for the parallel test cases!

rle_test() ->
	[ ?assertMatch([{1,1},{2,2},{3,3},{0,1},{5,2}], hw2:rle([1,2,2,3,3,3,0,5,5])),
	  ?assertMatch([{a,1},{b,1},{c,1},{a,3},{z,1}], hw2:rle([a,b,c,a,a,a,z])),
	  ?assertError(function_clause, hw2:rle(abcbd))
	].

longest_run_test() ->
	[ ?assertMatch({4,6}, hw2:longest_run(a, [a,b,b,b,c,a,a,a,a,z])),
	  ?assertMatch({3,1}, hw2:longest_run(a, [a,a,a]))
	].
	
best_match_test() ->
	[ ?assertMatch({3,2}, hw2:best_match([1,2,3,4], [0,1,1,2,3,5,8])),
	  ?assertMatch({3,0}, hw2:best_match([1,2,3,4], [1,2,3,5,1,5,2,3,4,5])),
	  ?assertMatch({2,-1}, hw2:best_match([1,2,4,8], [2,4,6,8,10])),
	  ?assertMatch({3, -4}, hw2:best_match([1,2,4,8,2,4,6], [2,4,6,8,10]))
	].
