-module(hw1_test).
-include_lib("eunit/include/eunit.hrl").

nthtail_test() ->
	[ ?assert([1,2,3] =:= hw1:nthtail(0, [1,2,3])),
	  ?assertMatch([2,3], hw1:nthtail(1, [1,2,3])),
	  ?assertMatch([3], hw1:nthtail(2, [1,2,3])),
	  ?assertMatch([], hw1:nthtail(3, [1,2,3])),
	  ?assertNotMatch([3], hw1:nthtail(3, [1,2,3])),
	  ?assertError(function_clause, hw1:nthtail(-1, [1,2,3])),
	  ?assertError(function_clause, hw1:nthtail(4, [1,2,3])),
	  ?assertError(function_clause, hw1:nthtail(1, 10)),
	  ?assertError(function_clause, hw1:nthtail(-1, a))
	  ].

prefix_test() ->
	[ ?assertMatch(true, hw1:prefix([], [1,2,3])),
	  ?assertMatch(true, hw1:prefix([1], [1,2,3])),
	  ?assertMatch(true, hw1:prefix([1,2,3], [1,2,3])),
	  ?assertMatch(false, hw1:prefix([1,2,3,4], [1,2,3])),
	  ?assertMatch(false, hw1:prefix([3,2,1], [1,2,3])),
	  ?assertError("no function clause matching", hw1:prefix(banana, [1,2,3])),
	  ?assertMatch(true, hw1:prefix("banana", "banana")),
	  ?assertMatch(false, hw1:prefix("alumna","alumni"))
	].

search_test() ->
	[ ?assertMatch([2,8], hw1:search([2,3], [1,2,3,4,3,2,1,2,3,4])),
	  ?assertMatch([2,4], hw1:search("an", "banana")),
	  ?assertMatch([2,4, 6], hw1:search("a", "banana")),
	  ?assertMatch([1,2,3,4,5,6], hw1:search([], "banana")),
	  ?assertMatch([], hw1:search("banananana", "banana")),
	  ?assertError("no function clause matching", hw1:search(10, [])) 
	].

subtract_test() ->
	[ ?assertMatch([], hw1:subtract([], [1,2,3])),
	  ?assertMatch([1,2,3], hw1:subtract([1,2,3], [])),
	  ?assertMatch([], hw1:subtract([1,2,3], [1,2,3])),
	  ?assertMatch([1], hw1:subtract([1,2,3], [2,3])),
	  ?assertMatch([1,3], hw1:subtract([1,2,3], [2])),
	  ?assertMatch([1], hw1:subtract([1], [2,3])),
	  ?assertMatch([], hw1:subtract([1], [1,2])),
	  ?assertMatch([2,1], hw1:subtract([3,2,1], [3])), % ensuring output is not sorted
	  ?assertError("no function clause matching", hw1:subtract(banana, [1,2,3]))
	  ].
