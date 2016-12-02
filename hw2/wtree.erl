%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc wtree - Functions to support trees of processes.
%%   We arrange the processes of a worker pool to a (nearly) balanced
%%   binary tree.  We add entries to the process state list for each
%%   worker so that it knows its parent and its children.  We then
%%   provide functions <code>broadcast</code>, <code>reduce</code>
%%   and <code>scan</code> to operate on such trees.
%%   <p>
%%   The <code>receive</code> operations in <code>reduce</code> and
%%   <code>scan</code> include a time-out that by default is set to
%%   <code>'infinity'</code> (i.e., they'll hang forever if the expected
%%   message is never received).  This value can be changed for all
%%   workers in a worker_pool by the <code>set_debug_timout</code>
%%   function.
%%   </p>
%% @end

-module wtree.  % A tree of workers

-export [alive/1, broadcast/3, children/1, create/1, create/0, init/1,
         nworkers/1, parent/1, reap/1, reduce/3, reduce/4,
	 scan/5, set_debug_timeout/2, test/0, test_reduce/0, test_scan/0].


%% @spec create(N::integer()) -> worker_pool()
%% @doc Create a worker-pool of <code>N</code> processes and
%%   initialize them to support the tree operations of this module.
create(N) -> init(workers:create(N)).

%% @spec create() -> worker_pool()
%% @doc Create a worker-pool with the default number of processes and
%%   initialize them to support the tree operations of this module.
create()  -> init(workers:create()).

%% @spec init(W::worker_pool) -> worker_pool()
%% @doc Initialze the process state of the workers of <code>W</code>
%%   to support the tree operations of this module.
init([]) -> [];
init(W) ->
  init(W, self(), length(W)),
  workers:update(W, {'$wtree$', debug_timeout}, fun(_) -> infinity end),
  W.

init(W, Parent, NW) -> init2(hd(W), Parent, init(W, NW)).

init([_Pid], 1) -> [];
init(W = [Pid | _], NW) ->
  NLeft = (NW+1) div 2,  % round high
  {W0, W1} = lists:split(NLeft, W),
  init(W1, Pid, NW-NLeft),
  [ {NLeft, hd(W1)} | init(W0, NLeft)].

init2(W, Parent, Children) ->
  W ! fun(ProcState) ->
    workers:put(ProcState,
      [ { {'$wtree$', parent}, Parent},
        { {'$wtree$', children}, Children}
      ]
    )
    end.


%% @equiv workers:alive
alive(W) -> workers:alive(W).

%% @equiv workers:reap(W)
reap(W) -> workers:reap(W).

%% @equiv workers:nworkers(W)
nworkers(W) -> workers:nworkers(W).

%% @spec parent(ProcState::worker_state()) -> pid()
%% @doc Return the pid for the parent of this worker process.
parent(ProcState) -> workers:get(ProcState, {'$wtree$', parent}).

%% @spec children(ProcState::worker_state()) -> [{integer(), pid()}]
%% @doc Return a list of the children of this process.
%%   The ``tree'' 
children(ProcState) -> workers:get(ProcState, {'$wtree$', children}).

%% @spec broadcast(W, Task, Args) -> ok
%% @doc Invoke <code>Task</code> on all workers.
%% <ul>
%%   <li><code>W</code> is a <code>worker_pool</code>.</li>
%%   <li><code>Task</code> is a function.  If
%%     <ul>
%%       <li><code>Task</code> has an arity of two, then it is invoked as
%%         <dl><dd><code>Task(ProcState, Arg)</code></dd></dl>
%%         where
%%         <ul>
%%           <li><code>ProcState</code> is the current state of the
%%		worker process, and</li>
%%           <li><code>Arg</code> is the <code>N</code><sup>th</sup> element
%%	        of <code>Args</code> when <code>Task></code> is invoked for
%%	  	the <code>N</code><sup>th</sup> worker of <code>W</code>.</li>
%%         </ul>
%%       </li>
%%       <li><code>Task</code> has an arity of three, then it is invoked as
%%         <dl><dd><code>Task(ProcState, Arg, N)</code></dd></dl>
%%	   where <code>ProcState</code>, <code>Arg</code>, and <code>N</code>
%%	   are as defined above.
%%       </li>
%%     </ul>
%%     The return value of <code>Task</code> becomes the new state of
%%     the process.
%%   </li>
%% </ul>
%% @todo Support the full functionality of <code>workers:broadcast</code>.

broadcast(W, Task, Args) ->
  case length(Args) == length(W) of
    true when W == [] -> ok;
    true ->
      hd(W) ! fun(ProcState) -> bcast(ProcState, Task, Args, 1) end,
      ok;
    false -> erlang:error(io_lib:format(
      "wtree:broadcast -- ~s: ~w vs. ~w~n",
      [ "length(Args) not equal to number of workers",
      length(Args), length(W)]))
  end.

bcast(ProcState, Task, Args, Index) ->
 bcast(ProcState, Task, Args, Index,
    element(2, lists:keyfind({'$wtree$', children}, 1, ProcState))).

bcast(ProcState, Task, Args, Index, [ {NLeft, RootRight} | ChildTail]) ->
  {A0, A1} = lists:split(NLeft, Args),
  RootRight ! fun(PS) -> bcast(PS, Task, A1, Index+NLeft) end,
  bcast(ProcState, Task, A0, Index, ChildTail);

bcast(ProcState, Task, Args, Index, []) ->
  try
    if
      is_function(Task, 2) -> Task(ProcState, hd(Args));
      is_function(Task, 3) -> Task(ProcState, hd(Args), Index)
    end
  catch
    throw:Ball ->
      io:format("Throw in worker ~w: ~w~n", [self(), Ball]),
      io:format("Trying to recover~n"),
      ProcState;
    exit:Reason ->
      io:format("Exit in worker ~w: ~w~n", [self(), Reason]),
      io:format("Trying to recover~n"),
      ProcState;
    error:Reason ->
      io:format("Error in worker ~w: ~w~n", [self(), Reason]),
      io:format("Trying to recover~n"),
      ProcState
  end.


%% @spec reduce(W, Leaf, Combine, Root) -> term2()
%%    W = worker_pool(),
%%    Leaf = fun((ProcState::worker_state) -> term1()),
%%    Combine = fun((Left::term1(), Right::term1()) -> term1()),
%%    Root = fun((term1()) -> term2())
%% @doc A generalized reduce operation.
%%   The <code>Leaf()</code> function is applied in each worker.
%%   The results of these are combined, using a tree,
%%   using <code>Combine</code>.
%%   The <code>Root</code> function is applied to the final result
%%   from the combine tree to produce the result of this function.
%%   <br/>
%%   <b>Note:</b> The workers are ordered.  In particular, if one were
%%   to invoke <code>update(W, 'WID', lists:seq(1:Nworkers)</code>
%%   then all of the workers contributing to the <code>Left</code>
%%   argument will have <code>'WID'</code> values less than those
%%   contributing to the <code>Right</code>.  This interface says
%%   nothing about whether or not the trees are balanced.  This means
%%   that to get deterministic results, <code>Combine</code> should
%%   be an <a href="http://en.wikipedia.org/wiki/Associative_property">associative</a>
%%   function.
%% @todo Add an optional <code>Args</code> parameter so that
%%   <code>Leaf</code> can be an arity-2 function that is called with
%%   the worker process state and the element of <code>Args</code> for
%%   its process.
reduce(W = [W0 | _], Leaf, Combine, Root) ->
  MyPid = self(),
  reduce_dispatch(W0, {Leaf, Combine}, fun(_) -> MyPid end),
  case reduce_receive(W0, []) of
    {ok, V} -> Root(V);
    {fail, F} ->
      {F2, N_fail} = label_failures(W, F),
      Msg = if (N_fail == 1) -> "failure in worker process";
      	       (N_fail > 0)  -> "failure in worker processes"
	    end,
      throw({fail, Msg, F2})
  end;
reduce([], _L, _C, _R) -> ok.  % empty worker pool, nothing to do

reduce(W, Leaf, Combine) -> reduce(W, Leaf, Combine, fun(X) -> X end).

label_failures(W, [Left, Right]) ->
  {Left2, N_left} = label_failures(W, Left),
  {Right2, N_right} = label_failures(W, Right),
  {[Left2, Right2], N_left + N_right};
label_failures(W, [Pid, Msg, Kind, Reason, Trace]) ->
  { [ [Pid, lists:flatten(io_lib:format("worker ~b", [index(W, Pid)]))],
      Msg, Kind, Reason,
      lists:takewhile(fun({Module,_,_,_}) -> Module /= wtree end, Trace)
    ],
    1
  }.

index(List, E) -> index(List, E, 1).
index([], _, _) -> -1;
index([E | _], E, N) -> N;
index([_ | Tl], E, N) -> index(Tl, E, N+1).

reduce_work(ProcState, [{_, RootRight} | CT], LC = {_, Combine}) ->
  reduce_dispatch(RootRight, LC),
  Left = reduce_work(ProcState, CT, LC),
  Right = reduce_receive(RootRight, ProcState),
  case {Left, Right} of
    {{ok, LeftValue}, {ok, RightValue}} ->
      try
	{ok, Combine(LeftValue, RightValue)}
      catch
	throw:Ball ->    failure("error in Combine function for reduce", throw, Ball);
	exit:Reason ->   failure("error in Combine function for reduce", exit,  Reason);
	error:Reason ->  failure("error in Combine function for reduce", error, Reason)
      end;
    {{fail, _}, {ok, _}} -> Left;
    {{ok, _}, {fail, _}} -> Right;
    {{fail, LeftFail}, {fail, RightFail}} -> {fail, [LeftFail, RightFail]};
    _ -> failure("internal error in reduce", error, badmatch)
  end;
reduce_work(ProcState, [], {Leaf, _}) ->
  try
    {ok, Leaf(ProcState)}
  catch
    throw:Ball ->    failure("error in Leaf function for reduce", throw, Ball);
    exit:Reason ->   failure("error in Leaf function for reduce", exit, Reason);
    error:Reason ->  failure("error in Leaf function for reduce", exit, Reason)
  end.

failure(What, Kind, Stuff) ->
  {fail, [self(), What, Kind, Stuff, erlang:get_stacktrace()]}.
  
reduce_dispatch(CPid, LC, HomeFn) ->
  CPid ! fun(PS) ->
    V = reduce_work(PS, children(PS), LC),
    HomeFn(PS) ! {'$wtree$', reduce, self(), V },
    PS
  end.
reduce_dispatch(CPid, LC) ->
  reduce_dispatch(CPid, LC, fun(PS) -> parent(PS) end).

reduce_receive(CPid, PS) ->
  receive
    {'$wtree$', reduce, CPid, V} -> V
    after debug_timeout(PS) ->
      misc:msg_dump("wtree:reduce",
        [io_lib:format("{'$wtree$', reduce, ~w, V}", [CPid])])
  end.



%% @spec scan(W, Leaf1, Leaf2, Combine, Acc0) -> term1()
%%    W = worker_pool(),
%%    Leaf1 = fun((ProcState::worker_state) -> term1()),
%%    Leaf2 = fun((ProcState::worker_state, AccIn::term1()) -> worker_state()),
%%    Combine = fun((Left::term1(), Right::term1()) -> term1()),
%%    Acc0 = term1()
%% @doc A generalized scan operation.
%%   The <code>Leaf1()</code> function is applied in each worker process.
%%   The results of these are combined, using a tree,
%%   using <code>Combine</code>.
%%   The return value of the scan is the result of applying the
%%   <code>Combine</code> function at the root of the tree.
%%   <br/>
%%   Furthermore, the <code>Leaf2()</code> function is applied in each
%%   worker process.  The <code>AccIn</code> argument is the results
%%   of the <code>Combine</code> for everything to the left of this
%%   node in the tree.  For the leftmost process, <code>Acc0</code>
%%   is used.  The return value of <code>Leaf2</code> becomes the
%%   state of the worker process.
%% @todo
%%   <ul>
%%     <li>Add an optional <code>Args</code> parameter so that
%%       <code>Leaf1</code> can be an arity-2 function that is called with
%%       the worker process state and the element of <code>Args</code> for
%%       its process.
%%     </li>
%%     <li>The <code>Acc0</code> argumnent should probably be replaced
%%       with a root function that returns a tuple of the form
%%       <code>{V, Left}</code> where <code>V</code> is the value that
%%	 <code>scan</code> will return, and <code>Left</code> is the value
%%       to pass down the left sub-tree for <code>AccIn</code>
%%     </li>
%%     <li>Alternatively, I could imagine making a simpler version that
%%       more closely matches the interface of <code>lists:mapfoldl</code>.
%%     </li>
%%   </ul>
scan([W0 | _], Leaf1, Leaf2, Combine, Acc0) ->
  MyPid = self(),
  scan_dispatch(W0, {Leaf1, Leaf2, Combine}, fun(_) -> MyPid end),
  W0 ! {'$wtree$', scan, self(), Acc0},
  scan_receive(W0, []);
scan([], _L1, _L2, _C, _A0) -> ok.  % empty worker pool, nothing to do

scan_dispatch(CPid, LLC, HomeFn) ->
  CPid ! fun(PS) ->
    V = scan_work1(PS, children(PS), LLC),
    HomeFn(PS) ! {'$wtree$', scan, self(), hd(V) },
    Left = scan_receive(parent(PS), PS),
    scan_work2(PS, children(PS), LLC, Left, tl(V))
  end.
scan_dispatch(CPid, LLC) -> scan_dispatch(CPid, LLC, fun(PS) -> parent(PS) end).

scan_work1(ProcState, [{_, RootRight} | CT], LLC) ->
  Combine = element(3, LLC),
  scan_dispatch(RootRight, LLC),
  Left  = scan_work1(ProcState, CT, LLC),
  Right = scan_receive(RootRight, ProcState),
  [ Combine(hd(Left), Right) | Left ];
scan_work1(ProcState, [], LLC) ->
  Leaf1 = element(1, LLC),
  [Leaf1(ProcState)].

scan_work2(ProcState, [{_, RootRight} | CT], LLC, Left, [LH | LT]) ->
  Combine = element(3, LLC),
  RootRight ! {'$wtree$', scan, self(), Combine(Left, LH)},
  scan_work2(ProcState, CT, LLC, Left, LT);
scan_work2(ProcState, [], LLC, Left, []) ->
  Leaf2 = element(2, LLC),
  Leaf2(ProcState, Left).
  

scan_receive(Pid, PS) ->
  receive
    {'$wtree$', scan, Pid, V} -> V
    after debug_timeout(PS) ->
      misc:msg_dump("wtree:scan",
        [io_lib:format("{'$wtree$', scan, ~w, V}", [Pid])])
  end.


debug_timeout(PS) ->
  workers:get(PS, {'$wtree$', debug_timeout}, fun() -> infinity end).

%% @spec set_debug_timeout(W, T) -> ok
%%   W = worker_pool(),
%%   T = integer() | infinity
%% @doc Set the time-out for receive operations for <code>reduce</code>
%%   and <code>scan</code> to <code>T</code>.
set_debug_timeout(W, T) ->
  workers:update(W, {'$wtree$', debug_timeout}, fun() -> T end),
  ok.

test_reduce() ->
  NWorkers = 12,
  Block = 5,
  W = create(NWorkers),
  set_debug_timeout(W, 1000),
  workers:update(W, seq,
    fun(_PS, I) -> lists:seq(Block*(I-1), (Block*I)-1) end),
  M = NWorkers*Block - 1,
  V0 = M*(M+1) div 2,
  V1 = reduce(W,
    fun(PS) -> lists:sum(workers:get(PS, seq)) end,
    fun(L, R) -> L+R end),
  V2 = reduce(W,
    fun(PS) -> lists:sum(workers:get(PS, seq)) end,
    fun(L, R) -> L+R end),
  case {V1, V2} of
    {V0, V0} ->  ok;
    {V0, _} -> 
      io:format("failed: got sum_{k=0}^~w k = ~w, should be ~w~n",
		[M, V2, V0]),
      failed;
    {_, _} -> 
      io:format("failed: got sum_{k=0}^~w k = ~w, should be ~w~n",
    	        [M, V1, V0]),
      failed
  end.
    

test_scan() ->
  W = create(12),
  workers:update(W, seq, fun(_PS, I) -> lists:seq(5*(I-1), (5*I)-1) end),
  scan(W,
    fun(PS) -> lists:sum(workers:get(PS, seq)) end,
    fun(PS, Acc0) ->
      workers:put(PS, cumseq, misc:cumsum(Acc0, workers:get(PS, seq)))
    end,
    fun(L, R) -> L+R end,
    0),
  V1 = lists:flatten(workers:retrieve(W, cumseq)),
  workers:update(W, char, fun(_PS, I) -> 96+I end),
  scan(W,
    fun(PS) -> [workers:get(PS, char)] end,
    fun(PS, Acc0) ->
      workers:put(PS, str, Acc0 ++ [workers:get(PS, char)])
    end,
    fun(L, R) -> L ++ R end,
    []),
  V2 = workers:retrieve(W, str),
  {V1, V2}.

test() -> test_scan().
