%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc misc - an assortment of various functions that I've found to
%%   be useful.
%% @end

-module misc.
-export [arity/1, cumsum/1, cumsum/2, index/2, logsteps/2, pow/2].
-export [neighbours/1, print/1, print/2, pr_tl/3, intervals/3, mod/2].
-export [rlist/1, rlist/2, rlist/3, msg_dump/1, msg_dump/2].
-export [sync/1, sync/2, sync/3, sync/4].
-export [cut/2, deal/2].

% internal functions, uncomment for debbuging
% -export([rp_pow/3, how_many_runs/3]).

%% @spec arity(Fun) -> integer()
%% @doc Return the arity of <code>Fun</code>.
%%   If <code>Fun</code> is not a function, then the atom
%%   <code>'false'</code> is returned.
arity(F) when is_function(F) -> arity(F,0);
arity(_) -> false.

% arity/2: a helper function for arity/1.  Try successive values for the
% arity until we find the right one.
arity(F,N) when is_function(F,N) -> N;
arity(F,N) -> arity(F, N+1).

%% @spec cumsum(List) -> [number()]
%%   List = [number()]
%% @doc
%%   Return a list where the <code>N</code><sup>th</sup> element of
%%   the result is the sum of the first <code>N</code> elements of
%%   <code>List</code>.  Corresponds to the cumsum function from
%%   Matlab.
cumsum(L) -> cumsum(0, L).

%% @spec cumsum(Acc0, List) -> [number()]
%%   List = [number()]
%% @doc
%%   Return a list where the <code>N</code><sup>th</sup> element of
%%   the result is the <code>Acc0</code> plus the sum of the first
%%   <code>N</code> elements of <code>List</code>.
cumsum(Acc0, L) -> element(1, lists:mapfoldl(
  fun(Elem, Acc) -> S = Elem+Acc, {S,S} end, Acc0, L)).

%% @spec pow(N::number(), M::number()) -> number()
%% @doc <code>N</code> raised to the <code>M</code><sup>th</sup> power.
%%    <ul>
%%      <li>
%%        If <code>M</code> is an integer, then <code>N<sup>M</sup></code>
%%        is computed using the Russian Peasant algorithm with
%%	  <it>O(</it>log <code>N</code><it>)</it> multiplications.
%%        This produces an exact answer when <code>N</code> is an integer.
%%        Furthermore, it avoids an exception when
%%	  <code>N <it>&#60; 0</it></code>.
%%      </li>
%%      <li>
%%        Otherwise, <code>M</code> must be a float, in which case
%%        <code>N<sup>M</sup></code> is computed using
%%        <a href="http://www.erlang.org/doc/man/math.html#pow-2"><code>math:pow/2</code></a>.
%%      </li>
%%    </ul>
%%    If <code>N <it>=0</it></code> and <code>M <it>&#60; 0</it></code>,
%%    then the atom <code>'undefined'</code> is returned.
%%    If <code>N <it>&#60; 0</it></code> and <code>M</code> is a float, then
%%    an arithmetic exception occurs.  Likewise, there is an exception when
%%    <code>M</code> is a float and the <code>N<sup>M</sup></code>
%%    produces a floating point overflow.
pow(N, M) when is_integer(N), is_integer(M), (M >= 0) ->
  rp_pow(N, M, 1);
pow(0, M) when (M < 0) -> undefined;
pow(N, M) when (M < 0) -> 1 / pow(N, -M);
pow(N, M) when is_number(N), (N < 0), is_integer(M) ->
  case (M rem 2) of
    0 -> pow(-N, M);
    1 -> -pow(-N, M)
  end;
pow(N, M) when is_number(N), (N > 0), is_number(M) ->
  math:pow(N,M);
pow(L, M) when is_list(L), is_number(M) -> [ pow(N, M) || N <- L];
pow(N, L) when is_number(N), is_list(L) -> [ pow(N, M) || M <- L];
pow(LN, LM) when is_list(LN), is_list(LM) ->
  [ pow(N, M) || {N, M} <- lists:zip(LN, LM) ].


% rp_pow: integer power, "Russian Peasant" algorithm
% P*N^M is invariant for each recursive call.
rp_pow(_N, 0, P) -> P;
rp_pow(N, M, P) when (M rem 2 == 0) -> rp_pow(N*N, M div 2, P);
rp_pow(N, M, P) when (M rem 2 == 1) -> rp_pow(N*N, M div 2, P*N).


%% @spec index(Key, List) -> integer() | false
%% @doc Return the position of the first occurrence of <code>Key</code>
%%   in <code>List</code>.
%%   If <code>List</code> has no element that matches <code>Key</code>,
%%   then false is returned.
index(Key, List) -> index(Key, List, 1).
index(_Key, [], _N) -> 'false';
index(Key, [Key | _T], N) -> N;
index(Key, [_Other | T], N) -> index(Key, T, N+1).


%% @spec logsteps(Mant, Exp) -> [ number() ]
%%    Mant = [ number() ],
%%    Exp = [ number() ]
%% @doc
%%   For each <code>M</code> in <code>Mant</code> and each <code>E</code> in
%%   <code>Exp</code>, <code>M*10<sup>E</sup></code> is an element of
%%   the return list.
%%   <br/>
%%   Example: <code>logsteps([1,2,5], lists:seq(-1,2) -> [0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500]. </code>
%%   <br/>
%%   <code>logsteps</code> can be handy for generating a sequence of
%%   problem sizes for run-time measurements.
logsteps(Mant, Exp) -> [ M*pow(10,E) || E <- Exp, M <- Mant ].


%% @spec neighbours(List) -> TupleList
%% @doc
%%   Return a list of tuples of all adjacent pairs of elements of list.
%%   <br/>
%%   Example: <code>neighbours([1,2,3,5,7,11]) -> [{1,2},{2,3},{3,5},{5,7},{7,11}].</code>
%%   <br/>
%%   If <code>List</code> is empty or a singleton, then <code>TupleList</code>
%%   is empty.
neighbours([]) -> [];
neighbours([H | T]) ->
  element(1, lists:mapfoldl(fun(E, Prev) -> {{Prev, E}, E} end, H, T)).


%% @spec rlist(N, M, State0) -> {List, State1}
%%   N = integer(),
%%   M = number(),
%%   State0 = ran(),
%%   List = [ integer() ],
%%   State1 = ran()
%% @doc Return a list of <code>N</code> elements.  If
%%   <ul>
%%     <li><code>M</code> is an integer, then the values are chosen
%%       uniformly from 1..<code>M</code>.</li>
%%     <li><code>M</code> is an floating point number, then the values are chosen
%%       uniformly from [0, M).</li>
%%   </ul>
%%   <code>State0</code> is the starting state for the pseudo-random number generator.
%%   <code>rlist</code> returns the random list and the state for the PRNG at the end of
%%   generating the list.
rlist(N, M, State0)
    when is_integer(N), is_integer(M), (N > 0), (M > 0) ->
  rlist2(N, M, [], State0);
rlist(N, A, State0)
    when is_integer(N), is_float(A), (N > 0) ->
  rlist2f(N, A, [], State0).

rlist2(0, _M, Rlist, State) -> {Rlist, State};
rlist2(N, M, Rlist, State0) ->
  {Rnum, State1} = random:uniform_s(M, State0),
  rlist2(N-1, M, [Rnum | Rlist], State1).

rlist2f(0, _A, Rlist, State) -> {Rlist, State};
rlist2f(N, A, Rlist, State0) ->
  {Rnum, State1} = random:uniform_s(State0),
  rlist2f(N-1, A, [A*Rnum | Rlist], State1).


%% @spec rlist(N, M) -> List
%%   N = integer(),
%%   M = number(),
%%   List = [ integer() ]
%% @doc Return a list of <code>N</code> elements.  If
%%   <ul>
%%     <li><code>M</code> is an integer, then the values are chosen
%%       uniformly from 1..<code>M</code>.</li>
%%     <li><code>M</code> is an floating point number, then the values are chosen
%%       uniformly from [0, M).</li>
%%   </ul>
rlist(N,M) when is_integer(N), is_integer(M), (N > 0), (M > 0) ->
  rlist2(N, M, []);
rlist(N,A) when is_integer(N), is_float(A), (N > 0) ->
  rlist2f(N, A, []).

rlist2(0, _M, Rlist) -> Rlist;
rlist2(N,  M, Rlist) -> rlist2(N-1, M, [random:uniform(M) | Rlist]).
rlist2f(0, _A, Rlist) -> Rlist;
rlist2f(N,  A, Rlist) -> rlist2f(N-1, A, [A*random:uniform() | Rlist]).

%% @spec rlist(N) -> List
%% @equiv rlist(N, 10)
rlist(N) -> rlist(N, 10).  % list of N random digits selected from 1..10

%% @spec msg_dump(Who, Patterns) -> failed
%%   Who = string() | nobody,
%%   Patterns = [ string() ]
%% @doc Print the pending messages of a process -- for debugging.
%%   <code>msg_dump</code> prints an error message where each line
%%   is prefixed by the process pid and a colon.  It states that a
%%   receive operation timed out, prints the list of patterns, and
%%   then prints all pending messages for the process.
%%   <ul>
%%     <li> If <code>Who</code> is a string, then the first line printed is
%%     	 <dl><dd><code>"$PID: receive timed out in $Who"</code></dd></dl>
%%       where <code>$PID$</code> is the PID of this process,
%%       and <code>$Who</code> is the string from <code>Who</code>.</li>
%%     <li> If <code>Who</code> is the atom <code>'nobody'</code>,
%%       then the first line printed is
%%       <dl><dd><code>"$PID: receive timed out</code></dd></dl>
%%       where <code>$PID$</code> is the PID of this process.</li>
%%   </ul>
%%   <code>Patterns</code> is the list of strings that describe what
%%   <code>receive</code> was looking for.
%%   <br/>
%%   <code>msg_dump</code> returns the atom <code>'failed'</code>
%%   <br/>
%%   <b>Note:</b> if the desired message were to arrive while we are printing,
%%   then it is possible that the pattern would be displayed, and later
%%   a message that matches that pattern would be printed.
msg_dump(Who, Patterns) ->
  MyPid = self(),
  M0 = io_lib:format("~w: receive timed out", [MyPid]),
  case Who of
    nobody -> io:format("~s~n", [M0]);
    _ -> io:format("~s in ~s~n", [M0, Who])
  end,
  io:format("~w: I was looking for~n", [MyPid]),
  msg_dump_patterns(Patterns, MyPid),
  io:format("~w: My pending messages are~n", [MyPid]),
  msg_dump_messages(MyPid),
  io:format("~w: giving up.~n", [MyPid]),
  failed.


%% @spec msg_dump(Patterns) -> failed
%%   Patterns = [ string() ]
%% @doc Print the pending messages of a process -- for debugging.
%%   <code>msg_dump(Patterns)</code> is equivalent to
%%   <code>msg_dump(nobody, Patterns)</code>.
msg_dump(Patterns) -> msg_dump(nobody, Patterns).

% msg_dump_patterns: a helper function for msg_dump.
%   Print the pattern strings.
msg_dump_patterns([], _MyPid) -> ok;
msg_dump_patterns([H | T], MyPid) ->
  io:format("~w:   ~s~n", [MyPid, H]),
  msg_dump_patterns(T, MyPid).

% msg_dump_messages: a helper function for msg_dump.
%   Print the pending messages of this process.
%
msg_dump_messages(MyPid) ->
  receive
    Msg ->
      io:format("~w:   ~w~n", [MyPid, Msg]),
      msg_dump_messages(MyPid)
    after 0 ->
      io:format("~w:   no more messages~n", [MyPid])
  end.


%% @spec sync(P, TimeOut, SndToken, RcvToken) -> ok | termR() | failed
%%   P = pid(),
%%   TimeOut = integer() | infinity
%%   SndToken = termS(),
%%   RcvToken = termR()
%% @doc  Synchronize with process <code>P</code>.
%%   This process sends <code>SndToken</code> to <code>P</code>, and
%%   <code>P</code> should respond by making a call to one of the versions
%%   of <code>sync</code> with this process as the target process.
%%   <ul>
%%     <li> If we receive <code>RcvToken</code>, we return it.</li>
%%     <li> If we receive the default token, we return <code>'ok'</code>.</li>
%%     <li> If a timout occurs, we print a message dump and return
%%		<code>'failed'</code>.</li>
%%   </ul>
%%   If <code>SndToken</code> is the empty tuple, <code>{}</code>, then
%%   the default <code>msg:sync</code> token is sent.  Likewise, if
%%   <code>RcvToken</code> is the empty tuple, then we only look to receive
%%   the default <code>msg:sync</code> token.
%%   <br/>
%%   A successful return from <code>sync</code> ensures that this process
%%   and process <code>P</code> have both reached the synchronization point
%%   before either proceeds.
%% @todo  Generalize the implementation to provide barriers.
sync(P, TimeOut, {}, {}) ->
  do_sync(P, TimeOut, default_sync(), default_sync(), false);
sync(P, TimeOut, SndToken, {}) ->
  do_sync(P, TimeOut, SndToken, default_sync(), false);
sync(P, TimeOut, {}, RcvToken) ->
  do_sync(P, TimeOut, default_sync(), RcvToken, true);
sync(P, TimeOut, SndToken, RcvToken) ->
  do_sync(P, TimeOut, SndToken, RcvToken, true).

%% @spec sync(P, SndToken, RcvToken) -> ok | termR() | failed
%% @equiv sync(P, infinity, SndToken, RcvToken)
sync(P, SndToken, RcvToken) -> sync(P, infinity, SndToken, RcvToken).

%% @spec sync(P, TimeOut) -> ok | failed
%%   P = pid(),
%%   TimeOut = integer() | infinity
%% @doc  Synchronize with process <code>P</code>.
%%   This process sends a 'sync' message to <code>P</code>, and
%%   <code>P</code> should call this function to send a 'sync'
%%   message to us.  When we receive the message from <code>P</code>
%%   we continue.  This ensures that both processes have made it to
%%   the point of their <code>sync</code> calls before either proceeds.
sync(P, TimeOut) -> sync(P, TimeOut, {}, {}).

%% @spec  sync(P) -> ok | exit | failed
%% @equiv sync(P, infinity)
sync(P) -> sync(P, infinity).

% do_sync(P, TimeOut, SndToken, RcvToken, Flag)
%   Send SndToken to P, then wait to receiv RecvToken or default_sync().
%   If Flag makes sure that we handle the corner case when someone call
%   sync/4 with default_sync() for RcvToken.  In this case, if we receive
%   default_sync(), we return default_sync().  In any other case, if we
%   receive default_sync(), we return the atom 'ok'.
do_sync(P, TimeOut, SndToken, RcvToken, Flag) ->
  P ! {self(), SndToken},
  DefSync = default_sync(),
  receive
    { P, RcvToken } when Flag -> RcvToken;
    { P, DefSync } -> ok
    after TimeOut ->
      P0 = [ io_lib:format("{~w, ~w}", [P, DefSync]) ],
      Patterns = case Flag of
        true  -> [ io_lib:format("{~w, ~w}", [P, RcvToken]) | P0 ];
	false -> P0
      end,
      msg_dump("msg:sync", Patterns),
      failed
  end.

% default_sync() -> the default token for msg:sync messages.
default_sync() -> 'msg:sync'.

%% @spec print(Term) -> ok
%% @equiv print(Term, 100)
print(Term) -> print(Term, 100).

%% @spec print(Term, N) -> ok
%% @doc  Print an erlang term on standard out.
%%   Unlike the erlang shell, print always prints a list of integers as a list of integers.
%%   Thus, <code>print("hello world"</code> is equivalent to
%%   <code>io:format("~s", "[104,101,108,108,111,32,119,111,114,108,100]")</code>.
%%   The point behind this is to prevent lists that really are supposed to be lists of
%%   integers from printing as strings just because every element of the list had a value
%%   in the range 32..127.
%%   <br/>
%%   <code>N</code> gives (roughly) the number of terms to print.  This means that if
%%   you ask to print a really big term, erlang won't be spewing stuff for several hours.
%%   If you want it to print the whole thing, use the atom <code>'infinity'</code> for
%%   <code>N</code>.
print(Term, N) -> pr(Term, N), io:format("~n"), ok.

pr(Int, N) when is_integer(Int) -> io:format("~b", [Int]), N-1;
pr([], N) -> io:format("[]"), N-1;
pr([H | T], N) -> io:format("["), pr_tl(T, pr(H, N-1), "]");
pr(Tuple, N) when is_tuple(Tuple) ->
  case tuple_to_list(Tuple) of
    [] -> io:format("{}"), N-1;
    [H | T] -> io:format("{"), pr_tl(T, pr(H, N-1), "}")
  end;
pr(X, N) -> io:format("~w", X), N-1.

pr_tl([], N, EndString) -> io:format("~s", [EndString]), N;
pr_tl([_ | _], 0, EndString) -> io:format(",...~s", [EndString]), 0;
pr_tl([H | T], N, EndString) -> io:format(","), pr_tl(T, pr(H, N), EndString).

%% @spec intervals(Lo, Hi, N) -> List
%% @doc Divide [Lo, Hi] into N roughly intervals of roughly equal size.
%% <ul>
%%   <li><code>Lo</code> and <code>Hi</code> must be integers or floats.</li>
%%   <li><code>N</code> must be an integer.</li>
%%   <li><code>List</code> is a list of intervals.  Each interval is
%%     a tuple, <code>{Lo_i, Hi_i}</code> where
%%     <code>Lo_i</code> is the lower bound for the
%%     <code>i<sup>th</sup></code> interval, and
%%     <code>Hi_i</code> is the upper bound.
%%     <ul>
%%       <li> If <code>Lo</code> and <code>Hi</code> are both integers,
%%         then intervals in <code>List</code> have integer bounds.
%%       </li>
%%       <li> If either <code>Lo</code> or <code>Hi</code> is a float,
%%         then intervals in <code>List</code> have floating
%%         point numbers for their bounds.
%%       </li>
%%       <li> If either <code>Lo</code> or <code>Hi</code> is an integer
%%         whose magnitude is too large to represent as a float and the
%%         other is a float, then <code>intervals(Lo, Hi, N)</code> will
%%         produce a 'bad argument in an arithmetic expression' error.
%%       </li>
%%     </ul>
%%   </li>
%% </ul>
%% Examples:
%% <ul>
%%  <li>Hello World</li>
%%  <li><code>misc:intervals(1,100,7) -&gt;</code><b/>
%%    <code>[{1,15},{15,29},{29,43},{43,57},{57,71},{71,85},{85,100}]</code>
%%  </li>
%%  <li><code>misc:intervals(1.0,100,7) -&gt;</code><b/>
%%    <code>[{1.00,15.1429},{15.1429,29.2857},{29.2857,43.4286},{43.4286,57.5714},{57.5714,71.7143},{71.7143,85.8571},{85.8571,100}]</code>
%%  </li>
%% </ul>

intervals(Lo, Hi, 1) -> [{Lo, Hi}];
intervals(Lo, Hi, N) when is_integer(N) and is_integer(Lo) and is_integer(Hi)
	and (N > 0) ->
  Next = Lo + ((Hi - Lo) div N),
  [{Lo, Next} | intervals(Next, Hi, N-1)];
intervals(Lo, Hi, N) when is_integer(N) and is_number(Lo) and is_number(Hi)
	and (N > 0) ->
  Next = Lo + ((Hi - Lo) / N),
  [{Lo, Next} | intervals(Next, Hi, N-1)].

%% @spec mod(N, M) -> rQ
%% @doc like the <code>rem</code> BIF but result has same sign as <code>M</code>.
%% <ul>
%%   <li><code>N</code> and <code>M</code> must be integers or floats.</li>
%%   <li>If <code>N</code> and <code>M</code> are both integers, then the result is an integer.</li>
%%   <li>If either <code>N</code> or <code>M</code> is a float, then the result is a float.</li>
%%     a tuple, <code>{Lo_i, Hi_i}</code> where
%% </ul>
%% Result:
%% <ul>
%%   <li>If <code>M == 0</code> then <code>Q = N</code>.</li>
%%   <li>If <code>M /= 0</code> then <code>Q IN [0, M)</code>.</li>
%%   <li>If <code>M</code> is not 0, then <code>N - Q</code> is a multiple of <code>M</code>.</li>
%% </ul>
mod(N, M) when M == 0 -> N;
mod(N, M) when is_integer(N) and is_integer(M) ->
  R = N rem M,
  if
    (R == 0) -> R;
    (N > 0) == (M > 0) -> R;
    true -> R + M
  end;
mod(N, M) when is_number(N) and is_number(M) ->
  case N / M of
    D when D >= 0 -> N - trunc(D)*M;
    D when D <  0 -> N + (trunc(-D)+1)*M
  end.

%% @spec cut(L, M) -> List_of_M_lists
%% @doc Break L into M, roughly equal length segments.
%% Parameters:
%% <ul>
%%   <li><code>L</code>: a list.</li>
%%   <li><code>M</code>: a positive integer.</li>
%% </ul>
%% Result:
%% <ul>
%%   <li><code>List_of_M_lists</code>: Each of the <code>M</code>
%%     segments has a length of <code>length(L) div M</code> or
%%     <code>(length(L) div M) + 1</code>.
%%   </li>
%% </ul>
%% Example:
%% <ul>
%%   <li><code>cut([0, 1, 2, 3, 4], 3) -> [[0], [1,2], [3,4]]</code></li>
%% </ul>
%% @see deal/2
cut(L, M) when is_integer(M), M > 0 ->
  lists:reverse(cut(L, length(L), M, [])).
cut([], 0, 0, LL) -> LL;
cut(L, N, M, LL) ->
   N1 = N div M,
  {L1, L2} = lists:split(N1, L),
  cut(L2, N-N1, M-1, [L1 | LL]).


%% @spec deal(L, M) -> List_of_M_lists
%% @doc Deal the elements of <code>M</code> around the <code>M</code> lists of <code>List_of_M_lists</code>.
%% Parameters:
%% <ul>
%%   <li><code>L</code>: a list.</li>
%%   <li><code>M</code>: a positive integer.</li>
%% </ul>
%% Result:
%% <ul>
%%   <li><code>List_of_M_lists</code>: The elements of <code>L</code> are
%%     'dealt' in a round-robin fashion in to the <code>M</code> lists
%%     of <code>List_of_M_lists</code>.  
%%     <ul>
%%       <li><code>hd(hd(List_of_M_lists)) = hd(L)</code></li>
%%       <li><code>hd(lists:nth(I, List_of_M_lists)) = lists:nth(I, L)</code>
%%         for <code>I</code> between 1 and <code>M</code> inclusive.
%%       </li>
%%       <li><code>lists:nth((N div M)lists:nth((N rem M)+1, List_of_M_lists))
%%             = lists:nth(N+1, L)</code>,
%%         for <code>N</code> between 0 and <code>length(L)-1</code> inclusive.
%%       </li>
%%     </ul>
%%   </li>
%% </ul>
%% Example:
%% <ul>
%%   <li><code>deal([0, 1, 2, 3, 4], 3) -> [[0,3], [1,4], [2]]</code></li>
%% </ul>
%% @see cut/2
deal(L, M) when is_integer(M), M > 0 ->
  LL = deal(L, [[] || _ <- lists:seq(1,M)], []),
  [ lists:reverse(X) || X <- LL].
deal(L, [], TT) -> deal(L, lists:reverse(TT), []);
deal([Hd | Tl], [A | B], TT) -> deal(Tl, B, [[Hd | A] | TT]);
deal([], HH, TT) -> lists:reverse(TT, HH).
