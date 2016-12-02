%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc time_it -- functions for measuring execution time.

-module time_it.
-export [t/5, t/3, t/2, t/1, log/1, log/2, log/3, print_log/1].
-export [how_many_runs/2, perseverate/2].
% -export [default_how_long/0, default_return_spec/0].

% internal functions, uncomment if needed for debugging
% -export [do_time/4, default_how_long/0, default_return_spec/0, print_log2/1].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%						%
% t: time measurement				%
%						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @type contfn0() =  fun((N:: integer, T_total::float()) -> boolean()).
%%   A "continue" function for the <code>t(...)</code> functions
%%   without an "accumulator" argument.

%% @type contfnA() =  fun((N:: integer, T_total::float(), Acc::term) ->boolean()).
%%   A "continue" function for the <code>t(...)</code> functions
%%   with an "accumulator" argument.

%% @type contfn() = contfn0() | contfnA().
%%   A "continue" function for the <code>t(...)</code> functions

%% @type cont_spec() = integer() | float() | { integer(), float() }.
%%  A "continue" specification -- a limit on the number of times to call
%%  the function being timed, and/or a limit on the total execution time
%%  for those calls.  For example, let <code>CS</code> be a parameter of
%%  type <code>cont_spec</code> to <code>t/2</code> or <code>t/3</code>,
%%  and let <code>Time_This</code> be the function whose execution time
%%  is being measured.
%%  <ul>
%%    <li>If <code>CS</code> is an integer, then it specifies how many times
%%	the <code>Time_This</code> function should be executed.</li>
%%    <li>If <code>CS</code> is a float, then it specifies the total elapsed
%%      time (in seconds) for executing the <code>Time_This</code> function.
%%	More precisely, the <code>Time_This</code> function will be
%%	called until this limit is met or exceeded.</li>
%%    <li>If <code>CS</code> is a tuple of the form
%%	<code>{N_iter, T_limit}</code> then the <code>Time_This</code>
%%	function will be executed at most <code>N_iter</code> times or
%%	until the total elapsed time exceeds <code>T_limit</code>.</li>
%%  </ul>
%%  <b>Failures</b>: If the <code>Time_This</code> function returns the
%%  atom <code>'failed'</code> it is deemed to have failed.  The
%%  <code>t(...)</code> function will terminate, returning the atom
%%  <code>'failed'</code>.

%% @type postfn0() = fun((N:: integer, T_total::float()) -> term()).
%%   Post-processing for the <code>t(...)</code> functions without an
%%   "accumulator".

%% @type postfnA() = fun((N:: integer, T_total::float()) -> term()).
%%   Post-processing for the <code>t(...)</code> functions with an
%%   "accumulator".

%% @type postfn() = postfn0() | postfnA().
%%   Post-processing for the <code>t(...)</code> functions.

%% @type result_atom() = mean | std | raw | values.
%% @type result_spec() = result_atom() | [ result_atom() ].
%% A "result" specification.  The return value of the 
%% <code>t/2</code> or <code>t/3</code> function will be an association
%% list.  The tuples to include in the list are specified by the
%% <code>result_atom</code> or list of such atoms as described below:
%% <ul>
%%   <li> <code>mean</code>: include a tuple of the form
%%	<code>{mean, Mean}</code> where <code>Mean</code> is the
%%	average execution time of the <code>Time_This</code> function.</li>
%%   <li> <code>std</code>: include a tuple of the form
%%	<code>{std, Std}</code> where <code>Std</code> is the (sample)
%%	standard-deviation of the execution times of the
%% 	<code>Time_This</code> function.</li>
%%   <li> <code>raw</code>: include a tuple of the form
%%	<code>{raw, Times}</code> where <code>Times</code> is a list
%%	of the elapsed times for the executions of the <code>Time_This</code>
%%	function.</li>
%%   <li> <code>values</code>: include a tuple of the form
%%	<code>{raw, V}</code> where <code>V</code> is a list
%%	of the return values from the executions of the
%%	<code>Time_This</code> function.</li>
%% </ul>

%% @spec t(Time_This, Continue, AccFn, Acc0, PostFn) -> term2()
%%   Time_This = fun(() -> term1()),
%%   Continue  = contfn(),
%%   AccFn     = accfn(),
%%   Acc0      = term2(),
%%   PostFn    = fun((N::integer(), T_Total::float(), AccOut::term2()) -> term3())
%% @doc Measure the execution time of a function provided by the caller.
%%   This is the most general form of the <code>t(...)</code> functions.
%%   The caller provides functions to determine how many times the
%%   <code>Time_This</code> function should be called, what data is
%%   accumulated from these calls, and how to process the accumulated
%%   data to produce the final result.
%%   <br/>
%%   Our parameters:
%%   <ul>
%%     <li> <code>Time_This</code> a zero-argument function that is
%%       called repeatedly.  The time for each execution is measured
%%	 by calling
%%       <a href="http://www.erlang.org/doc/man/erlang.html#monotonic_time-0"><code>erlang:monotonic_time()</code></a>
%%	 before and after.
%%     </li>
%%     <li> <code>Continue</code> a boolean valued function that
%%	 is called before each invocation of <code>Time_This</code>.
%%       If <code>Continue</code> returns <code>true</code>, then
%%	 we keep gathering timing data.  Otherwise, we're done.
%%	 <code>Continue</code> can take two or three arguments:
%%	 <ul>
%%         <li>Continue(N, T_total)</li>
%%         <li>Continue(N, T_total, Acc)</li>
%%	 </ul>
%%       where
%%	 <ul>
%%         <li> <code>N</code> is the number of times that
%%           <code>Time_This</code> has been called;</li>
%%         <li> <code>T_Total</code> is the total time spent in these calls;
%%           and </li>
%%	   <li> <code>Acc</code> is the value of the accumulator (see below).
%%	   </li>
%%	 </ul>
%%     </li>
%%     <li> <code>AccFn</code> and <code>AccIn</code> implement an
%%       accumulator of the timing values (and optionally the return
%%       values of <code>Time_This</code>).  <code>AccFn</code> can take from
%%	 2 to 5 arguments as described below:
%%	 <ul>
%%	   <li> <code>AccFn(AccIn, T_elapsed)</code></li>
%%	   <li> <code>AccFn(AccIn, T_elapsed, V)</code></li>
%%	   <li> <code>AccFn(AccIn, N, T_total, T_elapsed)</code></li>
%%	   <li> <code>AccFn(AccIn, N, T_total, T_elapsed, V)</code></li>
%%	 </ul>
%%	 where
%%	 <ul>
%%	   <li> <code>AccIn</code> is the result of <code>AccFn</code>
%%	     from the previous call.  The first time <code>Time_This</code>
%%	     is evaluated, <code>AccFn</code> is called with
%%	     <code>AccIn = Acc0</code>.</li>
%%	   <li> <code>T_elapsed</code> is the time that was spent for the
%%	     current evaluation of <code>Time_This</code>.</li>
%%	   <li> <code>N</code> is the total number of times that 
%%           <code>Time_This</code> has been called; and</li>
%%         <li> <code>T_Total</code> is the total time spent in these calls.
%%	   </li>
%%	 </ul>
%%	 Note that the values of <code>N</code> and <code>T_Total</code>
%%       are accumulated by the <code>t(...)</code> function; thus
%%       <code>AccFn</code> doesn't need to handle these.
%%     </li>
%%     <li> <code>PostFn</code> compute the final result.  In particular
%%	 the return value of <code>t(...)</code> is
%%       <code>PostFn(N, T_Total, AccOut)</code> where
%%	 <ul>
%%	   <li> <code>N</code> is the total number of times that 
%%           <code>Time_This</code> has been called;</li>
%%         <li> <code>T_Total</code> is the total time spent in these calls;
%%	     and </li>
%%         <li> <code>AccOut</code> is the final result from
%%	     <code>AccFn</code>.</li>
%%	 </ul>
%%     </li>
%%   </ul>
t(Time_This, Continue, AccFn, Acc0, PostFn) ->
  case do_time(Time_This, Continue, AccFn, {0, 0.0, Acc0}) of
    {N, T_total, AccOut} -> PostFn(N, T_total, AccOut);
    failed -> failed
  end.
    

%% @spec t(Time_This, Cont, Result) -> term2()
%%   Time_This = fun(() -> term1()),
%%   Cont = contfn0() | cont_spec(),
%%   Result = postfn0() | result_spec()
%% @doc A wrapper function that calls
%%   <code>t(TimeThis, ContinueFn, AccFn, Acc0, PostFn)</code>.
%%   The <code>Cont</code> parameter gives the condition for continuing to
%%   execute <code>Time_This</code> (and gathering data).
%%   <ul>
%%     <li> If <code>Cont</code> is a function, then it is used as
%%       <code>ContinueFn</code> in <code>t/5</code>.</li>
%%     <li> If <code>Cont</code> is a <code>cont_spec()</code>, then
%%       it is interpreted as described for the
%%	{@link cont_spec()} type.</li>
%%   </ul>
%%   The <code>Result</code> parameter describes what value should be
%%   returned.
%%   <ul>
%%     <li> If <code>Result</code> is a function, then it is used as
%%       <code>PostFn</code> in <code>t/5</code>.</li>
%%     <li> If <code>Result</code> is a <code>result_spec</code>, then
%%	 it is interpreted as described for the {@link result_spec()} type.</li>
%%   </ul>
%%  <b>Failures</b>: If the <code>Time_This</code> function returns the
%%  atom <code>'failed'</code> it is deemed to have failed.  The
%%  <code>t(...)</code> function will terminate, returning the atom
%%  <code>'failed'</code>.
t(Time_This, C, P) ->
  Continue = case C of
    {N_iter, T_limit} ->
      fun(N, T_total) ->
        (N < N_iter) and (T_total < T_limit) end;
    N_iter  when is_integer(N_iter) ->
      fun(N, _T_total) -> (N < N_iter) end;
    T_limit when is_float(T_limit)  ->
      fun(_N, T_total) -> (T_total < T_limit) end;
    ContFn when is_function(ContFn) -> ContFn
  end,
  P2 = if
    is_atom(P) -> [P];
    true -> P
  end,
  AccFn = if
    is_function(P) -> default;
    true -> fun(AccIn, T_elapsed, V) ->
      [ { X,
	  case X of
	    std    -> Y + T_elapsed*T_elapsed;
	    raw    -> [T_elapsed | Y];
	    values -> [V | Y]
	  end
	} || {X, Y} <- AccIn
      ] end
  end,
  Acc0 = if
    is_function(P) -> {0, 0.0};
    true ->
      [    { X, case X of std -> 0; raw -> []; values -> [] end}
        || X <- P2, X /= mean
      ]
  end,
  PostFn = if
    is_function(P) -> fun(N, T_total, _AccOut) -> P(N, T_total) end;
    true -> fun(N, T_total, AccOut) ->
      [ { X,
	  case X of
	    mean when N > 0 -> T_total/N;
	    mean when N == 0 -> undefined;
	    std  when N >  1 ->
	      {std, S2} = lists:keyfind(std, 1, AccOut),
	      D2 = S2 - (T_total*T_total/N),
	      case D2 >= 0 of
	        true  -> math:sqrt(D2/(N-1));
		false -> 0.0  % D2 can be tiny and negative due to round-off
	      end;
	    std  when N =< 1 -> undefined;
	    raw  ->
	      {raw, Raw} = lists:keyfind(raw, 1, AccOut),
	      Raw;
	    values  ->
	      {values, Values} = lists:keyfind(values, 1, AccOut),
	      Values
	  end
	} || X <- P2
      ] end
  end,
  t(Time_This, Continue, AccFn, Acc0, PostFn).


%% @spec t(Time_This, What) -> Stuff
%%   Time_This = fun(() -> term()),
%%   What = cont_spec() | result_spec(),
%%   Stuff = [ { result_atom(), term() } ]
%% @doc Measure the execution time of <code>Time_This</code>.
%%   <ul>
%%     <li> If <code>What</code> is a <code>cont_spec()</code>, then
%%       it is interpreted as described for the
%%	{@link cont_spec()} type.  The return value of
%%	<code>t(Time_This, What)</code> is an association list of the form
%%	<code>[{mean, Mean}, {std, Std}]</code> where <code>Mean</code> is
%%      the average time of the executions of <code>Time_This</code> and
%%	<code>Std</code> is the standard deviation.</li>
%%     <li> If <code>What</code> is a <code>result_spec</code>, it is
%%	interpreted as described for the {@link result_spec()} type.
%%	The <code>Time_This</code> function will be executed until
%%	the total elapsed time is at least one second.</li>
%%   </ul>
%%   <b>Failures</b>: If the <code>Time_This</code> function returns the
%%   atom <code>'failed'</code> it is deemed to have failed.  The
%%   <code>t(...)</code> function will terminate, returning the atom
%%   <code>'failed'</code>.
t(Time_This, How_Long) when (is_tuple(How_Long) or is_number(How_Long)) ->
  t(Time_This, How_Long, default_return_spec());
%% @equiv t(Time_This, 1.0, Return_Spec)
t(Time_This, Return_Spec) when is_list(Return_Spec) ->
  t(Time_This, default_how_long(), Return_Spec).

%% @spec t(Time_This) -> [ { result_atom(), number() } ]
%% @doc Measure the mean and standard deviation of the elapsed time for
%%   executing a function provided by the caller.
%%   The function will be called repeatedly until the total elapsed time
%%   is at least one second.  The return value is an association list
%%   of the form <code>[ {mean, Mean}, {std, Std} ]</code> where
%%   <code>Mean</code> is the average elapsed time and <code>Std</code>
%%   is the standard deviation.  There is no guarantee of the order of
%%   these two tuples in the list.  The function
%%   {@link lists:keyfind/3} should be used to extract specific values.
%%   <br/>
%%   <b>Failures</b>: If the <code>Time_This</code> function returns the
%%   atom <code>'failed'</code> it is deemed to have failed.  The
%%   <code>t(...)</code> function will terminate, returning the atom
%%   <code>'failed'</code>.
t(Time_This) -> t(Time_This, default_how_long(), default_return_spec()).


% helper functions for t(...):
%   elapsed(T0, T1), do_time(...), default_how_long(), default_return_spec()

% elapsed(T0, T1): report the time elapsed from T0 to T1.
%   our return value is a float, and the time is measured in seconds.
elapsed(T0, T1) when is_integer(T0), is_integer(T1) ->
  1.0e-9 * erlang:convert_time_unit(T1-T0, native, nano_seconds).

% do_time: repeatedly call Time_This accumulating the elapsed times
%   (and anything else that AccFn does).
do_time(Time_This, Continue, AccFn, AccIn) ->
  N = element(1, AccIn),
  T_total = element(2, AccIn),
  Cont = if
    is_function(Continue, 2) -> Continue(N, T_total);
    is_function(Continue, 3) -> Continue(N, T_total, element(3, AccIn))
  end,
  case Cont of
    true ->
      T0 = erlang:monotonic_time(),
      V = Time_This(),
      T1 = erlang:monotonic_time(),
      T_elapsed = elapsed(T0, T1),
      (fun() -> ok end)(),
      AccOut = if
        (AccFn==default) and tuple_size(AccIn) == 2 ->
	  {N+1, T_total + T_elapsed};
        (AccFn==default) and tuple_size(AccIn) == 3 ->
	  {N+1, T_total + T_elapsed, element(3, AccIn)};
        is_function(AccFn, 2) ->
	  {N+1, T_total + T_elapsed, AccFn(element(3, AccIn), T_elapsed)};
        is_function(AccFn, 3) ->
	  {N+1, T_total + T_elapsed, AccFn(element(3, AccIn), T_elapsed, V)};
        is_function(AccFn, 4) ->
	  { N+1, T_total + T_elapsed,
	    AccFn(element(3, AccIn), N, T_total, T_elapsed) };
        is_function(AccFn, 5) ->
	  { N+1, T_total + T_elapsed,
	    AccFn(element(3, AccIn), N, T_total, T_elapsed, V) }
      end,
      case V == failed of
        true  -> failed;
        false -> do_time(Time_This, Continue, AccFn, AccOut)
      end;
    false -> AccIn
  end.

%%  If the user invokes <code>t(F, ...)</code> without giving a
%%  specification of how many times to run <code>F</code>, then run
%% it until its total run-time is <code>default_how_long()</code>
%% seconds.
default_how_long() -> 1.0.


%   If the user invokes t(F, ...) without saying what results are
%   requested, it will return a tuple-list of the form
%   [{mean, Mean}, {std, Std}], where Mean is the average run-time
%   and Std is the standard deviation.
default_return_spec() -> [mean, std].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%						%
% log: log the time of events			%
% print_log: print such a log			%
%						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @spec log(Log, Format, Data) -> event_log()
%%   Log = event_log(),
%%   Format = string(),
%%   Data = [ term() ]
%% @doc Generate an event log.
%%   The parameters <code>Format</code> and <code>Data</code> are
%%   as for <a href="http://www.erlang.org/doc/man/io_lib.html#format-2"><code>io_lib:format(Format, Data)</code></a>.
%%   The <code>log</code> function generates a record to record
%%   <code>Format</code> and <code>Data</code> along with the pid of the
%%   calling process and the current time.  This record is appended to the
%%   log.  The log can be printed using the <code>print_log</code> function.
%%   <br/>
%%   <b>Notes</b>
%%   <ul>
%%     <li> <code>event_log()</code> is a deeplist.  Thus, if
%%       <code>Log1</code> and <code>Log2</code> are event logs, then so
%%       are <code>[Log1, Log2]</code> and <code>[Log1 | Log2]</code>.
%%	 This is to facilitate writing functions that return
%%	 <code>event_log</code>'s and sending <code>event_log</code>'s
%%	 in messages.  Merging such logs only requires wrapping them
%%	 them up as a list.</li>
%%     <li> The call to <code>io_lib:format(Format, Data)</code> is
%%	 deferred to <code>print_log</code>.  This is done to make
%%	 calls to the <code>log</code> function execute as quickly as
%%       possible.</li>
%%   </ul>
log(Log, Format, Data) -> [{erlang:monotonic_time(), self(), [Format | Data]} | Log].

%% @spec log(Format_or_Log, Data_or_PlainString) -> event_log()
%%   Format_or_Log = string() | event_log(),
%%   Data_or_PlainString = [ term() ] | string()
%% @doc Create or update an event log.
%%   There are two ways to call this function:
%%   <ul>
%%     <li> <code>log(Format::string(), Data::[term()]</code> <br/>
%%       Create a new event log with an entry for <code>Format</code> and
%%       <code>Data</code>.  The <code>Format</code> must be a non-empty
%%       string.</li>
%%     <li> <code>log(Log::event_log(), PlainString::string()</code> <br/>
%%       Add an entry to <code>Log</code> for <code>PlainString</code>.</li>
%%   </ul>
log(Format, Data)
    when (is_list(Format) andalso (length(Format) > 0)
  			  andalso is_integer(hd(Format)))
  -> log([], Format, Data);
log(Log, PlainString) -> log(Log, PlainString, []).

%% @spec log(PlainString) -> event_log()
%% @doc %%   Create a new event log with an entry for <code>PlainString</code>.
log(PlainString) -> log([], PlainString, []).


%% @spec print_log(Log::event_log()) -> ok
%% @doc Print the contents of an event log to the standard output.
%%   As noted above, an <code>event_log</code> is a deep list.
%%   <code>print_log</code> flattens this list and then prints the
%%   events in time order.  The time of the first event is taken as 0,
%%   and for other events, the time is reported as the time since the
%%   first event.
%% @todo
%%   <ul>
%%     <li> Add an optional parameter to specify an output file.</li>
%%     <li> Add an interface to the <code>workers</code> module so that
%%	 if a pid for a process is that of a worker, it can be given a
%%       meaningful name.</li>
%%   </ul>
print_log(Log) ->
  case lists:flatten(Log) of
    [] -> io:format("empty event log~n");
    FlatLog -> T0 = lists:min(FlatLog), % find the earliest event time
      % convert times to seconds and format the event description
      LogSec = [    { elapsed(T0, T), P,
		      case X of
			[] -> "";
			[Fmt] -> Fmt;
			[Fmt | Args] -> io_lib:format(Fmt, Args)
		      end
		    }
		 || {T, P, X} <- FlatLog ],
      print_log2(lists:sort(LogSec))
  end.

% print_log2: now that print_log has sorted the events, and prepared them
 %   for printing, we do the actual printing.
print_log2([]) -> ok;
print_log2([{T, P, S} | Tail]) ->
  if
    T == 0 -> io:format("~12.10b ~w: ~s~n", [0, P, S]);
    T /= 0 -> io:format("~12.6g ~w: ~s~n", [T,  P, S])
  end,
  print_log2(Tail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%						%
% A few helpful odds and ends			%
%   default_low_long()				%
%   default_return_spec()			%
%						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @spec how_many_runs(F, T) -> integer()
%% @doc  determine how many times a function needs to be called to use
%%   a total elapsed time in a given interval.
%%  In particular,
%%  <ul>
%%    <li> <code>how_many_runs(F, {Tlo, Thi})</code> estimates how many times
%%      to call <code>F</code> so that the total elapsed time will be
%%      between <code>Tlo</code> and <code>Thi</code>.</li>
%%    <li> <code>how_many_runs(F, T)</code> when <code>T</code> is an integer
%%      of float is equivalent to <code>how_many_runs(F, {T, T})</code> --
%%      in other words, <code>how_many_runs</code> tries to get as close
%%	to the target elapsed time, <code>T</code> as it can.  See the
%%      caveats below for the limits of the accuracy of
%%      <code>how_many_runs</code></li>
%%  </ul>
%%  The implementation uses binary search.  This means that we can't
%%  guarantee that the actual runtime will actually be in the interval:
%%  <ul>
%%    <li> Variations in the elapsed time for different executions can
%%	cause inconsistencies in the result.</li>
%%    <li> If <code>Tlo</code> and <code>Thi</code> are sufficiently
%%	close to each other there may be no suitable number of calls.</li>
%%    <li> If a single execution of <code>F</code> takes longer than
%%      <code>Thi</code>, then <code>how_many_runs</code> will return
%%      the integer <code>1</code>.</li>
%%  </ul>
%%  With these limitations in mind, the function
%%  <code>how_many_runs(...)</code> can be helpful for estimating a
%%  "reasonable" number of executions to use for making time measurements.
%%  In particular, if the execution time of <code>F</code> is small compared
%%  with the granularity of
%%  <a href="http://www.erlang.org/doc/man/erlang.html#monotonic_time-0"><code>erlang:monotonic_time()</code></a>,
%%  then <code>how_many_runs(F, ...)</code> can give a reasonable
%%  choice for how many times to call <code>F</code> to get meaningful
%%  time measurements.
%%  <br/>
%% See also: {@link time_it:perseverate/2}.
how_many_runs(F, Tbnds) when is_tuple(Tbnds) ->
  how_many_runs(F, {1, infinity}, Tbnds);
how_many_runs(F, T) when is_number(T) -> how_many_runs(F, {T, T}).


how_many_runs(F, {Nlo, infinity}, Tbnds = {Tlo, Thi}) ->
  T0 = erlang:monotonic_time(),
  perseverate(F, Nlo),
  T1 = erlang:monotonic_time(),
  T_elapsed = elapsed(T0, T1),
  if
    (Tlo =< T_elapsed) and ((T_elapsed =< Thi) or (Nlo == 1)) -> Nlo;
    (Tlo =< T_elapsed) ->
      how_many_runs(F, {Nlo div 2, Nlo}, Tbnds);
    (T_elapsed < Tlo) ->
      how_many_runs(F, {2*Nlo, infinity}, Tbnds)
  end;

how_many_runs(_F, {Nlo, Nhi}, _Tbnds) when Nlo >= (Nhi-1) -> Nhi;

how_many_runs(F, {Nlo, Nhi}, Tbnds = {Tlo, Thi}) ->
  Nmid = (Nlo + Nhi) div 2,
  T0 = erlang:monotonic_time(),
  perseverate(F, Nmid),
  T1 = erlang:monotonic_time(),
  T_elapsed = elapsed(T0, T1),
  if
    (Tlo =< T_elapsed) and (T_elapsed =< Thi) -> Nmid;
    (T_elapsed < Tlo) -> how_many_runs(F, {Nmid, Nhi}, Tbnds);
    (Thi < T_elapsed) -> how_many_runs(F, {Nlo, Nmid}, Tbnds)
  end.
  
%% @spec perseverate(F, N) -> term1()
%%   F = fun(() -> term1()),
%%   N = integer()
%% @doc Call a function multiple times.
%%   Call the function <code>F</code> <code>N</code> times.
%%   If N=0, return 'ok'.  Otherwise, return the value from the last
%%   call to <code>F</code>.
%% <br/>
%% See also: {@link time_it:how_many_runs/2}.
perseverate(_F, 0) -> ok;
perseverate(F, 1) -> F();
perseverate(F, N) -> F(), perseverate(F, N-1).
