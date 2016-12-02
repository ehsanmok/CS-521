/* time_it.h:  header file for time_it.cu
 *
 * Two struct definitions and a few functions for measuring the elapsed
 * time for code execution:
 *
 * struct time_it_raw:   used to gather timing data (opaque)
 * struct time_it_stats: mean and standard deviation for run-time.
 *
 * time_it_create:     create a time_it_raw structure
 * time_it_run:        measure elapsed times
 * time_it_get_stats:  calculate mean and standard deviation from a struct time_it_raw.
 * time_it_free:       free a struct time_it_raw.
 *
 * Typical usage:
 *   void my_fun(void *varg) { // function to time
 *     my_arg_type *my_arg = (my_arg_type *)varg;
 *     // measure the time for this code
 *   }
 *
 *   main(int argc, char **argv) {
 *     ...
 *     struct time_it_raw *tr = time_it_create(100);
 *     struct time_it_stats ts;
 *     my_arg_type my_arg;
 *     // initialize my_arg
 *
 *     time_it_run(tr, my_fun, (void *)(&my_arg));
 *     time_it_get_stats(tr, &ts);
 *     printf("elapsed time: mean = %10.3e, std = %10.3e\n",
 *            ts->mean, ts->std);
 *     time_it_free(tr);
 */

// struct time_it_raw: used by timing_run to record execution times

// struct timing_stats: mean and standard deviation of run times.
struct time_it_stats {
  double mean;  // average execution time, in seconds
  double std;   // standard deviation of execution time, in seconds
};

// time_it_create: create a structre for time_it_run.  
//   ntrials is the number of times to execute the given function.
//   Note that the pointer returned by time_it_create can be used for multiple
//   calls of time_it_run.  Each such trial will execute the user-provided
//   function ntrials times.  The results from previous calls will be
//   overwritten.
extern struct time_it_raw *time_it_create(int ntrials);

// Call fn(arg) ntrials times (where ntrials was given when creating tr).
extern void time_it_run(struct time_it_raw *tr, void (*fn)(void *), void *arg);

extern void time_it_get_stats(struct time_it_raw *tr, struct time_it_stats *s);
extern void time_it_free(struct time_it_raw *tr);
