#include <R.h>
#include <Rinternals.h>

#ifndef _YMPES
#define _YMPES

// convert ages to interval
// Assumes: ages are integer with 200 > age >= 0 (NA allowed)
//          not all ages are NA
//          limits are integer, unique, sorted (increasingly) and not NA
SEXP ages_to_interval(SEXP ages, SEXP limits);

// calculate the aggregated age counts
// Assumes: integer bounds
//          bounds equal length
//          no missing (NA) bounds
//          lower bounds less than upper bounds
//          weights >= 0 and of length max(upper_bounds) corresponding to ages 0:(max - 1)
SEXP split_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP max_upper, SEXP weights);

// calculate the aggregated age counts
// Assumes: ages are integer and non-negative
//          ages is the same length as counts
//          counts are numeric
//          limits are integer, unique, sorted (increasingly) and not NA
SEXP aggregate_age_counts(SEXP counts, SEXP ages, SEXP limits);

// calculate the aggregated age counts
// Assumes: integer bounds of equal length
//          lower bounds less than upper bounds
//          counts are numeric
//          limits are integer, unique, sorted (increasingly) and not NA
//          weights >= 0 and of length max(upper_bounds) corresponding to ages 0:(max - 1)
SEXP reaggregate_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP limits, SEXP max_upper, SEXP weights);

#endif
