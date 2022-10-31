#include <R.h>
#include <Rinternals.h>
#include <ympes.h>

//
// TODO - This file should be refactored to avoid code duplication
//

#define MAXBOUND 200

SEXP ages_to_interval(SEXP ages, SEXP limits) {

    // ensure numeric input
    if (!isReal(ages) && !isInteger(ages))
        error("`ages` must be integer(ish).");
    if (!isReal(limits) && !isInteger(limits))
        error("`limits` must be integer(ish).");

    // coerce to integer
    ages = PROTECT(coerceVector(ages, INTSXP));
    limits = PROTECT(coerceVector(limits, INTSXP));

    // check ages are appropriately bounded or NA
    int n_ages = LENGTH(ages);
    int* p_ages = INTEGER(ages);
    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age != NA_INTEGER) {
            if (age < 0 || age >= MAXBOUND)
                error("`ages` must be in the interval `[0, %d)` or NA.", MAXBOUND);
        }
    }

    // create vector of lower and upper bounds
    int n_limits = LENGTH(limits);
    int* p_limits = INTEGER(limits);
    int index[MAXBOUND];

    double* lower;
    lower = (double *) R_alloc(n_limits + 1, sizeof(double));
    double* upper;
    upper = (double *) R_alloc(n_limits + 1, sizeof(double));

    lower[0] = 0;
    for (int i = 0; i < n_limits; ++i) {
        int tmp = p_limits[i];
        // check limits
        if (tmp == NA_INTEGER || tmp <= lower[i])
            error("`limits` must be positive and in strictly increasing order.");
        lower[i + 1] = tmp;
        upper[i] = tmp;
        for (int j = lower[i]; j < upper[i]; j++)
            index[j] = i;
    }
    for (int j = lower[n_limits]; j < MAXBOUND; j++)
        index[j] = n_limits;
    upper[n_limits] = R_PosInf;

    // create output bounds corresponding to ages
    SEXP factor = PROTECT(allocVector(INTSXP, n_ages));
    int* p_factor = INTEGER(factor);

    SEXP lower_bound = PROTECT(allocVector(REALSXP, n_ages));
    double* p_lower_bound = REAL(lower_bound);

    SEXP upper_bound = PROTECT(allocVector(REALSXP, n_ages));
    double* p_upper_bound = REAL(upper_bound);

    for (int i = 0; i < n_ages; i++) {
        if (p_ages[i] == NA_INTEGER) {
            p_lower_bound[i] = NA_REAL;
            p_upper_bound[i] = NA_REAL;
            p_factor[i] = NA_INTEGER;
        } else {
            int tmp = index[p_ages[i]];
            p_lower_bound[i] = lower[tmp];
            p_upper_bound[i] = upper[tmp];
            p_factor[i] = tmp + 1;
        }
    }

    // create levels
    SEXP lvls = PROTECT(allocVector(STRSXP, n_limits + 1));

    // create first name "[0,%d)"
    int bufsz = snprintf(NULL, 0, "[0, %d)", (int) p_limits[0]);
    char* buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[0, %d)", (int) p_limits[0]);
    SET_STRING_ELT(lvls, 0, mkChar(buf));
    R_Free(buf);

    // create middle names "[%d,%d)"
    for (int i = 0; i < n_limits - 1; ++i) {
        bufsz = snprintf(NULL, 0, "[%d, %d)", (int) p_limits[i], (int) p_limits[i+1]);
        buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", (int) p_limits[i], (int) p_limits[i+1]);
        SET_STRING_ELT(lvls, i+1, mkChar(buf));
        R_Free(buf);
    }

    // create last name "[%d, Inf)"
    bufsz = snprintf(NULL, 0, "[%d, Inf)", (int) p_limits[n_limits - 1]);
    buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[%d, Inf)", (int) p_limits[n_limits - 1]);
    SET_STRING_ELT(lvls, n_limits, mkChar(buf));
    R_Free(buf);

    // add levels and class to factor
    setAttrib(factor, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(factor, fclass);

    // create list with lower and upper bound entries
    const char *names[] = {"interval", "lower_bound", "upper_bound", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, factor);
    SET_VECTOR_ELT(out, 1, lower_bound);
    SET_VECTOR_ELT(out, 2, upper_bound);

    // add the data frame class
    SEXP class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    // this format can be seen in the R function .set_row_names()
    SEXP rnms = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -n_ages;
    setAttrib(out, R_RowNamesSymbol, rnms);


    UNPROTECT(10);
    return out;
}

/////////////////////////////////////////////////////////

SEXP split_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP max_upper, SEXP weights) {

    // ensure numeric bounds, counts, max_upper and weights
    if (!isReal(lower_bounds) && !isInteger(lower_bounds))
        error("`lower_bounds` must be integer(ish).");
    if (!isReal(upper_bounds) && !isInteger(upper_bounds))
        error("`upper_bounds` must be integer(ish).");
    if (!isNumeric(counts))
        error("`counts` must be numeric.");
    if ((!isReal(max_upper) && !isInteger(max_upper)) || LENGTH(max_upper) != 1)
        error("`max_upper` must be an integer of length 1.");

    // check max_upper
    max_upper = PROTECT(coerceVector(max_upper, INTSXP));
    int max = INTEGER(max_upper)[0];
    if (max > MAXBOUND || max == NA_INTEGER)
        error("`max_upper` must be less than or equal to %d.", MAXBOUND);

    // check bounds have compatible lengths
    int n_lower_bounds = LENGTH(lower_bounds);
    int n_upper_bounds = LENGTH(upper_bounds);
    if (n_lower_bounds != n_upper_bounds)
        error("`lower_bounds` and `upper_bounds` must be the same length.");

    // check upper bounds are valid and replace infinite values with max_upper
    upper_bounds = PROTECT(coerceVector(upper_bounds, REALSXP));
    double* p_upper_bounds = REAL(upper_bounds);
    lower_bounds = PROTECT(coerceVector(lower_bounds, INTSXP));
    int* p_lower_bounds = INTEGER(lower_bounds);
    for (int i = 0; i < n_upper_bounds; i++) {
        double ubound = p_upper_bounds[i];
        int lbound = p_lower_bounds[i];
        if (R_FINITE(ubound) && ubound > max)
            error("`upper_bounds` can not be greater than `max_upper` unless infinite.");
        if (ubound == R_PosInf)
            p_upper_bounds[i] = max;

        if (lbound != NA_INTEGER && lbound >= ubound)
            error("`lower_bounds` must be less than `upper_bounds`.");
    }
    upper_bounds = PROTECT(coerceVector(upper_bounds, INTSXP));

    // coerce counts and check lengths
    counts = PROTECT(coerceVector(counts, REALSXP));
    double* p_counts = REAL(counts);
    if (n_upper_bounds != LENGTH(counts))
        error("`bounds` and `counts` must be the same length.");

    // check weights
    int null_weights = 1;
    double* p_weights;
    if (TYPEOF(weights) == NILSXP) {
        double value = 1.0 / max;
        p_weights = (double *) R_alloc(max, sizeof(double));
        for (int i = 0; i < max; i++)
            p_weights[i] = value;
    } else if (!isNumeric(weights)) {
        error("`weights` must be numeric.");
    } else {
        null_weights = 0;
        weights = PROTECT(coerceVector(weights, REALSXP));
        int n_weights = LENGTH(weights);
        if (n_weights != max)
            error("`weights` must be a vector of length %d (`max_upper`) representing ages 0:%d", max, max - 1);
        p_weights = REAL(weights);
        for (int i = 0; i < n_weights; i++) {
            if (ISNA(p_weights[i]) || p_weights[i] < 0)
                error("`weights` must be positive and not missing (NA).");
        }
    }

    // pointers to bounds
    int* p_lower = INTEGER(lower_bounds);
    int* p_upper = INTEGER(upper_bounds);

    // calculate length of output
    int total = 0;
    for (int i = 0; i < n_lower_bounds; ++i) {
        if (p_upper[i] == NA_INTEGER || p_lower[i] == NA_INTEGER)
            total += 1;
        else
            total += (p_upper[i] - p_lower[i]);
    }

    // allocate space for ages
    SEXP age = PROTECT(allocVector(INTSXP, total));
    int* p_age = INTEGER(age);

    // allocate space for count
    SEXP count = PROTECT(allocVector(REALSXP, total));
    double* p_count = REAL(count);

    // loop over inputs
    int index = 0;
    for (int i = 0; i < n_lower_bounds; ++i) {
        int interval_start = p_lower[i];
        int interval_end = p_upper[i];
        double ct = p_counts[i];
        if (interval_start != NA_INTEGER && interval_end != NA_INTEGER) {
            double sum = 0;
            int new_index = index;
            for (int j = interval_start; j < interval_end; j++) {
                sum += p_weights[j];
                p_age[new_index] = j;
                new_index++;
            }
            new_index = index;
            if (fabs(sum) > sqrt(DBL_EPSILON)) {
                for (int j = interval_start; j < interval_end; j++) {
                    p_count[new_index] = ct * p_weights[j] / sum;
                    new_index++;
                }
            } else {
                for (int j = interval_start; j < interval_end; j++) {
                    p_count[new_index] = ct * p_weights[j];
                    new_index++;
                }
            }
            index = new_index;
        } else {
            p_age[index] = NA_INTEGER;
            p_count[index] = ct;
            index++;
        }
    }

    // create list with age and count entries
    const char *names[] = {"age", "count", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, age);
    SET_VECTOR_ELT(out, 1, count);

    // add the data frame class
    SEXP class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    // this format can be seen in the R function .set_row_names()
    SEXP rnms = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -total;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(11 - null_weights);
    return out;
}

/////////////////////////////////////////////////////////

SEXP aggregate_age_counts(SEXP counts, SEXP ages, SEXP limits) {

    // ensure numeric input
    if (!isNumeric(counts))
        error("`counts` must be numeric.");
    if (!isReal(ages) && !isInteger(ages))
        error("`ages` must be integer(ish).");
    if (!isReal(limits) && !isInteger(limits))
        error("`limits` must be integer(ish).");

    // coerce input
    counts = PROTECT(coerceVector(counts, REALSXP));
    ages = PROTECT(coerceVector(ages, INTSXP));
    limits = PROTECT(coerceVector(limits, INTSXP));

    // check ages are appropriately bounded or NA
    int n_ages = LENGTH(ages);
    int* p_ages = INTEGER(ages);
    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age != NA_INTEGER) {
            if (age < 0 || age >= MAXBOUND)
                error("`ages` must be in the interval `[0, %d)` or NA.", MAXBOUND);
        }
    }

    // check ages and counts are of equal length
    if (n_ages != LENGTH(counts))
        error("`ages` and `counts` must be the same length.");

    // add MAXBOUND to limits
    int n_limits = LENGTH(limits);

    // Think we can get away with this as we have already ensured no NA in limits
    // on R side. May be better to coerce first to be on safe side
    SEXP new_limits = PROTECT(allocVector(REALSXP, n_limits + 1));
    int* p_limits = INTEGER(limits);
    double* p_new_limits = REAL(new_limits);
    int tmp = 0;
    for (int i = 0; i < n_limits; ++i) {
        int lim = p_limits[i];
        if (lim == NA_INTEGER || lim <= tmp)
            error("`limits` must be positive and in strictly increasing order.");
        p_new_limits[i] = p_limits[i];
        tmp = lim;
    }
    p_new_limits[n_limits] = MAXBOUND;
    ++n_limits;

    // order by age
    // ind = order(x, nalast = TRUE, decreasing = FALSE)
    // R_orderVector1(result, length, input to sort, nalast, decreasing)
    int* ind;
    ind = (int *) R_alloc(n_ages, sizeof(int));

    int* out_ages;
    out_ages = (int *) R_alloc(n_ages, sizeof(int));

    double* p_counts = REAL(counts);
    double* out_counts;
    out_counts = (double *) R_alloc(n_ages, sizeof(double));
    R_orderVector1(ind, n_ages, ages, TRUE, FALSE);
    for (int i = 0; i < n_ages; i++) {
        out_ages[i] = p_ages[ind[i]];
        out_counts[i] = p_counts[ind[i]];
    }

    // number of groups (allowing for an NA group)
    int n_groups = n_limits + 1;

    // allocate output and initialise to 0
    SEXP group_counts = PROTECT(allocVector(REALSXP, n_groups));
    double* p_groups = REAL(group_counts);
    Memzero(p_groups, n_groups);

    // calculate the counts
    int group_index = 0;
    for (int i = 0; i < n_ages; ++i) {
        int current_age = out_ages[i];
        double tmp = out_counts[i];
        if (current_age == NA_INTEGER) {
            p_groups[n_limits] += tmp;
        } else {
            while(group_index < n_groups - 2 && current_age >= p_limits[group_index])
                ++group_index;
            p_groups[group_index] += tmp;
        }
    }

    // generate the corresponding intervals
    SEXP start = PROTECT(allocVector(REALSXP, n_groups));
    SEXP end = PROTECT(allocVector(REALSXP, n_groups));
    double* p_start = REAL(start);
    double* p_end = REAL(end);

    SEXP factor = PROTECT(allocVector(INTSXP, n_groups));
    SEXP lvls = PROTECT(allocVector(STRSXP, n_groups - 1)); // No NA level
    int* p_factor = INTEGER(factor);

    p_start[0] = 0;
    p_end[0] = p_new_limits[0];
    p_factor[0] = 1;

    // create first name "[0,%d)"
    int bufsz = snprintf(NULL, 0, "[0, %d)", (int) p_new_limits[0]);
    char* buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[0, %d)", (int) p_new_limits[0]);
    SET_STRING_ELT(lvls, 0, mkChar(buf));
    R_Free(buf);

    for (int i=0; i < n_groups - 2; ++i) {
        p_factor[i+1] = i+2;
        p_start[i+1] = p_new_limits[i];
        p_end[i+1] = p_new_limits[i+1];
        // create middle names "[%d,%d)"
        bufsz = snprintf(NULL, 0, "[%d, %d)", (int) p_new_limits[i], (int) p_new_limits[i+1]);
        buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", (int) p_new_limits[i], (int) p_new_limits[i+1]);
        SET_STRING_ELT(lvls, i+1, mkChar(buf));
        R_Free(buf);
    }

    p_end[n_groups -2] = R_PosInf;
    p_start[n_groups - 1] = NA_REAL;
    p_end[n_groups -1] = NA_REAL;
    p_factor[n_groups - 1] = NA_INTEGER;

    // create last name "[%d,Inf)"
    bufsz = snprintf(NULL, 0, "[%d, Inf)", (int) p_new_limits[n_groups-3]);
    buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[%d, Inf)", (int) p_new_limits[n_groups-3]);
    SET_STRING_ELT(lvls, n_groups-2, mkChar(buf));
    R_Free(buf);

    // add levels and class to factor
    setAttrib(factor, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(factor, fclass);

    // array of names; note the null string
    const char *names[] = {"interval" ,"lower_bound", "upper_bound", "count", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, factor);
    SET_VECTOR_ELT(out, 1, start);
    SET_VECTOR_ELT(out, 2, end);
    SET_VECTOR_ELT(out, 3, group_counts);

    // add class
    SEXP class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    SEXP rnms = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -n_groups;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(13);

    return out;
}

/////////////////////////////////////////////////////////

SEXP reaggregate_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP limits, SEXP max_upper, SEXP weights) {
    SEXP split = PROTECT(split_interval_counts(lower_bounds, upper_bounds, counts, max_upper, weights));
    SEXP out = PROTECT(aggregate_age_counts(VECTOR_ELT(split, 1), VECTOR_ELT(split, 0), limits));
    UNPROTECT(2);
    return out;
}
