#include <R.h>
#include <Rinternals.h>

SEXP c_internal_aggregate_age_counts(SEXP counts, SEXP ages, SEXP limits) {

    // check and coerce counts input
    if (!isNumeric(counts))
        error("`counts` must be numeric.");
    SEXP dbl_counts = PROTECT(coerceVector(counts, REALSXP));
    int n_counts = LENGTH(dbl_counts);
    double* p_counts = REAL(dbl_counts);

    // check and coerce ages input
    if (!isNumeric(ages))
        error("`ages` must be numeric.");
    SEXP int_ages = PROTECT(coerceVector(ages, INTSXP));
    int n_ages = LENGTH(int_ages);
    int* p_ages = INTEGER(int_ages);
    int flag_na = 0;
    int age;
    for (int i = 0; i < n_ages; ++i) {
        age = p_ages[i];
        if (age == NA_INTEGER) {
            flag_na = 1;
        } else if (age < 0) {
            error("`ages` must be non-negative");
        }
    }

    // check ages and counts have the same length
    if (n_ages != n_counts)
        error("`counts` and `ages` must be the same length.");

    // check and coerce limits input to integer
    // we have to duplicate when integer as we will later be sorting and do not
    // want to accidentally sort the user input
    SEXP int_limits;
    switch (TYPEOF(limits)) {
        case INTSXP:
            int_limits = PROTECT(duplicate(limits));
            break;
        case REALSXP:
            int_limits = PROTECT(coerceVector(limits, INTSXP));
            break;
        default:
            error("`limits` must be numeric.");
    }

    // check limits contains no NA and are all strictly positive
    int n_limits = LENGTH(limits);
    int* p_limits = INTEGER(int_limits);
    int limit;
    for (int i = 0; i < n_limits; ++i) {
        limit = p_limits[i];
        if (limit == NA_INTEGER)
            error("`limits` must not contain missing (NA) values.");
        if (limit <= 0) {
            error("`limits` must be strictly positive.");
        }
    }

    // check limits are not duplicated
    if (any_duplicated(int_limits, FALSE))
        error("`limits` must be unique.");

    // order by age
    int* ind;
    ind = (int *) R_alloc(n_ages, sizeof(int));

    int* out_ages;
    out_ages = (int *) R_alloc(n_ages, sizeof(int));

    double* out_counts;
    out_counts = (double *) R_alloc(n_ages, sizeof(double));

    // ind = order(x, nalast = TRUE, decreasing = FALSE)
    // R_orderVector1(result, length, input to sort, nalast, decreasing)
    R_orderVector1(ind, n_ages, int_ages, TRUE, FALSE);
    for (int i = 0; i < n_ages; i++) {
        out_ages[i] = p_ages[ind[i]];
        out_counts[i] = p_counts[ind[i]];
    }

    // sort limits
    R_isort(p_limits, n_limits);

    // number of groups (allowing for an NA group)
    int n_groups = n_limits + 2;

    // allocate output and initialise to 0
    SEXP group_counts = PROTECT(allocVector(REALSXP, n_groups));
    double* p_groups = REAL(group_counts);
    Memzero(p_groups, n_groups);

    // calculate the counts
    int group_index = 0;
    int current_age = out_ages[0];
    double tmp = out_counts[0];
    if (flag_na) {
        for (int i = 0; i < n_ages; ++i) {
            if (current_age == NA_INTEGER) {
                p_groups[n_groups-1] += tmp;
            } else {
                while (group_index < n_groups - 2 && current_age >= p_limits[group_index])
                    ++group_index;
                p_groups[group_index] += tmp;
            }
            current_age = out_ages[i+1];
            tmp = out_counts[i+1];
        }
    } else {
        for (int i = 0; i < n_ages; ++i) {
            while (group_index < n_groups - 2 && current_age >= p_limits[group_index])
                ++group_index;
            p_groups[group_index] += tmp;
            current_age = out_ages[i+1];
            tmp = out_counts[i+1];
        }
    }

    // sort limits
    R_isort(p_limits, n_limits);

    // allocate space for the names
    SEXP names = PROTECT(allocVector(STRSXP, n_groups));

    // create first name "[0,%d)"
    int bufsz = snprintf(NULL, 0, "[0,%d)", p_limits[0]);
    char* buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[0,%d)", p_limits[0]);
    SET_STRING_ELT(names, 0, mkChar(buf));
    R_Free(buf);

    // create middle names "[%d,%d)"
    for (int i = 0; i < n_groups-2; ++i) {
        bufsz = snprintf(NULL, 0, "[%d,%d)", p_limits[i], p_limits[i+1]);
        buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d,%d)", p_limits[i], p_limits[i+1]);
        SET_STRING_ELT(names, i+1, mkChar(buf));
        R_Free(buf);
    }

    // create last name "[%d,Inf)"
    bufsz = snprintf(NULL, 0, "[%d,Inf)", p_limits[n_groups-3]);
    buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[%d,Inf)", p_limits[n_groups-3]);
    SET_STRING_ELT(names, n_groups-2, mkChar(buf));
    R_Free(buf);
    SET_STRING_ELT(names, n_groups-1, mkChar("NA"));

    // set names attribute
    setAttrib(group_counts, R_NamesSymbol, names);

    // cleanup
    UNPROTECT(5);

    return group_counts;

}
