#include <R.h>
#include <Rinternals.h>
#include <ympes.h>

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
