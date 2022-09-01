#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP c_internal_aggregate_age_counts(SEXP, SEXP, SEXP);
extern SEXP c_internal_aggregate_age_counts_unsafe(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_internal_aggregate_age_counts",        (DL_FUNC) &c_internal_aggregate_age_counts,        3},
    {"c_internal_aggregate_age_counts_unsafe", (DL_FUNC) &c_internal_aggregate_age_counts_unsafe, 3},
    {NULL, NULL, 0}
};

void R_init_ympes(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
