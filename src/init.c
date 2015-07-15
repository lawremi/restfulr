#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP R_raggedListToDF(SEXP x, SEXP uniq_nms, SEXP ind);
SEXP R_unstrsplit_list(SEXP x, SEXP sep);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef CallEntries[] = {
  CALLDEF(R_raggedListToDF, 3),
  CALLDEF(R_unstrsplit_list, 2)
};


void R_init_restfulr(DllInfo *dll)
{
  // Register C routines
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE); 
}
