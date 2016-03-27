#include <Rinternals.h>

SEXP R_raggedListToDF(SEXP x, SEXP uniq_nms, SEXP ind) {
  int *p_ind = INTEGER(ind);

  SEXP ans = allocVector(VECSXP, length(uniq_nms));
  PROTECT(ans);
  setAttrib(ans, R_NamesSymbol, duplicate(uniq_nms));
  
  for (int i = 0; i < length(x); i++) {
    SEXP xi = VECTOR_ELT(x, i);
    for (int j = 0; j < length(xi); j++, p_ind++) {
      int col = *p_ind - 1;
      SEXP xij = VECTOR_ELT(xi, j);
      if (xij == R_NilValue) {
          continue;
      }
      /* FIXME: should determine whether any length(xij) > 1 and use list */
      SEXP ansj = VECTOR_ELT(ans, col);
      if (ansj == R_NilValue) {
        ansj = allocVector(TYPEOF(xij), length(x));
        setAttrib(ansj, R_ClassSymbol, getAttrib(xij, R_ClassSymbol));
        if (TYPEOF(ansj) == REALSXP) {
          for (int k = 0; k < length(ansj); k++)
            REAL(ansj)[k] = NA_REAL;
        } else if (TYPEOF(ansj) == INTSXP) {
          for (int k = 0; k < length(ansj); k++)
            INTEGER(ansj)[k] = NA_INTEGER;
        } else if (TYPEOF(ansj) == STRSXP) {
          for (int k = 0; k < length(ansj); k++)
            SET_STRING_ELT(ansj, k, R_NaString);
        } else if (TYPEOF(ansj) == LGLSXP) {
          for (int k = 0; k < length(ansj); k++)
            LOGICAL(ansj)[k] = NA_LOGICAL;
        } else if (TYPEOF(ansj) == VECSXP) {
          for (int k = 0; k < length(ansj); k++)
            SET_VECTOR_ELT(ansj, k, R_NilValue);
        } else {
          error("Unhandled SEXP type: %s", type2char(TYPEOF(ansj)));
        }
        SET_VECTOR_ELT(ans, col, ansj);
      }
      if (TYPEOF(ansj) == REALSXP) {
        REAL(ansj)[i] = asReal(xij);
      } else if (TYPEOF(ansj) == INTSXP) {
        INTEGER(ansj)[i] = asInteger(xij);
      } else if (TYPEOF(ansj) == STRSXP) {
        SET_STRING_ELT(ansj, i, asChar(xij));
      } else if (TYPEOF(ansj) == LGLSXP) {
        LOGICAL(ansj)[i] = asLogical(xij);
      } else if (TYPEOF(ansj) == VECSXP) {
        SET_VECTOR_ELT(ansj, i, xij);
      }
    }
  }

  UNPROTECT(1);
  return ans;
}
