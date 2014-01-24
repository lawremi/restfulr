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
      SEXP ansj = VECTOR_ELT(ans, col);
      if (ansj == R_NilValue) {
        ansj = allocVector(TYPEOF(xij), length(x));
        if (TYPEOF(ansj) == REALSXP) {
          for (int k = 0; k < length(ansj); k++)
            REAL(ansj)[k] = R_NaReal;
        } else if (TYPEOF(ansj) == STRSXP) {
          for (int k = 0; k < length(ansj); k++)
            SET_STRING_ELT(ansj, k, R_NaString);
        } else if (ansj != R_NilValue) {
          error("Unhandled SEXP type: %s", TYPEOF(ansj));
        }
        SET_VECTOR_ELT(ans, col, ansj);
      }
      if (TYPEOF(ansj) == REALSXP) {
        REAL(ansj)[i] = asReal(xij);
      } else if (TYPEOF(ansj) == STRSXP) {
        SET_STRING_ELT(ansj, i, asChar(xij));
      }
    }
  }

  UNPROTECT(1);
  return ans;
}
