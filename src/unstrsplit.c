#include <Rinternals.h>
#include <stdlib.h>  /* for free() */
#include <string.h> /* for memcpy() */

static char errmsg_buf[200];

/****************************************************************************
 * unstrsplit_list(): taken from S4Vectors, should go into base R
 */

/*
 * Assumes 'x' is a character vector (this is NOT checked).
 * The destination string 'dest' must be large enough to receive the result.
 */
static void join_strings_in_buf(char *dest, SEXP x,
                                const char *sep, int sep_len)
{
	int x_len, i;
	SEXP x_elt;

	x_len = LENGTH(x);
	for (i = 0; i < x_len; i++) {
		if (i != 0) {
			memcpy(dest, sep, sep_len);
			dest += sep_len;
		}
		x_elt = STRING_ELT(x, i);
		memcpy(dest, CHAR(x_elt), LENGTH(x_elt));
		dest += LENGTH(x_elt);
	}
	return;
}

/*
 * Returns a CHARSXP if success, or R_NilValue if failure.
 */
static SEXP join_strings(SEXP x, const char *sep, int sep_len)
{
	SEXP ans;
	int x_len, bufsize, i;
	char *buf;

	if (TYPEOF(x) != STRSXP) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
             "join_strings() expects a character vector");
		return R_NilValue;
	}
	x_len = LENGTH(x);

	/* 1st pass: Loop over 'x' to compute the size of the buffer. */
	bufsize = 0;
	if (x_len != 0) {
		for (i = 0; i < x_len; i++)
			bufsize += LENGTH(STRING_ELT(x, i));
		bufsize += (x_len - 1) * sep_len;
	}

	/* Allocate memory for the buffer. */
	buf = (char *) malloc((size_t) bufsize);
	if (buf == NULL) {
		snprintf(errmsg_buf, sizeof(errmsg_buf), "malloc() failed");
		return R_NilValue;
	}

	/* 2nd pass: Loop over 'x' again to fill 'buf'. */
	join_strings_in_buf(buf, x, sep, sep_len);

	/* Turn 'buf' into a CHARSXP and return it. */
	PROTECT(ans = mkCharLen(buf, bufsize));
	free(buf);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP R_unstrsplit_list(SEXP x, SEXP sep)
{
	SEXP ans, sep0, x_elt, ans_elt, ans_names;
	int x_len, sep0_len, i;

	if (!isVectorList(x))
		error("'x' must be a list");
	if (!(TYPEOF(sep) == STRSXP && LENGTH(sep) == 1))
		error("'sep' must be a single string");
	x_len = LENGTH(x);
	sep0 = STRING_ELT(sep, 0);
	sep0_len = LENGTH(sep0);
	PROTECT(ans = allocVector(STRSXP, x_len));
	for (i = 0; i < x_len; i++) {
		x_elt = VECTOR_ELT(x, i);
		if (x_elt == R_NilValue)
			continue;
		PROTECT(ans_elt = join_strings(x_elt, CHAR(sep0), sep0_len));
		if (ans_elt == R_NilValue) {
			UNPROTECT(2);
			error("in list element %d: %s", i + 1, errmsg_buf);
		}
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	PROTECT(ans_names = duplicate(getAttrib(x, R_NamesSymbol)));
	setAttrib(ans, R_NamesSymbol, ans_names);
	UNPROTECT(2);
	return ans;
}
