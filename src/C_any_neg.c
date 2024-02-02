#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

// Inspired by collapse::anyv, though this function works differently

SEXP C_any_neg ( SEXP x ) {
  int n = length(x);
  const int *px = INTEGER(x);
  for(int i = 0; i != n; ++i) { 
    if(px[i] < 0) return ScalarLogical(1);
  }
  return ScalarLogical(0);
}