#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_any_nonpos ( SEXP x ) {
  int n = length(x);
  const int *px = INTEGER(x);
  for(int i = 0; i != n; ++i) { 
    if(px[i] < 1) return ScalarLogical(1);
  }
  return ScalarLogical(0);
}