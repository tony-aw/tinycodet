#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_any_badloc ( SEXP x, SEXP y ) {

int n = length(x);
  const int *px = INTEGER(x);
  const int *py = INTEGER(y);
  for(int i = 0; i != n; ++i) { 
    if(py[i] < px[i]) return ScalarLogical(1);
  }
  return ScalarLogical(0);

}