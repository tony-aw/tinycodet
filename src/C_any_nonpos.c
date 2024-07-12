#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_any_nonpos ( SEXP x ) {

R_xlen_t n = xlength(x);
switch(TYPEOF(x)) {
  case INTSXP:
    {
      const int *px = INTEGER(x);
      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 1) return ScalarLogical(1);
      }
      return ScalarLogical(0);
      break;
    }
  
  
  case REALSXP: 
    {
      const double *px = REAL(x);
      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 1) return ScalarLogical(1);
      }
      return ScalarLogical(0);
      break;
    }
  default: error("unsupported type");
}
return(R_NilValue);

}