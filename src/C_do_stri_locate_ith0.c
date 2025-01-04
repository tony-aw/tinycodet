#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_do_stri_locate_ith0 ( SEXP p1, SEXP i, SEXP dims ) {


int n = Rf_length(p1); // using regular integer, because maximum nrow/ncol for matrix with 2 columns is approx 2^30 -1 anyway
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n * 2));
  int *pout = INTEGER(out);
  
  const int *pi = INTEGER_RO(i);
  int n_matches;
  int i2;
  SEXP temp;
  
  for(int j = 0; j < n; ++j) {
    temp = VECTOR_ELT(p1, j);
    const int *ptemp = INTEGER_RO(temp);
    
    n_matches = Rf_length(temp) / 2;
    i2 = 0;
    
    if(pi[j] == NA_INTEGER) {
      error("`i` is not allowed to be zero or NA");
    }
    else if(pi[j] < 0) {
      i2 = n_matches - abs(pi[j] + 1);
      if(i2 < 1) {
        i2 = 1;
      }
    }
    else if(pi[j] > 0) {
      if(pi[j] < n_matches) {
        i2 = pi[j];
      } else {
        i2 = n_matches;
      }
    }
    else {
      error("`i` is not allowed to be zero or NA");
    }
    
    pout[j] = ptemp[i2 - 1];
    pout[j + n] = ptemp[i2 - 1 + n_matches];
  }
  
  Rf_setAttrib(out, R_DimSymbol, dims);
  
  UNPROTECT(1);
  return  out;


  warning("your C program does not return anything!");
  return R_NilValue;
}
