
#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_convert_i0)]]
IntegerVector rcpp_convert_i0(IntegerVector n_matches, IntegerVector i) {
  int n = i.length();
  IntegerVector out(n);
  int res = 0;
  
  for(int j = 0; j < n; ++j) {
    if(IntegerVector::is_na(i[j])) {
      stop("`i` is not allowed to be zero or NA");
    }
    else if(i[j] < 0) {
      res = n_matches[j] - abs(i[j] + 1);
      if(res < 1) {
        out[j] = 1;
      } else {
        out[j] = res;
      }
    }
    else if(i[j] > 0) {
      if(i[j] < n_matches[j]) {
        out[j] = i[j];
      } else {
        out[j] = n_matches[j];
      }
    }
    else {
      stop("`i` is not allowed to be zero or NA");
    }
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_convert_i1)]]
IntegerVector rcpp_convert_i1(IntegerVector n_matches, int i) {
  int n = n_matches.length();
  IntegerVector out(n);
  
  if(i < 0) {
    int res = 0;
    for(int j = 0; j < n; ++j) {
      res = n_matches[j] - abs(i + 1);
      if(res < 1) {
        out[j] = 1;
      } else {
        out[j] = res;
      }
    }
  }
  else if(i > 0) {
    for(int j = 0; j < n; ++j) {
      if(i < n_matches[j]) {
        out[j] = i;
      } else {
        out[j] = n_matches[j];
      }
    }
  }
  else {
    stop("`i` is not allowed to be zero or NA");
  }
  
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_alloc_stri_locate_ith)]]
IntegerMatrix rcpp_alloc_stri_locate_ith(List p1, IntegerVector i) {
    int n = p1.length(); // using regular integer, because maximum nrow/ncol for matrices is approx 2^30 -1 anyway
    IntegerMatrix out(n, 2);
    for(int j = 0; j < n; ++j) {
      IntegerMatrix temp = p1[j];
      IntegerMatrix::Row temprow = temp( i[j], _ );
      IntegerMatrix::Row mainrow = out( j , _ );
      mainrow = temprow;
    }
    return  out;
}