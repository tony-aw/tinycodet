#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_between_dbl_00)]]
LogicalVector rcpp_ntt_between_dbl_00(
  NumericVector x, NumericVector lowerb, NumericVector upperb, double tol
) {
  R_xlen_t n =x.length();
  LogicalVector out(n);
  for(R_xlen_t i =0; i < n; ++i) {
    if((lowerb[i] - upperb[i]) >= tol) {
      stop("lower bound must not be higher than upper bound");
    }
    out[i] = (x[i] >= lowerb[i] && x[i] <= upperb[i]) || (fabs(x[i] - lowerb[i]) < tol) || (fabs(x[i] - upperb[i]) < tol);
    //           x is between lowerb and upperb       || x is equal to lowerb           || x is equal to upper b
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_between_dbl_01)]]
LogicalVector rcpp_ntt_between_dbl_01(
  NumericVector x, double lowerb, double upperb, double tol
) {
  R_xlen_t n =x.length();
  LogicalVector out(n);
  if((lowerb - upperb) >= tol) {
      stop("lower bound must not be higher than upper bound");
  }
  for(R_xlen_t i =0; i < n; ++i) {
    out[i] = (x[i] >= lowerb && x[i] <= upperb) || (fabs(x[i] - lowerb) < tol) || (fabs(x[i] - upperb) < tol);
    //          x is between lowerb and upperb  || x is equal to lowerb        || x is equal to upper b
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_between_dbl_10)]]
LogicalVector rcpp_ntt_between_dbl_10(
  double x, NumericVector lowerb, NumericVector upperb, double tol
) {
  R_xlen_t n =upperb.length();
  LogicalVector out(n);
  for(R_xlen_t i =0; i < n; ++i) {
    if((lowerb[i] - upperb[i]) >= tol) {
      stop("lower bound must not be higher than upper bound");
    }
    out[i] = (x >= lowerb[i] && x <= upperb[i]) || (fabs(x - lowerb[i]) < tol) || (fabs(x - upperb[i]) < tol);
    //           x is between lowerb and upperb || x is equal to lowerb        || x is equal to upper b
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_between_int_00)]]
LogicalVector rcpp_ntt_between_int_00(
  IntegerVector x, IntegerVector lowerb, IntegerVector upperb
) {
  R_xlen_t n =x.length();
  LogicalVector out(n);
  for(R_xlen_t i =0; i < n; ++i) {
    if(lowerb[i] >= upperb[i]) { 
      stop("lower bound must not be higher than upper bound");
    }
    out[i] = (x[i] >= lowerb[i] && x[i] <= upperb[i]);
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_between_int_01)]]
LogicalVector rcpp_ntt_between_int_01(
  IntegerVector x, int lowerb, int upperb
) {
  R_xlen_t n =x.length();
  LogicalVector out(n);
  if(lowerb >= upperb) {
      stop("lower bound must not be higher than upper bound");
  }
  for(R_xlen_t i =0; i < n; ++i) {
    out[i] = (x[i] >= lowerb && x[i] <= upperb);
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_between_int_10)]]
LogicalVector rcpp_ntt_between_int_10(
  int x, IntegerVector lowerb, IntegerVector upperb
) {
  R_xlen_t n =lowerb.length();
  LogicalVector out(n);
  for(R_xlen_t i =0; i < n; ++i) {
    if(lowerb[i] >= upperb[i]) { 
      stop("lower bound must not be higher than upper bound");
    }
    out[i] = (x >= lowerb[i] && x <= upperb[i]);
  }
  return out;
}

