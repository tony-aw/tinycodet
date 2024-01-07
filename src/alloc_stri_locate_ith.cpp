
#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_alloc_stri_locate_ith)]]
IntegerMatrix rcpp_alloc_stri_locate_ith(List p1, IntegerVector i) {
    R_xlen_t n = p1.length();
    IntegerMatrix out(n, 2);
    for(R_xlen_t j = 0; j < n; ++j) {
      IntegerMatrix temp = p1[j];
      IntegerMatrix::Row temprow = temp( i[j], _ );
      IntegerMatrix::Row mainrow = out( j , _ );
      mainrow = temprow;
    }
    return  out;
}
