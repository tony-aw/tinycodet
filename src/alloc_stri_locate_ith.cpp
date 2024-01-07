
#include <Rcpp.h>
using namespace Rcpp;

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
