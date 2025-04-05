#include <Rcpp.h>
using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_prep_ns)]]
List rcpp_prep_ns(const List ns, const CharacterVector nms, const String pkg) {
  R_xlen_t n = nms.length();
  List ns2(n);
  ns2.attr("names") = nms;
  for(R_xlen_t i = 0; i < n; ++i) {
    String idx = nms[i];
    Function current = ns[idx];
    Function temp = clone(current);
    temp.attr("package") = pkg;
    temp.attr("function_name") = idx;
    temp.attr("tinyimport") = "tinyimport";
    temp.attr("class") = CharacterVector::create("function", "tinyimport");
    ns2[i] = temp;
  }
  return  ns2;
}