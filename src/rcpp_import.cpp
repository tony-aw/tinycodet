#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_address)]]
 String rcpp_address(SEXP x) {
   // based on 'R'-source `do_tracemem()`, but doing the 'C++' equivalent, and only getting the address (no explicit tracing involved):
   char buffer[20];
   std::snprintf(buffer, 20, "<%p>", (void *) x);
   std::string buffer2 = buffer;
   return buffer2;
   
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_get_function_name)]]
 SEXP get_function_name(const SEXP fun, const Environment env, const CharacterVector nms) {
   R_xlen_t n = nms.length();
   for(R_xlen_t i = 0; i < n; ++i) {
     String idx = nms[i];
     RObject temp = env[idx];
     if(rcpp_address(fun) == rcpp_address(temp)) {
       return(Rcpp::wrap(idx));
     }
   }
   return R_NilValue;
 }
