# set-up ====

library(stringi)

################################################################################
# equality - dbl ====
#

dimtypes <- c("00", "01", "10")
declareleft <- c("NumericVector", "NumericVector", "double")
declareright <- c("NumericVector", "double", "NumericVector")
declarelength <- c("x.length()", "x.length()", "y.length()")
accessleft <- c("x[i]", "x[i]", "x")
accessright <- c("y[i]", "y", "y[i]")


templatecode  <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_eq_dbl_DIM)]]
LogicalVector rcpp_ntt_eq_dbl_DIM(
    DECLARELEFT x, DECLARERIGHT y, double tol, bool equal
  ) {
    R_xlen_t n = DECLARELENGTH;
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = fabs(ACCESSLEFT - ACCESSRIGHT) < tol;
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = fabs(ACCESSLEFT - ACCESSRIGHT) >= tol;
      }
    }
    
    return out;
  }

"

rcpp_scripts <- character(length(dimtypes))
names(rcpp_scripts) <- dimtypes
for(i in seq_along(dimtypes)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("DIM", "DECLARELEFT", "DECLARERIGHT", "DECLARELENGTH", "ACCESSLEFT", "ACCESSRIGHT"),
    replacement = c(dimtypes[i], declareleft[i], declareright[i], declarelength[i], accessleft[i], accessright[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

fileConn <- file("src/dynamic_rcpp_ntt_eq_dbl.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



################################################################################
# greater - dbl ====
#

dimtypes <- c("00", "01", "10")
declareleft <- c("NumericVector", "NumericVector", "double")
declareright <- c("NumericVector", "double", "NumericVector")
declarelength <- c("x.length()", "x.length()", "y.length()")
accessleft <- c("x[i]", "x[i]", "x")
accessright <- c("y[i]", "y", "y[i]")

templatecode  <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_greater_dbl_DIM)]]
LogicalVector rcpp_ntt_greater_dbl_DIM(
    DECLARELEFT x, DECLARERIGHT y, double tol, bool equal
  ) {
    R_xlen_t n = DECLARELENGTH;
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (fabs(ACCESSLEFT - ACCESSRIGHT) < tol) || ACCESSLEFT >= ACCESSRIGHT;
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (ACCESSLEFT - ACCESSRIGHT) >= tol;
      }
    }
    
    return out;
  }

"

rcpp_scripts <- character(length(dimtypes))
names(rcpp_scripts) <- dimtypes
for(i in seq_along(dimtypes)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("DIM", "DECLARELEFT", "DECLARERIGHT", "DECLARELENGTH", "ACCESSLEFT", "ACCESSRIGHT"),
    replacement = c(dimtypes[i], declareleft[i], declareright[i], declarelength[i], accessleft[i], accessright[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

fileConn <- file("src/dynamic_rcpp_ntt_greater_dbl.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



################################################################################
# smaller - dbl ====
#

dimtypes <- c("00", "01", "10")
declareleft <- c("NumericVector", "NumericVector", "double")
declareright <- c("NumericVector", "double", "NumericVector")
declarelength <- c("x.length()", "x.length()", "y.length()")
accessleft <- c("x[i]", "x[i]", "x")
accessright <- c("y[i]", "y", "y[i]")

templatecode  <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_dbl_DIM)]]
LogicalVector rcpp_ntt_smaller_dbl_DIM(
    DECLARELEFT x, DECLARERIGHT y, double tol, bool equal
  ) {
    R_xlen_t n = DECLARELENGTH;
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (fabs(ACCESSLEFT - ACCESSRIGHT) < tol) || ACCESSLEFT <= ACCESSRIGHT;
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (ACCESSRIGHT - ACCESSLEFT) >= tol;
      }
    }
    
    return out;
  }

"

rcpp_scripts <- character(length(dimtypes))
names(rcpp_scripts) <- dimtypes
for(i in seq_along(dimtypes)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("DIM", "DECLARELEFT", "DECLARERIGHT", "DECLARELENGTH", "ACCESSLEFT", "ACCESSRIGHT"),
    replacement = c(dimtypes[i], declareleft[i], declareright[i], declarelength[i], accessleft[i], accessright[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

fileConn <- file("src/dynamic_rcpp_ntt_smaller_dbl.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)





################################################################################
# equality - int ====
#

dimtypes <- c("00", "01", "10")
declareleft <- c("IntegerVector", "IntegerVector", "int")
declareright <- c("IntegerVector", "int", "IntegerVector")
declarelength <- c("x.length()", "x.length()", "y.length()")
accessleft <- c("x[i]", "x[i]", "x")
accessright <- c("y[i]", "y", "y[i]")

templatecode <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_eq_int_DIM)]]
LogicalVector rcpp_ntt_eq_int_DIM(
    DECLARELEFT x, DECLARERIGHT y, bool equal
  ) {
    R_xlen_t n = DECLARELENGTH;
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (ACCESSLEFT == ACCESSRIGHT);
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (ACCESSLEFT != ACCESSRIGHT);
      }
    }
    
    return out;
  }

"


rcpp_scripts <- character(length(dimtypes))
names(rcpp_scripts) <- dimtypes
for(i in seq_along(dimtypes)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("DIM", "DECLARELEFT", "DECLARERIGHT", "DECLARELENGTH", "ACCESSLEFT", "ACCESSRIGHT"),
    replacement = c(dimtypes[i], declareleft[i], declareright[i], declarelength[i], accessleft[i], accessright[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

fileConn <- file("src/dynamic_rcpp_ntt_eq_int.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)




################################################################################
# greater- int ====
#

dimtypes <- c("00", "01", "10")
declareleft <- c("IntegerVector", "IntegerVector", "int")
declareright <- c("IntegerVector", "int", "IntegerVector")
declarelength <- c("x.length()", "x.length()", "y.length()")
accessleft <- c("x[i]", "x[i]", "x")
accessright <- c("y[i]", "y", "y[i]")

templatecode <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_greater_int_DIM)]]
LogicalVector rcpp_ntt_greater_int_DIM(
    DECLARELEFT x, DECLARERIGHT y, bool equal
  ) {
    R_xlen_t n = DECLARELENGTH;
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = ACCESSLEFT >= ACCESSRIGHT;
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = ACCESSLEFT > ACCESSRIGHT;
      }
    }
    
    return out;
  }

"


rcpp_scripts <- character(length(dimtypes))
names(rcpp_scripts) <- dimtypes
for(i in seq_along(dimtypes)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("DIM", "DECLARELEFT", "DECLARERIGHT", "DECLARELENGTH", "ACCESSLEFT", "ACCESSRIGHT"),
    replacement = c(dimtypes[i], declareleft[i], declareright[i], declarelength[i], accessleft[i], accessright[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

fileConn <- file("src/dynamic_rcpp_ntt_greater_int.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)




################################################################################
# smaller - int ====
#

dimtypes <- c("00", "01", "10")
declareleft <- c("IntegerVector", "IntegerVector", "int")
declareright <- c("IntegerVector", "int", "IntegerVector")
declarelength <- c("x.length()", "x.length()", "y.length()")
accessleft <- c("x[i]", "x[i]", "x")
accessright <- c("y[i]", "y", "y[i]")

templatecode <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_int_DIM)]]
LogicalVector rcpp_ntt_smaller_int_DIM(
    DECLARELEFT x, DECLARERIGHT y, bool equal
  ) {
    R_xlen_t n = DECLARELENGTH;
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = ACCESSLEFT <= ACCESSRIGHT;
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = ACCESSLEFT < ACCESSRIGHT;
      }
    }
    
    return out;
  }

"


rcpp_scripts <- character(length(dimtypes))
names(rcpp_scripts) <- dimtypes
for(i in seq_along(dimtypes)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("DIM", "DECLARELEFT", "DECLARERIGHT", "DECLARELENGTH", "ACCESSLEFT", "ACCESSRIGHT"),
    replacement = c(dimtypes[i], declareleft[i], declareright[i], declarelength[i], accessleft[i], accessright[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

fileConn <- file("src/dynamic_rcpp_ntt_smaller_int.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)


