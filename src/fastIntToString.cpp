#include "icd_types.h"
#include <stdio.h> // for sprintf, size_t
#include <string>
#include <vector>

using namespace Rcpp;

//' @title Convert integers to strings as quickly as possible
//' @description Have tried R, \code{sprintf} with \CRANpkg{Rcpp} and C++
//'   standard library. Doesn't do bounds checking, but limited by length of
//'   integers.
//' @param x Vector of integers
//' @return Vector of characters
//' @keywords internal manip
//' @noRd
// [[Rcpp::export]]
CharacterVector fastIntToStringRcpp(IntegerVector x) {
  size_t len = x.size();
  CharacterVector out(len);
  char buffer[32];
  for (size_t i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}
