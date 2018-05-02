// Copyright (C) 2014 - 2018  Jack O. Wasey
//
// This file is part of icd.
//
// icd is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd. If not, see <http://www.gnu.org/licenses/>.

#ifndef UTIL_H_
#define UTIL_H_

#include "config.h"
#include "local.h"                      // for ICD_OPENMP
#include "icd_types.h"                  // for VecStr
#include <cstddef>                      // for size_t
#include <string>                       // for string
#include <utility>                      // for pair
#include <vector>                       // for vector
#include <Rcpp.h> // IWYU pragma: keep

#ifdef ICD_OPENMP
#include <omp.h>
#endif

typedef std::pair<std::string, std::size_t> pas;

std::string trimLeftCpp(std::string s);
std::string strimCpp(std::string s);

int getOmpCores();
int getOmpThreads();
int getOmpMaxThreads();
void debug_parallel();
void debug_parallel_env();
Rcpp::NumericVector randomMajorCpp(int n);
VecStr icd9RandomShortN(VecStr::size_type n);
VecStr icd9RandomShortV(VecStr::size_type n);
VecStr icd9RandomShortE(VecStr::size_type n);
VecStr icd9RandomShort(VecStr::size_type n);

int valgrindCallgrindStart(bool zerostats);
int valgrindCallgrindStop();

bool icd9CompareStrings(std::string a, std::string b);
std::vector<std::size_t> icd9OrderCpp(VecStr x);

// concatenate a vector of vectors
template <class COCiter, class Oiter>
void my_concat (COCiter start, COCiter end, Oiter dest) {
  while (start != end) {
    dest = std::copy(start->begin(), start->end(), dest);
    ++start;
  }
}

// template for factors of different S types
template <int RTYPE>
Rcpp::IntegerVector fast_factor_template( const Rcpp::Vector<RTYPE>& x ) {
  Rcpp::Vector<RTYPE> levs = unique(x); // or sort_unique
  Rcpp::IntegerVector out = match(x, levs);
  out.attr("levels") = Rcpp::as<Rcpp::CharacterVector>(levs);
  out.attr("class") = "factor";
  return out;
}

#endif /* UTIL_H_ */
