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

#include <Rcpp.h>
#include <Rcpp/r/headers.h>              // for Rf_install
#include <iterator>                      // for advance
#include <vector>                        // for vector
#include "Rcpp.h"                        // for wrap
#include "Rcpp/Environment.h"            // for Environment
#include "Rcpp/api/meat/Environment.h"   // for Environment_Impl::Environmen...
#include "Rcpp/api/meat/proxy.h"         // for BindingPolicy::Binding::oper...
#include "Rcpp/exceptions.h"             // for warning
#include "Rcpp/proxy/Binding.h"          // for BindingPolicy<>::Binding
#include "Rcpp/sugar/functions/is_na.h"  // for is_na, IsNa
#include "Rcpp/sugar/functions/match.h"  // for match
#include "Rcpp/sugar/operators/not.h"    // for Not_Vector, operator!
#include "Rcpp/traits/storage_type.h"    // for storage_type<>::type
#include "Rcpp/vector/Subsetter.h"       // for SubsetProxy
#include "Rcpp/vector/Vector.h"          // for Vector<>::iterator, Vector<>...
#include "Rcpp/vector/VectorBase.h"      // for VectorBase
#include "Rcpp/vector/instantiation.h"   // for IntegerVector, List
#include "Rcpp/vector/proxy.h"           // for r_vector_name_proxy<>::type
#include "icd_types.h"                   // for CV, VecStr

//
//  # matches, nc, matches, last_row
//  for (i in seq_along(icd10Short)) {
//    # now the children, assuming the source file is sorted logically, will be
//    # subsequent codes, until a code of the same length is found
//    check_row <- matches[i] + 1
//    parent_len <- nc[matches[i]]
//    while (nc[check_row] > parent_len && check_row != last_row + 1)
//      check_row <- check_row + 1
//
//    kids <- c(kids, icd10cm2016[matches[i]:(check_row - 1), "code"])
//  }
//
//  as.icd10cm(kids, short_code)

// [[Rcpp::export(icd10cm_children_defined_cpp)]]
CV icd10cmChildrenDefined(CV &x) {

  // need namespace for sysdata (.nc) and package env for lazy data
  Rcpp::Environment env("package:icd"); // only works when package is loaded
  Rcpp::Environment ns = Rcpp::Environment::namespace_env("icd");
  Rcpp::List icd10cm2016 = env["icd10cm2016"];
  CV allCodes = icd10cm2016["code"];
  Rcpp::IntegerVector nc = ns[".nc"];
  Rcpp::IntegerVector matchesNa = Rcpp::match(x, allCodes);
  // now drop NA rows, Rcpp match doesn't do this, yet.
  // match(x, table, nomatch = 0L) > 0L
  Rcpp::IntegerVector matches = matchesNa[!is_na(matchesNa)]; // 1-based index (R style)

  VecStr kids;

  if (matches.length() == 0) {
    if (x.length() > 0) {
      Rcpp::warning("None of the provided ICD-10 codes matched the master ICD-10-CM list (currently 2016)");
    }
    return(CV(0));
  }

  kids.reserve(x.length() * 10);

  CV tmp = icd10cm2016[0];
  int last_row = tmp.length(); // zero-based index
  int check_row; // zero-based index
  int parent_len; // number of characters in original parent code

  for (int i = 0; i != matches.length(); ++i) {
    check_row = matches[i] + 1 - 1; // check the row after the parent (may be off end of vector)
    parent_len = nc[matches[i] - 1];
    while (check_row < last_row && nc[check_row] > parent_len)
      ++check_row;

    CV::iterator it = allCodes.begin();
    std::advance(it, matches[i] - 1);
    CV::iterator it2 = allCodes.begin();
    std::advance(it2, check_row);
    kids.insert(kids.end(), it, it2);
  }
  return Rcpp::wrap(kids);
}
