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

// [[Rcpp::interfaces(r, cpp)]]
#include "ranges.h"
#include <Rcpp/r/headers.h>                 // for Rf_install, NA_STRING
#include <algorithm>                        // for set_intersection
#include <iterator>                         // for insert_iterator, inserter
#include <set>                              // for _Rb_tree_const_iterator, set
#include <string>                           // for basic_string
#include <vector>                           // for vector, vector<>::iterator
#include "Rcpp.h"                           // for wrap
#include "Rcpp/Environment.h"               // for Environment
#include "Rcpp/api/meat/Environment.h"      // for Environment_Impl::Environ...
#include "Rcpp/api/meat/proxy.h"            // for AttributeProxyPolicy::Att...
#include "Rcpp/as.h"                        // for as
#include "Rcpp/exceptions.h"                // for stop
#include "Rcpp/generated/Vector__create.h"  // for Vector::create
#include "Rcpp/proxy/AttributeProxy.h"      // for AttributeProxyPolicy<>::A...
#include "Rcpp/proxy/Binding.h"             // for BindingPolicy<>::const_Bi...
#include "Rcpp/sugar/functions/is_na.h"     // for is_na, IsNa
#include "Rcpp/sugar/functions/match.h"     // for match
#include "Rcpp/sugar/operators/not.h"       // for Not_Vector, operator!
#include "Rcpp/vector/Vector.h"             // for Vector<>::iterator, Vecto...
#include "Rcpp/vector/VectorBase.h"         // for VectorBase
#include "Rcpp/vector/instantiation.h"      // for List, LogicalVector, Inte...
#include "Rcpp/vector/proxy.h"              // for r_vector_proxy<>::type
#include "Rcpp/vector/string_proxy.h"       // for string_proxy
#include "RcppCommon.h"                     // for Proxy_Iterator
#include "appendMinor.h"                    // for icd9MajMinToShort, icd9Ma...
#include "convert.h"                        // for icd9DecimalToShort, icd9S...
#include "convert_alt.h"                    // for icd9ShortToPartsCppStd
#include "icd_types.h"                      // for CV, VecStr, Str
#include "is.h"                             // for icd9IsASingleE
#include "local.h"                          // for icd_set
#include "range-const.h"                    // for v_empty_std, v0, v0_std, v1

// someday, can just directly memcopy the codes from the full set
// const char  icd9ExpandMinorC(const char * mnr, bool isE) {
//   if (!isE) {
//     switch (strlen(mnr)) {
//     case 0:
//       return vv_char;
//     case 1:
// // vv_char, skip indices 0-11
// return vv_char[12 +]
// }

// [[Rcpp::export]]
VecStr icd9ExpandMinorStd(const Str& mnr, bool isE) {
  if (!isE) {
    switch (mnr.size()) {
    case 0:
      return vv_std;
    case 1:
      switch (mnr[0]) { // use .at() for range check
      case '0':
        return v0_std;
      case '1':
        return v1_std;
      case '2':
        return v2_std;
      case '3':
        return v3_std;
      case '4':
        return v4_std;
      case '5':
        return v5_std;
      case '6':
        return v6_std;
      case '7':
        return v7_std;
      case '8':
        return v8_std;
      case '9':
        return v9_std;
      default:
        Rcpp::stop("unrecognized minor character");
      return v_empty_std;
      }
    case 2:
      return VecStr(1, mnr);
    default:
      Rcpp::stop("invalid minor in icd9ExpandMinorStd");
    return v_empty_std;
    }
  } else {
    // is E code, so minor must be just one character
    switch (mnr.size()) {
    case 0:
      return vbase_e_std;
    case 1:
      return VecStr(1, mnr);
    default:
      Rcpp::stop("invalid E code minor in icd9ExpandMinorStd");
    }
  }
}

// [[Rcpp::export(icd9_expand_minor_wrap)]]
CV icd9ExpandMinor(const Str& mnr, bool isE) {

  if (!isE) {
    switch (mnr.size()) {
    case 0:
      return vv;
    case 1:
      switch (mnr.at(0)) {
      case '0':
        return v0;
      case '1':
        return v1;
      case '2':
        return v2;
      case '3':
        return v3;
      case '4':
        return v4;
      case '5':
        return v5;
      case '6':
        return v6;
      case '7':
        return v7;
      case '8':
        return v8;
      case '9':
        return v9;
      default:
        Rcpp::stop("unrecognized minor character");
      return CV::create();
      }
      break;
    case 2:
      return Rcpp::wrap(mnr);
    default:
      Rcpp::stop("minor of more than two characters");
    return CV::create();
    }
  } else {
    // is E code, so minor must be just one character
    switch (mnr.size()) {
    case 0:
      return CV::create("", "0", "1", "2", "3", "4", "5",
                        "6", "7", "8", "9");
    case 1:
      return mnr;
    default:
      Rcpp::stop("too many characters for an E code minor\n");
    }
  }
  return (NA_STRING); // should never get here
}

// [[Rcpp::export]]
CV icd9ChildrenShort(CV icd9Short, bool onlyReal) {
  std::set<Str> out; // we are never going to put NAs in the output, so use std structure
  // this is a slower function, can the output set be predefined in size?
  if (icd9Short.size() != 0) {
    // TODO by reference or updating arguments instead? Unclear benefit, but
    // this does/did take a lot of cycles in valgrind
    Rcpp::List parts = icd9ShortToPartsCpp(icd9Short, "");
    CV mjr = parts[0];
    CV mnr = parts[1];

    CV::iterator itmjr = mjr.begin();
    CV::iterator itmnr = mnr.begin();
    for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
      Str thismjr = Rcpp::as<Str>(*itmjr);
      Str thismnr = Rcpp::as<Str>(*itmnr);

      const CV newminors = icd9ExpandMinor(thismnr, icd9IsASingleE(thismjr.c_str()));

      VecStr newshort = Rcpp::as<VecStr >(
        icd9MajMinToShort(thismjr, newminors));

      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      const Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      std::set<Str> out_real;
      const VecStr tmp = Rcpp::as<VecStr >(icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      const std::set<Str> reals(tmp.begin(), tmp.end());

      // set_intersection doesn't work for unordered sets
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
      out = out_real;
    }
  } // input length != 0
  CV rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// TODO: icd9ChildrenShortUnordered no NA version

// [[Rcpp::export]]
CV icd9ChildrenShortUnordered(CV icd9Short, bool onlyReal) {
  icd_set out; // we are never going to put NAs in the output, so use std structure
  // this is a slower function, can the output set be predefined in size?
  if (icd9Short.size() != 0) {
    Rcpp::List parts = icd9ShortToPartsCpp(icd9Short, "");
    CV mjr = parts[0];
    CV mnr = parts[1];

    CV::iterator itmjr = mjr.begin();
    CV::iterator itmnr = mnr.begin();
    for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
      Str thismjr = Rcpp::as<Str>(*itmjr);
      Str thismnr = Rcpp::as<Str>(*itmnr);

      const CV newminors = icd9ExpandMinor(thismnr, icd9IsASingleE(thismjr.c_str()));

      VecStr newshort = Rcpp::as<VecStr>(icd9MajMinToShort(thismjr, newminors));
      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      VecStr tmp = Rcpp::as<VecStr >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      icd_set reals(tmp.begin(), tmp.end());

#ifdef HAVE_CXX11
      for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
        if (reals.find(*j) != reals.end())
          out_real.insert(*j);
      }
#else
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
#endif
      out = out_real;
    }
  } // input length != 0
  CV rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export]]
VecStr icd9ChildrenShortNoNaUnordered(const VecStr& icd9Short, const bool onlyReal) {
  icd_set out; // we are never going to put NAs in the output, so use std structure
  // this is a slower function, can the output set be predefined in size?
  VecStr mjr(icd9Short.size());
  VecStr mnr(icd9Short.size());
  if (icd9Short.size() != 0) {
    icd9ShortToPartsCppStd(icd9Short, "", mjr, mnr);

    VecStr::iterator itmjr = mjr.begin();
    VecStr::iterator itmnr = mnr.begin();
    for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
      const VecStr& newminors = icd9ExpandMinorStd(*itmnr, icd9IsASingleE((*itmjr).c_str()));
      VecStr newshort = icd9MajMinToShortSingleStd(*itmjr, newminors);
      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      VecStr tmp = Rcpp::as<VecStr >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      icd_set reals(tmp.begin(), tmp.end());

#ifdef HAVE_CXX11
      for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
        if (reals.find(*j) != reals.end())
          out_real.insert(*j);
      }
#else
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
#endif
      out = out_real;
    }
  } // input length != 0
  // TODO in R wrapper: rcppOut.attr("icd_short_diag") = true;

  // sort from set or unordered set into a vector:

  VecStr out_vec(out.begin(), out.end());
#ifdef HAVE_CXX11
  // TODO
#endif

  return out_vec;
}

// [[Rcpp::export]]
CV icd9ChildrenDecimalCpp(CV icd9Decimal,
                          bool onlyReal) {
  CV shrt = icd9DecimalToShort(icd9Decimal);
  CV kids = icd9ChildrenShort(shrt, onlyReal);
  CV out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}

// [[Rcpp::export]]
CV icd9ChildrenCpp(CV icd9, bool isShort,
                   bool onlyReal = true) {
  if (isShort)
    return icd9ChildrenShort(icd9, onlyReal);
  return icd9ChildrenDecimalCpp(icd9, onlyReal);
}

//' @title match ICD9 codes
//' @description Finds children of \code{icd9Reference} and looks for \code{icd9} in the
//'   resulting vector.
//' @templateVar icd9AnyName "icd9,icd9Reference"
//' @template icd9-any
//' @template short_code
//' @param isShortReference logical, see argument \code{short_code}
//' @return logical vector
//' @keywords internal
// [[Rcpp::export]]
Rcpp::LogicalVector icd_in_reference_code(CV icd,
                                          CV icd_reference,
                                          bool short_code,
                                          bool short_reference = true) {
  if (!short_code)
    icd = icd9DecimalToShort(icd);

  CV y = icd9ChildrenCpp(icd_reference, short_reference, false);
  if (!short_reference)
    y = icd9DecimalToShort(y);
  // TODO: use hash/environment, although linear search in short maps may be ok
  Rcpp::LogicalVector res = !is_na(match(icd, y));
  return res;
}
