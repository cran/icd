// Copyright (C) 2014 - 2016  Jack O. Wasey
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
#include <Rcpp.h>
#include <class.h>
#include <ranges.h>
#include <convert.h>
#include <manip.h>
#include <is.h>

// this is simplest just to hard-code
const Rcpp::CharacterVector vbase = Rcpp::CharacterVector::create("", "0", "1", "2", "3",
                                                                  "4", "5", "6", "7", "8", "9", "00");
const Rcpp::CharacterVector v0 = Rcpp::CharacterVector::create("0", "00", "01", "02", "03",
                                                               "04", "05", "06", "07", "08", "09");
const Rcpp::CharacterVector v1 = Rcpp::CharacterVector::create("1", "10", "11", "12", "13",
                                                               "14", "15", "16", "17", "18", "19");
const Rcpp::CharacterVector v2 = Rcpp::CharacterVector::create("2", "20", "21", "22", "23",
                                                               "24", "25", "26", "27", "28", "29");
const Rcpp::CharacterVector v3 = Rcpp::CharacterVector::create("3", "30", "31", "32", "33",
                                                               "34", "35", "36", "37", "38", "39");
const Rcpp::CharacterVector v4 = Rcpp::CharacterVector::create("4", "40", "41", "42", "43",
                                                               "44", "45", "46", "47", "48", "49");
const Rcpp::CharacterVector v5 = Rcpp::CharacterVector::create("5", "50", "51", "52", "53",
                                                               "54", "55", "56", "57", "58", "59");
const Rcpp::CharacterVector v6 = Rcpp::CharacterVector::create("6", "60", "61", "62", "63",
                                                               "64", "65", "66", "67", "68", "69");
const Rcpp::CharacterVector v7 = Rcpp::CharacterVector::create("7", "70", "71", "72", "73",
                                                               "74", "75", "76", "77", "78", "79");
const Rcpp::CharacterVector v8 = Rcpp::CharacterVector::create("8", "80", "81", "82", "83",
                                                               "84", "85", "86", "87", "88", "89");
const Rcpp::CharacterVector v9 = Rcpp::CharacterVector::create("9", "90", "91", "92", "93",
                                                               "94", "95", "96", "97", "98", "99");

// horrible one-off code to pre-generate all the minor codes
Rcpp::CharacterVector MakeAllMinors() {
  Rcpp::CharacterVector vv = vbase;

  // create numbers 1 to 99 but cycle 10s first
  for (int i = 0; i < 10; ++i) {
    for (int j = 0; j < 10; ++j) {
      std::ostringstream s;
      s << j << i;
      if (i + j != 0)
        vv.push_back(s.str());
    }
  }
  return (vv);
}
// generate the lookups
const Rcpp::CharacterVector vv = MakeAllMinors();

// [[Rcpp::export]]
Rcpp::CharacterVector icd9ExpandMinorShim(std::string minor, bool isE = false) {

  if (!isE) {
    switch (minor.size()) {
    case 0:
      return vv;
    case 1:
      switch (minor.at(0)) {
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
      return Rcpp::CharacterVector::create();
      }
      break;
    case 2:
      return Rcpp::wrap(minor);
    default:
      Rcpp::stop("minor of more than two characters");
    return Rcpp::CharacterVector::create();
    }
  } else {
    // is E code, so minor must be just one character
    switch (minor.size()) {
    case 0:
      return Rcpp::CharacterVector::create("", "0", "1", "2", "3", "4", "5",
                                           "6", "7", "8", "9");
    case 1:
      return minor;
    default:
      Rcpp::stop("too many characters for an E code minor\n");
    }
  }
  return (NA_STRING); // should never get here
}

// [[Rcpp::export]]
Rcpp::CharacterVector icd9ChildrenShortCpp(Rcpp::CharacterVector icd9Short, bool onlyReal) {
  std::set<std::string> out; // we are never going to put NAs in the output?
  if (icd9Short.size() != 0) {
    Rcpp::List parts = icd9ShortToPartsCpp(icd9Short, "");
    Rcpp::CharacterVector major = parts[0];
    Rcpp::CharacterVector minor = parts[1];
    // the next iterator seems to be a Rcpp::CharacterVector of length 1
    Rcpp::CharacterVector::iterator itmajor = major.begin();
    Rcpp::CharacterVector::iterator itminor = minor.begin();
    for (; itmajor != major.end(); ++itmajor, ++itminor) {
      std::string thismajor = Rcpp::as<std::string>(*itmajor);
      std::string thisminor = Rcpp::as<std::string>(*itminor);

      Rcpp::CharacterVector newminors = icd9ExpandMinorShim(thisminor,
                                                            icd9IsASingleE(thismajor.c_str()));

      // push back slower, but difficult to predict size of output
      std::vector<std::string> newshort = Rcpp::as<std::vector<std::string> >(
        icd9MajMinToShort(thismajor, newminors));

      // std insert is a thousand times faster than looping through
      // Rcpp::CharacterVector and push_backing
      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      std::set<std::string> out_real;
      std::vector<std::string> tmp = Rcpp::as<std::vector<std::string> >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      std::set<std::string> reals(tmp.begin(), tmp.end());
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
      out = out_real;
    }
  }
  Rcpp::CharacterVector rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export]]
Rcpp::CharacterVector icd9ChildrenDecimalCpp(Rcpp::CharacterVector icd9Decimal,
                                             bool onlyReal) {
  Rcpp::CharacterVector shrt = icd9DecimalToShort(icd9Decimal);
  Rcpp::CharacterVector kids = icd9ChildrenShortCpp(shrt, onlyReal);
  Rcpp::CharacterVector out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}

// [[Rcpp::export]]
Rcpp::CharacterVector icd9ChildrenCpp(Rcpp::CharacterVector icd9, bool isShort,
                                      bool onlyReal = true) {
  if (isShort)
    return icd9ChildrenShortCpp(icd9, onlyReal);
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
Rcpp::LogicalVector icd_in_reference_code(Rcpp::CharacterVector icd,
                                          Rcpp::CharacterVector icd_reference, bool short_code,
                                          bool short_reference = true) {

  Rcpp::CharacterVector x = icd9AddLeadingZeroes(icd, short_code);
  if (!short_code)
    x = icd9DecimalToShort(x);

  Rcpp::CharacterVector y = icd9ChildrenCpp(icd_reference, short_reference, false);
  if (!short_reference)
    y = icd9DecimalToShort(y);
  Rcpp::LogicalVector res = !is_na(match(x, y));
  return res;
}
