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

#ifndef RANGES_H_
#define RANGES_H_

#include <Rcpp.h>

Rcpp::CharacterVector icd9ChildrenShortCpp(Rcpp::CharacterVector icd9Short, bool onlyReal = true);
Rcpp::CharacterVector icd9ChildrenDecimalCpp(Rcpp::CharacterVector icd9Decimal, bool onlyReal = true);

#endif /* RANGES_H_ */
