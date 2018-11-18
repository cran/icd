#include <Rcpp.h>
#include "local.h"
//#include "util.h"

using namespace Rcpp;

//' @describeIn factor_nosort \pkg{Rcpp} implementation, requiring character
//' vector inputs only, no argument checking.
//' @keywords internal manip
// [[Rcpp::export(factor_nosort_rcpp_worker)]]
IntegerVector factorNoSort(const CharacterVector& x,
                           const CharacterVector& levels,
                           const bool na_rm) {
  // TODO: use new factor code? but this is fine.
  IntegerVector out = match(x, levels);
  out.attr("levels") = (CharacterVector) levels;
  out.attr("class") = "factor";
  if (!na_rm)
    return out;
  return(out[!is_na(out)]);
}

// TODO someday: can the following be done using the R_StringHash global cache
// instead of making a new hash table to do the integer matching?

//' @title Re-generate a factor with new levels, without doing string matching
//' @description This is called by an R wrapper. There is an `na.rm` version,
//' too.
//' @md
//' @keywords internal manip
// [[Rcpp::export(refactor_worker)]]
Rcpp::IntegerVector refactor(const IntegerVector& x, const CV& new_levels, bool exclude_na) {
  TRACE_UTIL("Refactoring");
  IntegerVector f(x.size()); // too many if we are dropping NA values.
  CharacterVector lx = x.attr("levels");
  DEBUG_UTIL_VEC(x);
  DEBUG_UTIL_VEC(lx);
  DEBUG_UTIL_VEC(new_levels);
  if (lx.isNULL()) Rcpp::stop("icd codes must be in a factor");
  CV no_na_lx;
  CV no_na_new_levels;
  //bool any_na_lx = false;
  LogicalVector which_na_new_levels = is_na(new_levels);
  // TODO: get the position of the NA in new_levels for insertion later if needed.
  LogicalVector which_na_old_levels = is_na(lx);
  if (exclude_na) {
    DEBUG_UTIL("Dropping NA in input factor levels");
    no_na_lx = lx[!which_na_old_levels];
  } else {
    DEBUG_UTIL("Not looking for NA in the input data levels");
    no_na_lx = lx;
  }
  if (exclude_na) {
    DEBUG_UTIL("Dropping NA in input levels");
    no_na_new_levels = new_levels[!which_na_new_levels];
  } else {
    DEBUG_UTIL("Not dropping NA in input levels");
    no_na_new_levels = new_levels;
  }
  DEBUG_UTIL_VEC(no_na_lx);
  DEBUG_UTIL_VEC(no_na_new_levels);
  if (no_na_new_levels.size() == 0) {
    DEBUG("no_na_new_levels is empty");
    f = Rcpp::rep(NA_INTEGER, x.size());
    f.attr("levels") = CV::create();
    f.attr("class") = "factor";
    return f;
  }
  IntegerVector new_level_old_idx = Rcpp::match(no_na_lx, no_na_new_levels);
  DEBUG_UTIL_VEC(new_level_old_idx);
  R_xlen_t fsz = x.size();
  DEBUG_UTIL("fsz = " << fsz);
  LogicalVector matched_na_level(fsz, false);
  R_xlen_t fi = 0;
  R_xlen_t i;
  for (i = 0; i < fsz; ++i) {
    TRACE_UTIL("refactor considering i: " << i << ", x[i]: " << x[i] << ", "
                                          << "fi: " << fi);
    if (IntegerVector::is_na(x[i])) {
      TRACE_UTIL("fi++, leaving NA from pos " << i << " at fi " << fi <<
        " due to input NA value");
      f[fi++] = NA_INTEGER;
      continue;
    }
    assert(x[i] > 0);
    assert(x[i] <= which_na_old_levels.size()); // R index (to be used in C)
    if (which_na_old_levels[x[i] - 1]) {
      DEBUG_UTIL("inserting NA because vec previously referenced NA level. pos " << i << " with fi=" << fi);
      if (!exclude_na)
        Rcpp::stop("TODO: lookup index of NA in new levels, if it exists");
      else
        DEBUG("fi++ no NA levels in target factor, so inserting NA value in vector.");
      f[fi++] = NA_INTEGER;
    }
    assert(x[i] > 0);
    DEBUG("x[i]  = " << x[i] << ", " << new_level_old_idx.size());
    assert(x[i] <= new_level_old_idx.size()); // R index
    auto cur = new_level_old_idx[x[i] - 1]; // get new R index with R index in C vec
    if (IntegerVector::is_na(cur)) {
      TRACE_UTIL("fi++, leaving NA from " << i << " at pos " << fi << " due to no match with new levels");
      f[fi++] = NA_INTEGER; // in case user chooses to keep NA values
      matched_na_level[i] = true;
    } else {
      TRACE_UTIL("fi++, inserting " << cur << " from pos " << i << " at "<< fi);
      assert(cur > 0);
      assert(cur <= no_na_new_levels.size());
      f[fi++] = cur;
    }
  }
  DEBUG_UTIL_VEC(f);
  DEBUG_UTIL_VEC(matched_na_level);
  // if not removing NAs, and na levels are okay, then we need to also match
  // the NAs in the integer vector to the NA level. Base factor will keep NA
  // values when there was no match, but index the NA level if it exists.
  if (!exclude_na) {
    DEBUG_UTIL("fixing NA values when there are NA levels");
    DEBUG_UTIL_VEC(which_na_new_levels);
    R_xlen_t n = 0;
    while (n < which_na_new_levels.size()) {
      if (which_na_new_levels[n]) break;
      ++n;
    }
    DEBUG_UTIL("n = " << n);
    if (n != which_na_new_levels.size()) {
      DEBUG_UTIL("NA level found");
      f[is_na(x)] = (int)n + 1; // R index from C match. ?matched_na_level
    }
    else
      DEBUG_UTIL("No NA level found");
  }
  DEBUG_UTIL_VEC(x);
  DEBUG_UTIL_VEC(f);
  f.attr("levels") = no_na_new_levels;
  f.attr("class") = "factor";
  DEBUG_UTIL("max(f) " << max(f));
  DEBUG_UTIL("f.size() " << f.size());
  return(f);
}

//' @describeIn refactor_worker Drop all `NA` values from levels and values
//' @keywords internal
// [[Rcpp::export(refactor_narm_worker)]]
Rcpp::IntegerVector refactor_narm(const IntegerVector& x,
                                  const CV& new_levels) {
  TRACE_UTIL("Refactoring, dropping NA");
  IntegerVector f(x.size()); // too many if we are dropping NA values.
  f.attr("class") = "factor";
  CharacterVector lx = x.attr("levels");
  DEBUG_UTIL_VEC(x);
  DEBUG_UTIL("x size: " << x.size());
  DEBUG_UTIL_VEC(lx);
  DEBUG_UTIL("lx size: " << lx.size());
  DEBUG_UTIL_VEC(new_levels);
  DEBUG_UTIL("new_levels size: " << new_levels.size());
  if (!Rf_isFactor(x)) Rcpp::stop("input must be a factor");
  CV no_na_lx;
  CV no_na_new_levels;
  //bool any_na_lx = false;
  LogicalVector which_na_new_levels = is_na(new_levels);
  // TODO: get the position of the NA in new_levels for insertion later if needed.
  LogicalVector which_na_old_levels = is_na(lx);
  DEBUG_UTIL("Dropping NA in input factor levels");
  DEBUG_UTIL_VEC(which_na_old_levels);
  DEBUG_UTIL("Any old NA levels? " << Rcpp::is_true(any(which_na_old_levels)));
  no_na_lx = lx[!which_na_old_levels];
  DEBUG_UTIL("Dropping NA in input levels");
  no_na_new_levels = new_levels[!which_na_new_levels];
  DEBUG_UTIL("Any new NA levels? " << Rcpp::is_true(any(which_na_new_levels)));
  DEBUG_UTIL_VEC(which_na_new_levels);
  DEBUG_UTIL_VEC(no_na_lx);
  DEBUG_UTIL_VEC(no_na_new_levels);
  if (no_na_new_levels.size() == 0) {
    DEBUG_UTIL("no_na_new_levels is empty, so whole result must be empty");
    f = IntegerVector::create();
    f.attr("levels") = CV::create();
    f.attr("class") = "factor";
    return(f);
  }
  if (x.size() == 0) {
    DEBUG_UTIL("x size is 0, so returning empty vector with no-NA levels.");
    f.attr("levels") = no_na_new_levels;
    return(f);
  }
  IntegerVector new_level_old_idx = Rcpp::match(no_na_lx, no_na_new_levels);
  DEBUG_UTIL_VEC(new_level_old_idx);
  R_xlen_t fsz = x.size();
  DEBUG_UTIL("fsz = " << fsz);
  R_xlen_t fi = 0;
  R_xlen_t i;
  for (i = 0; i < fsz; ++i) {
    TRACE_UTIL("refactor considering i: " << i << ", x[i]: " << x[i] << ", "
                                          << "fi: " << fi);
    if (IntegerVector::is_na(x[i])) {
      TRACE_UTIL("dropping NA fom pos " << i << " (fi = " << fi <<
        ") due to input NA value");
      continue;
    }
    assert(x[i] > 0);
    assert(x[i] <= which_na_old_levels.size()); // R index (to be used in C)
    if (which_na_old_levels[x[i] - 1]) { // was an NA old level referenced?
      DEBUG_UTIL("input data referenced an NA level"
                   << "continuing without inserting from pos "
                   << i << " with fi=" << fi);
      continue;
    }
    assert(x[i] > 0);
    assert(x[i] <= new_level_old_idx.size()); // R index
    // get new R index from C vec or R indices. Must keep as IntegerVector
    // length one, so NA is preserved.
    if (IntegerVector::is_na(new_level_old_idx[x[i] - 1])) {
      TRACE_UTIL("dropping NA from " << i << " at pos " << fi << " due to no match with new levels");
    } else {
      int cur_i = new_level_old_idx[x[i] - 1];
      TRACE_UTIL("fi++, inserting " << cur_i << " from pos i = " << i << " at fi = "<< fi);
      assert(cur_i > 0);
      assert(cur_i <= no_na_new_levels.size());
      f[fi++] = cur_i;
    }
  } // for i
  DEBUG_UTIL_VEC(f);
  f.attr("levels") = no_na_new_levels;
  f.attr("class") = "factor";
  DEBUG_UTIL("max(f) " << max(f));
  DEBUG_UTIL("f.size() " << f.size());
  DEBUG_UTIL("final fi = " << fi);
  DEBUG_UTIL_VEC(f);
  if (fi == fsz) return(f);
  DEBUG_UTIL("copying f to shorten since NAs may have been dropped");
  IntegerVector f_no_na(f.begin(), f.begin() + fi);
  f_no_na.attr("levels") = no_na_new_levels;
  f_no_na.attr("class") = "factor";
  DEBUG_UTIL_VEC(f_no_na);
  return(f_no_na);
}
