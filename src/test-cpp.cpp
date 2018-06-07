#include "config.h"
#include "icd_types.h"
#include "local.h"
#include "is.h"
#include "manip.h"
#include "util.h"
#include "appendMinor.h"
#include "convert.h"

// nocov start
// because covr seems to misinterpret contents of context sections
#ifdef ICD_CATCH
#include <testthat.h>
using namespace Rcpp;
context("internal 'is' functions") {
  test_that("is n") {
    VecStr v;
    v.push_back("10");
    v.push_back("0");
    v.push_back("10.");
    v.push_back("10.9");
    v.push_back("100.99");
    v.push_back("9999");
    v.push_back("1");
    std::vector<bool> result = icd9_is_n_cpp(v);
    std::vector<bool> result2 = icd9_is_v_cpp(v);
    std::vector<bool> result3 = icd9_is_e_cpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }
  test_that("is v") {
    VecStr v;
    v.push_back("V10");
    v.push_back("V0");
    v.push_back("V10.");
    v.push_back("V10.9");
    v.push_back("V99.99");
    v.push_back("V9999");
    v.push_back("v1");
    std::vector<bool> result2 = icd9_is_n_cpp(v);
    std::vector<bool> result = icd9_is_v_cpp(v);
    std::vector<bool> result3 = icd9_is_e_cpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }
  test_that("is e basic") {
    VecStr v;
    v.push_back("E999");
    v.push_back("E0");
    v.push_back("E999.");
    v.push_back("E999.9");
    v.push_back("E9999");
    v.push_back("e000");
    std::vector<bool> result3 = icd9_is_n_cpp(v);
    std::vector<bool> result2 = icd9_is_v_cpp(v);
    std::vector<bool> result = icd9_is_e_cpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }
}

context("icd9ShortToPartsCpp") {
  test_that("icd9ShortToPartsCpp gives NA value") {
    Rcpp::List out = icd9ShortToPartsCpp("E12345678", "");
    CV j = out["mjr"];
    CV n = out["mnr"];
    expect_true(CV::is_na(j[0]));
    expect_true(CV::is_na(n[0]));
  }
  test_that("icd9ShortToPartsCpp multiple inptus gives multiple NA values") {
    CV cv = CV::create("E3417824921",
                       "E375801347",
                       "E8319473422");
    Rcpp::List out = icd9ShortToPartsCpp(cv, "");
    CV j = out["mjr"];
    CV n = out["mnr"];
    expect_true(CV::is_na(j[0]));
    expect_true(CV::is_na(n[0]));
    expect_true(CV::is_na(j[1]));
    expect_true(CV::is_na(n[1]));
    expect_true(CV::is_na(j[2]));
    expect_true(CV::is_na(n[2]));
  }
}

context("parallel debug") {
  test_that("debug parallel runs without error") {
    debug_parallel_env();
    debug_parallel();
    expect_true(true);
  }
}

context("random data") {
  test_that("random numeric vector of major codes is ok") {
    Rcpp::NumericVector nv = randomMajorCpp(1);
    // minimal test:
    expect_true(nv.size() == 1);
    nv = randomMajorCpp(10);
    expect_true(nv.size() == 10);
    nv = randomMajorCpp(10000);
    expect_true(nv.size() == 10000);
    Rcpp::NumericVector v = randomMajorCpp(10);
    expect_true(v.size() == 10);
    bool gt_zero = is_true(Rcpp::all(v > 0));
    bool lt_thousand = Rcpp::is_true(Rcpp::all(v < 1000));
    expect_true(gt_zero);
    expect_true(lt_thousand);
  }
  test_that("random N codes") {
    VecStr n = icd9RandomShortN(1);
    expect_true(n.size() == 1);
    expect_true(n[0].substr(0, 1) != "E");
    expect_true(n[0].substr(0, 1) != "V");
    n = icd9RandomShortN(999);
    expect_true(n.size() == 999);
  }
  test_that("random V codes") {
    VecStr v = icd9RandomShortV(1);
    expect_true(v.size() == 1);
    expect_true(v[0].substr(0, 1) == "V");
    v = icd9RandomShortV(999);
    expect_true(v.size() == 999);
    expect_true(v[998].substr(0, 1) == "V");
    expect_true(v[0].size() >= 3);
    expect_true(v[0].size() <= 5);
  }
  test_that("random E codes") {
    VecStr e = icd9RandomShortE(1);
    expect_true(e.size() == 1);
    expect_true(e[0].substr(0, 1) == "E");
    e = icd9RandomShortE(999);
    expect_true(e.size() == 999);
    expect_true(e[998].substr(0, 1) == "E");
    expect_true(e[0].size() >= 4);
    expect_true(e[0].size() <= 5);
  }
  test_that("random any code") {
    auto c = icd9RandomShort(1);
    expect_true(c.size() == 1);
    expect_true(c[0].size() >= 3);
    expect_true(c[0].size() <= 5);
  }
}

context("MajMin to code") {
  test_that("differing lengths gives error") {
    CV mj = CV::create("100");
    CV mn = CV::create("01");
    CV rs = CV::create("10001");
    expect_true(Rcpp::is_true(Rcpp::all(icd9MajMinToCode(mj, mn, true) == rs)));
    mj = CV::create("101", "102");
    mn = CV::create("01", "02");
    rs = CV::create("10101", "10202");
    expect_true(Rcpp::is_true(Rcpp::all(icd9MajMinToCode(mj, mn, true) == rs)));
    mj = CV::create("100");
    mn = CV::create("01", "02");
    //expect_error(icd9MajMinToCode(mj, mn, true));
    mn = CV::create("01", "02");
    mj = CV::create("100", "101", "102");
    //expect_error(icd9MajMinToCode(mj, mn, true));
  }
}

context("add leading zeroes to major") {
  test_that("when major len is 0, result is empty") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("") == "");
  }
  test_that("E code with two char major works") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("E1") == "E001");
  }
  test_that("E code with three char major works") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("E01") == "E001");
  }
}

context("refactor") {
  IntegerVector f;
  IntegerVector f2;
  CV new_levels;
  CV empty_levels(0);
  CV a_level = CV::create("a");
  CV b_level = CV::create("b");
  CV j;
  IntegerVector res;
  test_that("empty factor levels") {
    f = {1};
    f.attr("levels") = "old_level";
    f.attr("class") = "factor";
    res = refactor(f, empty_levels);
    expect_true(IntegerVector::is_na(res[0]));
    j = res.attr("levels");
    expect_true(j.size() == 0);
  }
  test_that("empty factor levels") {
    f = {1};
    f.attr("levels") = empty_levels;
    f.attr("class") = "factor";
    res = refactor(f, empty_levels);
    expect_true(IntegerVector::is_na(res[0]));
    j = res.attr("levels");
    expect_true(j.size() == 0);
  }
  test_that("NA with no NA factor levels") {
    f = { NA_INTEGER };
    f.attr("levels") = a_level;
    f.attr("class") = "factor";
    res = refactor(f, a_level, true);
    expect_true(IntegerVector::is_na(res[0]));
    j = res.attr("levels");
    expect_true(is_true(all(j == a_level)));
  }
  test_that("2 NAs, no NA factor levels") {
    f = { NA_INTEGER, NA_INTEGER };
    f.attr("levels") = a_level;
    f.attr("class") = "factor";
    res = refactor(f, b_level, true);
    expect_true(IntegerVector::is_na(res[0]));
    expect_true(IntegerVector::is_na(res[1]));
    j = res.attr("levels");
    expect_true(is_true(all(j == b_level)));
  }
  test_that("2 NAs, with NA factor levels") {
    f = { NA_INTEGER, NA_INTEGER };
    f.attr("levels") = CV::create("b", "c");
    f.attr("class") = "factor";
    new_levels= CV::create("a", "NA_STRING");
    new_levels[1] = NA_STRING;
    res = refactor(f, new_levels, false);
    expect_false(IntegerVector::is_na(res[0]));
    expect_false(IntegerVector::is_na(res[1]));
    expect_true(is_true(all(res == 2)));
    j = res.attr("levels");
    DEBUG_VEC(j);
    expect_true(CV::is_na(j[1]));
  }
  /* // skipping because not implemented or needed yet.
  test_that("don't drop NA values and levels") {
    f = { NA_INTEGER, 1, 3, NA_INTEGER };
    new_levels = CV::create("a", "c", "NA_STRING");
    new_levels[2] = NA_STRING;
    f.attr("levels") = new_levels;
    f.attr("class") = "factor";
    new_levels= CV::create("a", "NA_STRING");
    new_levels[1] = NA_STRING;
    res = refactor(f, new_levels, false);
    expect_false(IntegerVector::is_na(res[0]));
    expect_false(IntegerVector::is_na(res[1]));
    expect_false(IntegerVector::is_na(res[2]));
    IntegerVector expected_vec = IntegerVector::create(2, 1, 2, 2);
    expect_true(is_true(all(res == expected_vec)));
    j = res.attr("levels");
    DEBUG_VEC(j);
    expect_true(CV::is_na(j[1]));
  } */
  test_that("do drop NA values and levels") {
    f = { NA_INTEGER, 1, 3, NA_INTEGER };
    CV old_levels = CV::create("a", "c", "NA_STRING");
    old_levels[2] = NA_STRING;
    f.attr("levels") = old_levels;
    f.attr("class") = "factor";
    new_levels= CV::create("a", "NA_STRING");
    new_levels[1] = NA_STRING;
    res = refactor_narm(f, new_levels);
    expect_false(IntegerVector::is_na(res[0]));
    IntegerVector expected_vec = { 1 };
    DEBUG_VEC(res);
    expect_true(is_true(all(res == expected_vec)));
    j = res.attr("levels");
    DEBUG_VEC(j);
    expect_false(CV::is_na(j[0]));
    expect_true(is_true(all(j == CV::create("a"))));
  }
  test_that("new NA level") {
    f = { NA_INTEGER };
    f.attr("levels") = CV::create("a");
    f.attr("class") = "factor";
    CV na_level(1);
    na_level[0] = NA_STRING;
    res = refactor(f, na_level, false);
    expect_true(res[0] == 1);
  }
  test_that("factor self works") {
    f = {1, 2};
    new_levels = CV::create("a", "b");
    f.attr("levels") = new_levels;
    f.attr("class") = "factor";
    res = refactor(f, new_levels);
    expect_true(is_true(all(f == res)));
    bool levels_equal = is_true(all(new_levels == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor simple re-order") {
    f = {1, 2};
    f.attr("levels") = CV::create("a", "b");
    f.attr("class") = "factor";
    new_levels = CV::create("b", "a");
    f2 = {2, 1};
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    res = refactor(f, new_levels);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal = is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order") {
    f = {1, 3, 2};
    f.attr("levels") = CV::create("a", "b", "c");
    f.attr("class") = "factor";
    f2 = {3, 1, 2};
    new_levels = CV::create("c", "b", "a");
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    res = refactor(f, new_levels);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal = is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order w extra") {
    f = {1, 3, 2};
    f.attr("levels") = CV::create("a", "b", "c", "d");
    f.attr("class") = "factor";
    f2 = {3, 1, 2};
    new_levels = CV::create("c", "b", "a");
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    res = refactor(f, new_levels);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal = is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order w extra and missing") {
    f = {1, 3, 2};
    f.attr("levels") = CV::create("a", "b", "c", "d");
    f.attr("class") = "factor";
    f2 = {NA_INTEGER, 1, 2};
    new_levels = CV::create("c", "b", "e");
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    res = refactor(f, new_levels, false); // keep NA values (but not in levels)
    DEBUG_VEC(res);
    expect_true(res.size() == 3);
    expect_true(res[0] == f2[0]);
    expect_true(res[1] == f2[1]);
    expect_true(res[2] == f2[2]);
    expect_true(IntegerVector::is_na(res[0]));
    expect_false(IntegerVector::is_na(res[1]));
    expect_false(IntegerVector::is_na(res[2]));
    bool levels_equal = is_true(all(new_levels == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
    std::string cl = f2.attr("class");
    std::string factor_str = "factor";
    expect_true(cl == factor_str);
    // expect_true(is_true(all(res == f2))); // doesn't work because NA = NA is NA, not true
  }
  test_that("dropping NAs makes vector shorter") {
    f = { 1, 2 };
    f.attr("levels") = CV::create("a", "b", "c", "d");
    f.attr("class") = "factor";
    f2 = { 1 };
    new_levels = CV::create("c", "b", "e");
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    res = refactor_narm(f, new_levels);
    DEBUG_VEC(res);
    expect_false(IntegerVector::is_na(res[0]));
    expect_true(res[0] == 2);
    expect_true(res.size() == 1);
    expect_true(is_true(all(((CV) res.attr("levels")) == new_levels)));
  }
}
// endif have testthat ICD_CATCH
#endif
// nocov end
