#include <Rcpp.h>
#include "is.h"
#include "manip.h"
#include "util.h"
#include "convert.h"
#include "../inst/include/icd.h"
#include "config.h"
#ifdef HAVE_TESTTHAT_H
#include <testthat.h>

context("C++ Catch") {
  test_that("two plus two is four") {
    int result = 2 + 2;
    expect_true(result == 4);
  }
}

context("internal 'is' functions") {
  test_that("is n") {
    std::vector<std::string> v;
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
    std::vector<std::string> v;
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
    std::vector<std::string> v;
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

context("get OMP max threads") {
  test_that("max threads give a semi-sensible number") {
    int i = getOmpMaxThreads();
    expect_true(i >= 0);
    debug_parallel();
  }
}

context("get OMP current threads") {
  test_that("threads give a semi-sensible number") {
    int i = getOmpThreads();
    expect_true(i >= 0);
    debug_parallel();
  }
}

context("icd9ShortToPartsCpp") {
  test_that("icd9ShortToPartsCpp gives NA value") {
    Rcpp::List out = icd9ShortToPartsCpp("E12345678", "");

    Rcpp::CharacterVector j = out["major"];
    Rcpp::CharacterVector n = out["minor"];

    expect_true(Rcpp::CharacterVector::is_na(j[0]));
    expect_true(Rcpp::CharacterVector::is_na(n[0]));

  }

  test_that("icd9ShortToPartsCpp multiple inptus gives multiple NA values") {
    Rcpp::CharacterVector cv = Rcpp::CharacterVector::create("E3417824921",
                                                             "E375801347",
                                                             "E8319473422");
    Rcpp::List out = icd9ShortToPartsCpp(cv, "");

    Rcpp::CharacterVector j = out["major"];
    Rcpp::CharacterVector n = out["minor"];

    expect_true(Rcpp::CharacterVector::is_na(j[0]));
    expect_true(Rcpp::CharacterVector::is_na(n[0]));
    expect_true(Rcpp::CharacterVector::is_na(j[1]));
    expect_true(Rcpp::CharacterVector::is_na(n[1]));
    expect_true(Rcpp::CharacterVector::is_na(j[2]));
    expect_true(Rcpp::CharacterVector::is_na(n[2]));

  }
}

context("parallel debug") {
  test_that("debug parallel runs without error") {
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
    std::vector<std::string> n = icd9RandomShortN(1);
    expect_true(n.size() == 1);
    expect_true(n[0].substr(0, 1) != "E");
    expect_true(n[0].substr(0, 1) != "V");

    n = icd9RandomShortN(999);
    expect_true(n.size() == 999);
  }

  test_that("random V codes") {
    std::vector<std::string> v = icd9RandomShortV(1);
    expect_true(v.size() == 1);
    expect_true(v[0].substr(0, 1) == "V");

    v = icd9RandomShortV(999);
    expect_true(v.size() == 999);
    expect_true(v[998].substr(0, 1) == "V");
    expect_true(v[0].size() >= 3);
    expect_true(v[0].size() <= 5);

  }

  test_that("random E codes") {
    std::vector<std::string> e = icd9RandomShortE(1);
    expect_true(e.size() == 1);
    expect_true(e[0].substr(0, 1) == "E");

    e = icd9RandomShortE(999);
    expect_true(e.size() == 999);
    expect_true(e[998].substr(0, 1) == "E");
    expect_true(e[0].size() >= 4);
    expect_true(e[0].size() <= 5);
  }

  test_that("random any code") {
    std::vector<std::string> c = icd9RandomShort(1);
    expect_true(c.size() == 1);
    expect_true(c[0].size() >= 3);
    expect_true(c[0].size() <= 5);
  }
}

context("valgrind hooks") {
  test_that("start callgrind") {
    int i = valgrindCallgrindStart(false);
    expect_true(i == 0);
  }

  test_that("stop callgrind") {
    int i = valgrindCallgrindStop();
    expect_true(i == 0);
  }
}

context("fast int to string") {
  test_that("Rcpp version works") {
    Rcpp::IntegerVector iv;
    iv = Rcpp::IntegerVector::create(1);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "1");

    iv = Rcpp::IntegerVector::create(9);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "9");

    iv = Rcpp::IntegerVector::create(123456);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "123456");

    iv = Rcpp::IntegerVector::create(2, 33, 444, 5555, 66666, 123456);
    Rcpp::CharacterVector cv = Rcpp::CharacterVector::create("2", "33", "444", "5555", "66666", "123456");
    expect_true(Rcpp::is_true(Rcpp::all(fastIntToStringRcpp(iv) == cv)));

  }

  // duplicated code...
  test_that("std version works") {
    std::vector<int> iv;
    iv.push_back(1);
    std::vector<std::string> cv;
    cv.push_back("1");
    expect_true(fastIntToStringStd(iv) == cv);

    iv[0] = 9;
    cv[0] = "9";
    expect_true(fastIntToStringStd(iv) == cv);

    iv[0] = 123456;
    cv[0] = "123456";
    expect_true(fastIntToStringStd(iv) == cv);

    iv.push_back(2);
    iv.push_back(33);
    iv.push_back(444);
    iv.push_back(5555);
    iv.push_back(66666);
    cv.push_back("2");
    cv.push_back("33");
    cv.push_back("444");
    cv.push_back("5555");
    cv.push_back("66666");
    expect_true(fastIntToStringStd(iv) == cv);
  }

}

#ifdef ICD_DEBUG
context("MajMin to code") {
  test_that("differing lengths gives error") {
    Rcpp::CharacterVector mj = Rcpp::CharacterVector::create("100");
    Rcpp::CharacterVector mn = Rcpp::CharacterVector::create("01");
    Rcpp::CharacterVector rs = Rcpp::CharacterVector::create("10001");
    expect_true(Rcpp::is_true(Rcpp::all(icd9MajMinToCode(mj, mn, true) == rs)));

    mj = Rcpp::CharacterVector::create("101", "102");
    mn = Rcpp::CharacterVector::create("01", "02");
    rs = Rcpp::CharacterVector::create("10101", "10202");
    expect_true(Rcpp::is_true(Rcpp::all(icd9MajMinToCode(mj, mn, true) == rs)));

    mj = Rcpp::CharacterVector::create("100");
    mn = Rcpp::CharacterVector::create("01", "02");
    expect_error(icd9MajMinToCode(mj, mn, true));

    mn = Rcpp::CharacterVector::create("01", "02");
    mj = Rcpp::CharacterVector::create("100", "101", "102");
    expect_error(icd9MajMinToCode(mj, mn, true));
  }
}
#endif

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

// endif have testthat
#endif
