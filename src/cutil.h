/*
 * util.h
 *
 *  Created on: Mar 7, 2015
 *      Author: jack
 */

#ifndef CUTIL_H_
#define CUTIL_H_

#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP getRListOrDfElement(SEXP list, const char *str);

#endif // CUTIL_H_
